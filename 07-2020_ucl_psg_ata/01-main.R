
# Data source: https://data.world/sportsvizsunday/2020-september-champions-league-shots
# https://twitter.com/lastrowview/status/1300122691526156292

library(tidyverse)
file <- 'ucl_2020_psg_atl'
dir_proj <- '07-2020_ucl_psg_ata'
path_export_gif <- fs::path(dir_proj, sprintf('%s.gif', file))
dir_data <- fs::path(dir_proj, 'data')
path_export_pc <- fs::path(dir_data, sprintf('%s_pc.rds', file))
fs::dir_create(dir_data)
play_filt <- 'PSG [2]-1 Atalanta'
fps <- 25
# pitch_fill <- '#7fc47f'
# pitch_color <- 'white'
pitch_fill <- 'white'
pitch_color <- 'black'
pitch <-
  ggsoccer::annotate_pitch(
    fill = pitch_fill, 
    colour = pitch_color,
    limits = FALSE
  )
xlim_pitch <- c(66, 101)
ylim_pitch <- c(112, -12) # flipped this, relative to the ggsoccer readme

# events <- here::here('data-raw', '07', 'CL2020_final8_events.csv') %>% read_csv()
# events
tracking <- 
  here::here('data-raw', '07', 'CL2020_final8_tracking.csv') %>% 
  read_csv() %>% 
  select(-1)
tracking

frames <- 
  tracking %>% 
  filter(play == play_filt) %>% 
  mutate(
    team = case_when(is.na(bgcolor) ~ NA_character_, bgcolor == 'white' ~  'home', TRUE ~ 'away'),
    time = frame / fps
  ) %>% 
  select(frame, time, player, player_num, team, edgecolor, bgcolor, x, y) %>% 
  group_by(player) %>% 
  mutate(
    across(c(x, y, time), ~dplyr::lead(.x, 1L), .names = 'next_{col}'),
    across(c(x, y), ~dplyr::lead(.x, 10L), .names = 'forward_{col}')
  ) %>% 
  ungroup()
frames

frames_ball <- frames %>% filter(player == 0)
frames_players <- frames %>% anti_join(frames_ball)

pitch_gg <- function(...) {
  list(
    pitch,
    coord_flip(
      xlim = xlim_pitch,
      ylim = ylim_pitch
    ),
    ggsoccer::theme_pitch(aspect_ratio = 0.5), # changing the default aspect ratio cuz it looks weird
    theme(legend.position = 'none')
  )
}

frames_redux <- 
  frames_ball %>% 
  select(time, frame, ball_x = x, ball_y = y) %>% 
  inner_join(frames_players)
frames_redux

if(!fs::file_exists(path_export_pc)) {

  seq_pitch_dim <- function(dim = c('x', 'y'), length.out = 100L) {
    dim <- match.arg(dim)
    name_origin <- sprintf('origin_%s', dim)
    name_length <- switch(dim, x = 'length', y = 'width')
    start <- ggsoccer::pitch_opta[[name_origin]]
    length <- ggsoccer::pitch_opta[[name_length]]
    seq.int(start, start + length, length.out = length.out)
  }
  
  pitch_area <-
    crossing(
      x = seq_pitch_dim('x'),
      y = seq_pitch_dim('y')
    )
  pitch_area
  
  pitch_area_filt <-
    pitch_area %>% 
    filter(between(x, xlim_pitch[1], xlim_pitch[2])) %>% 
    filter(between(y, ylim_pitch[2], ylim_pitch[1]))
  pitch_area_filt
  
  get_speed <- function(coord, next_coord, time, next_time) {
    (next_coord - coord) / (next_time - time)
  }
  
  get_theta <- function(x_speed, y_speed) {
    hypotenuse_speed <- sqrt(x_speed^2 + y_speed^2)
    acos(x_speed / hypotenuse_speed)
  }
  
  get_mu <- function(location, speed) {
    location + speed / 2
  }
  
  get_srat <- function(speed_x, speed_y) {
    speed <- sqrt(speed_x^2 + abs(speed_y)^2)
    (speed / 13)^2
  }
  
  get_ri <- function(x, y, ball_x, ball_y) {
    ball_diff <- sqrt((x - ball_x) ^ 2 + (y - ball_y)^2)
    ri <- 4 + ((ball_diff^3) / ((18^3) / 6))
    min(ri, 10)
  }
  
  get_R <- function(theta) {
    matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow = 2)
  }
  
  get_S <- function(ri, srat) {
    top_left <- ri * (1 + srat) / 2
    bottom_right <- ri * (1-srat) / 2
    matrix(c(top_left, 0, 0, bottom_right), nrow = 2)
  }
  
  get_Sigma <- function(R, S) {
    inv_R <- solve(R)
    R %*% S %*% S %*% inv_R
  }
  
  calculate_I <- function(pitch_area, x, y, mu_x, mu_y, Sigma) {
    mu <- c(mu_x, mu_y)
    player_loc <- c(x, y)
    num <- mvtnorm::dmvnorm(as.matrix(pitch_area), mu, Sigma)
    den <- mvtnorm::dmvnorm(t(matrix(player_loc)), mu, Sigma)
    num / den
  }
  
  dir_data <- here::here('data', '07')
  fs::dir_create(dir_data)
  calculate_pitch_control <- 
    function(time, next_time, ball_x, ball_y, x, y, next_x, next_y, team, player, 
             pitch_area, 
             ..., 
             dir = dir_data, 
             basename = glue::glue('pc_{sprintf("%.02f", time)}_{sprintf("%04d", player)}.rds'), 
             path = fs::path(dir_data, basename), 
             overwrite = FALSE) {

      path_exists <- fs::file_exists(path)
      if(path_exists & !overwrite) {
        return(read_rds(path))
      }
      speed_x <- get_speed(x, next_x, time, next_time)
      speed_y <- get_speed(y, next_y, time, next_time)
      srat <- get_srat(speed_x, speed_y)
      theta <- get_theta(speed_x, speed_y)
      
      mu_x <- get_mu(x, speed_x)
      mu_y <- get_mu(y, speed_y)
      
      ri <- get_ri(x, y, ball_x, ball_y)
      
      R <- get_R(theta)
      S <- get_S(ri, srat)
      
      Sigma <- get_Sigma(R, S)
      
      pitch_area$I <- calculate_I(as.matrix(pitch_area), x, y, mu_x, mu_y, Sigma)
      write_rds(pitch_area, path)
      pitch_area
    }
  f <- partial(calculate_pitch_control, pitch_area = pitch_area, overwrite = TRUE, ... = )
  pc <- 
    frames_redux %>%
    mutate(
      data = 
        pmap(
          list(
            time = time, 
            next_time = next_time, 
            ball_x = ball_x, 
            ball_y = ball_y, 
            x = x, 
            y = y, 
            next_x = next_x, 
            next_y = next_y, 
            team = team, 
            player = player
          ), 
          f
        )
    )
  
  pc_agg <-
    pc %>%
    select(frame, time, player, team, data) %>% 
    unnest(data) %>% 
    group_by(frame, time, team, x, y) %>%
    summarise(team_sum = sum(I, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = team, values_from = team_sum) %>%
    mutate(pc = 1 / (1 + exp(home - away)))
  pc_agg
  
  write_rds(pc_agg, path_export_pc)
} else {
  pc_agg <- read_rds(path_export_pc)
}

color_low <- '#4E79A7'
color_high <- '#F28E2B'
arw_v <- arrow(length = unit(3, 'pt'), type = 'closed')

# Use this while experimenting with plotting settings.
.filter_frames <- function(data) {
  data %>% 
    # mutate(is_tenth = frame %% 20 == 0) %>% 
    # filter(is_tenth)
    filter(frame >= 5) # First handful of frames look weird.
    # filter(frame == 5)
}

update_geom_defaults('text', list(family = 'Karla', size = 4))
viz <-
  pc_agg %>%
  .filter_frames() %>% 
  ggplot() +
  aes(x = x, y = y) +
  pitch_gg() +
  geom_tile(aes(x = x, y = y, fill = pc), alpha = 0.7) +
  scale_fill_gradient2(low = color_low, high = color_high, mid = 'white', midpoint = 0.5) +
  geom_segment(
    data = frames_redux %>% .filter_frames() %>% filter(!is.na(forward_x)),
    color = 'black',
    size = 1.5, 
    arrow = arw_v,
    aes(x = x, y = y, xend = forward_x, yend = forward_y)
  ) +
  ggnewscale::new_scale_fill() +
  geom_point(
    data = frames_players %>% .filter_frames(),
    color = 'black',
    aes(fill = team),
    size = 7,
    alpha = 0.8,
    stroke = 1,
    shape = 21
  ) + 
  scale_fill_manual(values = c('away' = color_high, 'home' = color_low)) +
  geom_point(
    data = frames_ball %>% .filter_frames(),
    size = 3,
    color = 'black', 
    fill = 'black'
  ) +
  geom_text(
    data = frames_players %>% .filter_frames() %>% filter(!is.na(player_num)),
    aes(label = player_num),
    fontface = 'bold',
    color = 'black'
  ) +
  gganimate::transition_time(frame) +
  # facet_wrap(~frame) + # for dev +
  theme(
    plot.title = element_text('Karla', face = 'bold', size = 18, color = 'gray20'),
    plot.title.position = 'plot',
    plot.margin = margin(20, 10, 10, 10),
    plot.caption = element_text('Karla', size = 14, color = 'gray20', hjust = 0),
    plot.caption.position = 'plot'
  ) +
  labs(
    title = 'PSG [2]-1 Atalanta, UCL 2020 Quarter-Finals',
    caption = 'Viz: @TonyElHabr | Data: @lastrowview'
  )
# viz

n_frame <- frames_players %>% .filter_frames() %>% pull(frame) %>% max()
viz_anim <- gganimate::animate(viz, width = 900, height = 600, nframes = n_frame, fps = fps)
gganimate::anim_save(animation = viz_anim, path_export_gif)

       