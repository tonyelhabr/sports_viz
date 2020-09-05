
# Data source: https://data.world/sportsvizsunday/2020-september-champions-league-shots
# https://twitter.com/lastrowview/status/1300122691526156292

library(tidyverse)
file <- 'ucl_2020_psg_atl'
path_export_gif <- here::here('plots', sprintf('%s.gif', file))
path_export_pc <- here::here('data', sprintf('07_%s_pc.rds', file))
play_filt <- 'PSG [2]-1 Atalanta'
fps <- 25
pitch_fill <- '#7fc47f'
pitch_color <- 'white'
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

if(FALSE) {
  viz <-
    frames_players %>% 
    filter(frame == 0) %>% 
    ggplot() +
    aes(x = x, y = y) +
    pitch_gg() +
    # pitch +
    # coord_flip(
    #   xlim = xlim_pitch,
    #   ylim = ylim_pitch
    # ) +
    # scale_y_reverse() + 
    geom_point(
      aes(color = edgecolor, fill = bgcolor),
      size = 7,
      alpha = 0.8,
      stroke = 1,
      shape = 21
    ) + 
    scale_color_identity() +
    scale_fill_identity() +
    geom_point(
      data = frames_ball %>% filter(frame == 0),
      size = 3,
      color = 'black', 
      fill = 'black'
    ) +
    # gganimate::transition_time(frame) +
    # geom_path(aes(group = player)) + # for dev (no gganimate)
    # facet_wrap(~frame) + # for dev
    geom_text(
      data = . %>% filter(!is.na(player_num)),
      aes(label = player_num),
      size = 4.5,
      color = 'black'
    )
  viz
  n_frame <- frames %>% pull(frame) %>% max()
  viz_anim <- gganimate::animate(viz, width = 900, height = 600, nframes = n_frame, fps = fps)
  gganimate::anim_save(animation = viz_anim, path_export_gif)
  
}

if(!fs::file_exists(path_export_pc)) {

  frames_redux <- 
    frames_ball %>% 
    select(time, frame, ball_x = x, ball_y = y) %>% 
    inner_join(frames_players)
  frames_redux
  
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
             # basename = sprintf('pc_%03d_%s_%s.rds', time * 100, player, team),
             path = fs::path(dir_data, basename), 
             overwrite = FALSE) {
      # browser()
      # basename = glue::glue('pc_{sprintf("%03d", time * 100)}_{player}_{team}.rds')
      # path = fs::path(dir_data, basename)
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
      pitch_area$team <- team
      pitch_area$time <- time
      pitch_area$player <- player
      write_rds(pitch_area, path)
      pitch_area
    }
  f <- partial(calculate_pitch_control, pitch_area = pitch_area_filt, ... = )
  pc <- 
    frames_redux %>%
    # slice(1) %>% 
    mutate(
      data = 
        pmap(
          list(
            # pitch_area = !!pitch_area_filt
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
    select(frame, data) %>% 
    unnest(data) %>% 
    group_by(frame, time, team, x, y) %>%
    summarise(team_sum = sum(I, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = team, values_from = team_sum) %>%
    mutate(pc = 1 / (1 + exp(home - away)))
  pc_agg
  
  write_rds(pc_agg, path_export_pc)
} else {
  pc <- read_rds(path_export_pc)
}


viz_pc <-
  pc_agg %>%
  # filter(frame == max(frame)) %>% 
  mutate(is_tenth = frame %% 10 == 0) %>% 
  filter(is_tenth) %>% 
  ggplot() +
  aes(x = x, y = y) +
  pitch_gg() +
  geom_tile(aes(x = x, y = y, fill = pc), alpha = 0.7) +
  scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white', midpoint = 0.5) +
  # geom_segment(data = testing_data, aes(x = x, y = y, xend = forward_x, yend = forward_y, colour = team)
  facet_wrap(~frame)
viz_pc
