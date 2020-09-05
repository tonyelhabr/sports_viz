
# Data source: https://data.world/sportsvizsunday/2020-september-champions-league-shots
# https://twitter.com/lastrowview/status/1300122691526156292

library(tidyverse)
path_export_gif <- here::here('plots', 'ucl_2020_psg_atl.gif')
path_export_pc <- path_export_gif %>% str_replace('[.]gif', '_pc.rds')
events <- here::here('data-raw', '07', 'CL2020_final8_events.csv') %>% read_csv()
events

tracking <- 
  here::here('data-raw', '07', 'CL2020_final8_tracking.csv') %>% 
  read_csv() %>% 
  select(-1)
tracking
# tracking %>% count(frame)
# tracking %>% count(team)
# tracking %>% count(play)

play_filt <- 'PSG [2]-1 Atalanta'
# play_filt <- 'Manchester City [1]-1 Lyon'
events_filt <- events %>% filter(play == play_filt)
tracking_filt <- tracking %>% filter(play == play_filt)

# tracking_filt %>% count(frame)
# frames <-
#   tracking_filt %>% 
#   mutate(is_tenth = frame %% 10 == 0) %>% 
#   filter(is_tenth) %>% 
#   select(-is_tenth)
# frames
frames <- tracking_filt

pitch_fill <- '#7fc47f'
pitch_color <- 'white'
pitch <-
  ggsoccer::annotate_pitch(
    fill = pitch_fill, 
    colour = pitch_color,
    limits = FALSE
  )


frames_ball <- frames %>% filter(player == 0)
frames_players <- frames %>% anti_join(frames_ball)

fps <- 25

if(FALSE) {
viz <-
  frames_players %>% 
  ggplot() +
  aes(x = x, y = y) +
  pitch +
  coord_flip(
    xlim = c(66, 101),
    ylim = c(112, -12) # flipped this, relative to the ggsoccer readme
  ) +
  # scale_y_reverse() + 
  geom_point(
    data = frames_players, # not the ball
    aes(color = edgecolor, fill = bgcolor),
    size = 7,
    alpha = 0.8,
    stroke = 1,
    shape = 21
  ) +
  geom_point(
    data = frames_ball, # the ball
    size = 3,
    color = 'black', 
    fill = 'black'
  ) +
  gganimate::transition_time(frame) +
  scale_color_identity() +
  scale_fill_identity() +
  # geom_path(aes(group = player)) + # for dev (no gganimate)
  geom_text(
    data = frames_players %>% filter(!is.na(player_num)),
    aes(label = player_num),
    size = 4.5,
    color = 'black'
  ) +
  # facet_wrap(~frame) + # for dev
  ggsoccer::theme_pitch(aspect_ratio = 0.5) + # chancing the default aspect ratio cuz it looks weird
  theme(legend.position = 'none')
viz
n_frame <- frames %>% pull(frame) %>% max()
n_frame

viz_anim <- gganimate::animate(viz, width = 900, height = 600, nframes = n_frame, fps = fps)
gganimate::anim_save(animation = viz_anim, path_export_gif)

}

if(!fs::file_exists(path_export_pc)) {
frames_ball_proc <-
  frames_ball %>% 
  mutate(time = frame / fps) %>% 
  select(frame, time, ball_x = x, ball_y = y)
frames_ball_proc

frames_players_proc <-
  frames_players %>% 
  mutate(
    team = if_else(bgcolor == 'white', 'home', 'away'),
    time = frame / fps
  ) %>% 
  select(frame, time, player, player_num, team, x, y) %>% 
  group_by(player) %>% 
  mutate(
    across(c(x, y, time), dplyr::lead, .names = 'next_{col}')
    # next_x = dplyr::lead(x),
    # next_y = dplyr::lead(y),
    # next_time = dplyr::lead(time)
  ) %>% 
  ungroup()
frames_players_proc

frames_proc <- frames_ball_proc %>% inner_join(frames_players_proc)
frames_proc

seq_pitch_dim <- function(dim = c('x', 'y'), length.out = 200L) {
  dim <- match.arg(dim)
  name_origin <- sprintf('origin_%s', dim)
  name_length <- switch(dim, x = 'length', y = 'width')
  start <- ggsoccer::pitch_opta[[name_origin]]
  length <- ggsoccer::pitch_opta[[name_length]]
  seq.int(start, start + length, length.out = length.out)
}

n_rect <- 200L
pitch_area <-
  crossing(
    x = seq_pitch_dim('x'),
    y = seq_pitch_dim('y')
  )
pitch_area

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
  
  numerator <- mvtnorm::dmvnorm(as.matrix(pitch_area), mu, Sigma)
  denominator <- mvtnorm::dmvnorm(t(matrix(player_loc)), mu, Sigma)

  numerator / denominator
}

calculate_pitch_control <- 
  function(time, next_time, ball_x, ball_y, x, y, next_x, next_y, team, player, pitch_area) {
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
    pitch_area
  }

pc <- 
  frames_proc %>% 
  select(time, next_time, ball_x, ball_y, x, y, next_x, next_y, team, player) %>%
  pmap_df(., calculate_pitch_control, pitch_area = pitch_area)
pc <-
  pc %>% 
  group_by(team, x, y) %>% 
  summarise(team_sum = sum(I, na.rm = TRUE)) %>%
  ungroup() %>% 
  pivot_wider(names_from = team, values_from = team_sum) %>%
  mutate(pc = 1 / (1 + exp(home - away)))
pc

write_rds(pc, path_export_pc)
} else {
  pc <- path_export_pc
}
pc %>% 
  ggplot() +
  aes(x = x, y = y) +
  pitch +
  coord_flip(
    xlim = c(66, 101),
    ylim = c(112, -12) # flipped this, relative to the ggsoccer readme
  )
