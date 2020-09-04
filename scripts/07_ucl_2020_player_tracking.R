
# Data source: https://data.world/sportsvizsunday/2020-september-champions-league-shots
# setup ----
library(tidyverse)
events <- here::here('data-raw', '07', 'CL2020_final8_events.csv') %>% read_csv()
events

tracking <- 
  here::here('data-raw', '07', 'CL2020_final8_tracking.csv') %>% 
  read_csv() %>% 
  select(-1)
tracking
tracking %>% count(frame)
tracking %>% count(team)
tracking %>% count(play)

# filtering ----
play_filt <- 'PSG [2]-1 Atalanta'
# play_filt <- 'Manchester City [1]-1 Lyon'
events_filt <- events %>% filter(play == play_filt)

events_filt %>% 
  ggplot() +
  aes(x = from_x, y = from_y, group = from_team) +
  geom_path(aes(xend = to_x, yend = to_y)) +
  geom_label(aes(label = id))


tracking_filt <- tracking %>% filter(play == play_filt)
tracking_filt

# Period: ?
# frame: ferame
# Time: ?
# Player: name-num-player_ojb?
# x: x
# y: y
# Team: team
# is_gk: ?
# Second: ?

tracking_filt %>% count(frame)
players <- tracking_filt %>% count(player)
players

tracking_viz <-
  tracking_filt %>% 
  # filter(!is.na(player_num)) %>% 
  # mutate(
  #   lab = sprintf('%s%s', player, ifelse(is.na(player_num), '', paste0('-', player_num)))
  # ) %>% 
  # filter((frame / 10) == 0) %>% 
  mutate(lab = player_num) %>% 
  mutate(is_tenth = frame %% 10 == 0) %>% 
  filter(is_tenth) %>% 
  select(-is_tenth)
tracking_viz

pitch_fill <- '#7fc47f'
pitch_color <- 'white'
pitch <- 
  soccerAnimate:::get_pitch(
    pitch_fill = pitch_fill, 
    pitch_col = pitch_color
  )
pitch

viz <-
  pitch +
  geom_point(
    data = tracking_viz,
    aes(x = x, y = y, fill = team),
    inherit.aes = TRUE,
    col = 'black', shape = 21, stroke = 1, alpha = 0.8
  ) +
  geom_text(
    data = tracking_viz %>% filter(!is.na(player)),
    aes(x = x, y = y, label = lab),
    color = 'black'
  ) +
  facet_wrap(~frame) +
  # transition_time(frame) +
  theme(legend.position = 'none')
viz

pitch <-
  ggsoccer::annotate_pitch(
    fill = pitch_fill, 
    colour = pitch_color,
    limits = FALSE
  )
pitch

viz <-
  tracking_viz %>% 
  ggplot() +
  aes(x = x, y = y) +
  pitch +
  coord_flip(
    xlim = c(49, 101),
    ylim = c(-12, 112)
  ) +
  geom_point(
    data = tracking_viz,
    aes(x = x, y = y, fill = team),
    inherit.aes = TRUE,
    col = 'black', shape = 21, stroke = 1, alpha = 0.8
  ) +
  geom_path(
    aes(group = player)
  ) +
  geom_text(
    data = tracking_viz %>% filter(!is.na(player)),
    aes(x = x, y = y, label = lab),
    color = 'black'
  ) +
  # facet_wrap(~frame) +
  # transition_time(frame) +
  ggsoccer::theme_pitch() +
  theme(legend.position = 'none')
viz
# tracking_filt %>% ggplot() + geom_histogram(aes(x = x))





