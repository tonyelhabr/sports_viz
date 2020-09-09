
library(tidyverse)
file <- 'ucl_2020_psg_bay'
path_export_gif <- here::here('plots', sprintf('%s.gif', file))
path_export_pc <- here::here('data', sprintf('07_%s_pc.rds', file))
play_filt <- 'PSG 0-[1] Bayern Munich'
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
xlim_pitch <- c(40, 101)
ylim_pitch <- c(112, -12)

events_nondribble <- 
  here::here('data-raw', '07', 'CL2020_final8_events.csv') %>% 
  read_csv() %>% 
  filter(play == play_filt) %>% 
  mutate(across(id, ~{2 * (.x + 1)}))
events_nondribble

events_dribble <- 
  events_nondribble %>% 
  mutate(
    dframe = from_frame - dplyr::lag(to_frame, default = 0),
    dx = coalesce(from_x - dplyr::lag(to_x), 0),
    dy = coalesce(from_y - dplyr::lag(to_y), 0)
  ) %>% 
  mutate(
    to_frame = from_frame,
    from_frame = from_frame - dframe, 
    to_x = from_x,
    from_x = from_x - dx,
    to_y = from_y,
    from_y = from_y - dy,
    id = id - 1,
    Type = 'Dribble'
  ) %>% 
  filter(id != max(id)) %>% 
  select(-dframe, -dx, -dy)
events_dribble

events <-
  tibble(id = seq(1, 12, by = 1)) %>% 
  left_join(
    bind_rows(events_nondribble, events_dribble)
  ) %>% 
  filter(id != 11) %>% 
  arrange(id)
events

tracking %>% filter(frame == 226)
tracking %>% count(frame) %>% arrange(desc(frame))
# cols_spadl <- c('game_id', 'period_id', 'time_seconds', 'timestamp', 'team_id', 'home_team', 'team_name', 'player_id', 'player_name', 'action_id', 'type_name', 'bodypart_name', 'result_name', 'start_x', 'end_x', 'start_y', 'end_y', 'end_z', 'type_id', 'result_id', 'bodypart_id')
# cols_spadl_unneeded <- c('game_id', 'time_seconds', 'team_id', 'home_team', 'team_name', 'player_id', 'player_name', 'action_id', 'start_z', 'end_z')
# cols_spadl_needed <- setdiff(cols_spadl, cols_spadl_unneeded)
# cols_spadl_needed
# cols_events <- event_frames %>% colnames()
# intersect(cols_events, cols_spadl_needed)
# setdiff(cols_spadl_needed, cols_events)
# setdiff(cols_events, cols_spadl_needed)

types <-
  tibble::tribble(
          ~type_name, ~type_id,
              'pass',       0L,
             'cross',       1L,
          'throw_in',       2L,
  'freekick_crossed',       3L,
    'freekick_short',       4L,
    'corner_crossed',       5L,
      'corner_short',       6L,
           'take_on',       7L,
              'foul',       8L,
            'tackle',       9L,
      'interception',      10L,
              'shot',      11L,
      'shot_penalty',      12L,
     'shot_freekick',      13L,
       'keeper_save',      14L,
      'keeper_claim',      15L,
      'keeper_punch',      16L,
         'clearance',      18L,
         'bad_touch',      19L,
           'dribble',      21L,
          'goalkick',      22L
  )

bodyparts <-
  tibble::tribble(
    ~bodypart_name, ~bodypart_id,
    'foot',                   0L,
    'head',                   1L,
    'other',                  2L
  )

results <-
  tibble::tribble(
    ~result_name, ~result_id,
    'fail',               0L,
    'success',            1L,
    'offside',            2L,
    'owngoal',            3L,
    'yellow_card',        4L,
    'red_card',           5L
  )

rng_x_spadl <- c(0, 105)
rng_y_spadl <- c(0, 68)
rng_x_opta <- c(0, 100)
rng_y_opta <- c(0, 100)
# tracking %>% skimr::skim()
# to_statsbomb <- ggsoccer::to_spadl_coordinates(from = ggsoccer::pitch_opta, to = ggsoccer::pitch_statsbomb)

rescale <- function(x, rng1, rng2) {
  rng2[1] + ((x - rng1[1]) * (rng2[2] - rng2[1])) / (rng1[2] - rng1[1])
}

to_spadl_x <- function(x) {
  rescale(x, rng_x_opta, rng_x_spadl) 
}

to_spadl_y <- function(y) {
  rescale(y, rng_y_opta, rng_y_spadl) 
}

to_opta_x <- function(x) {
  rescale(x, rng_x_spadl, rng_x_opta) 
}

to_opta_y <- function(y) {
  rescale(y, rng_y_spadl, rng_y_opta) 
}

events_spadl_init <-
  events %>% 
  transmute(
    game_id = 1L, # Needed down the line for `vaep_score_actions` called in vaep_get_scores`
    action_id = id,
    period_id = 2L,
    team_id = 1L,
    home_team = TRUE,
    type_name = case_when(id == 4 ~ 'shot', id == '3' ~ 'cross', TRUE ~ 'pass'),
    bodypart_name = if_else(id != 4, 'foot', 'head'),
    result_name = 'success',
    time_seconds = from_frame / fps,
    timestamp = sprintf('00:00:0%1.3f', from_frame / fps),
    start_x = to_spadl_x(from_x),
    start_y = to_spadl_y(from_y),
    end_x = to_spadl_x(to_x),
    end_y = to_spadl_y(to_y)
  ) %>% 
  left_join(types) %>% 
  left_join(bodyparts) %>% 
  left_join(results)

events_spadl_init1 <- events_spadl_init %>% head(1)

events_spadl_dummy <-
  events_spadl_init1 %>% 
  select(-action_id, -period_id, -team_id, -time_seconds) %>% 
  mutate(dummy = 0) %>% 
  left_join(
    tibble(
      action_id = seq(-5L, -1L, by = 1L),
      period_id = c(rep(1L, 3), rep(2L, 2)),
      team_id = c(rep(2L, 3), rep(1L, 2)),
      time_seconds = seq(1, 5),
      dummy = 0
    )
  )
events_spadl <- bind_rows(events_spadl_dummy, events_spadl_init)
events_spadl

features <- c('type_id_a0', 'type_pass_a0', 'type_cross_a0', 'type_throw_in_a0', 'type_freekick_crossed_a0', 'type_freekick_short_a0', 'type_corner_crossed_a0', 'type_corner_short_a0', 'type_take_on_a0', 'type_foul_a0', 'type_tackle_a0', 'type_interception_a0', 'type_shot_a0', 'type_shot_penalty_a0', 'type_shot_freekick_a0', 'type_keeper_save_a0', 'type_keeper_claim_a0', 'type_keeper_punch_a0', 'type_keeper_pick_up_a0', 'type_clearance_a0', 'type_bad_touch_a0', 'type_non_action_a0', 'type_dribble_a0', 'type_goalkick_a0', 'bodypart_foot_a0', 'bodypart_head_a0', 'bodypart_other_a0', 'result_id_a0', 'result_fail_a0', 'result_success_a0', 'result_offside_a0', 'result_owngoal_a0', 'result_yellow_card_a0', 'result_red_card_a0', 'goalscore_team', 'goalscore_opponent', 'goalscore_diff', 'start_x_a0', 'start_y_a0', 'end_x_a0', 'end_y_a0', 'dx_a0', 'dy_a0', 'movement_a0', 'start_dist_to_goal_a0', 'start_angle_to_goal_a0', 'end_dist_to_goal_a0', 'end_angle_to_goal_a0')

# debugonce(Rteta::vaep_get_features)
vaep_features <- events_spadl %>% Rteta::vaep_get_features() %>% .[features]
vaep_labels <- events_spadl %>% Rteta::vaep_get_labels()

score_matrix <-
  xgboost::xgb.DMatrix(
    data = as.matrix(vaep_features),
    label = as.numeric(vaep_labels$scores)
  )

concede_matrix <- 
  xgboost::xgb.DMatrix(
    data = as.matrix(vaep_features),
    label = as.numeric(vaep_labels$concedes)
  )

get_model <- function(x = c('score', 'concede')) {
  x <- match.arg(x)
  path <- fs::path('data-raw', '09', sprintf('vaep.%smodel', x))
  xgboost::xgb.load(path)
}
score_model <- 'score' %>% get_model()
concede_model <- 'concede' %>% get_model()

require(xgboost)
events_spadl_aug <- events_spadl
events_spadl_aug$scores <- score_model %>% predict(newdata = score_matrix)
events_spadl_aug$concedes <- concede_model %>% predict(newdata = concede_matrix)

# debugonce(Rteta::vaep_get_scores)
# debugonce(Rteta::vaep_score_actions)
events_spadl_aug <- events_spadl_aug %>% Rteta::vaep_get_scores('scores', 'concedes')

tracking <- 
  here::here('data-raw', '07', 'CL2020_final8_tracking.csv') %>% 
  read_csv() %>% 
  select(-1) %>% 
  filter(play == play_filt)
tracking

frames <- 
  tracking %>% 
  mutate(
    team = case_when(is.na(bgcolor) ~ NA_character_, bgcolor == 'red' ~ 'away', bgcolor == 'darkblue' ~ 'home'),
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

color_low <- 'blue'
color_high <- 'red'
arw_v <- arrow(length = unit(3, 'pt'), type = 'closed')
.filter_frames <- function(data) {
  data %>% 
    mutate(is_second = frame %% 25 == 0) %>% 
    filter(is_second) # %>% 
    # filter(frame >= 5) # First handful of frames look weird.
}

viz <-
  frames %>%
  .filter_frames() %>% 
  ggplot() +
  aes(x = x, y = y) +
  pitch_gg() +
  # geom_tile(aes(x = x, y = y, fill = pc), alpha = 0.7) +
  # scale_fill_gradient2(low = color_low, high = color_high, mid = 'white', midpoint = 0.5) +
  geom_segment(
    data = frames_redux %>% .filter_frames() %>% filter(!is.na(forward_x)),
    color = 'black',
    size = 1.5, 
    arrow = arw_v,
    aes(x = x, y = y, xend = forward_x, yend = forward_y)
  ) +
  # ggnewscale::new_scale_fill() +
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
  facet_wrap(~frame)
viz

vaep <-
  events_spadl_aug %>% 
  filter(action_id >= 0)  %>% 
  mutate(
    end_x = to_opta_x(start_x),
    end_y = to_opta_y(start_y),
    to_x = to_opta_x(end_x),
    to_y = to_opta_y(end_y)
  ) %>% 
  select(
    id = action_id,
    scores,
    concedes,
    attack_score,
    defence_score,
    vaep_value
  ) %>% 
  inner_join(events)
vaep

tracking %>% filter(frame == 22)
tracking %>% filter(player == 0) %>% filter(z > 0)
frames
