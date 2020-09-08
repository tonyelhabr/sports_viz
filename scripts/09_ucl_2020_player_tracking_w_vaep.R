
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
xlim_pitch <- c(66, 101)
ylim_pitch <- c(112, -12)

events <- here::here('data-raw', '07', 'CL2020_final8_events.csv') %>% read_csv()

event_frames <-
  events %>% 
  filter(play == play_filt)
event_frames

cols_spadl <- c('game_id', 'period_id', 'time_seconds', 'timestamp', 'team_id', 'home_team', 'team_name', 'player_id', 'player_name', 'action_id', 'type_name', 'bodypart_name', 'result_name', 'start_x', 'end_x', 'start_y', 'end_y', 'end_z', 'type_id', 'result_id', 'bodypart_id')
cols_spadl_unneeded <- c('game_id', 'time_seconds', 'team_id', 'home_team', 'team_name', 'player_id', 'player_name', 'action_id', 'start_z', 'end_z')
cols_spadl_needed <- setdiff(cols_spadl, cols_spadl_unneeded)
cols_spadl_needed
cols_events <- event_frames %>% colnames()
intersect(cols_events, cols_spadl_needed)
setdiff(cols_spadl_needed, cols_events)
setdiff(cols_events, cols_spadl_needed)
events
type_names <- c('bad_touch', 'clearance', 'corner_crossed', 'corner_short', 'cross', 'dribble', 'foul', 'freekick_crossed', 'freekick_short', 'goalkick', 'interception', 'keeper_claim', 'keeper_punch', 'keeper_save', 'pass', 'shot', 'shot_freekick', 'shot_penalty', 'tackle', 'take_on', 'throw_in')
bodypart_names <- c('foot', 'head', 'other') # 0, 1, 2

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

features <- c(
  'type_id_a0', 'type_pass_a0', 'type_cross_a0', 'type_throw_in_a0', 'type_freekick_crossed_a0', 'type_freekick_short_a0', 'type_corner_crossed_a0', 
  'type_corner_short_a0', 'type_take_on_a0', 'type_foul_a0', 'type_tackle_a0', 'type_interception_a0', 'type_shot_a0', 'type_shot_penalty_a0',
  'type_shot_freekick_a0', 'type_keeper_save_a0', 'type_keeper_claim_a0', 'type_keeper_punch_a0', 'type_keeper_pick_up_a0', 'type_clearance_a0', 'type_bad_touch_a0',
  'type_non_action_a0', 'type_dribble_a0', 'type_goalkick_a0', 'bodypart_foot_a0', 'bodypart_head_a0', 'bodypart_other_a0', 'result_id_a0',
  'result_fail_a0', 'result_success_a0', 'result_offside_a0', 'result_owngoal_a0', 'result_yellow_card_a0', 'result_red_card_a0', 'goalscore_team',
  'goalscore_opponent', 'goalscore_diff', 'start_x_a0', 'start_y_a0', 'end_x_a0', 'end_y_a0', 'dx_a0',
  'dy_a0', 'movement_a0', 'start_dist_to_goal_a0', 'start_angle_to_goal_a0', 'end_dist_to_goal_a0', 'end_angle_to_goal_a0'
)

event_frames_aug <-
  event_frames %>% 
  transmute(
    id = id,
    period_id = 2,
    home_team = TRUE,
    type_name = case_when(id == 4 ~ 'shot', id == '3' ~ 'cross', TRUE ~ 'pass'),
    bodypart_name = if_else(id != 4, 'foot', 'head'),
    result_name = 'success',
    time_seconds = from_frame / fps,
    timestamp = sprintf('00:00:0%1.3f', from_frame / fps),
    start_x = from_x,
    start_y = from_y,
    end_x = to_x,
    end_y = to_y
  ) %>% 
  left_join(types) %>% 
  left_join(bodyparts) %>% 
  left_join(results)
debugonce(Rteta::vaep_get_features)
event_frames_aug %>% Rteta::vaep_get_features()

tracking <- 
  here::here('data-raw', '07', 'CL2020_final8_tracking.csv') %>% 
  read_csv() %>% 
  select(-1)
tracking
tracking %>% count(play)

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



frames
frames %>% count(player_num)
