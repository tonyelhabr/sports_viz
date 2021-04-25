
library(tidyverse)
dir_proj <- '24-202021_game_state_fouls'
path_shots <- file.path(dir_proj, 'shots.rds')
path_states <- file.path(dir_proj, 'states.rds')

shots <- path_shots %>% read_rds()
matches <-
  shots %>% 
  mutate(
    across(date, lubridate::date),
    across(c(season, match_id, minute), as.integer)
  ) %>% 
  rename(team_h = h_team, team_a = a_team, side = h_a)
matches

.f_select <- function(data, ...) {
  data %>% 
    select(league, season, date, match_id, team_h, team_a, side, ...)
}

goals <-
  matches %>% 
  filter(result == 'Goal') %>% 
  select(-result) %>% 
  arrange(date, match_id, minute) %>% 
  .f_select(minute)
goals

matches_nogoals <-
  matches %>% 
  distinct(match_id) %>% 
  anti_join(goals %>% distinct(match_id)) %>% 
  left_join(
    matches %>% .f_select()
  ) %>% 
  mutate(minute = NA_integer_)
matches_nogoals

# Pad such that minutes after last event for games with last shot before 90 min (e.g. last shot at 87th minute) gets counted.
goals_redux_init <-
  bind_rows(
    goals %>% 
      filter(side == 'h') %>% 
      mutate(g_h = 1L),
    goals %>% 
      filter(side == 'a') %>% 
      mutate(g_a = 1L)
  ) %>% 
  arrange(league, season, date, match_id, minute)
goals_redux_init

goals_redux_pad <-
  goals_redux_init %>% 
  group_by(league, season, date, match_id) %>% 
  slice_max(minute, with_ties = FALSE) %>% 
  ungroup() %>% 
  filter(minute < 90) %>% 
  mutate(minute = 90) %>% 
  select(league, season, date, match_id, team_h, team_a, side, minute)
goals_redux_pad

goals_redux <-
  bind_rows(goals_redux_init, goals_redux_pad) %>% 
  arrange(league, season, date, match_id, minute) %>% 
  group_by(league, season, date, match_id) %>% 
  mutate(
    dur = minute - dplyr::lag(minute, default = 0L),
    across(matches('^g_'), list(cumu = ~cumsum(coalesce(.x, 0))))
  ) %>% 
  mutate(
    g_state_h = dplyr::lag(g_h_cumu, 1) - dplyr::lag(g_a_cumu, 1),
    g_state_a = dplyr::lag(g_a_cumu, 1) - dplyr::lag(g_h_cumu, 1)
  ) %>%
  ungroup() %>%
  mutate(
    # The coalesce is for the first event in a match.
    across(matches('^g_'), ~coalesce(.x, 0) %>% as.integer())
  )
goals_redux

.f_rename <- function(side) {
  suffix <- sprintf('_%s$', side)
  side_opp <- ifelse(side == 'h', 'a', 'h')
  suffix_opp <- sprintf('_%s$', side_opp)
  goals_redux %>% 
    select(-matches('_cumu$')) %>% 
    rename_with(~str_replace(.x, suffix, ''), c(matches(suffix))) %>% 
    rename_with(~str_replace(.x, suffix_opp, '_opp'), c(matches(suffix_opp))) %>% 
    mutate(side = !!side)
}

goals_by_team <- bind_rows(.f_rename('h'), .f_rename('a'))

matches_bind <-
  bind_rows(
    goals_by_team, 
    matches_nogoals %>% mutate(g = 0L, g_opp = 0L, g_state = 0L, g_state_opp = 0L)
  )

matches_dummy <-
  matches_bind %>% 
  group_by(match_id, team) %>% 
  slice_min(minute, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(minute = 0L, g = 0L, g_opp = 0L, g_state = 0L, g_state_opp = 0L, dur = 0L)
matches_dummy

states <-
  bind_rows(matches_bind, batches_dummy) %>% 
  arrange(league, season, date, match_id, team, team_opp, minute) %>% 
  select(-g_state_opp) %>% 
  mutate(
    across(g_state, list(grp = ~case_when(.x < 0L ~ 'Behind', .x > 0L ~ 'Ahead', TRUE ~ 'Neutral')))
  ) %>%
  # Just to have something at the end that I don't comment out when testing.
  arrange(league, season, date, match_id, minute, team)
states
write_rds(states, path_states)
