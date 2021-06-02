
library(tidyverse)
dir_proj <- '31-wp_soccer'
path_shots <- file.path(dir_proj, 'shots.rds')
path_matches <- file.path(dir_proj, 'matches.rds')
path_states <- file.path(dir_proj, 'states.rds')

shots <- 
  path_shots %>% 
  read_rds() %>% 
  filter(league == 'EPL') %>% 
  mutate(
    across(date, lubridate::date),
    across(c(season, match_id, minute), as.integer)
  ) %>% 
  rename(team_h = h_team, team_a = a_team, side = h_a)
shots

match_ids <- 
  path_matches %>%
  read_rds() %>% 
  filter(league == 'EPL') %>% 
  distinct(match_id)
match_ids

.f_select <- function(data, ...) {
  data %>% 
    select(league, season, date, match_id, team_h, team_a, side, ...)
}

goals <-
  shots %>% 
  filter(result == 'Goal') %>% 
  select(-result) %>% 
  arrange(date, match_id, minute) %>% 
  .f_select(minute)
goals

matches_nogoals <-
  shots %>% 
  distinct(match_id) %>% 
  anti_join(goals %>% distinct(match_id))

shots_matches_nogoals <-
  matches_nogoals %>% 
  left_join(
    shots %>% .f_select()
  ) %>% 
  mutate(minute = NA_integer_)
shots_matches_nogoals

# Need to pad such that minutes after last event for games with last shot before 90 min (e.g. last shot at 87th minute) gets counted.
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

# Checking when the last shot comes in.
goals_redux_init %>% 
  group_by(league, season, date, match_id) %>% 
  slice_max(minute, with_ties = FALSE) %>% 
  ungroup() %>% 
  summarize(
    across(minute, list(min = min, max = max, median = median, mean = mean), .names = '{fn}')
  ) %>% 
  glimpse()

goals_redux_pad <-
  goals_redux_init %>% 
  group_by(league, season, date, match_id) %>% 
  slice_max(minute, with_ties = FALSE) %>% 
  ungroup() %>% 
  # For games where the last shot is before the last minute
  filter(minute < 90) %>% 
  mutate(minute = 96) %>% 
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

states_simple <- 
  bind_rows(.f_rename('h'), .f_rename('a')) %>% 
  mutate(
    across(g_state, list(grp = ~case_when(.x < 0L ~ 'behind', .x > 0L ~ 'ahead', TRUE ~ 'neutral')))
  ) %>%
  arrange(league, season, date, match_id, team, minute)
states_simple

# old? ----
goals_by_team <- bind_rows(.f_rename('h'), .f_rename('a'))

matches_bind <-
  bind_rows(
    goals_by_team, 
    shots_matches_nogoals %>% mutate(g = 0L, g_opp = 0L, g_state = 0L, g_state_opp = 0L)
  )

matches_dummy <-
  matches_bind %>% 
  group_by(match_id, team) %>% 
  slice_min(minute, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(minute = 0L, g = 0L, g_opp = 0L, g_state = 0L, g_state_opp = 0L, dur = 0L)
matches_dummy

states <-
  bind_rows(matches_bind, matches_dummy) %>% 
  arrange(league, season, date, match_id, team, team_opp, minute) %>% 
  select(-g_state_opp) %>% 
  mutate(
    across(g_state, list(grp = ~case_when(.x < 0L ~ 'Behind', .x > 0L ~ 'Ahead', TRUE ~ 'Neutral')))
  ) %>%
  # Just to have something at the end that I don't comment out when testing.
  arrange(league, season, date, match_id, minute, team)
states
states %>% filter(match_id == first(match_id), team == 'Liverpool')
write_rds(states, path_states)
