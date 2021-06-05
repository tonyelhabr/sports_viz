
library(tidyverse)
dir_proj <- '31-wp_soccer'

leagues_mapping <-
  tibble(
    league = c('epl'),
    league_understat = c('EPL'),
    # league_whoscored = c('England-Premier-League'),
    league_538 = c('Barclays Premier League')
  )

f_read <- function(x) {
  file.path(dir_proj, sprintf('201718-202021_epl_%s.parquet', x)) %>% 
    arrow::read_parquet()
}

events_init <- f_read('events')
meta <- 
  f_read('meta') %>% 
  mutate(across(start_date, ~lubridate::ymd_hms(.x) %>% lubridate::date())) %>% 
  rename(date = start_date)

events_teams <-
  events_init %>% 
  group_by(match_id, team = team_name) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(match_id, team, side) %>% 
  pivot_wider(
    names_from = side,
    values_from = team,
    names_prefix = 'team_'
  ) %>% 
  rename(team_h = team_home, team_a = team_away)
events_teams
teams <- events_teams %>% distinct(team = team_h) %>% arrange(team)
teams
add_time_col <- function(data) {
  data %>% 
    mutate(time = sprintf('%s:%s', expanded_minute, second) %>% lubridate::ms() %>% as.double())
}

events <- 
  events_init %>% 
  # rename(team = team_name, league_whoscored = league) %>% 
  # left_join(leagues_mapping) %>% 
  # select(-league_whoscored) %>% 
  rename_all(~str_remove(.x, '_name')) %>% 
  mutate(across(side, ~case_when(.x == 'home' ~ 'h', .x == 'away' ~ 'a'))) %>% 
  left_join(meta %>% select(match_id, date)) %>% 
  mutate(
    across(season_id, ~ str_sub(.x, 1, 4) %>% as.integer()),
    across(c(minute, expanded_minute, second), as.integer),
    across(second, ~coalesce(.x, 0L))
  ) %>% 
  add_time_col() %>% 
  left_join(events_teams) %>% 
  mutate(team_opp = ifelse(team == team_h, team_a, team_h)) %>% 
  arrange(competition_id, season_id, match_id, time, team) %>% 
  relocate(competition_id, season_id, match_id, date)
events
# events %>% filter(is.na(time))

f_rename <- function(side) {
  suffix <- sprintf('_%s$', side)
  side_opp <- ifelse(side == 'h', 'a', 'h')
  suffix_opp <- sprintf('_%s$', side_opp)
  col_team <- sprintf('team_%s', side)
  col_team_sym <- sym(col_team)
  events %>% 
    # filter(side == !!side) %>% 
    filter(!!col_team_sym == team) %>% 
    mutate(
      team = if_else(side == 'h', team_h, team_a),
      team_opp = if_else(side == 'a', team_a, team_h)
    ) %>% 
    select(
      competition_id,
      season_id,
      match_id,
      date,
      minute,
      expanded_minute,
      second,
      time,
      side,
      team,
      team_opp,
      team_h,
      team_a,
      player,
      type,
      outcome_type,
      card_type
    )
}

events <- 
  bind_rows(f_rename('h'), f_rename('a')) %>% 
  arrange(competition_id, season_id, date, match_id, time, team)
events

df <- file.path(dir_proj, 'model_data.parquet') %>% arrow::read_parquet()
df %>% filter(is_shot)

shots <-
  events %>% 
  # filter(type != 'SavedShot') %>% 
  filter(season_id >= 2019L)
  filter(type %>% str_detect('Shot|Goal'))
shots

shots %>% filter(match_id == 1375927L) %>% arrange(time)
df %>% filter(match_id == 11643L) %>% filter(is_shot) %>% arrange(minute)
# events %>% count(type, sort = TRUE) -> x
# events %>% filter(match_id == 1375927L) %>% filter(expanded_minute == 6L)

subs <-
  events %>% 
  filter(type %>% str_detect('Substitution'))
subs

cards <-
  events %>%
  filter(type == 'Card')
cards

cards_agg <-
  cards %>% 
  group_by(match_id, player) %>% 
  summarize(n = n(), time = max(time)) %>% 
  ungroup()
cards_agg
