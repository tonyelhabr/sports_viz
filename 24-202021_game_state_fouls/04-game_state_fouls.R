
library(tidyverse)
dir_proj <- '24-202021_game_state_fouls'
path_events <- file.path(dir_proj, 'events.rds')
path_meta <- file.path(dir_proj, 'meta.rds')
# path_states <- file.path(dir_proj, 'states.rds')

probs <- 
  read_csv(
    'https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv'
  ) %>% 
  filter(league == 'Barclays Premier League' & season >= 2019) %>% 
  select(-c(league, league_id)) %>% 
  rename(date_538 = date, team_538_h = team1, team_538_a = team2, probtie_538 = probtie) %>% 
  rename_with(~stringr::str_replace(.x, '1$', '_538_h'), matches('1$')) %>% 
  rename_with(~stringr::str_replace(.x, '2$', '_538_a'), matches('2$'))
probs

meta <- 
  path_meta %>% 
  read_rds() %>% 
  mutate(across(start_date, ~lubridate::ymd_hms(.x) %>% lubridate::date())) %>% 
  rename(date = start_date)

events_teams <-
  path_events %>% 
  read_rds() %>% 
  group_by(match_id, team = team_name) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # mutate(across(side, ~case_when(.x == 'home' ~ 'h', .x == 'away' ~ 'a'))) %>% 
  select(match_id, team, side) %>% 
  pivot_wider(
    names_from = side,
    values_from = team,
    names_prefix = 'team_'
  ) %>% 
  rename(team_h = team_home, team_a = team_away)
events_teams

leagues_mapping <-
  tibble(
    league = c('epl'),
    league_understat = c('EPL'),
    league_whoscored = c('England-Premier-League')
  )

events <- 
  path_events %>% 
  read_rds() %>% 
  rename(team = team_name, league_whoscored = league) %>% 
  left_join(leagues_mapping) %>% 
  select(-league_whoscored) %>% 
  mutate(across(side, ~case_when(.x == 'home' ~ 'h', .x == 'away' ~ 'a'))) %>% 
  left_join(meta %>% select(match_id, date)) %>% 
  mutate(
    across(season, ~ str_sub(.x, 1, 4) %>% as.integer()),
    across(c(minute, second), as.integer)
  ) %>% 
  left_join(events_teams) %>% 
  mutate(team_opp = ifelse(team == team_h, team_a, team_h)) %>% 
  arrange(league, season, match_id, team, expanded_minute, second) %>% 
  relocate(league, season, match_id, date)
events

# .f_slice <- function(f = slice_head) {
#   events %>% 
#     group_by(match_id) %>% 
#     f(n = 1) %>% 
#     ungroup()
# }
# 
# events_first <- .f_slice(slice_head)
# events_last <- .f_slice(slice_tail)
events_first <- events %>% filter(period_name == 'PreMatch', type_name == 'FormationSet', side == 'h')
events_last <- events %>% filter(period_name == 'SecondHalf', type_name == 'End', side == 'h')
# events_first %>% count(type_name)
# events_last %>% count(type_name)

fouls <- events %>% filter(type_name == 'Foul')
goals <- events %>% filter(type_name == 'Goal')
# states <- path_states %>% read_rds() %>% filter(league == 'EPL') %>% select(-league)

# main ----
.f_select <- function(data, ...) {
  data %>%
    select(
      league,
      season,
      date,
      match_id,
      team_h,
      team_a,
      # side,
      expanded_minute,
      second,
      ...
    )
}

rgx_g <- '^g_[ha]'
goals_redux_init <-
  bind_rows(
    # Could generalize this even more.
    goals %>% 
      filter(side == 'h') %>% 
      select(-side) %>% 
      .f_select() %>% 
      mutate(g_h = 1L),
    goals %>% 
      filter(side == 'a') %>% 
      select(-side) %>% 
      .f_select() %>% 
      mutate(g_a = 1L)
  ) %>% 
  mutate(across(matches(sprintf('%s$', rgx_g)), ~coalesce(.x, 0L))) %>% 
  arrange(league, season, date, match_id, expanded_minute, second) %>% 
  group_by(league, season, date, match_id) %>% 
  mutate(
    across(matches(sprintf('%s$', rgx_g)), list(cumu = ~cumsum(.x)))
  ) %>% 
  ungroup()
goals_redux_init

goals_redux <-
  bind_rows(
    events_first %>% 
      .f_select() %>% 
      mutate(g_h = 0L, g_a = 0L, g_h_cumu = 0L, g_a_cumu = 0L, comment = 'init'),
    goals_redux_init %>% mutate(comment = NA_character_),
    events_last %>% 
      .f_select() %>% 
      mutate(g_h = 0L, g_a = 0L, comment = 'end')
  ) %>% 
  arrange(league, season, date, match_id, expanded_minute, second) %>% 
  group_by(match_id) %>% 
  # mutate(across(matches(sprintf('%s_cumu$', rgx_g)), ~lag(.x, 1L))) %>% 
  fill(g_h_cumu, g_a_cumu, .direction = 'down') %>% 
  ungroup()
goals_redux

fouls_slim <- 
  fouls %>% 
  mutate(
    is_drawn = if_else(outcome_type_name == 'Successful', TRUE, FALSE)
  ) %>%
  select(
    season,
    match_id,
    date,
    id,
    minute,
    second,
    team,
    team_opp,
    side,
    player_name,
    is_drawn
  )
fouls_slim

# teams_fouls <- fouls_slim %>% count(team, name = 'n_fouls')
# teams_fouls
# teams_states <- states %>% filter(league == 'EPL') %>% count(team, name = 'n_states')
# teams_states
# teams_probs <- matches %>% count(team = team_538_h, name = 'n_prob')
# teams_probs
# res <-
#   teams_fouls %>%
#   full_join(teams_states) %>% 
#   full_join(teams_probs)
# res
# 
# res %>%
#   filter(is.na(n_fouls)) %>%
#   pull(team) %>%
#   datapasta::vector_paste()
# c("Manchester City", "Manchester United", "Newcastle United", "Sheffield United", "West Bromwich Albion", "Wolverhampton Wanderers", "AFC Bournemouth", "Brighton and Hove Albion", "Leeds United", "Leicester City", "Norwich City", "Tottenham Hotspur", "West Ham United", "Wolverhampton")
# res %>%
#   filter(is.na(n_states)) %>%
#   pull(team) %>%
#   datapasta::vector_paste()
# c("Man City", "Man Utd", "Newcastle", "Sheff Utd", "West Brom", "Wolves", "AFC Bournemouth", "Brighton and Hove Albion", "Leeds United", "Leicester City", "Norwich City", "Tottenham Hotspur", "West Ham United", "Wolverhampton")
# res %>%
#   filter(is.na(n_prob)) %>%
#   pull(team) %>%
#   datapasta::vector_paste()
# c("Bournemouth", "Brighton", "Leeds", "Leicester", "Man City", "Man Utd", "Norwich", "Sheff Utd", "Tottenham", "West Brom", "West Ham", "Wolves", "Newcastle United", "Wolverhampton Wanderers")

team_mapping <-
  xengagement::team_accounts_mapping %>% 
  select(team, team_538, team_whoscored)
team_mapping

states_prep <- 
  states %>% 
  select(-side) %>% 
  arrange(season, date, match_id, team) %>% 
  group_by(season, date, match_id, team) %>% 
  mutate(across(minute, list(lead1 = ~coalesce(lead(.x), 96L), lag1 = ~coalesce(lag(.x), 0L)))) %>% 
  ungroup() %>% 
  rename_with(~sprintf('%s_understat', .x), matches('^(match_id|team|minute|second|date)')) %>% 
  left_join(team_mapping %>% select(team, team_understat)) %>% 
  left_join(team_mapping %>% select(team_opp = team, team_opp_understat = team_understat)) %>% 
  select(-matches('team.*understat$'))
states_prep

fouls_prep <- 
  fouls_slim %>% 
  select(-side) %>% 
  arrange(season, date, match_id, team) %>% 
  rename_with(~sprintf('%s_whoscored', .x), matches('^(match_id|team|minute|second|date)')) %>% 
  left_join(team_mapping %>% select(team, team_whoscored))%>% 
  left_join(team_mapping %>% select(team_opp = team, team_opp_whoscored = team_whoscored)) %>% 
  select(-matches('team.*whoscored$'))
fouls_prep

matches_states <-
  states_prep %>% 
  distinct(season, date = date_understat, team, team_opp)
matches_states

matches_fouls <-
  fouls_prep %>% 
  distinct(season, date = date_whoscored, team, team_opp)
matches_fouls

fouls_by_state <-
  data.table::as.data.table(fouls_prep %>% mutate(minute_whoscored_copy = minute_whoscored))[
    data.table::as.data.table(states_prep), 
    on=.(season = season, team = team, team_opp = team_opp, minute_whoscored >= minute_lag1_understat, minute_whoscored_copy < minute_understat)
  ] %>% 
  as_tibble() %>%
  arrange(season, date_whoscored, team, team_opp, minute_lag1_whoscored, second_whoscored) %>% 
  relocate(
    season,
    date_whoscored,
    date_understat,
    team,
    team_opp,
    minute_lag1_whoscored,
    minute_lead1_whoscored,
    # second_whoscored,
    # minute_lag1_understat,
    minute_understat_lag1 = minute_whoscored,
    minute_understat = minute_whoscored.1,
    minute_lead1_understat
  )
fouls_by_state
fouls_by_state$g_state
fouls_by_state %>% filter(is.na(id)) %>% distinct(team, team_opp)

fouls_by_state2 <-
  data.table::as.data.table(states_prep)[
    data.table::as.data.table(fouls_prep), 
    on=.(season = season, team = team, team_opp = team_opp, minute_understat >= minute_whoscored, minute_understat < minute_lead1_whoscored)
  ] %>% 
  as_tibble()
fouls_by_state2

fouls_by_state %>% 
  drop_na(id) %>% 
  count(id, sort = TRUE) %>% 
  count(n)
