
library(tidyverse)
dir_proj <- '47-formation_symmetry'
dir_understat_proj <- '31-wp_soccer'
path_shots <- file.path(dir_understat_proj, 'shots.rds')
path_matches <- file.path(dir_understat_proj, 'matches.rds')
path_teams_players_stats <- file.path(dir_understat_proj, 'teams_players_stats.rds')

leagues_mapping <-
  tidyr::crossing(
    league = 'epl',
    league_understat = 'EPL',
    season = 2017L:2021L
  )
leagues_mapping

team_mapping <-
  xengagement::team_accounts_mapping %>% 
  select(team, team_understat, team_538)
team_mapping

fix_understat_meta <- function(data) {
  data %>% 
    rename(league_understat = league) %>% 
    inner_join(leagues_mapping %>% select(league, league_understat, season)) %>% 
    select(-league_understat) %>% 
    relocate(league, season)
}

teams_players_stats <- path_teams_players_stats %>% 
  read_rds() %>%
  select(
    league = league_name,
    season = year,
    player = player_name,
    team = team_name
  ) %>% 
  fix_understat_meta()
teams_players_stats %>% count(season, player, sort = TRUE)
teams_players_stats %>% count(season)

matches <- path_matches %>% 
  read_rds() %>% 
  fix_understat_meta()
matches

match_ids <- matches %>% distinct(league, season, match_id)

shots <- path_shots %>% 
  read_rds() %>% 
  inner_join(match_ids) %>% 
  mutate(
    across(date, lubridate::date),
    across(c(season, match_id, minute), as.integer)
  ) %>% 
  # Some weird bad record.
  mutate(
    across(date, ~case_when(match_id == 9220L ~ lubridate::ymd('2018-08-26'), TRUE ~ .x))
  ) %>% 
  select(
    league,
    season,
    date,
    match_id,
    id, 
    player,
    minute,
    result,
    xg = xG,
    team_h = h_team,
    team_a = a_team, 
    side = h_a,
    g_h_final = h_goals,
    g_a_final = a_goals
  )
shots

shots_w_teams <- shots %>% 
  select(-id, -matches('_final$')) %>% 
  ## unfortunately there is no indication if a player is on the home or away team directly in the shots data
  mutate(
    team = ifelse(side == 'h', team_h, team_a)
  )

rgx_xg_cumu <- '^x?g'
xg_cumu <- shots_w_teams %>% 
  arrange(league, season, match_id, minute) %>% 
  mutate(
    g_h = 
      case_when(
        team == team_h & result == 'Goal' ~ 1L, 
        team == team_a & result == 'OwnGoal' ~ 1L,
        TRUE ~ 0L
      ),
    g_a = 
      case_when(
        team == team_a & result == 'Goal' ~ 1L, 
        team == team_h & result == 'OwnGoal' ~ 1L,
        TRUE ~ 0L
      ),
    xg_h = 
      case_when(
        team == team_h ~ xg, 
        TRUE ~ 0
      ),
    xg_a = 
      case_when(
        team == team_a ~ xg, 
        TRUE ~ 0
      )
  ) %>% 
  group_by(league, season, match_id) %>%
  mutate(
    across(matches('x?g_[ha]$'), list(cumu = cumsum))
  ) %>%
  ungroup() %>%
  select(-xg) %>% 
  rename(team_understat_h = team_h, team_understat_a = team_a) %>% 
  left_join(team_mapping %>% select(team_h = team_538, team_understat_h = team_understat)) %>% 
  left_join(team_mapping %>% select(team_a = team_538, team_understat_a = team_understat)) %>% 
  left_join(team_mapping %>% select(team_z = team_538, team = team_understat)) %>% 
  select(-team) %>% 
  rename(team = team_z) %>% 
  relocate(team) %>% 
  select(-matches('^team_understat')) %>%  
  # Aggregate by last in minute. 
  group_by(league, season, date, match_id, team_h, team_a, minute) %>% 
  mutate(idx_intra = row_number(xg_h_cumu + xg_a_cumu + g_h_cumu + g_a_cumu)) %>% 
  slice_max(idx_intra) %>% 
  ungroup() %>% 
  select(
    # Add team back in and don't do redux
    team, side,
    league, season, date, match_id, team_h, team_a, minute,
    matches(rgx_xg_cumu)
  )
xg_cumu
xg_cumu %>% filter(match_id == 11695)
write_rds(xg_cumu, file.path(dir_proj, 'understat_xg.rds'))
