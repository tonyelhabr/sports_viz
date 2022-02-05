
library(tidyverse)
dir_proj <- '48-202122_game_state'

## input
path_shots <- file.path(dir_proj, 'shots.rds')
path_matches <- file.path(dir_proj, 'matches.rds')
path_league_teams_stats <- file.path(dir_proj, 'league_teams_stats.rds')
path_team_players_stats <- file.path(dir_proj, 'team_players_stats.rds')

## output
path_xg <- file.path(dir_proj, 'xg.rds')
# path_standings <- file.path(dir_proj, 'standings.rds')

leagues_mapping <- crossing(
    league = 'epl',
    league_understat = 'EPL',
    season = 2021L:2021L
  )
leagues_mapping

team_mapping <- xengagement::team_accounts_mapping %>% 
  select(team_clean = team, team_understat)
team_mapping

fix_understat_meta <- function(data) {
  data %>% 
    rename(league_understat = league) %>% 
    inner_join(leagues_mapping %>% select(league, league_understat, season)) %>% 
    select(-league_understat) %>% 
    relocate(league, season)
}

team_players_stats <- path_team_players_stats %>% 
  read_rds() %>%
  select(
    league = league_name,
    season = year,
    player = player_name,
    team = team_name
  ) %>% 
  fix_understat_meta()
team_players_stats %>% count(season, player, sort = TRUE)
team_players_stats %>% count(season)

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
    situation,
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

rgx_xg <- 'n?p?x?g'
xg_cumu_init <- shots_w_teams %>% 
  # filter(match_id == 16576) %>% 
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
      ),
    p_h = g_h & situation == 'Penalty',
    p_a = g_a & situation == 'Penalty',
    npxg_h = ifelse(p_h, 0, xg),
    npxg_a = ifelse(p_a, 0, xg_a)
  ) %>% 
  group_by(league, season, match_id) %>%
  mutate(
    across(matches(sprintf('%s_[ha]$', rgx_xg)), list(cumu = cumsum), .names = '{.fn}_{.col}')
  ) %>%
  ungroup() %>%
  select(-xg) %>% 
  rename(team_understat_h = team_h, team_understat_a = team_a) %>% 
  left_join(team_mapping %>% select(team_h = team_clean, team_understat_h = team_understat)) %>% 
  left_join(team_mapping %>% select(team_a = team_clean, team_understat_a = team_understat)) %>% 
  left_join(team_mapping %>% select(team_z = team_clean, team = team_understat)) %>% 
  select(-team) %>% 
  rename(team = team_z) %>% 
  relocate(team) %>% 
  select(-matches('^team_understat')) %>%  
  # Aggregate by last in minute. 
  group_by(league, season, date, match_id, team_h, team_a, minute) %>% 
  mutate(idx_intra = row_number(cumu_xg_h + cumu_xg_a + cumu_g_h + cumu_g_a)) %>% 
  slice_max(idx_intra) %>% 
  ungroup() %>% 
  select(
    # Add team back in and don't do redux
    team, side,
    league, season, date, match_id, team_h, team_a, minute,
    matches(rgx_xg)
  )
xg_cumu_init

f_filter_rename_xg <- function(.side) {
  suffix <- sprintf('_%s$', .side)
  side_opp <- ifelse(.side == 'h', 'a', 'h')
  suffix_opp <- sprintf('_%s$', side_opp)
  xg_cumu_init %>% 
    # filter(side == .side) %>% 
    select(-team) %>% 
    mutate(side = .side) %>% 
    rename_with(~str_replace(.x, suffix, ''), c(matches(suffix))) %>% 
    rename_with(~str_replace(.x, suffix_opp, '_opp'), c(matches(suffix_opp)))
}

xg_cumu_redux <-
  bind_rows(f_filter_rename_xg('h'), f_filter_rename_xg('a')) %>% 
  arrange(league, season, date, match_id, minute, team)
xg_cumu_redux

first_min_pad <- xg_cumu_redux %>% 
  # select(match_id, side, minute) %>% 
  group_by(match_id, side) %>% 
  slice_min(minute, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  filter(minute > 1) %>% 
  transmute(
    side,
    league,
    season,
    date,
    match_id,
    team,
    team_opp,
    minute = minute - 1L,
    g = 0L,
    g_opp = 0L,
    xg = 0,
    xg_opp = 0,
    npxg = 0,
    npxg_opp = 0,
    cumu_g = 0L,
    cumu_g_opp = 0L,
    cumu_xg = 0,
    cumu_xg_opp = 0,
    cumu_npxg = 0,
    cumu_npxg_opp = 0
  )

last_min_pad <- xg_cumu_redux %>% 
  # select(match_id, side, minute) %>% 
  group_by(match_id, side) %>% 
  slice_max(minute, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  filter(minute < 90) %>% 
  mutate(minute = 90)

xg <- bind_rows(
  first_min_pad,
  xg_cumu_redux, 
  last_min_pad
) %>% 
  arrange(match_id, minute, side) %>% 
  group_by(match_id, side) %>% 
  mutate(
    dur = minute - dplyr::lag(minute, default = 0L),
    g_state = coalesce(dplyr::lag(cumu_g, 1) - dplyr::lag(cumu_g_opp, 1), 0),
    xg_state = coalesce(dplyr::lag(cumu_xg, 1) - dplyr::lag(cumu_xg_opp, 1), 0),
    npxg_state = coalesce(dplyr::lag(cumu_npxg, 1) - dplyr::lag(cumu_npxg_opp, 1), 0)
  ) %>%
  ungroup()
write_rds(xg, path_xg)

# # For ordering teams on y-axis later.
# standings <- path_league_teams_stats %>%
#   read_rds() %>% 
#   group_by(team = team_name) %>% 
#   summarize(
#     across(pts, sum)
#   ) %>% 
#   ungroup() %>%
#   mutate(rnk = row_number(desc(pts))) %>% 
#   left_join(
#     team_mapping %>% 
#       select(team_z = team_clean, team = team_understat)
#   ) %>% 
#   select(-team) %>% 
#   rename(team = team_z) %>% 
#   arrange(rnk)
# standings
# write_rds(standings, path_standings)
