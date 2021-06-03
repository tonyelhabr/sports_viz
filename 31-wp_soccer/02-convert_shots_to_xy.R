
library(tidyverse)
dir_proj <- '31-wp_soccer'
path_shots <- file.path(dir_proj, 'shots.rds')
path_matches <- file.path(dir_proj, 'matches.rds')
path_team_players_stats <- file.path(dir_proj, 'team_players_stats.rds')
path_spi <- file.path(dir_proj, 'spi_538.csv')
path_matches <- file.path(dir_proj, 'matches.rds')

path_model_data <- file.path(dir_proj, 'model_data.rds')

leagues_mapping <-
  tibble(
    league = c('epl'),
    league_understat = c('EPL'),
    # league_whoscored = c('England-Premier-League'),
    league_538 = c('Barclays Premier League')
  )

team_mapping <-
  xengagement::team_accounts_mapping %>% 
  select(team, team_538, team_understat)
team_mapping

team_players_stats <- 
  path_team_players_stats %>% 
  read_rds() %>%
  filter(league_name == 'EPL') %>% 
  select(
    league = league_name,
    season = year,
    player_id,
    player = player_name,
    team = team_name
  )
team_players_stats

matches <- path_matches %>% read_rds()

match_ids <-
  matches %>% 
  filter(league == 'EPL') %>% 
  distinct(league, season, match_id)
match_ids

shots_init <- path_shots %>% read_rds()

shots <- 
  shots_init %>% 
  # janitor::clean_names() %>% 
  # filter(league == 'EPL') %>% 
  inner_join(match_ids) %>% 
  mutate(
    across(date, lubridate::date),
    across(c(season, match_id, minute), as.integer)
  ) %>% 
  # rename(team_h = h_team, team_a = a_team, side = h_a)
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
    # side,
    team_h = h_team,
    team_a = a_team, 
    side = h_a,
    g_h_final = h_goals,
    g_a_final = a_goals
  )
shots

# # Just use this for debugging, probably.
scorelines <-
  shots %>%
  distinct(league, season, match_id, team_h, team_a, g_h = g_h_final, g_a = g_a_final)
scorelines
# 
# match_ids <- 
#   path_matches %>%
#   read_rds() %>% 
#   filter(league == 'EPL') %>% 
#   distinct(match_id)
# match_ids

shots_w_teams <-
  shots %>% 
  select(-matches('_final$')) %>% 
  left_join(team_players_stats)
shots_w_teams

xg <-
  shots_w_teams %>% 
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
  ) %>% 
  select(-team) %>% 
  group_by(league, season, match_id) %>% 
  mutate(
    across(matches('x?g_[ha]$'), cumsum)
  ) %>% 
  ungroup() %>% 
  select(-xg) %>% 
  rename(team_understat_h = team_h, team_understat_a = team_a) %>% 
  left_join(team_mapping %>% select(team_h = team, team_understat_h = team_understat)) %>% 
  left_join(team_mapping %>% select(team_a = team, team_understat_a = team_understat)) %>% 
  select(-matches('^team_understat')) 
xg

probs_init <-
  path_spi %>%
  read_csv() %>% 
  rename(
    league_538 = league,
    team_538_h = team1,
    team_538_a = team2,
    prob_d_538 = probtie,
    imp_h = importance1,
    imp_a = importance2
  ) %>%
  left_join(leagues_mapping) %>% 
  select(-league_538) %>% 
  filter(season >= 2019) %>% 
  # select(-c(league_id)) %>% 
  filter(league == 'epl') %>% 
  mutate(across(season, as.integer)) %>% 
  rename_with(~stringr::str_replace(.x, '1$', '_538_h'), matches('1$')) %>% 
  rename_with(~stringr::str_replace(.x, '2$', '_538_a'), matches('2$')) %>% 
  left_join(team_mapping %>% select(team_h = team, team_538_h = team_538)) %>% 
  left_join(team_mapping %>% select(team_a = team, team_538_a = team_538)) %>% 
  select(-matches('^team_538')) %>% 
  rename(prob_h = prob_538_h, prob_a = prob_538_a, prob_d = prob_d_538) %>% 
  select(season, league, date, matches('^team_'), matches('^score_'), matches('imp'), matches('^prob'))
probs_init

# xg_probs <- xg %>% full_join(probs_init %>% rename(date2 = date, league2 = league))
xg_probs <- xg %>% full_join(probs_init %>% select(-c(date, league)))
# xg_probs <- xg %>% full_join(probs_init %>% select(season, team_h, team_a, prob_h, prob_a, date2 = date))
xg_probs %>% skimr::skim()
xg_probs %>% filter(is.na(match_id)) -> z

# xg %>% filter(team_h == 'Arsenal', team_a == 'Everton', season == 2020)
# write_rds(xg_probs, path_model_data)

# # Debug mis-match with teams.
xg_probs %>% filter(is.na(prob_h)) %>% distinct(match_id, team_h) %>% count(team_h, sort = TRUE)
xg_probs %>% filter(is.na(prob_a)) %>% distinct(match_id, team_a) %>% count(team_a, sort = TRUE)

# TODO: Add number of men on field and number of subs remaining.
.f_rename <- function(side) {
  suffix <- sprintf('_%s$', side)
  side_opp <- ifelse(side == 'h', 'a', 'h')
  suffix_opp <- sprintf('_%s$', side_opp)
  xg_probs %>% 
    # select(-matches('_cumu$')) %>% 
    rename_with(~str_replace(.x, suffix, ''), c(matches(suffix))) %>% 
    rename_with(~str_replace(.x, suffix_opp, '_opp'), c(matches(suffix_opp))) %>% 
    filter(side == !!side)
}

minute_max <- xg_probs %>% slice_max(minute, with_ties = FALSE) %>% pull(minute)
df <-
  bind_rows(.f_rename('h'), .f_rename('a')) %>% 
  mutate(
    idx = row_number(),
    is_w = if_else((score_538 - score_538_opp) > 0, 1L, 0L),
    is_h = if_else(side == 'h', 1L, 0L),
    gd = g - g_opp,
    xgd = xg - xg_opp,
    min_elapsed = minute / !!minute_max,
    prob_d = (1 - prob - prob_opp) * exp(-pi * min_elapsed)
  ) %>% 
  mutate(
    across(c(prob, prob_opp, imp, imp_opp), ~{.x * (exp(-pi * min_elapsed))}),
    gd_ratio = gd / (exp(-pi * min_elapsed))
  ) %>% 
  select(
    is_w,
    idx,
    league,
    season,
    match_id,
    minute,
    is_h,
    matches('^x?g'),
    matches('prob'),
    matches('ratio$')
  )
df
write_rds(df, path_model_data)
