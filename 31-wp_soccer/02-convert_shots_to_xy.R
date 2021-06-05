
library(tidyverse)
dir_proj <- '31-wp_soccer'
path_shots <- file.path(dir_proj, 'shots.rds')
path_matches <- file.path(dir_proj, 'matches.rds')
path_team_players_stats <- file.path(dir_proj, 'team_players_stats.rds')
path_spi <- file.path(dir_proj, 'spi_538.csv')
path_matches <- file.path(dir_proj, 'matches.rds')

path_df <- file.path(dir_proj, 'model_data.parquet')

leagues_mapping <-
  tibble(
    league = c('epl'),
    league_understat = c('EPL'),
    # league_whoscored = c('England-Premier-League'),
    # league_538 = c('Barclays Premier League')
    league_id_538 = c(2411L)
  )

team_mapping <-
  xengagement::team_accounts_mapping %>% 
  select(team, team_538, team_understat)
team_mapping

filter_understat <- function(data) {
  data %>%
    filter(league == 'EPL', season_id %in% 2017L:2020L)
}

team_players_stats <- 
  path_team_players_stats %>% 
  read_rds() %>%
  select(
    league = league_name,
    season_id = year,
    player_id,
    player = player_name,
    team = team_name
  ) %>% 
  filter_understat()
team_players_stats
teams2 <- team_players_stats %>% distinct(team) %>% arrange(team)
teams2
matches <- path_matches %>% read_rds()

match_ids <-
  matches %>% 
  rename(season_id = season) %>% 
  filter_understat() %>% 
  distinct(league, season_id, match_id)
match_ids

shots_init <- path_shots %>% read_rds()

shots <- 
  shots_init %>% 
  # janitor::clean_names() %>% 
  # filter(league == 'EPL') %>% 
  inner_join(match_ids) %>% 
  mutate(
    across(date, lubridate::date),
    across(c(season_id, match_id, minute), as.integer)
  ) %>% 
  # rename(team_h = h_team, team_a = a_team, side = h_a)
  select(
    league,
    season_id,
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
  distinct(league, season_id, date, match_id, team_h, team_a, g_h = g_h_final, g_a = g_a_final)
scorelines

shots_w_teams <-
  shots %>% 
  select(-id, -matches('_final$')) %>% 
  left_join(team_players_stats %>% select(-player_id))
shots_w_teams

shots_w_teams_0 <-
  shots_w_teams %>% 
  filter(minute == 0L) %>% 
  distinct(league, season_id, date, match_id, .keep_all = TRUE)
shots_w_teams_0

xg <-
  bind_rows(
    scorelines %>% anti_join(shots_w_teams_0) %>% mutate(minute = 0L, side = 'h', is_shot = FALSE), 
    scorelines %>% anti_join(shots_w_teams_0) %>% mutate(minute = 0L, side = 'a', is_shot = FALSE), 
    shots_w_teams %>% mutate(is_shot = TRUE)
  ) %>% 
  arrange(league, season_id, match_id, minute) %>% 
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
  group_by(league, season_id, match_id) %>% 
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
  select(-league) %>% 
  rename(
    season_id = season,
    league_id_538 = league_id,
    team_538_h = team1,
    team_538_a = team2,
    prob_d_538 = probtie,
    imp_h = importance1,
    imp_a = importance2
  ) %>%
  inner_join(leagues_mapping) %>% 
  select(-league_id_538) %>% 
  filter(season_id %>% between(2017L, 2020L)) %>% 
  # select(-c(league_id)) %>% 
  # filter(league == 'epl') %>% 
  mutate(across(season_id, as.integer)) %>% 
  rename_with(~stringr::str_replace(.x, '1$', '_538_h'), matches('1$')) %>% 
  rename_with(~stringr::str_replace(.x, '2$', '_538_a'), matches('2$')) %>% 
  left_join(team_mapping %>% select(team_h = team, team_538_h = team_538)) %>% 
  left_join(team_mapping %>% select(team_a = team, team_538_a = team_538)) %>% 
  select(-matches('^team_538')) %>% 
  rename(prob_h = prob_538_h, prob_a = prob_538_a, prob_d = prob_d_538) %>% 
  select(season_id, league, date, matches('^team_'), matches('^score_'), matches('imp'), matches('^prob'))
probs_init

# xg_probs <- xg %>% full_join(probs_init %>% rename(date2 = date, league2 = league))
xg_probs <- xg %>% full_join(probs_init %>% select(-c(date, league)))
xg_probs
# xg %>% filter(team_h == 'Arsenal', team_a == 'Everton', season_id == 2020)
# write_rds(xg_probs, path_df)

# # Debug mis-match with teams.
# xg_probs %>% filter(is.na(prob_h)) %>% distinct(match_id, team_h) %>% count(team_h, sort = TRUE)
# xg_probs %>% filter(is.na(prob_a)) %>% distinct(match_id, team_a) %>% count(team_a, sort = TRUE)

# TODO: Add number of men on field and number of subs remaining.
f_rename_xg <- function(side) {
  suffix <- sprintf('_%s$', side)
  side_opp <- ifelse(side == 'h', 'a', 'h')
  suffix_opp <- sprintf('_%s$', side_opp)
  col_team <- sprintf('team_%s', side)
  col_team_sym <- sym(col_team)
  xg_probs %>% 
    # filter(side == !!side) %>% 
    filter(!!col_team_sym == team) %>% 
    rename_with(~str_replace(.x, suffix, ''), c(matches(suffix), -matches('^team_'))) %>% 
    rename_with(~str_replace(.x, suffix_opp, '_opp'), c(matches(suffix_opp), -matches('^team_'))) %>% 
    mutate(
      team = if_else(side == 'h', team_h, team_a),
      team_opp = if_else(side == 'a', team_a, team_h)
    )
}

minute_max <- 110L # xg_probs %>% slice_max(minute, with_ties = FALSE) %>% pull(minute)
match_mins <- match_ids %>% crossing(tibble(minute = 0L:minute_max)) %>% crossing(tibble(side = c('h', 'a')))
match_mins
# # Can I determine first or second half?
# xg_probs %>% filter(minute == 46L) %>% filter(result == 'Goal') %>% arrange(desc(date))

# exp_decay <- pi
exp_decay <- 2
df <-
  bind_rows(f_rename_xg('h'), f_rename_xg('a')) %>% 
  # full_join(match_mins) %>% 
  arrange(league, season_id, match_id, minute, team) %>% 
  # fill(-c(is_shot), .direction = 'downup') %>% 
  # replace_na(list(is_shot = FALSE)) %>% 
  mutate(
    idx = row_number(),
    across(c(season_id, match_id, minute), as.integer),
    target = case_when(
      score_538 == score_538_opp ~ 0L,
      score_538 < score_538_opp ~ -1L,
      score_538 > score_538_opp ~ 1L
    ),
    is_h = if_else(side == 'h', 1L, 0L),
    gd = g - g_opp,
    xgd = xg - xg_opp,
    wt_decay = minute / !!minute_max,
    prob_d = (1 - prob - prob_opp) * exp(-!!exp_decay * wt_decay)
  ) %>% 
  mutate(
    across(c(prob, prob_opp, imp, imp_opp), ~{.x * (exp(-!!exp_decay * wt_decay))}),
    # xgd_ratio = xgd / (exp(-pi * wt_decay))
    gd_wt = gd / (exp(-!!exp_decay * wt_decay))
  ) %>% 
  select(
    target,
    idx,
    league,
    season_id,
    match_id,
    date,
    team_h,
    team_a,
    team,
    player,
    result,
    minute,
    is_h,
    is_shot,
    matches('^x?g'),
    matches('prob'),
    matches('wt$')
  )
df

# df %>%
#   filter(match_id == first(match_id)) %>%
#   select(-c(gd, xgd)) %>% 
#   pivot_longer(
#     matches('prob')
#     # matches('^x?g')
#   ) %>%
#   ggplot() +
#   aes(x = minute, y = value, color = name) +
#   geom_step() +
#   facet_wrap(~is_h)
# df %>% filter(minute == 100L) %>% distinct()
arrow::write_parquet(df, path_df)

df_tst_dummy <-
  crossing(
    is_h = c(0L, 1L),
    g = seq.int(0, 3),
    g_opp = seq.int(0, 3),
    xg = seq(0, 3, by = 0.25),
    xg_opp = seq(0, 3, by = 0.25)
  )
df_tst_dummy
