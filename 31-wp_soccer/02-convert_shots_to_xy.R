
# setup
library(tidyverse)
dir_proj <- '31-wp_soccer'

# inputs
path_shots <- file.path(dir_proj, 'shots.rds')
path_matches <- file.path(dir_proj, 'matches.rds')
path_team_players_stats <- file.path(dir_proj, 'team_players_stats.rds')
path_spi <- file.path(dir_proj, 'spi_538.csv')
path_matches <- file.path(dir_proj, 'matches.rds')

f_read_ws <- function(x) {
  file.path(dir_proj, sprintf('201718-202021_epl_%s.parquet', x)) %>% 
    arrow::read_parquet()
}

# outputs
path_df <- file.path(dir_proj, 'model_data.parquet')

# understat ----
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

fix_understat_league_col <- function(data) {
  data %>% 
    rename(league_understat = league) %>% 
    inner_join(leagues_mapping %>% select(league, league_understat)) %>% 
    select(-league_understat)
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
  filter_understat() %>% 
  fix_understat_league_col()
team_players_stats

matches <-
  path_matches %>% 
  read_rds() %>% 
  fix_understat_league_col()
matches

match_ids <-
  matches %>% 
  rename(season_id = season) %>% 
  # filter_understat() %>% 
  distinct(league, season_id, match_id)
match_ids

shots_init <- 
  path_shots %>% 
  read_rds()
shots_init

shots <- 
  shots_init %>% 
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

# 538 ----
filter_538 <- function(data) {
  data %>%
    filter(league_id == 2411L, season %in% 2017L:2020L)
}
probs_init <-
  path_spi %>%
  read_csv() %>% 
  filter_538() %>% 
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
  # filter(season_id %>% between(2017L, 2020L)) %>% 
  # select(-c(league_id)) %>% 
  # filter(league == 'epl') %>% 
  mutate(across(season_id, as.integer)) %>% 
  rename_with(~stringr::str_replace(.x, '1$', '_538_h'), matches('1$')) %>% 
  rename_with(~stringr::str_replace(.x, '2$', '_538_a'), matches('2$')) %>% 
  left_join(team_mapping %>% select(team_h = team, team_538_h = team_538)) %>% 
  left_join(team_mapping %>% select(team_a = team, team_538_a = team_538)) %>% 
  select(-matches('^team_538')) %>% 
  rename(prob_h = prob_538_h, prob_a = prob_538_a, prob_d = prob_d_538) %>% 
  select(league, season_id, date, matches('^team_'), matches('^score_'), matches('imp'), matches('^prob'))
probs_init

xg_probs <- xg %>% full_join(probs_init %>% select(-c(date, league)))
xg_probs

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
# match_mins <- match_ids %>% crossing(tibble(minute = 0L:minute_max)) %>% crossing(tibble(side = c('h', 'a')))
# # Can I determine first or second half?
# xg_probs %>% filter(minute == 46L) %>% filter(result == 'Goal') %>% arrange(desc(date))

# exp_decay <- pi
exp_decay <- 2
df_understat_538 <-
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
    side,
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
df_understat_538

# df_understat_538 %>%
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
# df_understat_538 %>% filter(minute == 100L) %>% distinct()
# arrow::write_parquet(df_understat_538, path_df)
# 
# df_tst_dummy <-
#   crossing(
#     is_h = c(0L, 1L),
#     g = seq.int(0, 3),
#     g_opp = seq.int(0, 3),
#     xg = seq(0, 3, by = 0.25),
#     xg_opp = seq(0, 3, by = 0.25)
#   )
# df_tst_dummy
# ws ----
events_init <- f_read_ws('events')
meta <- 
  f_read_ws('meta') %>% 
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
  arrange(competition_id, season_id, match_id, date, time, team) %>% 
  mutate(idx = row_number()) %>% 
  relocate(idx, competition_id, season_id, match_id, date, time)
events
# events %>% filter(is.na(time))

f_rename_ws <- function(side) {
  suffix <- sprintf('_%s$', side)
  side_opp <- ifelse(side == 'h', 'a', 'h')
  suffix_opp <- sprintf('_%s$', side_opp)
  col_team <- sprintf('team_%s', side)
  col_team_sym <- sym(col_team)
  events %>% 
    filter(!!col_team_sym == team) %>% 
    mutate(
      team = if_else(side == 'h', team_h, team_a),
      team_opp = if_else(side == 'h', team_a, team_h)
    ) %>% 
    select(
      idx,
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
      related_player_id,
      type,
      outcome_type,
      card_type
    )
}

events <- 
  bind_rows(f_rename_ws('h'), f_rename_ws('a')) %>% 
  arrange(competition_id, season_id, date, match_id, time, team)
events

# Avoid the name conflict with understat shots.
shots_ws <-
  events %>% 
  filter(type %>% str_detect('Shot|Goal'))
shots_ws

if(FALSE) {
  full_join(
    shots_ws %>% 
      mutate(idx1 = row_number()) %>% 
      select(idx1, season_id, date, team_h, team_a, team, side, minute, expanded_minute, second, type_ws = type),
    df_understat_538 %>%
      mutate(idx2 = row_number()) %>% 
      select(idx2, season_id, date, team_h, team_a, team, side, minute, xg, type_understat = result)
  ) %>% 
    relocate(idx1, idx2) %>% 
    arrange(idx1, idx2) %>% 
    group_by(idx1) %>% 
    slice_min(idx2) %>% 
    ungroup()
  
  shots_ws %>% filter(match_id == 1375927L) %>% arrange(time)
  df_understat_538 %>% filter(match_id == 11643L) %>% filter(is_shot) %>% arrange(minute)
}

subs_off <-
  events %>% 
  # filter(type %>% str_detect('Substitution'))
  filter(type == 'SubstitutionOff') %>% 
  mutate(sub = 1L)
subs_off

# subs_off_cumu <-
#   subs_off %>% 
#   group_by(match_id, team) %>% 
#   mutate(sub = cumsum(sub)) %>% 
#   ungroup()
# subs_off_cumu
# subs %>% filter(type == 'SubstitutionOff' & lead(type) != 'SubstitutionOn')
# events %>% filter(idx %>% between (1006176, 1006185))

cards <- events %>% filter(type == 'Card')
cards

cards_y <- cards %>% filter(card_type == 'Yellow') %>% mutate(cards_y = 1L) 
cards_y

# cards_y_cumu <-
#   cards_y %>% 
#   group_by(match_id, team) %>% 
#   mutate(cards_y = cumsum(cards_y)) %>% 
#   ungroup()
# cards_y_cumu

cards_r <- cards %>% filter(card_type == 'Red') %>% mutate(cards_r = 1L)
cards_r

# cards_r_cumu <-
#   cards_r %>% 
#   group_by(match_id, team) %>% 
#   mutate(cards_r = cumsum(cards_r)) %>% 
#   ungroup()
# cards_r_cumu %>% filter(cards_r > 1L)

cards_agg <-
  cards %>% 
  group_by(match_id, player) %>% 
  summarize(n = n(), minute = max(minute)) %>% 
  ungroup()
cards_agg

cards_double <- cards_agg %>% filter(n > 1L)
cards_double

send_offs <-
  full_join(
    cards_red %>% drop_na(player) %>% distinct(match_id, player, minute) %>% mutate(which = 'red'),
    cards_double %>% select(match_id, player, minute) %>% mutate(which = 'double')
  )
send_offs

cards_non_send_offs <-
  cards %>% 
  anti_join(send_offs)
cards_non_send_offs

events_ws <-
  bind_rows(
    subs_off,
    cards_y,
    cards_r
  ) %>% 
  arrange(idx) %>%
  select(
    competition_id,
    season_id,
    match_id,
    date,
    time,
    team,
    team_opp,
    team_h,
    team_a,
    side,
    minute,
    sub,
    cards_y,
    cards_r
  )
events_ws

subs_off %>% filter(side == 'a') %>% filter(match_id == first(match_id))
events_ws %>% filter(side == 'a') %>% filter(match_id == first(match_id))
cards_y %>% filter(match_id == 1190174)

events_ws_agg <-
  events_ws %>% 
  group_by(competition_id, season_id, match_id, date, team, team_opp, team_h, team_a, side, minute) %>% 
  summarize(across(c(sub, cards_y, cards_r), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(competition_id, season_id, match_id, date, team, minute) %>% 
  group_by(competition_id, season_id, match_id, date, team, team_opp, team_h, team_a, side) %>% 
  mutate(across(c(sub, cards_y, cards_r), cumsum)) %>% 
  ungroup()
events_ws_agg

events_ws_redux <-
  events_ws_agg %>% 
  select(competition_id, season_id, match_id, team_h, team_a, minute) %>% 
  left_join(
    events_ws_agg %>% 
      filter(side == 'h') %>% 
      select(match_id, minute, team, sub, cards_y, cards_r) %>% 
      rename_with(~sprintf('%s_h', .x), c(team, sub, cards_y, cards_r))
  ) %>% 
  left_join(
    events_ws_agg %>% 
      filter(side == 'a') %>% 
      select(match_id, minute, team, sub, cards_y, cards_r) %>% 
      rename_with(~sprintf('%s_a', .x), c(team, sub, cards_y, cards_r))
  ) %>% 
  arrange(competition_id, season_id, match_id, minute, team_h) %>% 
  group_by(match_id) %>% 
  fill(matches('.*')) %>% 
  ungroup() %>% 
  mutate(across(where(is.integer), ~replace_na(.x, 0L))) %>% 
  mutate(
    is_h = !is.na(sub_h),
    team = if_else(is_h, team_h, team_a),
    sub = if_else(is_h, sub_h, sub_a),
    sub_opp = if_else(is_h, sub_a, sub_h),
    cards_y = if_else(is_h, cards_y_h, cards_y_a),
    cards_y_opp = if_else(is_h, cards_y_a, cards_y_h),
    cards_r = if_else(is_h, cards_r_h, cards_r_a),
    cards_r_opp = if_else(is_h, cards_r_a, cards_r_h)
  )
events_ws_redux

full_join(
  events_ws %>% filter(side == 'h'),
  events_ws %>% filter(side == 'a')
)
