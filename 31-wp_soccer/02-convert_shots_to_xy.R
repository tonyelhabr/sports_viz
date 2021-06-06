
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
    competition_id_ws = 8L,
    league_id_538 = c(2411L)
  ) %>% 
  crossing(season = 2017L:2020L)
leagues_mapping

team_mapping <-
  xengagement::team_accounts_mapping %>% 
  select(team, team_538, team_understat, team_ws = team_whoscored)
team_mapping

fix_understat_meta <- function(data) {
  data %>% 
    rename(league_understat = league) %>% 
    inner_join(leagues_mapping %>% select(league, league_understat, season)) %>% 
    select(-league_understat) %>% 
    relocate(league, season)
}

team_players_stats <- 
  path_team_players_stats %>% 
  read_rds() %>%
  select(
    league = league_name,
    season = year,
    player = player_name,
    team = team_name
  ) %>% 
  fix_understat_meta()
team_players_stats

matches <-
  path_matches %>% 
  read_rds() %>% 
  fix_understat_meta()
matches

match_ids <-
  matches %>% 
  distinct(league, season, match_id)
match_ids

shots <- 
  path_shots %>% 
  read_rds() %>% 
  inner_join(match_ids) %>% 
  mutate(
    across(date, lubridate::date),
    across(c(season, match_id, minute), as.integer)
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
    # side,
    team_h = h_team,
    team_a = a_team, 
    side = h_a,
    g_h_final = h_goals,
    g_a_final = a_goals
  )
shots

# scorelines <-
#   shots %>%
#   distinct(league, season, date, match_id, team_h, team_a, g_h = g_h_final, g_a = g_a_final)
# scorelines

shots_w_teams <-
  shots %>% 
  select(-id, -matches('_final$')) %>% 
  left_join(team_players_stats)
shots_w_teams

shots_w_teams_0 <-
  shots_w_teams %>% 
  filter(minute == 0L) %>% 
  distinct(league, season, date, match_id, .keep_all = TRUE)
shots_w_teams_0

rgx_xg_cumu <- '^x?g'
xg_cumu_init <-
  # # TBH I don't think i even need this scorelines stuff anymore.
  # bind_rows(
  #   # scorelines %>% anti_join(shots_w_teams_0) %>% mutate(minute = 0L, side = 'h', is_shot = FALSE), 
  #   # scorelines %>% anti_join(shots_w_teams_0) %>% mutate(minute = 0L, side = 'a', is_shot = FALSE), 
  #   shots_w_teams %>% mutate(is_shot = TRUE)
  # ) %>% 
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
      )
  ) %>% 
  group_by(league, season, match_id) %>%
  mutate(
    across(matches('x?g_[ha]$'), cumsum)
  ) %>%
  ungroup() %>%
  select(-xg) %>% 
  rename(team_understat_h = team_h, team_understat_a = team_a) %>% 
  left_join(team_mapping %>% select(team_h = team, team_understat_h = team_understat)) %>% 
  left_join(team_mapping %>% select(team_a = team, team_understat_a = team_understat)) %>% 
  select(-matches('^team_understat')) %>%  
  # Aggregate by last in minute. 
  group_by(league, season, date, match_id, team_h, team_a, minute, side, team) %>% 
  mutate(idx_intra = row_number(xg_h + xg_a + g_h + g_a)) %>% 
  slice_max(idx_intra) %>% 
  ungroup() %>% 
  select(
    league, season, date, match_id, team_h, team_a, minute, side, team,
    matches(rgx_xg_cumu)
  )
xg_cumu_init

f_rename_xg_cumu <- function(.side) {
  xg_cumu_init %>% 
    filter(side == .side) %>% 
    select(match_id, minute, team, matches(rgx_xg_cumu)) %>% 
    rename_with(~sprintf('%s_%s', .x, .side), -c(match_id, minute))
}

xg_cumu <-
  xg_cumu_init %>% 
  select(match_id, team_h, team_a, minute) %>% 
  left_join(f_rename_xg_cumu('h')) %>% 
  left_join(f_rename_xg_cumu('a')) %>% 
  arrange(match_id, minute, team_h) %>% 
  group_by(match_id) %>% 
  fill(matches('.*')) %>% 
  ungroup() %>% 
  mutate(
    across(where(matches('^g')), ~replace_na(.x, 0L)),
    across(where(matches('^xg')), ~coalesce(.x, 0))
  )
xg_cumu


# old?
f_rename_xg <- function(.side) {
  suffix <- sprintf('_%s$', .side)
  side_opp <- ifelse(.side == 'h', 'a', 'h')
  suffix_opp <- sprintf('_%s$', side_opp)
  col_team <- sprintf('team_%s', .side)
  col_team_sym <- sym(col_team)
  xg %>% 
    filter(!!col_team_sym == team) %>% 
    rename_with(~str_replace(.x, suffix, ''), c(matches(suffix), -matches('^team_'))) %>% 
    rename_with(~str_replace(.x, suffix_opp, '_opp'), c(matches(suffix_opp), -matches('^team_')))
}

xg_redux_init <-
  bind_rows(f_rename_xg('h'), f_rename_xg('a')) %>% 
  arrange(league, season, date, match_id, minute, team)
xg_redux_init


xg_redux <-
  bind_rows(
    xg_redux_init %>% mutate(team = team_h),
    xg_redux_init %>% 
      mutate(team = team_a) %>% 
      rename_with(~sprintf('%s_z', .x), c(matches(rgx_xg_cumu), -matches(sprintf('%s.*_opp$', rgx_xg_cumu)))) %>% 
      rename_with(~str_remove(.x, '_opp'), c(matches(sprintf('%s.*_opp$', rgx_xg_cumu)))) %>% 
      rename_with(~str_replace(.x, '_z', '_opp'), c(matches(sprintf('%s.*_z$', rgx_xg_cumu))))
  ) %>% 
  arrange(league, season, date, match_id, minute, team)
xg_redux

# ws ----
fix_ws_meta <- function(data) {
  data %>% 
    rename(season = season_id, competition_id_ws = competition_id) %>% 
    inner_join(leagues_mapping %>% select(league, competition_id_ws, season)) %>% 
    select(-competition_id_ws)
}

events_init <- 
  f_read_ws('events') %>% 
  fix_ws_meta() %>% 
  rename_all(~str_remove(.x, '_name'))
events_init

meta <- 
  f_read_ws('meta') %>% 
  fix_ws_meta() %>% 
  mutate(across(start_date, ~lubridate::ymd_hms(.x) %>% lubridate::date())) %>% 
  mutate(has_attendance = attendance > 0L) %>% 
  select(match_id, date = start_date, has_attendance, max_minute)
meta
meta %>% filter(!has_attendance)

events_teams <-
  events_init %>% 
  group_by(match_id, team) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(match_id, team, side) %>% 
  pivot_wider(
    names_from = side,
    values_from = team,
    names_prefix = 'team_'
  ) %>% 
  rename(team_ws_h = team_home, team_ws_a = team_away) %>% 
  left_join(team_mapping %>% select(team_h = team, team_ws_h = team_ws)) %>% 
  left_join(team_mapping %>% select(team_a = team, team_ws_a = team_ws)) %>% 
  select(-matches('_ws'))
events_teams
events_teams %>% skimr::skim()

add_time_col <- function(data) {
  data %>% 
    mutate(time = sprintf('%s:%s', expanded_minute, second) %>% lubridate::ms() %>% as.double())
}

events <- 
  events_init %>% 
  mutate(across(side, ~case_when(.x == 'home' ~ 'h', .x == 'away' ~ 'a'))) %>% 
  left_join(meta) %>% 
  mutate(
    # across(season, ~ str_sub(.x, 1, 4) %>% as.integer()),
    across(c(minute, expanded_minute, second), as.integer),
    across(second, ~coalesce(.x, 0L))
  ) %>% 
  add_time_col() %>% 
  left_join(events_teams) %>% 
  mutate(team_opp = ifelse(team == team_h, team_a, team_h)) %>% 
  arrange(league, season, match_id, date, time, team) %>% 
  mutate(idx = row_number()) %>% 
  relocate(idx, league, season, match_id, date, time)
events

f_rename_ws <- function(.side) {
  suffix <- sprintf('_%s$', .side)
  side_opp <- ifelse(.side == 'h', 'a', 'h')
  suffix_opp <- sprintf('_%s$', side_opp)
  col_team <- sprintf('team_%s', .side)
  col_team_sym <- sym(col_team)
  events %>% 
    filter(!!col_team_sym == team) %>% 
    mutate(
      team = if_else(side == 'h', team_h, team_a),
      team_opp = if_else(side == 'h', team_a, team_h)
    ) %>% 
    select(
      idx,
      league,
      season,
      match_id,
      date,
      has_attendance, 
      max_minute,
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
  bind_rows(f_rename_ws('h'), f_rename_ws('a')) %>% 
  arrange(league, season, date, match_id, time, team)
events

# Avoid the name conflict with understat shots.
# shots_ws <- events %>% filter(type %>% str_detect('Shot|Goal'))
subs_off <- events %>% filter(type == 'SubstitutionOff') %>% mutate(sub = 1L)
cards <- events %>% filter(type == 'Card')
cards_y <- cards %>% filter(card_type == 'Yellow') %>% mutate(cards_y = 1L) 
cards_r <- cards %>% filter(card_type == 'Red') %>% mutate(cards_r = 1L)

cards_agg <-
  cards %>% 
  group_by(match_id, team, player) %>% 
  summarize(n = n(), minute = max(minute)) %>% 
  ungroup()
cards_agg

cards_double <- cards_agg %>% filter(n > 1L)

send_offs <-
  full_join(
    cards_r %>% drop_na(player) %>% distinct(match_id, team, player, minute) %>% mutate(which = 'red'),
    cards_double %>% select(match_id, team, player, minute) %>% mutate(which = 'double')
  )
send_offs

send_offs_agg <-
  send_offs %>% 
  group_by(match_id, team, player, minute) %>% 
  mutate(which = case_when(n() > 1L ~ paste(which, collapse = ', ', sep = ''), TRUE ~ which), idx_intra = row_number()) %>% 
  ungroup() %>% 
  filter(idx_intra == 1L) %>% 
  select(match_id, minute, team, player, which) %>% 
  mutate(send_off = 1L)
send_offs_agg
send_offs_agg %>% filter(which %>% str_detect(','))

events_ws <-
  bind_rows(
    subs_off,
    cards_y,
    cards_r
  ) %>% 
  arrange(idx) %>%
  select(
    idx,
    league,
    season,
    match_id,
    date,
    has_attendance, 
    max_minute,
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
  ) %>% 
  left_join(send_offs_agg %>% select(match_id, team, minute, send_off)) %>% 
  mutate(across(send_off, ~case_when(!is.na(sub) ~ NA_integer_, TRUE ~ send_off)))
events_ws

# This is hard basically because we have to generate the unique match_id-minute combinations ourselves.
# Will join back league, season, etc. at the end of this data processing.
rgx_ws_cumu <- '^(sub|cards|send)'
events_ws_cumu_init <-
  events_ws %>% 
  group_by(match_id, date, has_attendance, max_minute, team, team_opp, team_h, team_a, side, minute) %>% 
  summarize(across(matches(rgx_ws_cumu), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(match_id, date, team, minute) %>% 
  group_by( match_id, date, has_attendance, max_minute, team, team_opp, team_h, team_a, side) %>% 
  mutate(across(matches(rgx_ws_cumu), cumsum)) %>% 
  ungroup()
events_ws_cumu_init

f_rename_ws_cumu <- function(.side) {
  events_ws_cumu_init %>% 
    filter(side == .side) %>% 
    select(match_id, minute, team, matches(rgx_ws_cumu)) %>% 
    rename_with(~sprintf('%s_%s', .x, .side), -c(match_id, minute))
}

events_ws_cumu <-
  events_ws_cumu_init %>% 
  select(match_id, team_h, team_a, minute) %>% 
  left_join(f_rename_ws_cumu('h')) %>% 
  left_join(f_rename_ws_cumu('a')) %>% 
  arrange(match_id, minute, team_h) %>% 
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
    cards_r_opp = if_else(is_h, cards_r_a, cards_r_h),
    send_off = if_else(is_h, send_off_h, send_off_a),
    send_off_opp = if_else(is_h, send_off_a, send_off_h)
  ) %>% 
  select(-is_h, -matches(sprintf('%s.*_[ha]$', rgx_ws_cumu)))
events_ws_cumu

events_ws_cumu_redux <-
  bind_rows(
    events_ws_cumu %>% mutate(side = 'h'),
    events_ws_cumu %>% 
      mutate(team = team_a) %>% 
      rename_with(~sprintf('%s_z', .x), c(matches(rgx_ws_cumu), -matches(sprintf('%s.*_opp$', rgx_ws_cumu)))) %>% 
      rename_with(~str_remove(.x, '_opp'), c(matches(sprintf('%s.*_opp$', rgx_ws_cumu)))) %>% 
      rename_with(~str_replace(.x, '_z', '_opp'), c(matches(sprintf('%s.*_z$', rgx_ws_cumu)))) %>% 
      mutate(side = 'a')
  ) %>% 
  left_join(events_ws %>% distinct(league, season, date, match_id, has_attendance, max_minute)) %>% 
  arrange(league, season, date, match_id, minute, team)
events_ws_cumu_redux

# # 2 red card instances
# events_ws_cumu_redux %>%
#   filter(send_off == 2L) %>% 
#   group_by(match_id) %>% 
#   slice_min(minute, with_ties = FALSE) %>% 
#   ungroup()

df_intra_game <-
  bind_rows(
    # xg_probs %>% select(-c(league, match_id)),
    xg %>% select(-c(match_id)),
    events_ws_cumu_redux %>% select(-c(match_id))
  ) %>% 
  arrange(league, season, date, minute, team_h, team_a, team) %>% 
  group_by(league, season, date, team_h, team_a, team) %>% 
  fill(matches('.*')) %>% 
  ungroup() %>% 
  group_by(league, season, date, team_h, team_a) %>% 
  fill(has_attendance, max_minute, .direction = 'up') %>% 
  ungroup() %>% 
  mutate(
    across(where(is.integer), ~coalesce(.x, 0L)),
    # across(where(~is.double(.x) & !lubridate::is.Date(.x)), ~coalesce(.x, 0))
    across(matches('^x?g'), ~coalesce(.x, 0))
  )
df_intra_game
# df_intra_game %>% count(team = team_h) %>% filter(is.na(team))
# df_intra_game %>% count(team = team_a) %>% filter(is.na(team))
# df_intra_game %>% filter(is.na(date))

# 538 ----
# filter_538 <- function(data) {
#   data %>%
#     filter(league_id == 2411L, season %in% 2017L:2020L)
# }
probs <-
  path_spi %>%
  read_csv() %>% 
  # filter_538() %>% 
  select(-league) %>% 
  mutate(across(season, as.integer)) %>% 
  rename(league_id_538 = league_id) %>% 
  inner_join(leagues_mapping) %>% 
  select(-league_id_538) %>% 
  rename(
    # league_id_538 = league_id,
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
  rename_with(~stringr::str_replace(.x, '1$', '_538_h'), matches('1$')) %>% 
  rename_with(~stringr::str_replace(.x, '2$', '_538_a'), matches('2$')) %>% 
  left_join(team_mapping %>% select(team_h = team, team_538_h = team_538)) %>% 
  left_join(team_mapping %>% select(team_a = team, team_538_a = team_538)) %>% 
  select(-matches('^team_538')) %>% 
  rename(prob_h = prob_538_h, prob_a = prob_538_a, prob_d = prob_d_538) %>% 
  select(league, season, date, matches('^team_'), matches('^score_'), matches('imp'), matches('^prob'))
probs

# xg_probs <- xg %>% full_join(probs %>% select(-c(date, league)))
# xg_probs
probs %>% filter(date == '2017-08-12')
team_mapping

# 538 has the Fulham Burnley 2018-08-26 game correct, while other data has it on 2018-08-25
df_intra_game %>% 
  select(-date) %>% 
  left_join(probs) %>% 
  relocate(date, .after = season)

df_intra_game %>% 
  anti_join(probs) %>% 
  # count(team = team_a, sort = TRUE) %>% 
  # filter(date == '2017-08-12') %>% 
  distinct(league, season, date, team_h, team_a)

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
xg_probs_redux <-
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
    xgd = xg - xg_opp
  )
xg_probs_redux

df <-
  xg_probs_redux %>% 
  mutate(
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
df

