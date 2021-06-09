
# setup
library(tidyverse)
dir_proj <- '31-wp_soccer'

# inputs
path_shots <- file.path(dir_proj, 'shots.rds')
path_matches <- file.path(dir_proj, 'matches.rds')
path_team_players_stats <- file.path(dir_proj, 'team_players_stats.rds')
path_spi <- file.path(dir_proj, 'spi_538.csv')
path_matches <- file.path(dir_proj, 'matches.rds')

f_read_opta <- function(x) {
  file.path(dir_proj, sprintf('201718-202021_epl_%s.parquet', x)) %>% 
    arrow::read_parquet()
}

# outputs
path_df <- file.path(dir_proj, 'model_data.parquet')

# setup ----
leagues_mapping <-
  tibble(
    league = c('epl'),
    league_understat = c('EPL'),
    competition_id_opta = 8L,
    league_id_538 = c(2411L)
  ) %>% 
  crossing(season = 2017L:2020L)
leagues_mapping

team_mapping <-
  xengagement::team_accounts_mapping %>% 
  select(team, team_538, team_understat, team_opta = team_whoscored)
team_mapping

# understat ----
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
  group_by(league, season, date, match_id, team_h, team_a, minute) %>% 
  mutate(idx_intra = row_number(xg_h + xg_a + g_h + g_a)) %>% 
  slice_max(idx_intra) %>% 
  ungroup() %>% 
  select(
    # Add team back in and don't do redux
    team, side,
    league, season, date, match_id, team_h, team_a, minute,
    matches(rgx_xg_cumu)
  )
xg_cumu_init

f_filter_rename_xg <- function(.side) {
  suffix <- sprintf('_%s$', .side)
  side_opp <- ifelse(.side == 'h', 'a', 'h')
  suffix_opp <- sprintf('_%s$', side_opp)
  xg_cumu_init %>% 
    # filter(!!col_team_sym == team) %>% 
    filter(side == .side) %>% 
    rename_with(~str_replace(.x, suffix, ''), c(matches(suffix), -matches('^team_'))) %>% 
    rename_with(~str_replace(.x, suffix_opp, '_opp'), c(matches(suffix_opp), -matches('^team_')))
}

xg_cumu_redux <-
  bind_rows(f_filter_rename_xg('h'), f_filter_rename_xg('a')) %>% 
  arrange(league, season, date, match_id, minute, team)
xg_cumu_redux

if(FALSE) {
  xg_cumu_h <-
    xg_cumu_init %>% 
    rename(
      xg = xg_h,
      xg_opp = xg_a,
      g = g_h,
      g_opp = g_a
    )
  xg_cumu_h
  
  xg_cumu_redux <-
    bind_rows(
      xg_cumu_h %>% mutate(team = team_h, side = 'h'),
      xg_cumu_h %>% 
        mutate(team = team_a, side = 'a') %>% 
        rename_with(~sprintf('%s_z', .x), c(matches(rgx_xg_cumu), -matches(sprintf('%s.*_opp$', rgx_xg_cumu)))) %>% 
        rename_with(~str_remove(.x, '_opp'), c(matches(sprintf('%s.*_opp$', rgx_xg_cumu)))) %>% 
        rename_with(~str_replace(.x, '_z', '_opp'), c(matches(sprintf('%s.*_z$', rgx_xg_cumu))))
    ) %>% 
    arrange(league, season, date, match_id, minute, team)
  xg_cumu_redux
}
# ws ----
fix_opta_meta <- function(data) {
  data %>% 
    rename(season = season_id, competition_id_opta = competition_id) %>% 
    inner_join(leagues_mapping %>% select(league, competition_id_opta, season)) %>% 
    select(-competition_id_opta)
}

events_init <- 
  f_read_opta('events') %>% 
  fix_opta_meta() %>% 
  rename_all(~str_remove(.x, '_name'))
events_init

meta_init <- 
  f_read_opta('meta') %>% 
  fix_opta_meta()
meta_init

meta <-
  meta_init %>% 
  mutate(across(start_date, ~lubridate::ymd_hms(.x) %>% lubridate::date())) %>% 
  mutate(has_attendance = attendance > 0L) %>% 
  select(match_id, date = start_date, has_attendance, max_minute)
meta
# meta %>% filter(!has_attendance)
# meta %>% filter(match_id == 1284763L)
# events_init %>% filter(date == '2018-08-25', team_h == 'Fulham')

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
  rename(team_opta_h = team_home, team_opta_a = team_away) %>% 
  left_join(team_mapping %>% select(team_h = team, team_opta_h = team_opta)) %>% 
  left_join(team_mapping %>% select(team_a = team, team_opta_a = team_opta)) %>% 
  select(-matches('_opta'))
events_teams
# events_teams %>% skimr::skim()

add_time_col <- function(data) {
  data %>% 
    mutate(time = sprintf('%s:%s', expanded_minute, second) %>% lubridate::ms() %>% as.double())
}

events <- 
  events_init %>% 
  rename(team_opta = team) %>% 
  left_join(team_mapping %>% select(team, team_opta = team_opta)) %>% 
  select(-team_opta) %>% 
  mutate(across(side, ~case_when(.x == 'home' ~ 'h', .x == 'away' ~ 'a'))) %>% 
  left_join(meta) %>% 
  mutate(
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

f_filter_rename_opta <- function(.side) {
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
  bind_rows(f_filter_rename_opta('h'), f_filter_rename_opta('a')) %>% 
  arrange(league, season, date, match_id, time, team)
events

# Avoid the name conflict with understat shots.
# shots_opta <- events %>% filter(type %>% str_detect('Shot|Goal'))
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
# send_offs_agg %>% filter(which %>% str_detect(','))

events_opta <-
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
events_opta

# This is hard basically because we have to generate the unique match_id-minute combinations ourselves.
# Will join back league, season, etc. at the end of this data processing.
rgx_opta_cumu <- '^(sub|cards|send)'
events_opta_cumu_init <-
  events_opta %>% 
  group_by(match_id, date, has_attendance, max_minute, team, team_opp, team_h, team_a, side, minute) %>% 
  summarize(across(matches(rgx_opta_cumu), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(match_id, date, team, minute) %>% 
  group_by( match_id, date, has_attendance, max_minute, team, team_opp, team_h, team_a, side) %>% 
  mutate(across(matches(rgx_opta_cumu), cumsum)) %>% 
  ungroup()
events_opta_cumu_init


f_filter_rename_opta_cumu <- function(.side) {
  suffix <- sprintf('_%s$', .side)
  side_opp <- ifelse(.side == 'h', 'a', 'h')
  suffix_opp <- sprintf('_%s$', side_opp)
  events_opta_cumu_init %>% 
    filter(side == .side) %>% 
    # select(match_id, minute, team, matches(rgx_opta_cumu)) %>% 
    # rename_with(~sprintf('%s_%s', .x, .side), -c(match_id, minute))
    rename_with(~str_replace(.x, suffix, ''), c(matches(suffix), -matches('^team_'))) %>% 
    rename_with(~str_replace(.x, suffix_opp, '_opp'), c(matches(suffix_opp), -matches('^team_')))
}

events_opta_cumu_redux <-
  bind_rows(f_filter_rename_opta_cumu('h'), f_filter_rename_opta_cumu('h')) %>% 
  # arrange(season, date, match_id, minute, team)
  left_join(events_opta %>% distinct(league, season, date, match_id, has_attendance, max_minute)) %>% 
  arrange(league, season, date, match_id, minute, team)
events_opta_cumu_redux

if(FALSE) {
  f_filter_rename_opta_cumu <- function(.side) {
    events_opta_cumu_init %>% 
      filter(side == .side) %>% 
      select(match_id, minute, team, matches(rgx_opta_cumu)) %>% 
      rename_with(~sprintf('%s_%s', .x, .side), -c(match_id, minute))
  }
  
  events_opta_cumu <-
    events_opta_cumu_init %>% 
    select(match_id, team_h, team_a, minute) %>% 
    left_join(f_filter_rename_opta_cumu('h')) %>% 
    left_join(f_filter_rename_opta_cumu('a')) %>% 
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
    select(-is_h, -matches(sprintf('%s.*_[ha]$', rgx_opta_cumu)))
  events_opta_cumu
  
  events_opta_cumu_redux <-
    bind_rows(
      events_opta_cumu %>% mutate(side = 'h'),
      events_opta_cumu %>% 
        mutate(team = team_a, side = 'a') %>% 
        rename_with(~sprintf('%s_z', .x), c(matches(rgx_opta_cumu), -matches(sprintf('%s.*_opp$', rgx_opta_cumu)))) %>% 
        rename_with(~str_remove(.x, '_opp'), c(matches(sprintf('%s.*_opp$', rgx_opta_cumu)))) %>% 
        rename_with(~str_replace(.x, '_z', '_opp'), c(matches(sprintf('%s.*_z$', rgx_opta_cumu))))
    ) %>% 
    left_join(events_opta %>% distinct(league, season, date, match_id, has_attendance, max_minute)) %>% 
    arrange(league, season, date, match_id, minute, team)
  events_opta_cumu_redux
  df_intra_game <-
    bind_rows(
      xg_cumu_redux %>% select(-match_id),
      events_opta_cumu_redux %>% select(-match_id)
    ) %>% 
    arrange(league, season, date, minute, team_h, team_a, team) %>% 
    group_by(league, season, date, team_h, team_a, team) %>% 
    fill(matches('.*'), .direction = 'down') %>% 
    ungroup() %>% 
    group_by(league, season, date, team_h, team_a) %>% 
    fill(has_attendance, max_minute, .direction = 'updown') %>% 
    ungroup() %>% 
    mutate(
      idx = row_number(),
      across(where(is.integer), ~coalesce(.x, 0L)),
      across(matches(rgx_xg_cumu), ~coalesce(.x, 0))
    ) %>% 
    relocate(idx)
  df_intra_game
}
# # 2 red card instances
# events_opta_cumu_redux %>%
#   filter(send_off == 2L) %>% 
#   group_by(match_id) %>% 
#   slice_min(minute, with_ties = FALSE) %>% 
#   ungroup()

df_intra_game <-
  bind_rows(
    xg_cumu_redux %>% select(-match_id), #  rename(match_id_understat = match_id),
    events_opta_cumu_redux %>% select(-c(match_id, team_opp)) # rename(match_id_opta = match_id)
  ) %>% 
  arrange(league, season, date, minute, team_h, team_a, team) %>% 
  group_by(league, season, date, team_h, team_a, team) %>% 
  fill(matches('.*'), .direction = 'down') %>% 
  ungroup() %>% 
  # group_by(league, season, date, team_h, team_a, team) %>% 
  fill(has_attendance, max_minute, .direction = 'up') %>% 
  ungroup() %>% 
  mutate(
    idx = row_number(),
    across(where(is.integer), ~coalesce(.x, 0L)),
    across(matches(rgx_xg_cumu), ~coalesce(.x, 0))
  ) %>% 
  relocate(idx)
df_intra_game
# assertthat::assert_that(0 == df_intra_game %>% filter(is.na(has_attendance)) %>% nrow())

# # I'm missing some games?
# events_opta_cumu_redux %>% filter(season == 2020L, team_h == 'Man United') %>% distinct(match_id, team_h, team_a)
# events_init %>% distinct(match_id) %>% filter(match_id %>% between(1485306 - 4, 1485306 + 4))
# events_init %>% filter(match_id == 1485306)
# events %>% filter(match_id == 1485306)
# df_intra_game %>% filter(is.na(has_attendance)) %>% count(date, team_h, team_a)
# df_intra_game %>% filter(is.na(has_attendance)) %>% count(date, team_h, team_a)

# 538 ----
probs <-
  path_spi %>%
  read_csv() %>% 
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
  rename(prob_h = prob_538_h, prob_a = prob_538_a, prob_d = prob_d_538, score_h = score_538_h, score_a = score_538_a) %>% 
  select(league, season, date, matches('^team_'), matches('^score_'), matches('imp'), matches('^prob'))
probs

# Check bad dates.
# df_intra_game %>%
#   anti_join(probs) %>%
#   distinct(league, season, date, team_h, team_a)

rgx_538 <- '^(prob|score|imp)'
df_all <-
  df_intra_game %>% 
  select(-date) %>% 
  left_join(probs) %>% 
  relocate(date, .after = season) %>% 
  mutate(
    is_h = side == 'h',
    prob = if_else(is_h, prob_h, prob_a),
    prob_opp = if_else(is_h, prob_a, prob_h),
    imp = if_else(is_h, imp_h, imp_a),
    imp_opp = if_else(is_h, imp_a, imp_h),
    score = if_else(is_h, score_h, score_a),
    score_opp = if_else(is_h, score_a, score_h)
  ) %>% 
  select(-is_h, -matches(sprintf('%s.*_[ha]$', rgx_538)))
df_all

df  <-
  df_all %>% 
  arrange(league, season, date, team_h, team_a, minute, team) %>% 
  mutate(
    idx = row_number(),
    across(has_attendance, as.integer),
    across(c(season, minute), as.integer),
    target = case_when(
      score == score_opp ~ 0L,
      score < score_opp ~ -1L,
      score > score_opp ~ 1L
    ),
    is_h = if_else(side == 'h', 1L, 0L),
    gd = g - g_opp,
    xgd = xg - xg_opp
  ) %>% 
  relocate(
    target,
    idx,
    league,
    season,
    date,
    team_h,
    team_a,
    matches('^score'),
    side,
    team,
    max_minute,
    minute,
    is_h
  )
df
df %>% skimr::skim()
arrow::write_parquet(df, path_df)

# data checking ----
# df %>% 
#   filter(is.na(imp)) %>% 
#   distinct(league, season, date, team_h, team_a)
