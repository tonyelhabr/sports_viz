
library(tidyverse)
dir_proj <- '24-202021_game_state_fouls'
path_events <- file.path(dir_proj, 'events.rds')
path_meta <- file.path(dir_proj, 'meta.rds')

leagues_mapping <-
  tibble(
    league = c('epl'),
    league_understat = c('EPL'),
    league_whoscored = c('England-Premier-League'),
    league_538 = c('Barclays Premier League')
  )

team_mapping <-
  xengagement::team_accounts_mapping %>% 
  select(team, team_538, team_whoscored)
team_mapping

probs_init <-
  read_csv(
    'https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv'
  ) %>% 
  rename(league_538 = league, team_538_h = team1, team_538_a = team2, probtie_538 = probtie) %>% 
  left_join(leagues_mapping) %>% 
  select(-league_538) %>% 
  filter(league == 'epl' & season >= 2019) %>% 
  select(-c(league_id)) %>% 
  mutate(across(season, as.integer)) %>% 
  rename_with(~stringr::str_replace(.x, '1$', '_538_h'), matches('1$')) %>% 
  rename_with(~stringr::str_replace(.x, '2$', '_538_a'), matches('2$')) %>% 
  left_join(team_mapping %>% select(team_h = team, team_538_h = team_538)) %>% 
  left_join(team_mapping %>% select(team_a = team, team_538_a = team_538)) %>% 
  rename(prob_h = prob_538_h, prob_a = prob_538_a, probtie = probtie_538) %>% 
  select(season, date, matches('^team_'), matches('^score_'), matches('^prob'))
probs_init

.f_rename_538 <- function(side) {
  suffix <- sprintf('_%s$', side)
  side_opp <- ifelse(side == 'h', 'a', 'h')
  suffix_opp <- sprintf('_%s$', side_opp)
  probs_init %>% 
    rename_with(~str_replace(.x, suffix, ''), c(matches(suffix))) %>% 
    rename_with(~str_replace(.x, suffix_opp, '_opp'), c(matches(suffix_opp))) %>% 
    mutate(side = !!side)
}

probs_redux <- 
  bind_rows(.f_rename_538('h'), .f_rename_538('a')) %>% 
  select(-matches('team_538')) %>% 
  mutate(prob_diff = prob - prob_opp)
probs_redux

probs_grps <-
  probs_redux %>% 
  mutate(
    across(
      prob,
      list(
        grp4 = ~case_when(
          .x > prob_opp & prob_opp > probtie  ~ 'fav_small',
          .x > prob_opp~ 'fav_big',
          .x < prob_opp & .x < probtie ~ 'dog_big',
          .x < prob_opp ~ 'dog_small',
        ),
        grp2 = ~case_when(
          .x > prob_opp ~ 'fav',
          .x < prob_opp ~ 'dog',
          TRUE ~ NA_character_
        )
      )
    )
  )
probs_grps
probs_grps %>% count(prob_grp4)

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
  select(match_id, team, side) %>% 
  pivot_wider(
    names_from = side,
    values_from = team,
    names_prefix = 'team_'
  ) %>% 
  rename(team_h = team_home, team_a = team_away)
events_teams

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
  arrange(league, season, match_id, expanded_minute, second, team) %>% 
  relocate(league, season, match_id, date)
events

events_first <- events %>% filter(period_name == 'PreMatch', type_name == 'FormationSet', side == 'h')
events_last <- events %>% filter(period_name == 'SecondHalf', type_name == 'End', side == 'h')

.add_time_col <- function(data) {
  data %>% 
    mutate(time = sprintf('%s:%s', expanded_minute, second) %>% lubridate::ms() %>% as.double())
}

fouls <-
  events %>% 
  filter(type_name == 'Foul') %>% 
  mutate(
    action = if_else(outcome_type_name == 'Successful', 'drawn', 'committed')
  ) %>%
  .add_time_col() %>%
  select(
    league,
    season,
    match_id,
    date,
    id,
    time,
    expanded_minute,
    second,
    team,
    team_opp,
    # team_h,
    # team_a,
    side,
    player_name,
    action
  )
fouls

goals <- events %>% filter(type_name == 'Goal')

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
      side,
      expanded_minute,
      second,
      ...
    )
}

rgx_g <- '^g_[ha]'
goals_redux_init <-
  bind_rows(
    # Could generalize this even more.
    bind_rows(
      goals %>% 
        filter(side == 'h' & is.na(is_own_goal)),
      goals %>% 
        filter(side == 'a' & is_own_goal)
    ) %>% 
      .f_select() %>% 
      mutate(g_h = 1L),
    bind_rows(
      goals %>% 
        filter(side == 'a' & is.na(is_own_goal)),
      goals %>% 
        filter(side == 'h' & is_own_goal)
    ) %>% 
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
  fill(g_h_cumu, g_a_cumu, .direction = 'down') %>% 
  ungroup() %>% 
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
    # select(-matches('_cumu$')) %>% 
    rename_with(~str_replace(.x, suffix, ''), c(matches(suffix))) %>% 
    rename_with(~str_replace(.x, suffix_opp, '_opp'), c(matches(suffix_opp))) %>% 
    mutate(side = !!side)
}

states <- 
  bind_rows(.f_rename('h'), .f_rename('a')) %>% 
  mutate(
    across(g_state, list(grp = ~case_when(.x < 0L ~ 'behind', .x > 0L ~ 'ahead', TRUE ~ 'neutral')))
  ) %>%
  arrange(league, season, date, match_id, team, expanded_minute, second) %>% 
  .add_time_col()
states

final_scores <- 
  states %>% 
  filter(comment == 'end') %>% 
  select(match_id, team, team_opp, g_h_final = g_h_cumu, g_a_final = g_a_cumu, g_state_final = g_state)
final_scores

# pre-joining ----
# This returns the same number of results as a lag join (as expected).
fouls_by_time <-
  data.table::as.data.table(fouls)[
    data.table::as.data.table(
      states %>% 
        select(match_id, team, time, g_state, g_state_grp) %>% 
        group_by(match_id) %>% 
        mutate(across(time, list(hi = ~dplyr::lead(.x)))) %>% 
        ungroup() %>% 
        rename(time_lo = time)
    ), 
    on=.(match_id = match_id, team = team, time >= time_lo, time < time_hi)
  ] %>% 
  as_tibble() %>%
  drop_na(id) %>% 
  rename(time_lo = time, time_hi = time.1) %>% 
  .add_time_col() %>%
  group_by(match_id, team) %>%
  arrange(time, .by_group = TRUE) %>%
  mutate(time_diff = time - lag(time, default = 0)) %>%
  ungroup() %>% 
  mutate(across(matches('^time'), as.integer)) %>% 
  relocate(
    league,
    season,
    date,
    match_id,
    time,
    time_diff,
    time_lo,
    time_hi,
    expanded_minute,
    second,
    team
  ) %>%
  rename(team_whoscored = team, team_whoscored_opp = team_opp) %>% 
  left_join(team_mapping %>% select(team, team_whoscored = team_whoscored)) %>% 
  left_join(team_mapping %>% select(team_opp = team, team_whoscored_opp = team_whoscored)) %>% 
  # left_join(probs_grps) %>% 
  select(-matches('^team.*whoscored')) %>% 
  arrange(league, season, date, match_id, time, team)
fouls_by_time
fouls_by_time %>% count(id, sort = TRUE) %>% filter(n > 1L)

.add_n_foul_p90_col <- function(data) {
  data %>% 
    mutate(n_foul_p90 = 90 * n_foul / (time_diff / 60))
}

fouls_by_state <-
  fouls_by_time %>%
  group_by(
    league,
    season,
    date,
    team,
    team_opp,
    side,
    match_id,
    action,
    g_state,
    g_state_grp,
    time_lo,
    time_hi
  ) %>%
  # Summing `time_diff`s is wrong, since it won't account for time after last foul and before `time_hi`...
  summarize(
    n_foul = sum(time_diff > 0L)
  ) %>% 
  ungroup() %>% 
  # ...so re-calculate `time_diff`
  mutate(time_diff = time_hi - time_lo) %>% 
  left_join(final_scores) %>% 
  left_join(probs_grps) %>% 
  .add_n_foul_p90_col()%>% 
  arrange(league, season, date, match_id, time_lo, time_hi, team, team_opp)
fouls_by_state

.rejoin <- function(side1 = 'h') {
  side2 <- ifelse(side1 == 'h', 'a', 'h')
  left_join(
    fouls_by_state %>% filter(side == !!side1),
    fouls_by_state %>% 
      filter(side == !!side2) %>% 
      select(
        match_id,
        time_lo,
        time_hi,
        action,
        n_foul,
        n_foul_p90
      ) %>%
      rename(n_foul_opp = n_foul, n_foul_p90_opp = n_foul_p90) 
  )
}

fouls_by_state_redux <- 
  bind_rows(.rejoin('h'), .rejoin('a')) %>% 
  mutate(
    across(c(n_foul, n_foul_opp), ~coalesce(.x, 0L)),
    across(c(n_foul_p90, n_foul_p90_opp), ~coalesce(.x, 0))
  ) %>%
  mutate(
    n_foul_diff = n_foul - n_foul_opp, 
    n_foul_p90_diff = n_foul_p90 - n_foul_p90_opp
  ) %>% 
  arrange(league, season, date, match_id, time_lo, time_hi, team, team_opp)
fouls_by_state_redux

fouls_by_state_redux %>% select(where(is.numeric)) %>% skimr::skim()
# fouls_by_state_redux %>% filter(side == 'h' & g_h_final != score_538)
# fouls_by_state_redux %>% filter(side == 'a' & g_a_final != score_538)

fouls_by_state_redux2 %>% 
  filter(action == 'committed') %>% 
  filter(time_diff > (60 * 10)) %>% 
  # filter(abs(g_state) <= 3L) %>% 
  mutate(across(g_state_grp, factor)) %>% 
  # filter(prob_diff > 0) %>% 
  ggplot() +
  aes(x = n_foul_diff) +
  geom_density() +
  facet_grid(g_state_grp~prob_grp4)

fouls_by_state_redux %>% 
  filter(action == 'committed') %>% 
  filter(time_diff > (60 * 10)) %>% 
  filter(abs(g_state) <= 3L) %>% 
  # filter(prob_diff > 0) %>% 
  ggplot() +
  aes(x = n_foul_diff) +
  geom_point(aes(color = team)) +
  theme(legend.position = 'top') +
  facet_wrap(g_state~prob_grp4)

fouls_by_state2 <-
  fouls_by_state_redux %>%
  group_by(
    league,
    season,
    date,
    team,
    team_opp,
    side,
    match_id,
    action,
    g_state_grp,
    time_lo,
    time_hi
  ) %>%
  summarize(
    n_foul = sum(n_foul)
  ) %>% 
  ungroup() %>% 
  # ...so re-calculate `time_diff`
  mutate(time_diff = time_hi - time_lo) %>% 
  left_join(final_scores) %>% 
  left_join(probs_grps) %>% 
  .add_n_foul_p90_col()%>% 
  arrange(league, season, date, match_id, time_lo, time_hi, team, team_opp)
fouls_by_state2

.rejoin2 <- function(side1 = 'h') {
  side2 <- ifelse(side1 == 'h', 'a', 'h')
  left_join(
    fouls_by_state2 %>% filter(side == !!side1),
    fouls_by_state2 %>% 
      filter(side == !!side2) %>% 
      rename(team_z = team) %>% 
      rename(team = team_opp) %>% 
      rename(team_opp = team_z) %>% 
      select(match_id, date, g_state_grp, team, team_opp, time_lo, time_hi, action, n_foul, n_foul_p90) %>% 
      mutate(
        across(n_foul, ~coalesce(.x, 0L)),
        across(n_foul_p90, ~coalesce(.x, 0))
      ) %>% 
      rename(n_foul_opp = n_foul, n_foul_p90_opp = n_foul_p90) %>%
      mutate(across(g_state_grp, ~case_when(.x == 'ahead' ~ 'behind', .x == 'behind' ~ 'neutral', TRUE ~ .x)))
  )
}

fouls_by_state_redux2 <- 
  bind_rows(.rejoin2('h'), .rejoin2('a')) %>% 
  mutate(n_foul_p90_diff = n_foul_p90 - n_foul_p90_opp) %>% 
  arrange(league, season, date, match_id, time_lo, time_hi, team, team_opp)
fouls_by_state_redux2

fouls_by_state_redux2 %>% select(where(is.numeric)) %>% skimr::skim()

fouls_by_state_redux %>%
  filter(action == 'committed') %>% 
  # filter(n_foul_p90 <= 60) %>% 
  filter(time_diff > (60 * 10)) %>% 
  select(date, team, team_opp, g_state, g_state_grp, prob_grp4, n_foul_p90_diff, prob_diff) %>% 
  pivot_longer(
    -c(date, team, team_opp, g_state, g_state_grp, prob_grp4)
  ) %>% 
  mutate(grp = sprintf('%s, %s', prob_grp4, g_state)) %>% 
  ggplot() +
  aes(y = grp, x = value, color = team) +
  ggbeeswarm::geom_quasirandom(groupOnX = FALSE) +
  facet_wrap(~name, scales = 'free')

fouls_by_time_by_team <-
  fouls_by_state_redux2 %>% 
  left_join(probs_grps) %>% 
  group_by(league, season, team, action, prob_grp2, prob_grp4, g_state_grp) %>% 
  summarize(
    # prob = mean(prob),
    n_foul = sum(n_foul),
    n_foul_p90
    time_diff = sum(time_diff)
  ) %>% 
  ungroup() %>% 
  .add_n_foul_p90_col()
fouls_by_time_by_team

fouls_by_time_by_team %>%
  filter(season == 2020L) %>%
  filter(time_diff > (10 * 60)) %>% 
  arrange(desc(n_foul_p90))

fouls_stats_init <-
  fouls_by_time_by_team %>% 
  filter(!action) %>% 
  select(-action) %>% 
  group_by(league, season, team) %>% 
  mutate(
    frac = time_diff / sum(time_diff)
  ) %>% 
  ungroup()

fouls_stats_wide <-
  fouls_stats_init %>% 
  select(-c(n_foul, matches('^time_diff'))) %>% 
  pivot_wider(
    names_from = g_state_grp,
    values_from = c(frac, n_foul_p90, prob)
  ) %>% 
  mutate(
    diff_behind = n_foul_p90_behind - n_foul_p90_neutral,
    diff_ahead = n_foul_p90_ahead - n_foul_p90_neutral
  ) %>% 
  arrange(desc(diff_ahead))
fouls_stats_wide

fouls_stats_wide %>% 
  ggplot() +
  aes(x = prob_ahead, y = n_foul_p90_ahead) + # , color = prob_grp) +
  geom_point() +
  geom_smooth()

fouls_stats_wide %>% 
  ggplot() +
  aes(x = frac_ahead, y = n_foul_p90_ahead) + # , color = prob_grp) +
  geom_point() +
  geom_smooth()

fouls_stats_wide %>% 
  ggplot() +
  aes(x = frac_behind, y = n_foul_p90_behind) + # , color = prob_grp) +
  geom_point() +
  geom_smooth()

fouls_by_time_by_team_filt_wide %>% 
  arrange(desc(diff)) %>% 
  ggplot() +
  aes(x = frac_dog, y = diff) +
  geom_point()

fouls_by_time_by_team %>% 
  ggplot() +
  aes(x = factor(g_state_grp), n_foul_p90) +
  geom_boxplot() +
  facet_wrap(~action, scales = 'free')
