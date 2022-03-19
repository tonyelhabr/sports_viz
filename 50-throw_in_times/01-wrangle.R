
## setup ----
library(tidyverse)
library(arrow)

dir_proj <- '50-throw_in_times'
.parquet_path <- function(filename) {
  file.path(dir_proj, sprintf('%s.parquet', filename))
}

import_parquet <- function(filename) {
  .parquet_path(filename) %>% 
    arrow::read_parquet()
}

add_minute_col <- function(df) {
  df %>% 
    mutate(
      minute = (time + 60) %/% 60
    )
}
teams <- import_parquet('teams')
team_mapping <- xengagement::team_accounts_mapping %>% 
  select(team_538, team = team_whoscored)

spi <- read_csv(
  'https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv',
  show_col_types = FALSE
)

probs <- spi %>% 
  filter(league_id == 2411, season >= 2017L) %>% 
  left_join(team_mapping %>% select(home_team = team, team1 = team_538)) %>%
  left_join(team_mapping %>% select(away_team = team, team2 = team_538)) %>%
  transmute(
    season_id = as.integer(season),
    game_date = date,
    home_team,
    away_team,
    home_score = score1,
    away_score = score2,
    home_prob = prob1,
    away_prob = prob2,
    draw_prob = probtie,
    home_imp = importance1,
    away_imp = importance2
  ) %>% 
  arrange(season_id, game_date, home_team)
probs

games <- import_parquet('games') %>%
  left_join(
    teams %>% 
      select(home_team_id = team_id, home_team = team_name)
  ) %>% 
  left_join(
    teams %>% 
      select(away_team_id = team_id, away_team = team_name)
  ) %>% 
  transmute(
    game_id,
    home_team_id,
    home_team,
    away_team_id,
    away_team,
    season_id,
    across(game_date, lubridate::date),
    away_score,
    home_score
  ) %>% 
  left_join(
    probs %>% 
      select(-c(home_score, away_score))
  )

all_actions <- import_parquet('all_actions') %>% 
  mutate(
    across(original_event_id, as.character)
  ) %>% 
  left_join(
    games %>% 
      select(game_id, home_team_id, away_team_id, home_prob, away_prob, draw_prob)
  ) %>% 
  arrange(game_id, period_id, action_id) %>% 
  add_minute_col()

## extra time weights ----
first_half <- all_actions %>% filter(period_id == 1)
second_half <- all_actions %>% 
  filter(period_id == 2) %>% 
  mutate(
    across(
      time,
      ~.x - min(.x, na.rm = TRUE) + 1
    )
  ) %>% 
  ## re-do it for the second half since it's now starting at minute 1 instead of minute 45 or whatever
  add_minute_col()

halves <- bind_rows(
  first_half,
  second_half
) %>% 
  arrange(game_id, period_id, action_id)

last_half_mins <- halves %>%
  group_by(game_id, period_id) %>% 
  slice_max(time, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(game_id, period_id, time, minute, minute)

## give less weighting to halves that go on longer. arguably this should be done the opposite way
##  but i prefer to let interpolation dominate empericalness in extra time
half_wts <- last_half_mins %>% 
  count(period_id, minute) %>% 
  arrange(period_id, desc(minute)) %>% 
  mutate(
    across(n, list(cumu = cumsum))
  ) %>% 
  mutate(
    wt = n_cumu / last(n_cumu)
  ) %>% 
  ungroup() %>% 
  arrange(desc(wt))

halves_trunc <- halves %>%
  left_join(
    last_half_mins %>% 
      select(game_id, period_id, last_minute = minute) %>% 
      left_join(
        half_wts %>% 
          select(period_id, last_minute = minute, wt)
      )
  ) %>% 
  mutate(
    across(
      wt,
      ~case_when(
        minute <= 45 ~ 1,
        minute > 45 & minute < last_minute ~ 1,
        TRUE ~ .x
      )
    )
  ) %>% 
  filter(
    minute <= 50
  )

## goals (for game state) ----
g <- bind_rows(
  all_actions %>% 
    filter(type_name == 'shot' & result_name == 'success'),
  all_actions %>% 
    filter(type_name == 'shot' & result_name == 'owngoal') %>% 
    left_join(
      games %>% 
        select(game_id, home_team_id, away_team_id)
    ) %>% 
    mutate(
      team_id = ifelse(team_id == home_team_id, away_team_id, home_team_id)
    )
) %>% 
  distinct(
    game_id,
    team_id,
    period_id,
    action_id,
    time
  ) %>% 
  left_join(
    games %>% 
      select(
        game_id,
        home_team_id,
        away_team_id
      )
  ) %>% 
  mutate(
    home_g = ifelse(team_id == home_team_id, 1L, 0L),
    away_g = ifelse(team_id == away_team_id, 1L, 0L)
  ) %>% 
  group_by(game_id) %>% 
  arrange(time, .by_group = TRUE) %>% 
  mutate(
    across(
      c(home_g, away_g),
      cumsum
    )
  ) %>% 
  ungroup() 

## big boy ----
finalize_df <- function(.type_name, filename, subs_filename) {

  export_path <- .parquet_path(filename)
  subs_df <- .parquet_path(subs_filename) %>% arrow::read_parquet()
  df <- halves_trunc %>% 
    left_join(
      g
    ) %>% 
    arrange(game_id, period_id, time) %>% 
    group_by(game_id) %>% 
    fill(matches('_g$')) %>% 
    ungroup() %>% 
    mutate(
      across(
        matches('_g$'),
        replace_na,
        0L
      )
    ) %>% 
    group_by(game_id, period_id) %>% 
    mutate(
      across(
        time, 
        list(
          diff = ~{.x - dplyr::lag(.x)}
        )
      )
    ) %>% 
    ungroup() %>% 
    mutate(
      is_home = ifelse(team_id == home_team_id, 1L, 2L),
      g_diff = ifelse(
        is_home,
        home_g - away_g,
        away_g - home_g
      ),
      g_state = case_when(
        home_g == away_g ~ 2L,
        is_home & (home_g > away_g) ~ 3L,
        !is_home & (away_g > home_g) ~ 3L,
        TRUE ~ 1L
      ),
      wp_diff = ifelse(
        is_home,
        home_prob - away_prob,
        away_prob - home_prob
      ),
      wp_state = case_when(
        (draw_prob > home_prob) & (draw_prob > away_prob) ~ 2L,
        is_home & (home_prob > away_prob) ~ 3L,
        !is_home & (away_prob > home_prob) ~ 3L,
        TRUE ~ 1L
      )
    ) %>% 
    filter(
      type_name == .type_name
    ) %>% 
    select(
      game_id,
      period_id,
      original_event_id,
      is_home,
      time,
      home_g,
      away_g,
      g_diff,
      g_state,
      wp_diff,
      wp_state,
      wt,
      minute,
      time_diff
    ) %>% 
    left_join(
      subs_df %>% 
        transmute(game_id, original_event_id, is_after_sub = 1L)
    ) %>% 
    mutate(
      across(
        is_after_sub,
        replace_na,
        0L
      )
    )
  arrow::write_parquet(df, export_path)
  df
}

throw_ins <- finalize_df(
  'throw_in', 
  'throw_ins',
  '201522_epl_throw_ins_after_subs'
)

gks <- finalize_df(
  'goalkick', 
  'gks',
  '201522_epl_gks_after_subs'
)
