
library(tidyverse)
library(arrow)

dir_proj <- '50-throw_in_times'
understat_xg <- file.path('47-formation_symmetry', 'understat_xg.rds') %>% read_rds()

.f_import <- function(name) {
  path <- file.path(dir_proj, sprintf('%s.parquet', name))
  res <- path %>% arrow::read_parquet()
  assign(value = res, x = name, envir = .GlobalEnv)
}

c(
  'all_actions',
  'player_games',
  'games_by_team',
  'players',
  'games',
  'teams'
) %>% 
  walk(.f_import)

team_mapping <- xengagement::team_accounts_mapping %>% 
  select(team = team_538, team_opta = team_whoscored)

.change_team_name <- function(df, .side = NULL) {
  prefix <- ifelse(is.null(.side), '', paste0(.side, '_'))
  team_name_col <- sprintf('%steam_name', prefix)
  team_name_sym <- sym(team_name_col)
  df %>% 
    left_join(
      team_mapping %>% 
        select(
          team_name = team_opta, new_team_name = team
        ) %>% 
        rename_with(~sprintf('%s%s', prefix, .x), -c(new_team_name)),
      by = sprintf('%steam_name', prefix)
    ) %>% 
    select(-all_of(team_name_col)) %>% 
    rename(!!team_name_sym := new_team_name)
}

teams <- teams %>% .change_team_name()

meta <- games %>% 
  select(game_id, home_team_id, away_team_id, season_id, game_date, away_score, home_score) %>% 
  left_join(teams %>% rename_all(~sprintf('home_%s', .x))) %>% 
  left_join(teams %>% rename_all(~sprintf('away_%s', .x)))

xg <- meta %>% 
  transmute(
    game_id,
    date = game_date %>% lubridate::date(),
    home_team_name,
    away_team_name,
    home_team_id,
    away_team_id
  ) %>% 
  left_join(
    understat_xg %>% 
      select(
        home_team_name = team_h,
        away_team_name = team_a,
        minute,
        date,
        home_xg = xg_h,
        away_xg = xg_a
      )
  ) %>% 
  select(-c(date, home_team_name, away_team_name)) %>% 
  group_by(game_id) %>% 
  arrange(minute, .by_group = TRUE) %>% 
  mutate(
    across(
      c(home_xg, away_xg),
      cumsum
    )
  ) %>% 
  ungroup()
xg

add_minute_col <- function(df) {
  df %>% 
    mutate(
      minute = as.integer((period_id - 1) * 45 + (time + 60) %/% 60),
      minute = (time + 60) %/% 60
    )
  
}

all_actions <- all_actions %>% 
  left_join(meta) %>% 
  arrange(game_id, period_id, action_id) %>% 
  add_minute_col()

first_half <- all_actions %>% filter(period_id == 1)
second_half <- all_actions %>% 
  filter(period_id == 2) %>% 
  mutate(
    across(
      time,
      ~.x - min(.x, na.rm = TRUE) + 1
    )
  ) %>% 
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

half_wts <- last_half_mins %>% 
  count(period_id, minute) %>% 
  arrange(period_id, minute) %>% 
  mutate(
    across(n, list(cumu = cumsum))
  ) %>% 
  mutate(
    wt = n_cumu / last(n_cumu)
  ) %>% 
  ungroup()

halves_trunc <- halves %>%
  left_join(
    last_half_mins %>% 
      left_join(half_wts)
  ) %>% 
  mutate(
    across(
      wt,
      ~case_when(
        minute <= 45 ~ 1,
        TRUE ~ .x
      )
    )
  ) %>% 
  filter(
    (period_id == 1 & minute <= 50) |
      (period_id == 2 & minute <= 55)
  )


g <- bind_rows(
  all_actions %>% 
    filter(type_name == 'shot' & result_name == 'success'),
  all_actions %>% 
    filter(type_name == 'shot' & result_name == 'owngoal') %>% 
    left_join(
      meta %>% 
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
  # mutate(g = 1) %>% 
  left_join(
    meta %>% 
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
g  

throw_ins <- halves_trunc %>% 
  left_join(
    xg %>% filter(!is.na(minute))
  ) %>% 
  left_join(
    g
  ) %>% 
  arrange(game_id, period_id, time) %>% 
  group_by(game_id) %>% 
  fill(matches('_x?g$')) %>% 
  ungroup() %>% 
  mutate(
    across(
      matches('_g$'),
      replace_na,
      0L
    ),
    across(
      matches('_xg$'),
      replace_na,
      0
    )
  ) %>% 
  group_by(game_id, period_id) %>% 
    across(
      time, 
      list(
        diff = ~{.x - dplyr::lag(.x)}
      )
    )
  ) %>% 
  ungroup() %>% 
  mutate(
    is_home = team_id == home_team_id,
    g_state = case_when(
      home_g == away_g ~ 2L,
      is_home & (home_g > away_g) ~ 3L,
      !is_home & (away_g > home_g) ~ 3L,
      TRUE ~ 1L
    ),
    xg_state = case_when(
      home_xg == away_xg ~ 2L,
      is_home & (home_xg > away_xg) ~ 3L,
      !is_home & (away_xg > home_xg) ~ 3L
      TRUE ~ 1L
    ),
    minute = (time + 60) %/% 60
  ) %>% 
  ungroup() %>% 
  filter(
    type_name == 'throw_in'
  ) %>% 
  mutate(
    idx = row_number()
  ) %>% 
  select(
    # idx,
    game_id,
    period_id,
    action_id,
    team_id,
    player_id,
    time,
    wt,
    minute,
    minute,
    time_diff,
    result_name,
    matches('^x?g_state'),
    matches('type_name_(lead|lag)'),
    matches('_x?g$')
  )
throw_ins
write_parquet(throw_ins, file.path(dir_proj, 'throw_ins.parquet'))

library(tidymodels)
rec_tree <- throw_ins %>% 
  recipe(
    time_diff ~ g_state + minute,
    data = .
  )
spec_rf <- rand_forest(
  mode = 'regression'
) %>% 
  set_engine(
    engine = 'ranger',
    quantreg=TRUE
  )

wf_rf <- workflow() %>% 
  add_recipe(rec_tree) %>% 
  add_model(spec_rf)

fit_rf <- wf_rf %>% fit(throw_ins)
fit_rf

newdata_grid <- crossing(
  minute = 1L:95L,
  g_state = c('ahead', 'tied', 'behind')
)

preds_rf <- bind_cols(
  newdata_grid %>% 
    augment(fit_rf, .),
  fit_rf %>% 
    extract_fit_engine() %>% 
    predict(
      data = newdata_grid,
      type = 'quantiles',
      quantiles = c(0.25, 0.5, 0.75)
    ) %>% 
    pluck('predictions') %>% 
    as_tibble() %>% 
    set_names(sprintf('q%s', c('25', '50', '75')))
)
q
q$predictions

preds_rf %>% 
  ggplot() +
  aes(
    x = minute,
    y = .pred
  ) +
  geom_ribbon(
    alpha = 0.1,
    aes(
      fill = g_state,
      ymin = q25,
      ymax = q75
    )
  ) +
  geom_line(
    aes(
      color = g_state
    )
  )

rec_lin <- throw_ins %>% 
  recipe(
    time_diff ~ g_state + minute,
    data = .
  ) %>% 
  step_interact(~g_state:minute)

spec_lin <- linear_reg()

wf_lin <- workflow() %>% 
  add_recipe(rec_lin) %>% 
  add_model(spec_lin)

fit_lin <- wf_lin %>% fit(throw_ins)
fit_lin

preds_lin <- newdata_grid %>% 
  augment(fit_lin, .)

preds_lin %>% 
  ggplot() +
  aes(
    x = minute,
    y = .pred,
    color = g_state
  ) +
  geom_line()
