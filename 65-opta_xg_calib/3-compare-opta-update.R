library(readr)
library(dplyr)
library(probably)
library(ggplot2)

proj_dir <- '65-opta_xg_calib'
data_dir <- file.path(proj_dir, 'data')
clean_shotss_compare_path <- file.path(data_dir, 'clean_updated_shots.rds')

updated_shots <- read_rds(clean_updated_shots_path)

compare_by <- function(updated_shots, ...) {
  updated_shots |> 
    filter(!is_penalty) |> 
    group_by(...) |> 
    summarize(
      n_shots = n(),
      new_npxg = sum(new_xg, na.rm = TRUE),
      old_npxg = sum(old_xg, na.rm = TRUE),
      npg = sum(as.character(is_goal) == 'yes')
    ) |> 
    ungroup() |> 
    mutate(
      d = new_npxg - old_npxg,
      drate = d / n_shots
    ) |> 
    arrange(desc(abs(drate)))
}

baseline_drate <- updated_shots |> compare_by() |> pull(drate)

tidy_updated_shots <- updated_shots |> 
  select(
    is_penalty,
    new_xg,
    old_xg,
    is_goal,
    
    country,
    distance,
    sca1,
    primary_foot,
    is_from_deflection,
    is_from_volley,
    is_free_kick,
    is_open_play,
    is_primary_foot
  ) |> 
  mutate(
    across(
      country,
      factor
    ),
    across(
      distance,
      ~cut(
        .x,
        breaks = c(seq(0, 18, by = 2), 20, 25, 30, 35, Inf)
      )
    ),
    across(sca1, ~na_if(.x, '')),
    across(
      sca1, 
      ~.x |> 
        str_remove_all( '\\(|\\)') |> 
        str_replace_all('\\s|[-]', '_') |> 
        tolower() |> 
        factor()
    ),
    # country,
    across(
      c(
        is_from_deflection,
        is_from_volley,
        is_free_kick,
        is_open_play,
        is_primary_foot
      ),
      ~ifelse(.x, 'yes', 'no') |> 
        factor()
    ),
    across(
      c(is_primary_foot, primary_foot, sca1, distance), 
      ~fct_explicit_na(.x, na_level = 'missing')
    ),
    is_primary_foot2 = sprintf('%s_%s', primary_foot, is_primary_foot) |> factor()
  ) |> 
  pivot_longer(
    -c(is_penalty, is_goal, old_xg, new_xg),
    names_to = 'feature',
    values_to = 'group'
  ) |> 
  compare_by(feature, group) |> 
  mutate(
    drate_diff = drate - !!baseline_drate
  )
tidy_updated_shots
tidy_updated_shots |> 
  filter(d < 0)


updated_shots |> 
  filter(!is_penalty) |> 
  count(old_xg, xgd) |> 
  ggplot() +
  aes(x = old_xg, y = xgd) +
  geom_point(
    aes(size = n, alpha = n, color = n)
  ) +
  scale_alpha_continuous(range = c(0.5, 1)) +
  scale_color_viridis_c(option = 'D')

updated_shots |> 

  ggplot() +
  aes(x = old_xg, y = new_xg) +
  geom_abline(linetype = 2) +
  geom_point(
    aes(size = n)
  ) +
  coord_equal()


library(tidymodels)
library(forcats)
df <- updated_shots |> 
  filter(!is.na(xgd), !is_penalty) |> 
  transmute(
    xgd,
    is_xgd_pos = xgd > 0,
    old_xg,
    log_distance = log(1 + distance),
    across(
      sca1, 
      ~.x |> 
        str_remove_all( '\\(|\\)') |> 
        str_replace_all('\\s|[-]', '_') |> 
        tolower()
    ),
    # country,
    across(
      c(
        is_xgd_pos,
        is_from_deflection,
        is_free_kick,
        is_open_play,
        is_primary_foot
      ),
      ~ifelse(.x, 'yes', 'no') |> 
        factor()
    ),
    across(
      c(is_primary_foot, primary_foot, sca1), 
      ~fct_explicit_na(.x, na_level = 'missing')
    )
  )

rec <- recipe(
  xgd ~ .,
  data = df
) |> 
  step_rm(is_xgd_pos) |> 
  step_dummy(all_nominal_predictors())

spec <- linear_reg()
wf <- workflow(rec, spec)
fit <- fit(wf, df)

rmse(
  augment(fit, df),
  truth = xgd,
  estimate = .pred
)

tidy_coefs <- tidy(fit) |> 
  mutate(
    across(p.value, round, 3)
  ) |> 
  arrange(estimate)
tidy_coefs

rec <- recipe(
  is_xgd_pos ~ .,
  data = df
) |> 
  step_rm(xgd) |> 
  step_nzv(all_predictors()) |> 
  step_dummy(all_nominal_predictors())

spec <- logistic_reg()
wf <- workflow(rec, spec)
fit <- fit(wf, df)

preds <- augment(fit, df)
accuracy(
  preds,
  truth = is_xgd_pos,
  estimate = .pred_class,
  event_level = 'second'
)

source(file.path(proj_dir, 'helpers.R'))
brier_score(
  preds,
  truth = is_xgd_pos,
  estimate = .pred_yes,
  event_level = 'second'
)

roc_auc(
  preds,
  is_xgd_pos,
  .pred_yes,
  event_level = 'second'
)

tidy_coefs <- tidy(fit) |> 
  mutate(
    across(p.value, round, 3)
  ) |> 
  arrange(estimate)

pivot_shots_longer <- function(df) {
  df |>
    pivot_longer(
      -c(season_end_year, country, team),
      names_to = 'stat',
      values_to = 'value'
    )
}

updated_shots_pivoted <- updated_shots |>
  select(
    source,
    match_url, 
    date, 
    half,
    minute,
    team,
    player,
    xg
  ) |> 
  pivot_wider(
    -c(
      source,
      match_url, 
      date, 
      half,
      minute,
      team,
      player
    ),
    names_from = 'source',
    values_from = 'xg'
  ) |> 
  pivot_wider(
    names_from = source,
    values_from = value
  ) |> 
  group_by(stat) |> 
  mutate(
    new_rescaled = scales::rescale(new, to = c(0, 1)),
    old_rescaled = scales::rescale(old, to = c(0, 1))
  )
combined |> arrange(desc(raw_rescaled))
