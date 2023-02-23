suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(probably)
  library(ggplot2)
  packageVersion('probably')
})

proj_dir <- '65-opta_xg_calib'
data_dir <- file.path(proj_dir, 'data')
# clean_shots_path <- file.path(data_dir, 'clean_shots.rds')
clean_shots_path <- 'c:/users/antho/documents/projects/sports_viz/65-opta_xg_calib/data/clean_shots.rds'
np_shots <- read_rds(clean_shots_path) |> 
  filter(!is_penalty) |> 
  mutate(
    league = sprintf('%s_%s_%s', country, tier, gender)
  )

np_shots |> count(league, name = 'n_shots')

npxg_by <- function(shots, ...) {
  shots |> 
    group_by(...) |> 
    summarize(
      n_shots = n(),
      npxg = sum(xg, na.rm = TRUE),
      npg = sum(as.character(is_goal) == 'yes')
    ) |> 
    ungroup() |> 
    mutate(
      d = npxg - npg,
      d_rate = d / n_shots
    ) |> 
    arrange(desc(n_shots))
}



npxg_by_season <- np_shots |>
  npxg_by(season_end_year)

npxg_by_league <- np_shots |> 
  npxg_by(
    group,
    country,
    tier,
    gender
  )

npxg_by_league_season <- np_shots |> 
  npxg_by(
    group,
    country,
    tier,
    gender,
    season_end_year
  )

npxg_by_body_part <- np_shots |>
  filter(group == 'big5') |>
  npxg_by(body_part)

npxg_by_primary_foot <- np_shots |>
  filter(group == 'big5') |> 
  npxg_by(is_primary_foot)

npxg_by_foot <- np_shots |>
  filter(group == 'big5') |> 
  npxg_by(primary_foot, is_primary_foot)

npxg_by_true_open_play <- np_shots |>
  filter(group == 'big5') |> 
  npxg_by(is_true_open_play)

npxg_by_open_play <- np_shots |>
  filter(group == 'big5') |> 
  npxg_by(is_open_play)

npxg_by_deflection <- np_shots |>
  filter(group == 'big5') |> 
  npxg_by(is_from_deflection)

npxg_by_volley <- np_shots |>
  filter(group == 'big5') |> 
  npxg_by(is_from_volley)

npxg_by_free_kick <- np_shots |>
  filter(group == 'big5') |> 
  npxg_by(is_free_kick)

## bss ----
np_shots <- np_shots |> 
  filter(!is_penalty, !is.na(xg))

goal_rate <- np_shots |> 
  count(is_goal) |> 
  mutate(prop = n / sum(n)) |> 
  filter(is_goal == 'yes') |> 
  pull(prop)

goal_rate_bs <- np_shots |> 
  brier_score(
    truth = is_goal,
    estimate = !!goal_rate,
    event_level = 'second'
  ) |> 
  pull(.estimate)

xg_bs <- np_shots |> 
  brier_score(
    truth = is_goal,
    estimate = xg,
    event_level = 'second'
  ) |> 
  pull(.estimate)

# xg_bss <- np_shots |>
#   brier_skill_score(
#     truth = is_goal,
#     estimate = xg,
#     ref_estimate = !!goal_rate,
#     event_level = 'second'
#   ) |>
#   pull(.estimate)
# xg_bss
xg_bss <- 1 - (xg_bs / goal_rate_bs)

np_shots |> 
  group_by(is_primary_foot, primary_foot) |> 
  brier_skill_score(
    truth = is_goal,
    estimate = xg,
    ref_estimate = !!goal_rate,
    event_level = 'second'
  ) |> 
  select(is_primary_foot, primary_foot, foot_bs = .estimate)

bs_by_foot <- np_shots |> 
  group_by(is_big5 = group == 'big5', is_primary_foot, primary_foot) |> 
  brier_score(
    truth = is_goal,
    estimate = xg,
    event_level = 'second'
  ) |> 
  select(is_big5, is_primary_foot, primary_foot, foot_bs = .estimate) |> 
  mutate(
    goal_rate_foot_bss = 1 - (foot_bs / !!goal_rate_bs),
    xg_foot_bss = 1 - (foot_bs / !!xg_bs)
  ) |> 
  filter(!is.na(is_primary_foot)) |> 
  arrange(goal_rate_foot_bss)

## custom cal plot ----

np_shots |> 
  mutate(
    bucket = cut(xg, xg_breaks, include.lowest = TRUE)
  ) |> 
  make_calibration_table(
    estimate = bucket,
    truth = is_goal,
    event_level = 'second'
  ) |> 
  mutate(
    estimate = !!xg_breaks[1:(length(xg_breaks)-1)],
    across(estimate, ~(.x + lead(.x, n = 1L, default = 1L)) / 2)
  ) |> 
  make_calibration_plot(
    estimate = estimate,
    truth = is_goal
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))



## cal plot ----
set.seed(42)
shots_sample <- np_shots |> slice_sample(n = 50000)
shots_sample |> 
  cal_plot_windowed(
    truth = is_goal,
    estimate = xg,
    window_size = 0.05,
    conf_level = 0.95,
    event_level = 'second'
  )

shots_sample |> 
  cal_plot_breaks(
    truth = is_goal,
    estimate = xg,
    num_breaks = 19,
    conf_level = 0.95,
    event_level = 'second'
  )

res <- shots_sample |> 
  filter(!is.na(is_primary_foot), primary_foot != 'both') |> 
  cal_plot_breaks(
    truth = is_goal,
    estimate = xg,
    group = is_primary_foot,
    num_breaks = 20,
    conf_level = 0.95,
    event_level = 'second'
  )
res$data

debugonce(ggplot2::cut_number)
shots_sample |> 
  filter(!is.na(is_primary_foot), primary_foot != 'both') |> 
  group_by(is_primary_foot, primary_foot) |> 
  make_cal_table(
    truth = is_goal,
    estimate = xg,
    cut_fn = ggplot2::cut_width,
    cut_args = list(width = 0.05),
    event_level = 'second'
  ) |> 
  ungroup() |> 
  ggplot() +
  aes(x = predicted_midpoint, y = event_rate) +
  geom_abline(linetype = 2) +
  geom_line() +
  geom_point(
    show.legend = FALSE,
    aes(size = sqrt(total))
  ) +
  geom_ribbon(
    color = '#ffffff00',
    alpha = 0.08,
    aes(ymin = lower, ymax = upper)
  ) +
  facet_wrap(is_primary_foot~primary_foot)
