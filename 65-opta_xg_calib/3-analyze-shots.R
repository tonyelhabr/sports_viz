library(readr)
library(dplyr)
library(probably)
packageVersion('probably')

proj_dir <- '65-opta_xg_calib'
data_dir <- file.path(proj_dir, 'data')
clean_shots_path <- file.path(data_dir, 'clean_shots.rds')

shots <- read_rds(clean_shots_path)

npxg_by <- function(shots, ...) {
  shots |> 
    filter(!is_penalty) |> 
    group_by(...) |> 
    summarize(
      n_shots = n(),
      npxg = sum(xg, na.rm = TRUE),
      npg = sum(as.character(is_goal) == 'yes')
    ) |> 
    ungroup() |> 
    mutate(
      d = npxg - npg,
      drate = d / n_shots
    ) |> 
    arrange(desc(abs(drate)))
}

shots |> 
  filter(
    country == 'ENG',
    tier == '1st',
    gender == 'M'
  ) |> 
  mutate(
    g = as.character(is_goal) == 'yes'
  ) |> 
  group_by(season_end_year, team) |> 
  summarize(
    n_shots = n(),
    npxg = sum(ifelse(is_penalty, 0, 1) * xg, na.rm = TRUE),
    xg = sum(xg, na.rm = TRUE),
    npg = sum(ifelse(is_penalty, 0, 1) & g),
    g = sum(g)
  ) |> 
  ungroup() |> 
  mutate(
    d = xg - g,
    npd = npxg - npg
  ) |> 
  filter(season_end_year == 2021) |> 
  arrange(desc(g))

npxg_by_season <- shots |>
  npxg_by(season_end_year)

npxg_by_league <- shots |> 
  npxg_by(
    group,
    country,
    tier,
    gender
  )

npxg_by_league_season <- shots |> 
  npxg_by(
    group,
    country,
    tier,
    gender,
    season_end_year
  )

npxg_by_body_part <- shots |>
  filter(group == 'big5') |>
  npxg_by(body_part)

npxg_by_primary_foot <- shots |>
  filter(group == 'big5') |> 
  npxg_by(is_primary_foot)

npxg_by_foot <- shots |>
  filter(group == 'big5') |> 
  npxg_by(primary_foot, is_primary_foot)

npxg_by_true_open_play <- shots |>
  filter(group == 'big5') |> 
  npxg_by(is_true_open_play)

npxg_by_open_play <- shots |>
  filter(group == 'big5') |> 
  npxg_by(is_open_play)

npxg_by_deflection <- shots |>
  filter(group == 'big5') |> 
  npxg_by(is_from_deflection)

npxg_by_volley <- shots |>
  filter(group == 'big5') |> 
  npxg_by(is_from_volley)

npxg_by_free_kick <- shots |>
  filter(group == 'big5') |> 
  npxg_by(is_free_kick)

set.seed(42)
shots_sample <- shots |> slice_sample(n = 50000)
shots_sample |> 
  filter(!is_penalty) |> 
  cal_plot_windowed(
    truth = is_goal,
    estimate = xg,
    window_size = 0.05,
    conf_level = 0.95,
    event_level = 'second'
  )

shots_sample |> 
  filter(!is_penalty) |> 
  cal_plot_breaks(
    truth = is_goal,
    estimate = xg,
    num_breaks = 20,
    conf_level = 0.95,
    event_level = 'second'
  )

source(file.path(proj_dir, 'helpers.R'))
shots_sample |> 
  filter(!is_penalty) |>
  make_calibration_table(
    truth = is_goal,
    estimate = xg,
    # db,
    width = 0.05,
    event_level = 'second'
  ) |> 
  make_calibration_plot(
    truth = xg,
    estimate = is_goal
  )

shots |> 
  filter(!is_penalty) |> 
  cal_plot_breaks(
    truth = is_goal,
    estimate = xg,
    group = is_primary_foot,
    num_breaks = 20,
    conf_level = 0.95,
    event_level = 'second'
  )

shots |> 
  mutate(
    db = cut(distance, breaks = c(0, 7, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, Inf))
  ) |> 
  cal_plot_breaks(
    truth = is_goal,
    estimate = xg,
    group = db,
    num_breaks = 20,
    conf_level = 0.95,
    event_level = 'second'
  )

