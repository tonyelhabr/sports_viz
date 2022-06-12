
library(tidyverse)
library(qs)
library(lubridate)

dir_proj <- '56-attendance'
path_team_logos <- file.path(dir_proj, 'team_logos.qs')
path_clean_data <- file.path(dir_proj, 'clean_data.qs')

## outputs
path_model_data <- file.path(dir_proj, 'model_data.qs')
clean_df <- path_clean_data |> qs::qread()

make_season_label <- function(season, is_pre_pandemic) {
  case_when(
    season == 2020 & is_pre_pandemic ~ '2020, Pre-Pandemic',
    season == 2020 & !is_pre_pandemic ~ '2020, Post-Pandemic',
    TRUE ~ as.character(season)
  ) |> 
    factor(levels = c('2018', '2019', '2020, Pre-Pandemic', '2020, Post-Pandemic', '2021', '2022'))
}

.seasons <- 2016L:2022L
matches_per_season <- tibble(
  league = rep('USL', 7),
  season = .seasons,
  matches_per_season = c(30L, 32L, 34L, 34L, 34L, 32L, 34L)
) |> 
  bind_rows(
    crossing(
      tibble(
        league = c('EFL', 'EPL', 'MLS'),
        matches_per_season = c(46L, 38L, 34L)
      ),
      season = .seasons
    )
  )

model_df <- clean_df |> 
  # left_join(
  #   venue_capacities |>
  #     select(venue, season, lat, long, capacity),
  #   by = c('venue', 'season')
  # ) |>
  left_join(
    matches_per_season,
    by = c('league', 'season')
  ) |> 
  ## TODO:
  ## 1. Factor month?
  mutate(
    is_pre_pandemic = date <= lubridate::ymd('2020-03-15'),
    is_post_pandemic = date <= lubridate::ymd('2021-07-01'),
    season_label = make_season_label(season, is_pre_pandemic),
    is_weekend = day %in% c('Sat', 'Sun'),
    across(matches('importance$'), ~.x / 100),
    importance = (importance + importance_opp) / 2,
    attendance_prop = attendance / capacity,
    trunc_attendance_prop = ifelse(attendance_prop > 1, 1, attendance_prop),
    is_second_half = case_when(
      season == 2020 ~ NA,
      gw <= (matches_per_season / 2) ~ FALSE,
      TRUE ~ TRUE
    )
  )