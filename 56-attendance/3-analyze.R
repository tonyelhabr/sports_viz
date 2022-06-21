
library(tidyverse)
library(qs)
library(lubridate)

dir_proj <- '56-attendance'
path_team_logos <- file.path(dir_proj, 'team_logos.qs')
path_clean_data <- file.path(dir_proj, 'clean_data.qs')
path_venue_capacities <- file.path(dir_proj, 'venue_capacities.csv')
path_team_mapping <- file.path(dir_proj, 'fbref_538_team_mapping.csv')

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

## TODO: make a venue_capacities_by_season var in 2-clean.R?
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

init_venue_capacities <- path_venue_capacities |> 
  read_csv(
    col_types = cols(
      venue = 'c', 
      season = 'i',
      max_attendance = 'i',
      capacity = 'i'
    )
  )

team_mapping <- path_team_mapping |> 
  read_csv(show_col_types = FALSE)

venue_capacities <- bind_rows(
  init_venue_capacities |> 
    filter(is.na(season)) |> 
    select(-season) |> 
    full_join(
      tibble(season = .seasons),
      by = character()
    ),
  crossing(
    venue = init_venue_capacities |> 
      filter(!is.na(season)) |> 
      distinct(venue) |> 
      pull(venue),
    season = .seasons
  ) |> 
    left_join(
      init_venue_capacities |> 
        filter(!is.na(season)) |> 
        select(season, venue, team, capacity), 
      by = c('venue', 'season')
    ) |> 
    arrange(venue, season) |> 
    group_by(venue) |> 
    fill(team, capacity, .direction = 'downup') |> 
    ungroup()
) |> 
  select(season, venue, team, capacity) |> 
  inner_join(
    team_mapping |> select(team = team_fbref, team_538),
    by = 'team'
  ) |> 
  select(-team) |> 
  rename(team = team_538)


model_df <- clean_df |> 
  inner_join(
    venue_capacities |>
      select(venue, season, team, capacity),
    by = c('venue', 'season', 'team')
  ) |>
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
    is_weekend = lubridate::day(date) %in% c('Sat', 'Sun'),
    across(matches('importance$'), ~.x / 100),
    avg_importance = (importance + importance_opp) / 2,
    attendance_prop = attendance / capacity,
    trunc_attendance_prop = ifelse(attendance_prop > 1, 1, attendance_prop),
    gw_prop = gw / matches_per_season,
    is_second_half = case_when(
      season == 2020 ~ NA,
      gw <= (matches_per_season / 2) ~ FALSE,
      TRUE ~ TRUE
    )
  )

# model_df |> 
#   filter(league == 'MLS') |> 
#   filter(!is.na(trunc_attendance_prop)) |> 
#   group_by(league, season, team) |> 
#   mutate(
#     lo = min(trunc_attendance_prop),
#     hi = max(trunc_attendance_prop),
#     gwprnk = round(percent_rank(gw), 1),
#     r = round(percent_rank(trunc_attendance_prop), 1),
#     mgw = max(gw)
#   ) |> 
#   ungroup() |> 
#   select(league, season, team, gw, gwprnk, trunc_attendance_prop, lo, hi, r) |>  
#   # count(gwprnk, r, sort = TRUE) |> 
#   filter(gwprnk == 0.1, r == 0.1)
#   ggplot() +
#   aes(x = gwprnk, y = r) +
#   geom_tile(aes(fill = n)) +
#   scale_fill_viridis_c()
# filter(gw == mgw) |> 
#   select(league, season, team, gw, trunc_attendance_prop, lo, hi, r) |> 
#   # arrange(-r)
#   ggplot() +
#   aes(x = r) +
#   geom_histogram()

df <- model_df |> 
  filter(league == 'MLS', season != 2020, !is.na(avg_importance)) |> 
  select(
    league,
    season,
    date,
    venue,
    gw,
    gw_prop,
    # gd_per_game_prnk_lag1,
    pts_per_game_prnk_lag1,
    is_weekend,
    is_second_half,
    avg_importance,
    trunc_attendance_prop
  )

df |> 
  filter(!is.na(avg_importance)) |> 
  group_by(venue) |> 
  slice_max(gw, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  count(season, gw, sort = TRUE)

library(tidymodels)
rec <- recipe(
  trunc_attendance_prop ~ gw_prop + is_weekend  + is_second_half + pts_per_game_prnk_lag1,
  data = df
)
rec

s
