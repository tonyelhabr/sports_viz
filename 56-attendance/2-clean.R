
library(tidyverse)
library(qs)
library(lubridate)

dir_proj <- '56-attendance'
path_attendance <- file.path(dir_proj, 'attendance.qs')
path_importance <- file.path(dir_proj, 'importance.qs')
path_team_logos <- file.path(dir_proj, 'team_logos.qs')
path_venue_capacities <- file.path(dir_proj, 'venue_capacities.csv')
path_team_mapping <- file.path(dir_proj, 'fbref_538_team_mapping.csv')

## outputs
path_data <- file.path(dir_proj, 'data.qs')
path_logos <- file.path(dir_proj, 'logos.qs')

filter_dates <- function(df) {
  df |> 
    filter(!is.na(date), date < lubridate::ymd('2022-06-02')) ## scrape date
}
attendance <- path_attendance |> 
  qs::qread() |> 
  filter_dates()
importance <- path_importance |> 
  qs::qread() |> 
  filter_dates()
team_logos <- path_team_logos |> qs::qread()

venue_capacities <- path_venue_capacities |> 
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

logos <- team_mapping |>
  left_join(
    team_logos |>
      select(team_fbref_stats = name, home_logo_path = path),
    by = 'team_fbref_stats'
  ) |> 
  select(team = team_538, path = home_logo_path)

league_mapping <- tibble(
  league_fbref = c('Premier League', 'EFL Championship', 'Major League Soccer', 'USL Championship'),
  league_538 = c('Barclays Premier League', 'English League Championship', 'Major League Soccer', 'United Soccer League'),
  league_abbrv = c('EPL', 'EFL', 'MLS', 'USL')
)

make_season_label <- function(season, is_pre_pandemic) {
  case_when(
    season == 2020 & is_pre_pandemic ~ '2020, Pre-Pandemic',
    season == 2020 & !is_pre_pandemic ~ '2020, Post-Pandemic',
    TRUE ~ as.character(season)
  ) |> 
    factor(levels = c('2018', '2019', '2020, Pre-Pandemic', '2020, Post-Pandemic', '2021', '2022'))
}

df <- attendance |> 
  transmute(
    season,
    date,
    across(wk, as.integer),
    is_weekend = day %in% c('Sat', 'Sun'),
    league,
    home_team,
    away_team,
    venue,
    attendance
  ) |> 
  left_join(
    team_mapping |>
      select(home_team = team_fbref, home_team_stats = team_fbref_stats),
    by = 'home_team'
  ) |>
  select(-home_team_stats) |> 
  left_join(
    venue_capacities |> 
      select(venue, season, lat, long, capacity),
    by = c('venue', 'season')
  ) |> 
  inner_join(
    team_mapping |> 
      select(home_team = team_fbref, team_538),
    by = 'home_team'
  ) |> 
  select(-home_team) |> 
  rename(home_team = team_538) |> 
  inner_join(
    team_mapping |> 
      select(away_team = team_fbref, team_538),
    by = 'away_team'
  ) |> 
  select(-away_team) |> 
  rename(away_team = team_538) |> 
  inner_join(
    league_mapping |> 
      select(league = league_fbref, league_538, league_abbrv),
    by = 'league'
  ) |> 
  select(-league) |> 
  rename(league = league_538) |> 
  left_join(
    importance |> 
      select(
        date,
        league,
        home_team,
        home_importance,
        away_importance
      ),
    by = c('date', 'league', 'home_team')
  ) |> 
  select(-league) |> 
  rename(league = league_abbrv) |> 
  ## TODO:
  ## 1. Factor month?
  ## 2. Add a is_second_half column (harder to calculate)
  mutate(
    is_pre_pandemic = date <= lubridate::ymd('2020-03-15'),
    is_post_pandemic = date <= lubridate::ymd('2021-07-01'),
    season_label = make_season_label(season, is_pre_pandemic),
    across(matches('importance$'), ~.x / 100),
    importance = (home_importance + away_importance) / 2,
    attendance_prop = attendance / capacity,
    trunc_attendance_prop = ifelse(attendance_prop > 1, 1, attendance_prop)
  )
qs::qsave(df, path_data)
qs::qsave(logos, path_logos)
