
library(tidyverse)
library(qs)
library(lubridate)

dir_proj <- '56-attendance'
dir_data <- file.path(dir_proj, 'data')
path_attendance <- file.path(dir_proj, 'attendance.qs')
path_importance <- file.path(dir_proj, 'importance.qs')
path_team_logos <- file.path(dir_proj, 'team_logos.qs')
path_venue_capacities <- file.path(dir_proj, 'venue_capacities.csv')
path_team_mapping <- file.path(dir_proj, 'fbref_538_team_mapping.csv')
filter_dates <- function(df) {
  df |> 
    ## scrape date
    filter(!is.na(date), date < lubridate::ymd('2022-06-02'))
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
      max_attendance = 'i',
      capacity = 'i',
      max_capacity = 'i'
    )
  )

team_mapping <- path_team_mapping |> 
  read_csv(show_col_types = FALSE)

league_mapping <- tibble(
  league_fbref = c('Premier League', 'EFL Championship', 'Major League Soccer', 'USL Championship'),
  league_538 = c('Barclays Premier League', 'English League Championship', 'Major League Soccer', 'United Soccer League'),
  league_abbrv = c('EPL', 'EFL', 'MLS', 'USL')
)

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
    team_logos |>
      select(home_team = team, home_logo_path = path),
    by = 'home_team'
  ) |>
  left_join(
    venue_capacities |> 
      select(venue, lat, long, capacity, max_capacity),
    by = 'venue'
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
  mutate(
    is_pre_pandemic = date <= lubridate::ymd('2020-03-15'),
    is_post_pandemic = date <= lubridate::ymd('2021-07-01'),
    across(matches('importance$'), ~.x / 100),
    importance = (home_importance + away_importance) / 2,
    attendance_prop = attendance / capacity,
    max_attendance_prop = attendance / max_capacity
  )

season_windows <- df |> 
  group_by(league, season) |> 
  summarize(
    across(
      date,
      list(
        min = min,
        max = max
      )
    )
  ) |> 
  ungroup()
season_windows

df |> 
  filter(importance < 1, !is.na(attendance_prop)) %>%
  lm(attendance_prop ~ home_importance + away_importance, data = .)

safc <- df |> 
  filter(home_team == 'San Antonio FC')
safc
team_logos |> 
  filter

usl_teams <- df |> 
  filter(
    league == 'USL'
  ) |> 
  group_by(
    season, 
    team = home_team, logo_path = home_logo_path,
    venue, lat, long, capacity, max_capacity
  ) |> 
  summarize(
    n = n(),
    n_non_na = sum(!is.na(attendance)),
    across(
      c(attendance, attendance_prop, max_attendance_prop),
      median,
      na.rm = TRUE
    )
  ) |> 
  ungroup() |> 
  # filter(n > 1) |> 
  arrange(
    desc(attendance_prop)
  )

us_map <- map_data('state') |> 
  as_tibble()

base <- ggplot() +
  geom_polygon(
    data = us_map,
    aes(x = long, y = lat, group = group),
    color = 'grey50',
    size = 0.25,
    fill = NA
  ) +
  theme_void() +
  coord_map('albers',  lat0 = 45.5, lat1 = 29.5)

base +
  geom_point(
    data = usl_teams,
    aes(
      fill = max_attendance_prop,
      x = long,
      y = lat
    )
  ) +
  scale_color_stepsn(
    # n = 10,
    colors = viridis::magma(10, direction = -1)
  )

make_season_label <- function(season, is_pre_pandemic) {
  case_when(
    season == 2020 & is_pre_pandemic ~ '2020, Pre-Pandemic',
    season == 2020 & !is_pre_pandemic ~ '2020, Post-Pandemic',
    TRUE ~ as.character(season)
  ) |> 
    factor(levels = c('2018', '2019', '2020, Pre-Pandemic', '2020, Post-Pandemic', '2021', '2022'))
}

usl_teams_by_season <- df |> 
  filter(
    league == 'USL'
  ) |>
  group_by(
    season,
    season_label = make_season_label(season, is_pre_pandemic),
    team = home_team, logo_path = home_logo_path,
    venue, lat, long, capacity, max_capacity
  ) |> 
  summarize(
    n = n(),
    n_non_na = sum(!is.na(attendance)),
    across(
      c(attendance, attendance_prop, max_attendance_prop),
      median,
      na.rm = TRUE
    )
  ) |> 
  ungroup() |> 
  arrange(team, season_label)

diff_props <- usl_teams_by_season |> 
  filter(
    season != 2020
  ) |> 
  select(team, season, season_label, n, prop = max_attendance_prop) |> 
  group_by(team) |> 
  mutate(
    prion_n = lag(n),
    prior_prop = lag(prop)
  ) |> 
  ungroup() |> 
  mutate(
    diff_prop = prop - prior_prop,
    pct_prop = diff_prop / prior_prop
  ) |> 
  ungroup() |> 
  arrange(desc(diff_prop))
diff_props |> filter(team == 'Birmingham Legion FC')

diff_props |> 
  ggplot() +
  aes(x = season_label, y = prop, color = team, group = team) +
  geom_point(aes(size = n)) +
  geom_line() +
  guides(color = 'none', size = 'none')


usl_teams_by_season |> 
  filter(n > 1) |> 
  filter(is_pre_pandemic) |> 
  filter(season == 2019) |> 
  group_by(team) |> 
  slice_max(n, n = 1, with_ties = FALSE) |> 
  ungroup()
  ggplot() +
  aes(x = long, y = lat) +
  geom_point(
    aes(color = attendance_prop),
    size = 3
  ) +
  scale_color_stepsn(
    colours = viridis::magma(10, direction = -1)
  )




df |> 
  ggplot() +
  aes(
    x = home_importance,
    y = attendance_prop
  ) +
  geom_point(
    aes(color = league)
  )
