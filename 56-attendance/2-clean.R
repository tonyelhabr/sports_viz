
library(tidyverse)
library(qs)
library(lubridate)
library(googlesheets4)

## inputs
dir_proj <- '56-attendance'
path_attendance <- file.path(dir_proj, 'attendance.qs')
path_importance <- file.path(dir_proj, 'importance.qs')
path_team_logos <- file.path(dir_proj, 'team_logos.qs')

## outputs
path_clean_data <- file.path(dir_proj, 'clean_data.qs')

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

team_mapping <- read_sheet(ss = '1nIw1PgozYsb11W-QSzjgPlW-qrsJWLiPB8x6fTTMqiI', sheet = 'teams') |> 
  transmute(
    rn = row_number(),
    across(is_alternative_fbref, ~replace_na(.x, FALSE)),
    team_538,
    team_fbref,
    team_fbref_stats,
    team_fotmob
  )

league_mapping <- tibble(
  league_fbref = c('Premier League', 'EFL Championship', 'Major League Soccer', 'USL Championship'),
  league_538 = c('Barclays Premier League', 'English League Championship', 'Major League Soccer', 'United Soccer League'),
  league_abbrv = c('EPL', 'EFL', 'MLS', 'USL')
)

seasons <- 2016:2022
matches_mapping <- list(
  'EPL' = list(`38` = seasons),
  'EFL' = list(`46` = seasons),
  'MLS' = list(
    `34` = c(2016:2019, 2021:2022),
    `23` = 2020
  ),
  'USL' = list(
    `34` = c(2018:2019),
    `32` = c(2017, 2021:2022),
    `30` = 2016,
    `23` = 2020
  )
) |> 
  enframe('league', 'season') |> 
  unnest_longer(season) |> 
  unnest(season) |> 
  transmute(
    league, 
    across(season, as.integer), 
    n_matches = as.integer(season_id)
  )

library(tidylog)
init_df <- attendance |> 
  ## only completed matches
  filter(!is.na(home_goals)) |> 
  transmute(
    season,
    date,
    league,
    home_team,
    away_team,
    home_goals,
    away_goals,
    venue,
    attendance
  ) |> 
  left_join(
    team_mapping |>
      distinct(home_team = team_fbref, home_team_stats = team_fbref_stats),
    by = 'home_team'
  ) |> 
  select(-home_team_stats) |> 
  inner_join(
    team_mapping |> 
      distinct(home_team = team_fbref, team_538),
    by = 'home_team'
  ) |> 
  select(-home_team) |> 
  rename(home_team = team_538) |> 
  inner_join(
    team_mapping |> 
      distinct(away_team = team_fbref, team_538),
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
  rename(league = league_abbrv)

init_stacked_df <- bind_rows(
  init_df |>
    transmute(
      league,
      season,
      date,
      venue,
      team = home_team,
      team_opp = away_team,
      goals = home_goals,
      goals_opp = away_goals,
      importance = home_importance,
      importance_opp = away_importance,
      attendance,
      is_home = TRUE
    ),
  init_df |>
    transmute(
      league,
      season,
      date,
      venue,
      team = away_team,
      team_opp = home_team,
      goals = away_goals,
      goals_opp = home_goals,
      importance = away_importance,
      importance_opp = home_importance,
      attendance,
      is_home = FALSE
    )
) |> 
  arrange(league, season, date, team) |> 
  mutate(
    pts = case_when(
      goals > goals_opp ~ 3L,
      goals < goals_opp ~ 0L,
      TRUE ~ 1L
    )
  ) |> 
  group_by(league, season, team) |> 
  mutate(
    gw = row_number(date),
    across(
      c(goals, goals_opp, pts), list(cumu = cumsum)
    )
  ) |> 
  ungroup() |> 
  mutate(
    gd_cumu = goals_cumu - goals_opp_cumu,
    gd_cumu_per_game =  gd_cumu / gw
  ) |> 
  group_by(league, season) |> 
  mutate(
    gd_per_game_prnk = percent_rank(desc(gd_cumu_per_game)),
    pts_per_game_prnk = percent_rank(desc(pts / gw))
  ) |> 
  ungroup() |> 
  group_by(league, season, team) |> 
  mutate(
    across(
      c(gd_per_game_prnk, pts_per_game_prnk),
      list(
        lag1 = ~dplyr::lag(.x, default = 0)
      )
    )
  ) |> 
  ungroup()

n_gws_by_team <- init_stacked_df |>
  group_by(league, season, team) |>
  slice_max(gw) |>
  ungroup()|> 
  select(league, season, team, max_gw = gw) 
n_gws_by_team |> arrange(desc(max_gw)) |> filter(max_gw > 50)

stacked_df <- init_stacked_df |> 
  inner_join(
    n_gws_by_team,
    by = c('league', 'season', 'team')
  ) |>
  inner_join(
    matches_mapping,
    by = c('league', 'season')
  ) |> 
  mutate(
    true_max_gw = case_when(
      season == 2022 ~ n_matches,
      max_gw > n_matches ~ n_matches,
      TRUE ~ max_gw
    ),
    gw_prop = pmin(gw / true_max_gw, 1)
  )

stacked_opp_df <- stacked_df |> 
  left_join(
    stacked_df |> 
      rename_with(
        ~sprintf('%s_opp', .x), c(matches('^(gd|pts)_'))
      ) |> 
      select(league, season, date, venue, team, gw, max_gw, n_matches, matches('^(gd|pts)_.*_opp$')),
    by = c('league', 'season', 'date', 'venue', 'team_opp' = 'team')
  )

clean_df <- stacked_df |> 
  filter(!is.na(attendance)) |> 
  filter(is_home) |> 
  select(-is_home)

qs::qsave(clean_df, path_clean_data)
