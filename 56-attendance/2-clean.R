
library(tidyverse)
library(qs)
library(lubridate)
library(googlesheets4)

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

team_mapping <-  read_sheet(ss = '1nIw1PgozYsb11W-QSzjgPlW-qrsJWLiPB8x6fTTMqiI') |> 
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
      select(home_team = team_fbref, home_team_stats = team_fbref_stats),
    by = 'home_team'
  ) |> 
  select(-home_team_stats) |> 
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
  rename(league = league_abbrv)

stacked_df <- bind_rows(
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

stacked_opp_df <- stacked_df |> 
  left_join(
    stacked_df |> 
      rename_with(
        ~sprintf('%s_opp', .x), matches('^(gd|pts)_')
      ) |> 
      select(league, season, gw, venue, team, matches('^(gd|pts)_.*_opp$')),
    by = c('league', 'season', 'gw', 'venue', 'team_opp' = 'team')
  )

n_gws_by_league_season <- stacked_opp_df |> 
  group_by(league, season, team) |> 
  slice_max(gw) |> 
  ungroup() |> 
  count(league, season, gw, sort = TRUE)

## Montreal FC got some weird shit going on for 2022
n_gws_majority_by_league_season <- n_gws_by_league_season |> 
  group_by(league, season) |> 
  mutate(prop = n / sum(n)) |> 
  slice_max(n, n = 1, with_ties = FALSE) |> 
  ungroup()

clean_df <- stacked_df |> 
  filter(!is.na(attendance)) |> 
  filter(is_home) |> 
  select(-is_home)

qs::qsave(clean_df, path_clean_data)