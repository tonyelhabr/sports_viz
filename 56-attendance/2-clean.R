
library(tidyverse)
library(qs)

dir_proj <- '56-attendance'
path_attendance <- file.path(dir_proj, 'attendance.qs')
path_importance <- file.path(dir_proj, 'importance.qs')
path_team_mapping <- file.path(dir_proj, 'fbref_538_team_mapping.csv')

## outputs
path_clean_data <- file.path(dir_proj, 'clean_data.qs')
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

init_df <- attendance |> 
  transmute(
    season,
    date,
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

clean_df <- bind_rows(
  init_df |>
    transmute(
      league,
      season,
      date,
      team = home_team,
      team_opp = away_team,
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
      team = away_team,
      team_opp = home_team,
      importance = away_importance,
      importance_opp = home_importance,
      attendance,
      is_home = FALSE
    )
) |> 
  arrange(league, season, date, team) |> 
  group_by(league, season, team) |> 
  mutate(
    gw = row_number(date)
  ) |> 
  ungroup()

qs::qsave(clean_df, path_clean_data)
qs::qsave(logos, path_logos)
