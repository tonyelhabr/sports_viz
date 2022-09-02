library(worldfootballR)
library(tidyverse)
library(lubridate)

dir_proj <- '59-xg_xpoints'
team_mapping <- file.path(dir_proj, 'team_mapping.csv') |> read_csv()
compute_goal_prob <- function(df){
  
  prob_idx <- rep(0, nrow(df))
  for (i in 1:nrow(df)) {
    if (i == 1){
      prob_idx[i] <- df[i, 2]
      prob_idx[i+1] <- df[i, 1]
    } else if(i == 2) {
      prob_idx[i+1] <- df[i, 1]*prob_idx[i]
      prob_idx[i] <- df[i, 2]*prob_idx[i] + df[i, 1]*prob_idx[i-1]
      prob_idx[1] <- df[i, 2]*prob_idx[1]
    } else {
      prob_idx[i+1] <- df[i, 1]*prob_idx[i]
      prob_idx[i] <- df[i, 2]*prob_idx[i] + df[i, 1]*prob_idx[i-1]
      for (j  in 1:(i-2)){
        prob_idx[i-j] <- df[i, 2]*prob_idx[i-j] + df[i, 1]*prob_idx[i-j-1]
      }
      prob_idx[1] <- df[i, 2]*prob_idx[1]
    }
  }
  prob_idx |> 
    unlist() |> 
    as_tibble() |> 
    mutate(cumprob = cumsum(value))
}

understat_match_shots <- understat_match_shots('https://understat.com/match/18220') |> 
  as_tibble() |> 
  janitor::clean_names()
fotmob_match_shots <- fotmob_get_match_details('3900946')

rename_teams <- function(df, src) {
  team_src <- sprintf('team_%s', src)
  df |> 
    left_join(
      team_mapping |> 
        select(.data$team_538, .data[[team_src]]),
      by = c('home_team' = team_src)
    ) |> 
    select(-.data$home_team) |> 
    rename(home_team = .data$team_538) |> 
    left_join(
      team_mapping |> 
        select(.data$team_538, .data[[team_src]]),
      by = c('away_team' = team_src)
    ) |> 
    select(-.data$away_team) |> 
    rename(away_team = .data$team_538) |> 
    mutate(
      team = ifelse(is_home, home_team, away_team),
      opponent = ifelse(is_home, away_team, home_team)
    ) |> 
    select(-c(home_team, away_team)) 
}

postprocess_permuted_xg <- function(df) {
  df |> 
    nest(data = c(make, miss)) |>
    mutate(
      data = map(data, compute_goal_prob)
    ) |> 
    unnest(cols = c(data)) |> 
    group_by(across(-c(value, cumprob))) |>
    mutate(g = row_number() - 1L) |>
    ungroup() |> 
    arrange(match_id, is_home, g)
}

understat_permuted_xg <- understat_match_shots |>
  transmute(
    match_id,
    date = lubridate::date(date),
    home_team,
    away_team,
    is_home = home_away == 'h',
    make = x_g, 
    miss = 1 - x_g
  ) |>
  rename_teams('understat') |> 
  postprocess_permuted_xg()

fotmob_permuted_xg <- fotmob_match_shots |> 
  transmute(
    match_id,
    date = strptime(match_time_utc, "%a, %b %d, %Y, %H:%M UTC", tz = 'UTC') |> lubridate::date(),
    home_team,
    away_team,
    is_home = team_id == home_team_id,
    make = expected_goals, 
    miss = 1 - make
  ) |>
  rename_teams('fotmob') |>
  postprocess_permuted_xg()


## everything ----
all_fotmob_shots <- load_fotmob_match_details(league_id = 47)
all_understat_shots <- load_understat_league_shots(league = 'EPL') |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  filter(season >= 2021)

all_understat_permuted_xg <- all_understat_shots |>
  transmute(
    match_id,
    date = lubridate::date(date),
    home_team,
    away_team,
    is_home = coalesce(h_a == 'h', home_away == 'h'), ## home_away used in 2022 season
    make = coalesce(xG, x_g), 
    miss = 1 - make
  ) |>
  rename_teams('understat') |> 
  postprocess_permuted_xg()

all_fotmob_permuted_xg <- all_fotmob_shots |> 
  transmute(
    match_id,
    date = strptime(match_time_utc, "%a, %b %d, %Y, %H:%M UTC", tz = 'UTC') |> lubridate::date(),
    home_team,
    away_team,
    is_home = team_id == home_team_id,
    make = expected_goals, 
    miss = 1 - make
  ) |>
  rename_teams('fotmob') |>
  postprocess_permuted_xg()
