library(worldfootballR)
library(tidyverse)

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

understat_match_shots <- understat_match_shots('https://understat.com/match/18220')
fotmob_match_shots <- fotmob_get_match_details('3900946')

sim_understat_xg <- understat_match_shots |> 
  transmute(
    match_id,
    team = ifelse(home_away == 'h', home_team, away_team),
    opponent = ifelse(home_away == 'h', away_team, home_team),
    side = home_away,
    make = xG, 
    miss = 1 - xG
  ) |>
  # group_by(match_id, team, opponent, side) |>
  nest(data = -c(match_id, team, opponent, side)) |>
  # nest() |> 
  mutate(
    data = map(data, compute_goal_prob)
  ) |> 
  unnest(cols = c(data)) |> 
  group_by(match_id, team, opponent, side) |>
  mutate(g = row_number() - 1L) |>
  ungroup() |> 
  arrange(match_id, side, g)
sim_understat_xg

sim_fotmob_xg <- fotmob_match_shots |> 
  transmute(
    match_id,
    # home_team,
    # away_team,
    team_id,
    make = expected_goals, 
    miss = 1 - make
  ) |>
  # group_by(match_id, team_id) |>
  nest(data = -c(match_id, team_id)) |>
  mutate(
    data = map(data, compute_goal_prob)
  ) |> 
  unnest(cols = c(data)) |> 
  group_by(match_id, team_id) |>
  mutate(g = row_number() - 1L) |>
  ungroup() |> 
  arrange(team_id, g)
sim_fotmob_xg

## everything ----
fotmob_shots <- load_fotmob_match_details(league_id = 47)
understat_shots <- load_understat_league_shots(league = 'EPL') |> as_tibble()
fotmob_team_mapping <- bind_rows(
  fotmob_shots |> 
    distinct(league_id, team_id = home_team_id, team = home_team),
  fotmob_shots |> 
    distinct(league_id, team_id = away_team_id, team = away_team)
) |> 
  distinct(league_id, team_id, team)

understat_teams <- bind_rows(
  understat_shots |> 
    distinct(league, team = home_team),
  understat_shots |> 
    distinct(league, team = away_team)
) |> 
  filter(!is.na(league)) |> 
  distinct(league, team)


understat_shots |> 
  janitor::clean_names() |> 
  group_by(season, match_id) |> 
  nest() |> 
  nest(data = c(match_id, data))
