library(worldfootballR)  ## version: 0.5.12.5000
library(dplyr)
library(lubridate)
library(tidyr)
library(qs)

proj_dir <- '70-2023_vaep/checks'

rename_home_away_teams <- function(df) {
  df |> 
    mutate(
      team = ifelse(is_home, home_team, away_team),
      opponent = ifelse(is_home, away_team, home_team)
    ) |> 
    select(-c(home_team, away_team)) 
}

raw_fotmob_shots <- load_fotmob_match_details(
  country = 'ENG',
  league_name = 'Premier League'
)

bad_match_ids <- 3901196
## raw data is missing Solly March's goal at 61' https://www.fotmob.com/match/3901196/matchfacts/leeds-united-vs-brighton--hove-albion
fixed_matched_details <- fotmob_get_match_details(bad_match_ids)

fotmob_shots <- raw_fotmob_shots |> 
  filter(!(match_id %in% bad_match_ids)) |> 
  bind_rows(fixed_matched_details) |> 
  mutate(
    game_date = date(strptime(match_time_utc, "%a, %b %d, %Y, %H:%M UTC", tz = 'UTC')),
    season = case_when(
      # game_date >= ymd('2021-08-13') & game_date <= ymd('2022-05-22') ~ '2021/22',
      game_date >= ymd('2022-08-05') & game_date <= ymd('2023-05-28') ~ '2022/23',
      TRUE ~ NA_character_
    )
  ) |> 
  # filter(season %in% c('2021/22', '2022/23')) |> 
  filter(season == '2022/23', !is_own_goal, situation != 'Penalty') |> 
  transmute(
    season,
    game_date,
    match_id,
    id,
    across(period, ~ifelse(.x == 'FirstHalf', 1L, 2L)),
    min,
    min_added,
    team_id,
    home_team_id,
    home_team,
    away_team_id,
    away_team,
    player_id,
    player_name,
    situation,
    xg = coalesce(expected_goals, 0),
    g = as.integer(event_type == 'Goal')
  ) |>
  group_by(match_id) |> 
  mutate(
    shot_idx = row_number(period * (min + coalesce(min_added, 0L))),
    .after = id
  ) |> 
  ungroup() |> 
  arrange(season, game_date, shot_idx)
qs::qsave(fotmob_shots, file.path(proj_dir, 'fotmob_shots.qs'))
