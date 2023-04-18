library(worldfootballR)  ## version: 0.5.12.5000
library(dplyr)
library(lubridate)
library(tidyr)
library(qs)

proj_dir <- '68-opta_xg_calib_by_gamestate'

rename_home_away_teams <- function(df) {
  df |> 
    mutate(
      team = ifelse(is_home, home_team, away_team),
      opponent = ifelse(is_home, away_team, home_team)
    ) |> 
    select(-c(home_team, away_team)) 
}

raw_shots <- load_fotmob_match_details(
  country = 'ENG',
  league_name = 'Premier League'
)

shots <- raw_shots |> 
  mutate(
    date = date(strptime(match_time_utc, "%a, %b %d, %Y, %H:%M UTC", tz = 'UTC')),
    season = case_when(
      date >= ymd('2020-09-12') & date <= ymd('2021-05-23') ~ '2020/21',
      date >= ymd('2021-08-13') & date <= ymd('2022-05-22') ~ '2021/22',
      date >= ymd('2022-08-06') & date <= ymd('2023-05-28') ~ '2022/23',
      TRUE ~ NA_character_
    )
  ) |> 
  drop_na(season) |> 
  transmute(
    match_id,
    season,
    date,
    id,
    across(period, ~ifelse(.x == 'FirstHalf', 1L, 2L)),
    min,
    min_added,
    is_home = team_id == home_team_id,
    # team = ifelse(is_home, home_team, away_team),
    # opponent = ifelse(is_home, away_team, home_team),
    home_team,
    away_team,
    player_id,
    # player_name,
    situation,
    # shot_type,
    # event_type,
    # is_own_goal,
    home_g = case_when(
      event_type == 'Goal' & is_home ~ 1L,
      is_own_goal & !is_home ~ 1L,
      TRUE ~ 0L
    ),
    away_g = case_when(
      event_type == 'Goal' & !is_home ~ 1L,
      is_own_goal & is_home ~ 1L,
      TRUE ~ 0L
    ),
    ## some shots with NAs for some reason
    home_xg = case_when(
      is_home ~ coalesce(expected_goals, 0),
      is_own_goal & !is_home ~ coalesce(expected_goals, 0),
      TRUE ~ 0L
    ),
    away_xg = case_when(
      !is_home ~ coalesce(expected_goals, 0),
      is_own_goal & is_home ~ coalesce(expected_goals, 0),
      TRUE ~ 0L
    )
  ) |>
  group_by(match_id) |> 
  mutate(
    shot_idx = row_number(period * (min + coalesce(min_added, 0L))),
    .after = id
  ) |> 
  ungroup() |> 
  arrange(season, date, shot_idx)

restacked_shots <- bind_rows(
  shots |> 
    filter(is_home) |> 
    mutate(
      team = home_team,
      opponent = away_team,
      g = home_g,
      g_conceded = away_g,
      xg = home_xg,
      xg_conceded = away_xg,
      .keep = 'unused'
    ),
  shots |> 
    filter(!is_home) |> 
    mutate(
      team = away_team,
      opponent = home_team,
      g = away_g,
      g_conceded = home_g,
      xg = away_xg,
      xg_conceded = home_xg,
      .keep = 'unused'
    )
) |> 
  arrange(season, date, match_id, shot_idx)

doublecounted_restacked_shots <- bind_rows(
  restacked_shots |> mutate(pov = 'primary', .before = is_home),
  restacked_shots |> 
    rename(
      team1 = team,
      team2 = opponent,
      g1 = g,
      g2 = g_conceded,
      xg1 = xg,
      xg2 = xg_conceded
    ) |> 
    rename(
      team = team2,
      opponent = team1,
      g = g2,
      g_conceded = g1,
      xg = xg2,
      xg_conceded = xg1
    ) |> 
    mutate(
      is_home = !is_home,
      pov = 'secondary'
    )
) |> 
  arrange(season, date, match_id, shot_idx, pov)

cumu_doublecounted_restacked_shots <- doublecounted_restacked_shots |> 
  group_by(match_id, team) |> 
  mutate(
    across(
      c(g, g_conceded, xg, xg_conceded),
      list(cumu = cumsum)
    )
  ) |> 
  ungroup() |> 
  mutate(
    is_goal = factor(ifelse(g == 1L, 'yes', 'no')),
    g_state = g_cumu - g_conceded_cumu
  )
qs::qsave(cumu_doublecounted_restacked_shots, file.path(proj_dir, 'shots.qs'))
