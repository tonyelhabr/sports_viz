library(worldfootballR)  ## version: 0.6.4.1
library(dplyr)
library(lubridate)
library(tidyr)
library(qs)

PROJ_DIR <- '68-opta_xg_calib_by_gamestate'
source(file.path(PROJ_DIR, 'load_fb.R')) ## until these are added to worldfootballR

rename_home_away_teams <- function(df) {
  df |> 
    mutate(
      team = ifelse(is_home, home_team, away_team),
      opponent = ifelse(is_home, away_team, home_team)
    ) |> 
    select(-c(home_team, away_team)) 
}

raw_shots <- load_fb_match_shooting(
  country = 'ENG',
  gender = 'M',
  tier = '1st',
  season_end_year = 2018:2023
)

long_shots <- raw_shots |> 
  transmute(
    # match_id = MatchURL,
    match_id = basename(dirname(MatchURL)),
    season = sprintf('%s/%s', Season_End_Year, substr(Season_End_Year, 3, 4)),
    date = ymd(Date),
    period = as.integer(Match_Half),
    min = ifelse(
      grepl('+', Minute),
      as.integer(gsub('(^[0-9]+)[+]([0-9]+$)', '\\1', Minute)), 
      as.integer(Minute)
    ),
    min_added = ifelse(
      grepl('+', Minute), 
      as.integer(gsub('(^[0-9]+)[+]([0-9]+$)', '\\2', Minute)), 
      as.integer(Minute)
    ),
    is_home = Home_Away == 'Home',
    team = Squad,
    # team = ifelse(is_home, home_team, away_team),
    # opponent = ifelse(is_home, away_team, home_team),
    player = Player,
    is_goal = Outcome == 'Goal',
    xg = xG
  )

match_teams <- long_shots |> 
  distinct(match_id, team, side = ifelse(is_home, 'home', 'away')) |> 
  pivot_wider(
    names_from = side,
    values_from = team,
    names_glue = '{side}_{.value}'
  )

long_shots
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
    home_xg = case_when(
      is_home ~ coalesce(xG, 0),
      is_own_goal & !is_home ~ coalesce(xG, 0),
      TRUE ~ 0L
    ),
    away_xg = case_when(
      !is_home ~ coalesce(xG, 0),
      is_own_goal & is_home ~ coalesce(xG, 0),
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
qs::qsave(cumu_doublecounted_restacked_shots, file.path(PROJ_DIR, 'shots.qs'))
