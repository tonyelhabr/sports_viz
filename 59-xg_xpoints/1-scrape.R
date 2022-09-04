library(worldfootballR)  ## version: 0.5.12.5000
library(tidyverse)
library(lubridate)
library(qs)
library(poibin)

dir_proj <- '59-xg_xpoints'
team_mapping <- file.path(dir_proj, 'team_mapping.csv') |> read_csv()

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

permute_xg <- function(xg) {
  n <- length(xg)
  x <- seq.int(0, n)
  poibin::dpoibin(x, xg)
}

calculate_permuted_xg <- function(df) {
  df |> 
    group_by(across(c(everything(), -xg))) |> 
    summarize(across(xg, ~list(.x))) |> 
    mutate(
      prob = map(xg, ~permute_xg(.x))
    ) |> 
    select(-c(xg)) |> 
    unnest(cols = c(prob)) |> 
    group_by(across(-c(prob))) |>
    mutate(
      # cumu_prob = cumsum(prob),
      g = row_number() - 1L
    ) |>
    ungroup() |> 
    arrange(match_id, is_home, g)
}

## understat ----
understat_shots <- load_understat_league_shots(league = 'EPL') |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  filter(season < 2022) |> 
  mutate(
    across(season, ~sprintf('%s/%s', .x, .x + 1)),
    across(date, lubridate::date)
  )

understat_permuted_xg <- understat_shots |>
  transmute(
    match_id,
    season,
    date,
    home_team,
    away_team,
    is_home = h_a == 'h',
    xg = x_g
  ) |>
  rename_teams('understat') |> 
  calculate_permuted_xg()
qs::qsave(understat_permuted_xg, file.path(dir_proj, 'understat_permuted_xg.qs'))

## understat, xpts by match ----
init_understat_xpts_by_match <- 2014:2021 |> 
  set_names() |> 
  map_dfr(
    ~understatr::get_league_teams_stats('EPL', .x),
    .id = 'year'
  ) |> 
  mutate(
    across(year, as.integer),
    season = sprintf('%s/%s', year, year + 1),
    .before = 1
  ) |> 
  select(-year)

clean_understat_xpts_by_match <- init_understat_xpts_by_match |> 
  left_join(
    team_mapping |> 
      select(team_538, team_understat),
    by = c('team_name' = 'team_understat')
  ) |> 
  select(-team_name) |> 
  rename(team = team_538) |> 
  select(
    season,
    date,
    team,
    result,
    pts,
    xpts,
    xg = xG
  ) |> 
  inner_join(
    understat_permuted_xg |> 
      distinct(season, date, team, opponent, is_home),
    by = c('season', 'date', 'team')
  )

raw_understat_xpts_by_match <- clean_understat_xpts_by_match |> 
  inner_join(
    clean_understat_xpts_by_match |> 
      select(season, date, opponent = team, opponent_xg = xg),
    by = c('season', 'date', 'opponent')
  ) |> 
  mutate(
    xgd = xg - opponent_xg
  )
qs::qsave(raw_understat_xpts_by_match, file.path(dir_proj, 'raw_understat_xpts_by_match.qs'))

## fotmob ----
season_date_ranges <- understat_shots |> 
  group_by(season) |> 
  summarize(
    across(date, list(min = min, max = max))
  ) |> 
  nest(data = -c(season)) |> 
  deframe()

init_fotmob_shots <- load_fotmob_match_details(league_id = 47) |> 
  mutate(
    date = strptime(match_time_utc, "%a, %b %d, %Y, %H:%M UTC", tz = 'UTC') |> lubridate::date()
  )

fotmob_match_id_seasons <- init_fotmob_shots |>
  distinct(match_id, date) |> 
  mutate(
    ## this is brittle... yolo
    season = case_when(
      date >= season_date_ranges[['2020/2021']]$date_min & date <= season_date_ranges[['2020/2021']]$date_max ~ '2020/2021',
      date >= season_date_ranges[['2021/2022']]$date_min & date <= season_date_ranges[['2021/2022']]$date_max ~ '2021/2022',
      TRUE ~ NA_character_
    )
  ) |> 
  drop_na(season)

fotmob_shots <- init_fotmob_shots |> 
  inner_join(
    fotmob_match_id_seasons |> 
      distinct(match_id, season),
    by = 'match_id'
  )

fotmob_permuted_xg <- fotmob_shots |> 
  transmute(
    match_id,
    season,
    date,
    home_team,
    away_team,
    is_home = team_id == home_team_id,
    xg = coalesce(expected_goals, 0)
  ) |>
  rename_teams('fotmob') |>
  calculate_permuted_xg()
qs::qsave(fotmob_permuted_xg, file.path(dir_proj, 'fotmob_permuted_xg.qs'))

## table ----
match_results <- 2014:2021 |> 
  map_dfr(~understat_league_match_results('EPL', .x)) |> 
  as_tibble()

init_table <- match_results |> 
  as_tibble() |> 
  transmute(
    match_id,
    season,
    date = strptime(datetime, '%Y-%m-%d %H:%M:%S', tz = 'UTC') |> lubridate::date(),
    home_team,
    home_goals,
    away_team,
    away_goals
  )

table <- bind_rows(
  init_table |>
    mutate(is_home = TRUE) |> 
    rename_teams('understat') |> 
    transmute(
      match_id,
      date,
      season,
      is_home,
      team, 
      opponent,
      goals = home_goals,
      opponent_goals = away_goals
    ),
  init_table |> 
    mutate(
      is_home = FALSE
    ) |> 
    rename_teams('understat') |> 
    transmute(
      match_id,
      date,
      season,
      is_home,
      team, 
      opponent,
      goals = away_goals,
      opponent_goals = home_goals
    )
) |> 
  mutate(
    pts = case_when(
      goals > opponent_goals ~ 3L,
      goals < opponent_goals ~ 0L,
      TRUE ~ 1L
    )
  ) |> 
  group_by(season, team) |> 
  summarize(
    across(pts, sum)
  ) |> 
  ungroup() |> 
  group_by(season) |> 
  mutate(rank = row_number(desc(pts))) |> 
  ungroup() |> 
  arrange(season, rank)
qs::qsave(table, file.path(dir_proj, 'table.qs'))
