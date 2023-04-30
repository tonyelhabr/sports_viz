library(worldfootballR)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(tibble)
library(qs)
library(cli)

PROJ_DIR <- '69-soccer_gei'
DATA_DIR <- file.path(PROJ_DIR, 'data')
MOMENTUM_DATA_DIR <- file.path(DATA_DIR, 'momentum')
MATCH_TEAM_STATS_DATA_DIR <- file.path(DATA_DIR, 'match_team_stats')
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MOMENTUM_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MATCH_TEAM_STATS_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
matches_path <- file.path(PROJ_DIR, 'matches.qs')
momentum_path <- file.path(PROJ_DIR, 'momentum.qs')
match_team_stats <- file.path(PROJ_DIR, 'match_team_stats.qs')

league_matches <- map_dfr(
  c(42, 47),
  ~fotmob_get_league_matches(league_id = .x, season = '2022/2023') |> 
    mutate(
      league_id = .x,
      across(round_name, as.character)
    )
)

matches <- league_matches |>
  unnest(status, names_sep = '_') |> 
  filter(status_finished, !status_cancelled) |> 
  select(
    league_id,
    round,
    match_time = status_utcTime, 
    match_id = id, 
    home, 
    away
  ) |>
  unnest_wider(c(home, away), names_sep = '_') |> 
  transmute(
    league_id,
    round,
    match_time = ymd_hms(match_time), 
    across(match_id, as.integer),
    home_id,
    home_team = home_name,
    away_id,
    away_team = away_name
  )
qs::qsave(matches, matches_path)

possibly_fotmob_get_match_momentum <- possibly(
  fotmob_get_match_momentum,
  otherwise = tibble(),
  quiet = FALSE
)

get_and_cache_fotmob_match_momentum <- function(match_id) {
  path <- file.path(MOMENTUM_DATA_DIR, paste0(match_id, '.qs'))
  if (file.exists(path)) {
    return(qs::qread(path))
  }
  Sys.sleep(runif(1, min = 1, max = 2))
  cli_inform('Scraping {.var match_id} = {.val {match_id}}.')
  res <- possibly_fotmob_get_match_momentum(match_id)
  qs::qsave(res, path)
  res
}

momentum <- map_dfr(
  unnested_matches$match_id, 
  get_and_cache_fotmob_match_momentum
)

qs::qsave(momentum, momentum_path)

momentum_match_ids <- momentum |> distinct(match_id)

possibly_fotmob_get_match_team_stats <- possibly(
  fotmob_get_match_team_stats,
  otherwise = tibble(),
  quiet = FALSE
)

get_and_cache_fotmob_match_team_stats <- function(match_id) {
  path <- file.path(MATCH_TEAM_STATS_DATA_DIR, paste0(match_id, '.qs'))
  if (file.exists(path)) {
    return(qs::qread(path))
  }
  Sys.sleep(runif(1, min = 1, max = 2))
  cli_inform('Scraping {.var match_id} = {.val {match_id}}.')
  res <- possibly_fotmob_get_match_team_stats(match_id)
  qs::qsave(res, path)
  res
}

match_team_stats <- map_dfr(
  momentum_match_ids$match_id, 
  get_and_cache_fotmob_match_team_stats
)

qs::qsave(match_team_stats, match_team_stats_path)
