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
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
momentum_path <- file.path(PROJ_DIR, 'momentum.qs')

matches <- fotmob_get_league_matches(league_id = 47, season = '2022/2023')
unnested_matches <- matches |>
  unnest(status, names_sep = '_') |> 
  filter(status_finished, !status_cancelled) |> 
  select(match_time = status_utcTime, match_id = id, home, away) |>
  unnest_wider(c(home, away), names_sep = '_') |> 
  transmute(
    match_time = ymd_hms(match_time), 
    across(match_id, as.integer),
    home_id,
    home_team = home_name,
    away_id,
    away_team = away_name
  )
## only have momentum after this date
filt_unnested_matches <- unnested_matches |> filter(match_time >= '2022-10-18')

possibly_fotmob_get_match_momentum <- possibly(
  fotmob_get_match_momentum,
  otherwise = tibble(),
  quiet = FALSE
)

get_and_cache_fotmob_match_momentum <- function(match_id) {
  path <- file.path(DATA_DIR, paste0(match_id, '.qs'))
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

filt_unnested_matches |> 
  filter(
    match_id %in% setdiff(filt_unnested_matches$match_id, momentum$match_id)
  ) |> 
  arrange(match_time)

qs::qsave(momentum, momentum_path)
