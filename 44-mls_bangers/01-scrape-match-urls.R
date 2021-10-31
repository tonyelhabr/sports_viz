
library(httr)
library(jsonlite)
library(dplyr)
library(janitor)
library(tibble)
library(lubridate)
library(tidyr)
library(stringr)
library(qs)
library(cli)
library(purrr)

dir_proj <- '44-mls_bangers'
dir_data <- file.path(dir_proj, 'data')
# fs::dir_create(dir_data)

base_url <- 'https://www.fotmob.com/'

get_matches_by_date <- function(date, overwrite = FALSE, path = file.path(dir_data, sprintf('matches-%s.qs', date)), delay = 1) {
  url <- paste0(base_url, 'matches?date=', str_remove_all(as.character(date), '-'))

  if(file.exists(path) & !overwrite) {
    cli::cli_alert_info(sprintf('Reading in matches from %s.', date))
    return(qs::qread(path))
  }
  Sys.sleep(delay)
  cli::cli_alert_info(sprintf('%s: Scraping matches on %s.', Sys.time(), date))
  resp <- jsonlite::fromJSON(url)
  qs::qsave(resp, path)
  resp
}

get_match <- function(match_id, overwrite = FALSE, path = file.path(dir_data, sprintf('match_id-%s.qs', match_id)), delay = 0.5) {
  url <- paste0(base_url, 'matchDetails?matchId=', match_id)
  
  if(file.exists(path) & !overwrite) {
    cli::cli_alert_info(sprintf('Reading in `match_id = %d`.', match_id))
    return(qs::qread(path))
  }
  Sys.sleep(delay)
  cli::cli_alert_info(sprintf('%s: Scraping `match_id = %d`.', Sys.time(), match_id))
  resp <- jsonlite::fromJSON(url)
  qs::qsave(resp, path)
  resp
}

dates_v <- seq.Date(lubridate::ymd('2016-08-01'), lubridate::ymd('2021-10-31'), by = 'day')
matches <- dates_v %>% map(get_matches_by_date)
pluck_matches <- function(x) {
  bind_cols(tibble(date = lubridate::ymd(x$date)), x$leagues)
}

matches_flat <- matches %>% 
  map_dfr(~pluck_matches(.x))

league_mapping <- tibble(
  league_id = c(47, 54, 87, 53, 55, 130),
  league_name = c('EPL', 'Bundesliga', 'LaLiga', 'Ligue 1', 'Serie A', 'MLS')
)

match_ids <- matches_flat %>% 
  semi_join(
    league_mapping,
    by = c('primaryId' = 'league_id')
  ) %>% 
  janitor::clean_names() %>% 
  hoist(
    matches,
    'match_id' = 'id'
  ) %>% 
  select(date, ccode, id, primary_id, name, match_id) %>% 
  unnest_longer(match_id)

pluck_match_data <- function(x) {
  content <- x$content
  bind_cols(
    tibble(match_id = content$matchFacts$matchId),
    content$shotmap$shots
  )
}

match_data <- match_ids %>% 
  pull(match_id) %>% 
  map(get_match) %>% 
  map_dfr(pluck_match_data) %>% 
  janitor::clean_names()
match_data
