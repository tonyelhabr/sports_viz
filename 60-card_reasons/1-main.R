library(tidyverse)
library(lubridate)
library(worldfootballR)
library(jsonlite)
library(janitor)

dir_proj <- '60-card_reasons'
dir_data <- file.path(dir_proj, 'data')
dir.create(dir_data, showWarnings = FALSE)

fotmob_api_url <- 'https://www.fotmob.com/api'
scrape_ticker <- function(match_id, overwrite = FALSE) {
  
  path <- file.path(dir_data, sprintf('%s.json.gz', match_id))
  if (file.exists(path) & !overwrite) {
    cli::cli_inform('Returning early for {match_id}.')
    return(path)
  }
  cli::cli_inform('Scraping data for {match_id}.')
  url <- sprintf('%s/matchDetails?matchId=%s', fotmob_api_url, match_id)
  resp <- jsonlite::fromJSON(url)
  ticker_url <- sprintf('http://%s', resp$content$liveticker$url)
  download.file(ticker_url, destfile = path, quiet = TRUE)
  path
}

read_json_gz <- function(path) {
  suppressWarnings(lines <- path |> gzfile() |> readLines())
  jsonlite::fromJSON(lines)
}

matches_by_date <- load_fotmob_matches_by_date(league_id = 130)
filt_matches_by_date <- matches_by_date |> 
  filter(date == lubridate::ymd('2022-05-15'))

tickers <- filt_matches_by_date$match_id |> 
  map_dfr(
    ~scrape_ticker(.x) |> 
      read_json_gz() |> 
      pluck('Events'),
    .id = 'match_id'
  ) |> 
  clean_names() |> 
  as_tibble() |> 
  select(incident_code, elapsed, elapsed_plus, description, event_id)
