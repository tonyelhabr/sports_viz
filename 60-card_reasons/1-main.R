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
  Sys.sleep(0.5)
  cli::cli_inform('Scraping data for {match_id}.')
  url <- sprintf('%s/matchDetails?matchId=%s', fotmob_api_url, match_id)
  resp <- jsonlite::fromJSON(url)
  ticker_url <- sprintf('http://%s', resp$content$liveticker$url)
  download.file(ticker_url, destfile = path, quiet = TRUE)
  path
}
slowly_scrape_ticker <- slowly(scrape_ticker, rate = rate_delay(pause = 0.5), quiet = FALSE)

read_json_gz <- function(path) {
  suppressWarnings(lines <- path |> gzfile() |> readLines())
  jsonlite::fromJSON(lines)
}
possibly_read_json_gz <- possibly(read_json_gz, otherwise = list('Events' = tibble()), quiet = FALSE)

league_id_mapping <- c(
  'ENG' = 47,
  'FRA' = 53,
  'GER' = 54,
  'ITA' = 55,
  'ESP' = 87,
  'USA' = 130
)

league_start_dates <- league_id_mapping |> 
  imap_dfr(
    ~load_match_results(
      country = .y, 
      gender = 'M', 
      season_end_year = 2021, 
      tier = '1st'
    ) |> 
      slice_min(Date, n = 1, with_ties = FALSE) |> 
      select(date = Date) |> 
      mutate(league_id = .x, .before = 1)
  )
league_start_date_mapping <- setNames(league_start_dates$date, league_start_dates$league_id)

scrape_tickers_for_league <- function(league_id, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('%s.qs', league_id))
  if (file.exists(path) & !overwrite) {
    return(qs::qread(path))
  }
  
  first_date <- if (league_id %in% other_league_ids) {
    as.Date('2020-06-01')
  } else {
    as.Date(league_start_date_mapping[[as.character(league_id)]])
  }
  
  matches_by_date <- load_fotmob_matches_by_date(league_id = league_id)
  filt_matches_by_date <- matches_by_date |> 
    filter(date >= first_date)
  
  tickers <- filt_matches_by_date |> 
    distinct(league_id = primary_id, match_id) |> 
    mutate(
      data = map(
        match_id,
        ~scrape_ticker(.x) |> 
          possibly_read_json_gz() |> 
          pluck('Events')
      )
    ) |> 
    unnest(data) |> 
    clean_names() |> 
    as_tibble() |> 
    select(league_id, match_id, incident_code, elapsed, elapsed_plus, description, event_id)
  qs::qsave(tickers, path)
  tickers
}

tickers <- league_id_mapping |> 
  map_dfr(~scrape_tickers_for_league(.x, overwrite = TRUE)) |> 
  mutate(
    idx = row_number(),
    .before = 1
  )
tickers

card_incident_codes <- c('YC', 'RC', 'Y2C')

words <- tickers |> 
  filter(incident_code %in% card_incident_codes) |> 
  tidytext::unnest_tokens('word', description) |> 
  anti_join(tidytext::stop_words, by = 'word') |> 
  mutate(
    stem = word |> SnowballC::wordStem()
  )

word_counts <- words |> 
  count(stem, sort = TRUE)

stem_counts <- words |> 
  count(stem, sort = TRUE)

interesting_stems <- list(
  # 'foul',
  # 'tackle',
  #   # 'ground',
  'celebration' = 'celebr',
  'excessive' = 'excess',
  'dissent' = 'dissent',
  'clumsy' = c('clumsi', 'clumsili'),
  'timewasting' = c('wast', 'timewast'),
  'violent' = 'violent',
  'conduct' = 'conduct'
)

flat_interesting_stems <- flatten_chr(interesting_stems)

interesting_stems <- words |> 
  filter(stem %in% flat_interesting_stems) |> 
  distinct(idx, league_id, stem)

interesting_stems |> 
  count(stem, league_id) |> 
  tidytext::bind_tf_idf(stem, league_id, n) |> 
  # filter(stem %in% flat_interesting_stems) |> 
  arrange(desc(tf_idf))

ns <- tickers |> 
  group_by(league_id) |> 
  summarize(
    n_matches = n_distinct(match_id),
    n_cards = sum(incident_code %in% card_incident_codes)
  )

interesting_ns <- tickers |> 
  semi_join(
    interesting_stems,
    by = 'idx'
  )|> 
  group_by(league_id) |> 
  summarize(
    n_interesting = n()
  )

ns |> 
  inner_join(
    interesting_ns
  ) |> 
  mutate(
    ratio = n_cards / n_matches,
    ratio = n_interesting / n_matches
  )
