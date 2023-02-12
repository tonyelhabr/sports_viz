library(rvest)
library(xml2)
library(purrr)
library(tibble)
library(readr)
library(dplyr)

proj_dir <- '65-opta_xg_calib'
data_dir <- file.path(proj_dir, 'data')
footedness_data_dir <- file.path(data_dir, 'footedness')
players_meta_data_dir <- file.path(data_dir, 'players', 'meta')
dir.create(footedness_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(players_meta_data_dir, recursive = TRUE, showWarnings = FALSE)

generate_league_url <- function(season_start_year) {
  if (season_start_year == 2022) {
    first_sep <- ''
    second_sep <- ''
    season_str <- ''
  } else {
    first_sep <- '/'
    second_sep = '-'
    season_str <- sprintf('%04d-%04d', season_start_year, season_start_year + 1)
  }
  
  sprintf(
    'https://fbref.com/en/comps/9/%s%sstats/%s%sPremier-League-Stats',
    season_str, 
    first_sep,
    season_str,
    second_sep
  )
}

retrieve_league_players <- function(url) {
  # url <- league_urls[1]
  page <- read_html_live(url)
  cells <- page |>
    html_elements('tbody') |> 
    pluck(3) |> 
    html_elements('tr > td > a')
  stats <- html_attr(cells, 'data-stat')
  hrefs <- html_attr(cells, 'href')
  text <- html_text2(cells)
  stopifnot(length(hrefs) == length(text))
  idx <- seq(1, length(cells), by = 4)
  tibble(
    player = text[idx],
    team = text[idx + 2],
    link = paste0('https://fbref.com/', hrefs[idx])
  )
}

insistently_retrieve_league_players <- insistently(
  retrieve_league_players, 
  quiet = FALSE
)

retrieve_and_save_league_players <- function(season) {
  url <- generate_league_url(season)
  path <- file.path(footedness_data_dir, sprintf('ENG-1st-M-%s.rds', season))
  if (file.exists(path)) {
    message(sprintf('Reading in from "%s".', path))
    return(read_rds(path))
  }
  message(sprintf('Collecting data at "%s".', url))
  
  res <- insistently_retrieve_league_players(url)
  write_rds(res, path)
  res
}

league_players <- 2017:2022 |> 
  map_dfr(
    ~retrieve_and_save_league_players(.x) |>
      mutate(
        season_start_year = as.integer(.x),
        .before = 1
      )
  ) |> 
  distinct(player_url = link)

retrieve_player_meta <- function(url) {
  page <- read_html(url)
  page |> 
    html_elements('#meta > div > p') |> 
    html_text2()
}

retrieve_and_save_players_meta <- function(url) {
  path <- file.path(players_meta_data_dir, paste0(basename(dirname(url)), '.txt'))
  if (file.exists(path)) {
    message(sprintf('Reading in from "%s".', path))
    return(read_lines(path))
  }
  message(sprintf('Collecting data at "%s".', url))
  Sys.sleep(3)
  res <- retrieve_player_meta(url)
  write_lines(res, path)
  res
}

possibly_retrieve_and_save_players_meta <- possibly(
 retrieve_and_save_players_meta,
 otherwise = character(),
 quiet = FALSE
)

players_meta <- league_players$player_url |> 
  map_dfr(
    ~{
      res <- possibly_retrieve_and_save_players_meta(.x)
      tibble(text = res) |> 
        mutate(
          player_url = .x,
          .before = 1
        )
    }
  )
players_meta |>
  filter(stringr::str_detect(text, '???'))
  tidyr::separate(
    text,
    c('x1', 'x2', 'x3'),
    sep = ':'
  )
  