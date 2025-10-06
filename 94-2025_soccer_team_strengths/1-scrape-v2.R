library(dplyr)
library(readr)
library(lubridate)
library(stringr)

library(janitor)
library(rvest)
library(httr)

PROJ_DIR <- '94-2025_soccer_team_strengths'
DATA_DIR <- file.path(PROJ_DIR, 'data')
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)

BASE_URL <- 'https://www.pitchrank.fyi'
resp_base <- httr::GET(BASE_URL)
index_filename <- httr::content(resp_base) |> 
  rvest::html_elements('head > script[type="module"]') |> 
  rvest::html_attr('src')
resp_index <- httr::GET(paste0(BASE_URL, index_filename))
cont_index <- httr::content(resp_index, as = 'text')
TODAY <- lubridate::today()

temp_path <- file.path(DATA_DIR, paste0(TODAY, '.txt'))
readr::write_lines(cont_index, temp_path)

rm(list = c('cont_index', 'resp_index'))

init_idx_guess <- 100
final_idx_guess <- 139000
init_data <- readr::read_lines(temp_path, skip = init_idx_guess, n_max = 50)
final_data <- readr::read_lines(temp_path, skip = final_idx_guess, n_max = 1000)
init_idx_offset <- stringr::str_which(init_data, '2012-10-24,AC')
final_idx_offset <- max(stringr::str_which(final_data, 'Zaragoza'))
rm(list = c('init_data', 'final_data'))
gc()

raw <- readr::read_delim(
  temp_path,
  delim = ',',
  skip = init_idx_guess + init_idx_offset,
  n_max = (init_idx_guess + init_idx_offset) - (final_idx_guess + final_idx_offset),
  col_names = c(
    'Date', 'Team', 'League', 'NumMatches', 'Strength_adjusted', 'is_active_2526'
  )
) |> 
  janitor::clean_names() |>
  dplyr::transmute(
    date = as.Date(date),
    team,
    country = league,
    num_matches = as.integer(num_matches),
    strength_adjusted = as.numeric(strength_adjusted),
    is_active_2526 = as.logical(is_active_2526)
  )

readr::write_csv(
  raw,
  file.path(DATA_DIR, sprintf('py_%s.csv', TODAY)),
  na = ''
)
