
library(tidyverse)
library(worldfootballR)
library(glue)
library(cli)
library(qs)
library(janitor)
library(readr)

dir_proj <- '56-attendance'
dir_data <- file.path(dir_proj, 'data')
dir_img <- file.path(dir_proj, 'img')
dir.create(dir_data, showWarnings = FALSE)
dir.create(dir_img, showWarnings = FALSE)

## outputs
path_attendance <- file.path(dir_proj, 'attendance.qs')
path_importance <- file.path(dir_proj, 'importance.qs')
path_team_logos <- file.path(dir_proj, 'team_logos.qs')

params <- crossing(
  country = c('USA', 'ENG'),
  gender = 'M',
  season_end_year = c(2016:2022),
  tier = c('1st', '2nd')
)
params

get_team_urls <- function(...) {
  res <- worldfootballR::fb_league_urls(...)
  
  if(length(res) == 0) {
    return(tibble())
  }
  urls <- res |> 
    worldfootballR::fb_teams_urls()
  tibble(url = urls)
}
possibly_get_team_urls <- possibly(get_team_urls, otherwise = tibble(), quiet = TRUE)

scrape_team_urls <- function(country, gender, season_end_year, tier, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('team_urls-%s-%s-%s-%s.qs', country, gender, season_end_year, tier))
  suffix <- glue::glue('for `country = {country}`, `gender = "{gender}"`, `season_end_year = "{season_end_year}"`, `tier = "{tier}"`.')
  if(file.exists(path) & !overwrite) {
    cli::cli_inform(glue::glue('{Sys.time()}: Returning early {suffix}'))
    return(qs::qread(path))
  }
  res <- possibly_get_team_urls(country, gender, season_end_year, tier)
  cli::cli_inform(glue::glue('{Sys.time()}: Retrieved data {suffix}'))
  qs::qsave(res, path)
  res
}
possibly_scrape_team_urls <- possibly(scrape_team_urls, otherwise = tibble(), quiet = FALSE)

team_urls <- params |> 
  mutate(
    urls = 
      pmap(
        list(country, gender, season_end_year, tier), 
        ~possibly_scrape_team_urls(..1, ..2, ..3, ..4)
      )
  ) |> 
  unnest(urls)

scrape_fbref_team_img <- function(url, name, overwrite = FALSE) {
  path_img <- file.path(dir_img, sprintf('%s.png', name))
  path <- file.path(dir_data, sprintf('fbref_team_img-%s.qs', name))
  suffix <- sprintf('for %s.', name)
  if(file.exists(path) & !overwrite) {
    cli::cli_inform(glue::glue('{Sys.time()}: Returning early {suffix}'))
    return(qs::qread(path))
  }
  page <- rvest::read_html(url)
  team <- page |> 
    rvest::html_element(xpath = '//*[@id="inner_nav"]/ul/li[1]/a') |> 
    rvest::html_text2() |> 
    str_remove('\\sStats & History')
  img_url <- page |> 
    rvest::html_element(xpath = '//*[@id="meta"]/div[1]/img') |>
    rvest::html_attr('src')
  if(!file.exists(path_img)) {
    download.file(img_url, destfile = path_img, mode = 'wb', quiet = TRUE)
  }
  res <- tibble(
    url = url,
    name = name,
    team = team,
    path = path_img
  )
  cli::cli_inform(glue::glue('{Sys.time()}: Retrieved data {suffix}'))
  qs::qsave(res, path)
  res
}

possibly_scrape_fbref_team_img <- possibly(scrape_fbref_team_img, otherwise = tibble(), quiet = FALSE)
slowly_scrape_fbref_team_img <- slowly(possibly_scrape_fbref_team_img)

## logic used in worldfootballR
extract_team_name <- function(url) {
  sub('.*\\/', '', url) %>% gsub('-Stats', '', .) %>% gsub('-', ' ', .)
}

team_logos <- team_urls |> 
  mutate(
    season = url |> dirname() |> basename(),
    name = url |> extract_team_name(),
    .before = 1
  ) |> 
  group_by(name) |> 
  slice_max(season, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  distinct(name, url) |> 
  deframe() |> 
  imap_dfr(
    ~slowly_scrape_fbref_team_img(.x, .y)
  )
team_logos |> qs::qsave(path_team_logos)

possibly_get_match_results <- possibly(worldfootballR::get_match_results, otherwise = tibble())

scrape_results <- function(country, gender, season_end_year, tier, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('results-%s-%s-%s-%s.qs', country, gender, season_end_year, tier))
  suffix <- glue::glue('for `country = {country}`, `gender = "{gender}"`, `season_end_year = "{season_end_year}"`, `tier = "{tier}"`.')
  if(file.exists(path) & !overwrite) {
    cli::cli_inform(glue::glue('{Sys.time()}: Returning early {suffix}'))
    return(qs::qread(path))
  }
  res <- possibly_get_match_results(country, gender, season_end_year, tier)
  cli::cli_inform(glue::glue('{Sys.time()}: Retrieved data {suffix}'))
  qs::qsave(res, path)
  res
}

possibly_scrape_results <- possibly(scrape_results, otherwise = tibble(), quiet = FALSE)

results <- params %>% 
  mutate(
    data = 
      pmap(
        list(country, gender, season_end_year, tier), 
        possibly_scrape_results
      )
  )

attendance <- results %>% 
  unnest(data) |> 
  janitor::clean_names() |> 
  mutate(
    across(venue, ~str_remove(.x, '\\s+(Neutral Site)')),
    across(
      attendance,
      ~case_when(
        venue == 'Al Lang Stadium' & date == lubridate::ymd('2022-03-26') ~ round(.x / 10),
        TRUE ~ .x
      )
    )
  ) |> 
  select(
    country,
    gender,
    league = competition_name,
    season = season_end_year,
    date,
    wk,
    day,
    home_team = home,
    away_team = away,
    attendance,
    venue
  ) |> 
  arrange(season, league, date, home_team)
attendance |> filter(venue |> str_detect('Pratt')) |> distinct(venue)
attendance |> qs::qsave(path_attendance)

matches_538 <-  read_csv(
  'https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv',
  col_types = cols(
    .default = col_double(),
    date = col_date(format = ''),
    league = col_character(),
    team1 = col_character(),
    team2 = col_character()
  )
)
matches_538 |> 
  filter(
    league %in% c(
      'Barclays Premier League',
      'English League Championship',
      'Major League Soccer',
      'United Soccer League'
    )
  ) |> 
  rename_with(
    ~str_replace(.x, '(^.*)1$', 'home_\\1'), 
    matches('1$')
  ) %>% 
  rename_with(
    ~str_replace(.x, '(^.*)2$', 'away_\\1'), 
    matches('2$')
  ) %>% 
  select(
    season,
    date,
    league,
    home_team,
    away_team,
    home_importance,
    away_importance
  ) |> 
  arrange(season, league, date, home_team) |> 
  qs::qsave(path_importance)

