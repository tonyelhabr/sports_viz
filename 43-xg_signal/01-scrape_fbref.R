

library(tidyverse)
dir_proj <- '43-xg_signal'
dir_data <- file.path(dir_proj, 'data')
fs::dir_create(dir_data)

params <-
  crossing(
    country = c('ENG', 'ITA', 'GER', 'FRA', 'ESP', 'USA', 'MEX'),
    gender = c('M'),
    season_end_year = c(2018:2021),
    tier = '1st'
  )
params

urls <-
  params %>%
  mutate(url = pmap(list(country, gender, season_end_year, tier), worldfootballR::get_match_urls)) %>% 
  unnest(url)
urls

.display_info <- function(..., .envir = parent.frame(), .stem = '') {
  x <- glue::glue_collapse(..., sep = '\n')
  x <- glue::glue(x, .envir = .envir)
  cli::cat_line(sprintf('%s: %s%s.', Sys.time(), .stem, x))
}

.display_info_early <- function(..., .envir = parent.frame()) {
  .display_info(..., .envir = .envir, .stem = 'Returning early ')
}

.display_info_after <- function(..., .envir = parent.frame()) {
  .display_info(..., .envir = .envir, .stem = 'Returning data ')
}

scrape_results <- function(country, gender, season_end_year, tier, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('results-%s-%s-%s-%s.rds', country, gender, season_end_year, tier))
  suffix <- glue::glue('for `country = {country}`, `gender = "{gender}"`, `season_end_year = "{season_end_year}"`, `tier = "{tier}"`.')
  if(file.exists(path) & !overwrite) {
    .display_info_early(suffix)
    return(read_rds(path))
  }
  res <- worldfootballR::get_match_results(country, gender, season_end_year, tier)
  .display_info_after(suffix)
  write_rds(res, path)
  res
}

scrape_misc_stats <- function(url, stat_type = 'summary', team_or_player = 'team', overwrite = FALSE) {
  
  path <- file.path(dir_data, sprintf('stats-%s-%s-%s.rds', basename(url), stat_type, team_or_player))
  suffix <- glue::glue('for `url = "{url}"`, `stat_type = "{stat_type}"`, `team_or_player = "{team_or_player}"`')
  
  if(file.exists(path) & !overwrite) {
    .display_info_early(suffix)
    return(read_rds(path))
  }
  res <- worldfootballR::get_advanced_match_stats(url, stat_type = stat_type, team_or_player = team_or_player)
  .display_info_after(suffix)
  write_rds(res, path)
  res
}

f <- possibly(scrape_results, otherwise = tibble(), quiet = FALSE)
results <- 
  params %>% 
  mutate(data = pmap(list(country, gender, season_end_year, tier), f))
results %>% unnest(data)

g <- possibly(scrape_misc_stats, otherwise = tibble(), quiet = FALSE)
stats <- urls %>% mutate(data = map(url, g))
