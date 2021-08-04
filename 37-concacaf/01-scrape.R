

library(tidyverse)
dir_proj <- '37-concacaf'
dir_data <- file.path(dir_proj, 'data')
fs::dir_create(dir_data)

params <-
  bind_rows(
    crossing(
      country = '', 
      gender = 'M', 
      # season_end_year = c(2017, 2019, 2021), # c(2016, 2021), 
      season_end_year = c(2016, 2021), 
      tier = '', 
      non_dom_league_url = 'https://fbref.com/en/comps/676/history/European-Championship-Seasons'
      # non_dom_league_url = 'https://fbref.com/en/comps/681/history/CONCACAF-Gold-Cup-Seasons'
    ) %>% 
      mutate(league = 'UEFA'),
    crossing(
      country = '', 
      gender = 'M', 
      # season_end_year = c(2017, 2019, 2021), # c(2016, 2021), 
      season_end_year = c(2015, 2016, 2019, 2021), 
      tier = '', 
      non_dom_league_url = 'https://fbref.com/en/comps/685/history/Copa-America-Seasons'
      # non_dom_league_url = 'https://fbref.com/en/comps/681/history/CONCACAF-Gold-Cup-Seasons'
    ) %>% 
      mutate(league = 'CONMEBAL'),
    crossing(
      country = '', 
      gender = 'M', 
      season_end_year = c(2015, 2017, 2019, 2021),
      tier = '', 
      non_dom_league_url = 'https://fbref.com/en/comps/681/history/CONCACAF-Gold-Cup-Seasons'
    ) %>% 
      mutate(league = 'CONCACAF')
  )
params

urls <-
  params %>%
  # filter(season_end_year == 2021) %>% 
  filter(league == 'CONMEBAL') %>% 
  mutate(url = pmap(list(country, gender, season_end_year, tier, non_dom_league_url), worldfootballR::get_match_urls)) %>% 
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

scrape_results <- function(country, gender, season_end_year, tier, non_dom_league_url, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('results-%s-%s-%s-%s-%s.rds', country, gender, season_end_year, tier, basename(non_dom_league_url)))
  suffix <- glue::glue('for `country = {country}`, `gender = "{gender}"`, `season_end_year = "{season_end_year}"`, `tier = "{tier}"`, `non_dom_league_url = "{non_dom_league_url}"`')
  if(file.exists(path) & !overwrite) {
    .display_info_early(suffix)
    return(read_rds(path))
  }
  res <- worldfootballR::get_match_results(country, gender, season_end_year, tier, non_dom_league_url)
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
  filter(league == 'CONMEBAL') %>% 
  mutate(data = pmap(list(country, gender, season_end_year, tier, non_dom_league_url), f))
results %>% unnest(data)

g <- possibly(scrape_misc_stats, otherwise = tibble(), quiet = FALSE)
stats <- urls %>% filter(league == 'CONMEBAL') %>% mutate(data = map(url, g))
urls %>% filter(league == 'CONMEBAL', season_end_year == 2021) %>% mutate(data = map(url, g, stat_type = 'misc'))
stats %>% unnest(data) %>% count(league, season_end_year)
