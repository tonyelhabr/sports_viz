
library(tidyverse)
dir_proj <- '25-202021_soccer_refs'
dir_data <- file.path(dir_proj, 'data')
fs::dir_create(dir_data)
path_results <- file.path(dir_proj, 'results.rds')
path_stats <- file.path(dir_proj, 'stats.rds')

params <-
  crossing(
    country = c('ENG', 'ITA', 'GER', 'FRA', 'ESP'),
    gender = c('M'),
    season_end_year = c(2020:2021),
    tier = '1st'
  )
params

urls <-
  params %>% 
  mutate(url = pmap(list(country, gender, season_end_year, tier), worldfootballR::get_match_urls))%>% 
  unnest(url)
urls

f <- possibly(worldfootballR::get_match_results, otherwise = tibble())
results_nest <- 
  params %>% 
  mutate(data = pmap(list(country, gender, season_end_year, tier), f))
beepr::beep(3)

results <-
  results_nest %>% 
  unnest(data) %>% 
  janitor::clean_names() %>% 
  select(-matches('_2$'))
results
write_rds(results, path_results)

scrape_misc_stats <- function(url, stat_type = 'misc', team_or_player = 'player', overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('%s-%s-%s.rds', basename(url), stat_type, team_or_player))
  suffix <- glue::glue('for `url = {url}`, `stat_type = "{stat_type}"`, `team_or_player = "{team_or_player}"`.')
  if(file.exists(path) & !overwrite) {
    cat(glue::glue('{Sys.time()}: Returning early {suffix}'), sep = '\n')
    return(read_rds(path))
  }
  res <- worldfootballR::get_advanced_match_stats(url, stat_type = stat_type, team_or_player = team_or_player)
  cat(glue::glue('{Sys.time()}: Retrieved data {suffix}'), sep = '\n')
  write_rds(res, path)
  res
}

g <- possibly(scrape_misc_stats, otherwise = tibble())
stats <-
  urls %>% 
  mutate(data = map(url, g)) %>% 
  unnest(data) %>% 
  as_tibble() %>% 
  janitor::clean_names()
stats
beepr::beep(3)
write_rds(stats, path_stats)

