
library(tidyverse)
dir_proj <- '49-yellow_cards'
dir_data <- file.path(dir_proj, 'data')
fs::dir_create(dir_data)

params <- crossing(
  country = c('ENG'), # , 'ITA', 'GER', 'FRA', 'ESP'), # , 'USA', 'MEX'),
  gender = c('M'),
  season_end_year = c(2022),
  tier = '1st'
)
params

urls <- params %>% 
  mutate(
    url = 
      pmap(
        list(country, gender, season_end_year, tier),
        worldfootballR::get_match_urls
      )
  ) %>% 
  unnest(url)
urls

scrape_misc_stats <- function(url, stat_type = 'misc', team_or_player = 'player', overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('stats-%s-%s-%s.rds', basename(url), stat_type, team_or_player))
  suffix <- glue::glue('for `url = "{url}"`, `stat_type = "{stat_type}"`, `team_or_player = "{team_or_player}"`.')
  
  if(file.exists(path) & !overwrite) {
    cat(glue::glue('{Sys.time()}: Returning early {suffix}'), sep = '\n')
    return(read_rds(path))
  }
  res <- worldfootballR::get_advanced_match_stats(url, stat_type = stat_type, team_or_player = team_or_player)
  cat(glue::glue('{Sys.time()}: Retrieved data {suffix}'), sep = '\n')
  write_rds(res, path)
  res
}

f <- possibly(scrape_misc_stats, otherwise = tibble())
stats <- urls %>% 
  mutate(data = map(url, f))
stats %>% 
  select(data) %>% 
  unnest(data)
