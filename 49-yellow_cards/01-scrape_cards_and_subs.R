
library(tidyverse)
dir_proj <- '49-yellow_cards'
dir_data <- file.path(dir_proj, 'data')
fs::dir_create(dir_data)

params <- crossing(
  country = c('ENG', 'ITA', 'GER', 'FRA', 'ESP'),
  gender = 'M',
  season_end_year = c(2013:2022),
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

scrape_f <- function(url, f, prefix, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('%s-%s.rds', prefix, basename(url)))
  suffix <- glue::glue('for `url = "{url}"`.')
  if(file.exists(path) & !overwrite) {
    cat(glue::glue('{Sys.time()}: Returning early {suffix}'), sep = '\n')
    return(read_rds(path))
  }
  res <- f(url)
  cat(glue::glue('{Sys.time()}: Retrieved data {suffix}'), sep = '\n')
  write_rds(res, path)
  res
}

scrape_match_summary <- partial(
  scrape_f,
  f = worldfootballR::get_match_summary,
  prefix = 'summary',
  ... = 
)

scrape_match_lineups <- partial(
  scrape_f,
  f = worldfootballR::get_match_lineups,
  prefix = 'lineups',
  ... = 
)

f <- possibly(scrape_match_summary, otherwise = tibble())
g <- possibly(scrape_match_lineups, otherwise = tibble())
urls$url %>% walk(f)
urls$url %>% walk(g)

