

library(tidyverse)
dir_proj <- '39-cmsac'
dir_data <- file.path(dir_proj, 'data')
path_urls <- file.path(dir_proj, 'urls.rds')
fs::dir_create(dir_data)
overwrite <- FALSE

params <-
  crossing(
    country = c('ENG', 'ITA', 'GER', 'FRA', 'ESP', 'USA'),
    gender = c('M'),
    season_end_year = c(2021),
    tier = '1st'
  )
params

urls <-
  params %>% 
  # slice(1) %>% 
  mutate(
    url = 
      pmap(
        list(country, gender, season_end_year, tier), 
        ~worldfootballR::fb_league_urls(..1, ..2, ..3, ..4) %>% 
          # For some reason, piping directly into `fb_player_urls` was causing issues,
          # so I've separated out into its own step.
          worldfootballR::fb_teams_urls()
      )
  ) %>% 
  unnest(url) %>% 
  mutate(url_player = map(url, worldfootballR::fb_player_urls)) %>%
  unnest(url_player) %>% 
  select(-url) %>% 
  rename(url = url_player)
write_rds(urls, path_urls)
# urls <- path_urls %>% read_rds()

scrape_player <- function(country, gender, season_end_year, tier, url) {
  # browser()
  player <- url %>% basename() %>% tools::file_path_sans_ext()
  path <- file.path(dir_data, sprintf('%s-%s-%s-%s-%s.rds', country, gender, season_end_year, tier, player))
  suffix <- glue::glue('for `country = {country}`, `gender = "{gender}"`, `season_end_year = "{season_end_year}"`, `tier = "{tier}"`, player = "{player}".')
  # browser()
  if(file.exists(path) & !overwrite) {
    cat(glue::glue('{Sys.time()}: Returning leauge url early {suffix}'), sep = '\n')
    return(read_rds(path))
  }
  res <- worldfootballR::fb_player_season_stats(url, 'standard')
  cat(glue::glue('{Sys.time()}: Retrieved data {suffix}'), sep = '\n')
  write_rds(res, path)
  res
}

scrape_player_safely <- safely(scrape_player, otherwise = tibble())
res <-
  urls %>% 
  mutate(
    data = pmap(
      list(country, gender, season_end_year, tier, url),
      scrape_player_safely
    )$result
  )
res
