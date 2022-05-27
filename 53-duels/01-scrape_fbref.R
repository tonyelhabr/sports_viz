
library(tidyverse)

dir_proj <- '53-duels'
dir_data <- file.path(dir_proj, 'data')
path_urls <- file.path(dir_proj, 'urls.rds')
path_data <- file.path(dir_proj, 'misc.rds')
dir.create(dir_data, showWarnings = FALSE)

params <- crossing(
  country = 'ENG',
  gender = 'M',
  season_end_year = 2022,
  tier = '1st'
)

if(!file.exists(path_urls)){
  urls <- params |> 
    mutate(
      url = 
        pmap(
          list(country, gender, season_end_year, tier), 
          ~worldfootballR::fb_league_urls(..1, ..2, ..3, ..4) |> 
            worldfootballR::fb_teams_urls()
        )
    ) |> 
    unnest(url) |> 
    mutate(url_player = map(url, worldfootballR::fb_player_urls)) |>
    unnest(url_player) |> 
    select(-url) |> 
    rename(url = url_player)
  write_rds(urls, path_urls)
} else {
  urls <- read_rds(path_urls)
}

scrape_player <- function(country, gender, season_end_year, tier, url, overwrite = FALSE) {
  player <- url |> basename() |> tools::file_path_sans_ext()
  path <- file.path(dir_data, sprintf('%s-%s-%s-%s-%s.rds', country, gender, season_end_year, tier, player))
  suffix <- glue::glue('for `country = {country}`, `gender = "{gender}"`, `season_end_year = "{season_end_year}"`, `tier = "{tier}"`, player = "{player}".')
  if(file.exists(path) & !overwrite) {
    cat(glue::glue('{Sys.time()}: Returning leauge url early {suffix}'), sep = '\n')
    return(read_rds(path))
  }
  res <- worldfootballR::fb_player_season_stats(url, 'misc')
  cat(glue::glue('{Sys.time()}: Retrieved data {suffix}'), sep = '\n')
  write_rds(res, path)
  res
}

possibly_scrape_player <- possibly(scrape_player, otherwise = tibble(), quiet = FALSE)
misc <- urls |> 
  mutate(
    data = pmap(
      list(country, gender, season_end_year, tier, url),
      possibly_scrape_player
    )
  )
misc %>% 
  select(data) |> 
  unnest(data) |> 
  janitor::clean_names() |> 
  filter(season == '2021-2022', comp == '1. Premier League') |> 
  write_rds(path_data)
