
library(tidyverse)
library(worldfootballR)
library(glue)
library(cli)
library(janitor)

dir_proj <- '54-202122_ucl_final'
dir_data <- file.path(dir_proj, 'data')
path_urls <- file.path(dir_proj, 'urls.rds')
path_data <- file.path(dir_proj, 'data.rds')
dir.create(dir_data, showWarnings = FALSE)

if(!file.exists(path_urls)){
  urls <- tibble(
    url = c(
      'https://fbref.com/en/squads/822bd0ba/Liverpool-Stats',
      'https://fbref.com/en/squads/53a2f082/Real-Madrid-Stats'
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

scrape_player <- function(url, stat_type, overwrite = FALSE) {
  player <- url |> basename()
  path <- file.path(dir_data, sprintf('%s-%s.rds', stat_type, player))
  suffix <- glue::glue('for `stat_type = "{stat_type}"`, `player = "{player}"`.')
  if(file.exists(path) & !overwrite) {
    cli::cli_inform(glue::glue('{Sys.time()}: Returning early {suffix}'), sep = '\n')
    return(read_rds(path))
  }
  res <- worldfootballR::fb_player_season_stats(url, stat_type)
  cli::cli_inform(glue::glue('{Sys.time()}: Retrieved data {suffix}'), sep = '\n')
  write_rds(res, path)
  res
}

scrape_player_profile <- function(url, overwrite = FALSE) {
  player <- url |> basename()
  path <- file.path(dir_data, sprintf('profile-%s.rds', player))
  suffix <- glue::glue('for `player = "{player}"`.')
  if(file.exists(path) & !overwrite) {
    cli::cli_inform(glue::glue('{Sys.time()}: Returning early {suffix}'), sep = '\n')
    return(read_rds(path))
  }
  res <- worldfootballR::fb_player_scouting_report(url, 'primary')
  cli::cli_inform(glue::glue('{Sys.time()}: Retrieved data {suffix}'), sep = '\n')
  write_rds(res, path)
  res
}

possibly_scrape_player <- possibly(scrape_player, otherwise = tibble(), quiet = FALSE)
misc <- setNames(
  urls$url,
  basename(urls$url)
) |> 
  map_dfr(
    possibly_scrape_player,
    stat_type = 'misc',
    .id = 'player_name'
  )

possibly_scrape_player_profile <- possibly(scrape_player_profile, otherwise = tibble(), quiet = FALSE)
profiles <- setNames(
  urls$url,
  basename(urls$url)
) |> 
  map_dfr(
    possibly_scrape_player_profile,
    .id = 'player_name'
  )

teams <- misc |> 
  filter(Season == '2021-2022') |> 
  distinct(player_name, player_url, team_name = Squad) |> 
  filter(team_name %in% c('Liverpool', 'Real Madrid')) |> 
  as_tibble()

profiles |> 
  inner_join(
    teams,
    by = 'player_name'
  ) |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  write_rds(path_data)
