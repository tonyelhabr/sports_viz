
library(worldfootballR)
library(tidyverse)

dir_proj <- '42-202122_new_players'
path_stats <- file.path(dir_proj, 'tm_stats.rds')
path_player_bios <- file.path(dir_proj, 'tm_player_bios.rds')
urls <- 
  crossing(
    start_year = 2020:2021,
    league_url = sprintf('https://www.transfermarkt.us/championship/startseite/wettbewerb/GB%d', 1:2)
  ) %>% 
  mutate(team_url = map2(start_year, league_url, ~worldfootballR::tm_league_team_urls(start_year = ..1, league_url = ..2)))
urls %>% unnest(team_url)

player_urls <-
  urls %>% 
  unnest(team_url) %>% 
  mutate(
    player_url = map(team_url, tm_team_player_urls)
  ) %>% 
  unnest(player_url)
player_urls

n_urls <- nrow(player_urls)
i <- 1
retreive_bio <- function(url) {
  res <- worldfootballR::tm_player_bio(url)
  i <<- i + 1
  cat(paste0('Downloaded url ', i, ' of ', n_urls, '.', sep = ''), '\n')
  res
}

player_bios <- player_urls %>% mutate(bio = map(player_url, retreive_bio))
player_bios_cleaned <-
  player_bios %>% 
  # head(10) %>% 
  # mutate(
  #   
  #   bio = map(bio, ~mutate_all(.x, as.character))
  # ) %>% 
  mutate(
    bio = map(bio, ~mutate(.x, across(everything(), as.character)))
  ) %>% 
  unnest(bio) %>% 
  type_convert()
player_bios_cleaned
write_rds(player_bios_cleaned, path_player_bios)

stats <- read_rds(path_stats)
stats <- 
  urls %>%
  mutate(
    league = ifelse(str_sub(league_url, -1) == '1', 'Premier League', 'Championship')
  ) %>% 
  select(start_year, league, team_url) %>% 
  unnest(team_url) %>% 
  mutate(data = map(team_url, worldfootballR::tm_squad_stats)) %>% 
  unnest(data)
write_rds(stats, path_stats)
stats %>% 
  distinct(player_name) %>% 
  left_join(player_bios_cleaned %>% distinct(player_name, player_url))
