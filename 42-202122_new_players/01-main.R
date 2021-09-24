
library(tidyverse)
library(understatr)

dir_proj <- '42-202122_new_players'
path_teams_players_stats <- file.path(dir_proj, 'teams_players_stats.rds')

leagues_meta <- understatr::get_leagues_meta()
leagues_meta

get_league_teams_stats_safely <- possibly(get_league_teams_stats, otherwise = tibble(), quiet = FALSE)

league_teams_stats <-
  leagues_meta %>% 
  filter(league_name == 'EPL' & year >= 2020) %>% 
  mutate(data = map2(league_name, year, get_league_teams_stats_safely)) %>% 
  select(data) %>% 
  unnest(data)
league_teams_stats

teams <- league_teams_stats %>% distinct(league_name, year, team_name)
teams

team_players_stats <-
  teams %>% 
  mutate(
    data = map2(team_name, year, understatr::get_team_players_stats)
  ) %>% 
  select(-c(team_name, year)) %>% 
  unnest(data)
team_players_stats

write_rds(team_players_stats, path_teams_players_stats)

