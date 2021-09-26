
library(tidyverse)

dir_proj <- '42-202122_new_players'
path_teams_players_stats <- file.path(dir_proj, 'teams_players_stats.rds')
teams_players_stats <- path_teams_players_stats %>% read_rds() %>% janitor::clean_names()
teams_players_stats

new_players <-
  teams_players_stats %>% 
  filter(!(team_name %in% c('Brentford', 'Norwich', 'Watford'))) %>% 
  select(team_name, player_name, year, time) %>% 
  pivot_wider(names_from = year, values_from = time, names_prefix = 'year_') %>% 
  filter(is.na(year_2020))
new_players %>% count(team_name, sort = TRUE)
new_players %>% group_by(team_name) %>% summarize(n = n(), across(year_2021, sum)) %>% arrange(desc(year_2021))
