
library(tidyverse)
# library(nbastatR)
# nbastatR::dictionary_bref_teams() %>% janitor::clean_names() %>% filter(is_active_team)
# nbastatR::assign_nba_teams()
# tms_nba <- df_dict_nba_teams %>% janitor::clean_names() %>% filter(is_non_nba_team == 0L)
# tms_nba
tm_shots <- nbastatR::teams_shots(all_active_teams = T, seasons = 2020) %>% janitor::clean_names()
tm_shots
tm_shots %>% count(name_player, sort = T)
