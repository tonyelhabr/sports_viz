
library(nbastatR)
path_game_logs_player <- here::here('data', '04_game_logs_nba.rds')
year_min <- 2010
year_max <- 2019
seasons_played_require <- 2
if(!fs::file_exists(path_game_logs_nba)) {
  game_logs_player <- nbastatR::game_logs(seasons = year_min:year_max, result_types = 'player') %>% janitor::clean_names()
  write_rds(game_logs_player, path_game_logs_player)
} else {
  game_logs_player <- read_rds(path_game_logs_player)
}

game_logs_player
nbastatR::assign_nba_players()
players_nba_young <-
  df_nba_player_dict %>% 
  janitor::clean_names() %>% 
  # filter(has_games_played_flag) %>% 
  # filter(is_active) %>% 
  filter(count_seasons >= seasons_played_require) %>% 
  filter(year_season_first >= year_min) %>% 
  select(id_player, player = name_player, n_season = count_seasons, year_season_first, year_season_last, is_active)
players_nba_young

game_logs_young <-
  game_logs_player %>% 
  rename(player = name_player) %>% 
  inner_join(players_nba_young) %>% 
  select(year_season, date_game, id_game, idx_game = number_game_team_season, id_player, player, fg3m, fg3a, pct_fg3, ftm, fta, pct_ft)
game_logs_young

shots_nba_young <-
  game_logs_young %>% 
  arrange(date_game) %>% 
  group_by(player) %>% 
  mutate(across(c(fg3m, fg3a, ftm, fta), list(cumu = cumsum))) %>% 
  mutate(idx_game = row_number()) %>% 
  ungroup()
shots_nba_young



game_ids <- game_logs_tm %>% distinct(id_game) %>% arrange(id_game) %>% pull(id_game)
game_ids
box_scores <- nbastatR::box_scores(game_ids = 21200001) %>% janitor::clean_names()
box_scores_unnest <- box_scores %>% unnest(data_box_score) %>% janitor::clean_names()
box_scores_unnest

nbastatR::assign_nba_players()
df_dict_nba_players
df_dict_nba_players %>% filter(isRookie) %>% arrange(yearSeasonFirst)
teams <- teams_players_stats() %>% janitor::clean_names()
teams
players <- teams %>% unnest(data_table) %>% janitor::clean_names() %>% distinct(player = name_player, id_player = id_player)
players
players <- players_careers()
