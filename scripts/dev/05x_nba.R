
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

x <- 'c:/users/aelhabr/downloads/data/data/game_details/data_0021900971.json' %>% jsonlite::read_json() %>% jsonlite::fromJSON() %>% as_tibble() %>% janitor::clean_names()
x

game_ids <- game_logs_player %>% distinct(id_game) %>% arrange(id_game) %>% pull(id_game)
game_ids

pbp <- nbastatR::play_by_play_v2(game_ids = game_ids[1])
pbp %>% select(marginScore)

game_logs_player <- nbastatR::game_logs(seasons = year_min:year_max, result_types = 'player') %>% janitor::clean_names()

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
  select(year_season, date_game, id_game, id_player, player, fg3m, fg3a) #  ftm, fta)
game_logs_young

shots_nba_young_by_game <-
  game_logs_young %>% 
  arrange(date_game) %>% 
  group_by(player) %>% 
  mutate(across(c(fg3m, fg3a), list(cumu = cumsum))) %>% 
  mutate(
    idx_game = row_number(),
    n_game = n_distinct(id_game)
  ) %>% 
  ungroup()
shots_nba_young_by_game
fg3a_max <- shots_nba_young_by_game %>% filter(fg3a == max(fg3a)) %>% slice(1) %>% pull(fg3a)

shots_nba_young_by_game_last <-
  shots_nba_young_by_game %>% 
  group_by(id_player, player, n_game) %>% 
  filter(idx_game == max(idx_game)) %>% 
  ungroup()
shots_nba_young_by_game_last
shots_nba_young_by_game_last %>% arrange(desc(n_game))
shots_nba_young_by_game_last %>% skimr::skim()
players_nba_young_filt <- shots_nba_young_by_game_last %>% filter(fg3a_cumu >= 200)
players_nba_young_filt
x <- shots_nba_young_by_game %>% filter(player == 'Evan Turner') # %>% mutate(fg3a_dummy = rep(1, times = fg3a))
x
dummy <- x %>% distinct(id_game, id_player) %>% mutate(fg3a_dummy = 1) %>% crossing(fg3a = seq.int(1, fg3a_max))
dummy
x_dummy <- x %>% fuzzyjoin::fuzzy_inner_join(dummy, by = c('id_game', 'id_player', 'fg3a'), match_fun = c(`==`, `==`, `<=`))
x_dummy

do_kr20_fg3m_at <- partial(do_kr20_at, shots_mini, col = 'fg3m', ... = )

# krs_g <-
#   tibble(k = seq.int(1, 200, by = 10)) %>% 
#   mutate(
#     n = map_int(k, ~filter(shots_mini, idx >= .x) %>% distinct(player) %>% nrow()),
#     kr20 = map_dbl(k, do_kr20_g_at)
#   )
