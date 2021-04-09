
library(tidyverse)
# urls <- worldfootballR::get_match_urls(country = 'ENG', gender = 'M', season_end_year = 2021, tier='1st')
# urls
# x <- worldfootballR::get_match_report(urls[1]) %>% as_tibble()
# x
# y <- worldfootballR::get_match_summary(urls[1]) %>% as_tibble()
# y
playing_times <- 
  worldfootballR::get_season_team_stats(
    country = 'ENG', 
    gender = 'M', 
    season_end_year = '2020', 
    tier = '1st', 
    stat_type = 'playing_time'
  ) %>% 
  as_tibble()
playing_times
urls_team <- worldfootballR::fb_teams_urls('https://fbref.com/en/comps/9/Premier-League-Stats')
urls_team
urls_player <- 
  urls_team %>%
  map(worldfootballR::fb_player_urls) %>% 
  flatten_chr()
urls_player
players <- urls_player[1:3] %>% map_dfr(~worldfootballR::fb_player_season_stats(.x, stat_type = 'standard')) %>% as_tibble()
playerss %>% count(player_name)
