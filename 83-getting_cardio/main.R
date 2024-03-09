library(worldfootballR)
library(janitor)
library(dplyr)
library(stringr)

player_stats <- fb_league_stats(
  country = 'ENG',
  tier = '1st',
  gender = 'M',
  season_end_year = 2024,
  team_or_player = 'player',
  stat_type = 'playing_time'
) |> 
  clean_names() 

player_poss <- fb_league_stats(
  country = 'ENG',
  tier = '1st',
  gender = 'M',
  season_end_year = 2024,
  team_or_player = 'player',
  stat_type = 'possession'
) |> 
  clean_names() 

player_stats |> 
  filter(
    str_detect(pos, 'FW'),
    mp_playing_time >= 3L
  ) |> 
  arrange(min_playing_time) |> 
  select(
    player,
    player_href,
    squad,
    age,
    pos,
    nation,
    squad,
    mp_playing_time,
    min_playing_time
  ) |> 
  head(20) |> 
  left_join(
    player_poss |> 
      select(player_href, squad, touches = touches_touches),
    by = join_by(player_href, squad)
  )

player_stats |> 
  filter(
    min_playing_time > 1000
  ) |> 
  left_join(
    player_poss |> 
      select(player_href, squad, touches = touches_touches),
    by = join_by(player_href, squad)
  ) |> 
  mutate(
    touches_per_90 = touches / min_playing_time
  ) |> 
  filter(
    pos |> str_detect('^MF$')
  ) |> 
  select(
    player,
    # player_href,
    squad,
    age,
    pos,
    nation,
    squad,
    mp_playing_time,
    min_playing_time,
    touches,
    touches_per_90
  ) |> 
  arrange(touches_per_90)
  
