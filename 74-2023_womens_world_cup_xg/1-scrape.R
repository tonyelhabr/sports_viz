library(StatsBombR)
library(dplyr)
library(janitor)
library(tibble)
library(readr)
library(worldfootballR)

sb_free_comps <- FreeCompetitions()
sb_comp <- filter(
  sb_free_comps, 
  competition_name == "Women's World Cup",
  season_name == '2023'
)
sb_matches <- FreeMatches(sb_comp)
sb_shots <- free_allevents(MatchesDF = sb_matches) |> 
  as_tibble() |> 
  clean_names() |> 
  filter(!is.na(shot_statsbomb_xg))
write_rds(sb_shots, file.path(proj_dir, 'sb_shots.rds'))

fb_match_urls <- fb_match_urls(
  country = '', 
  gender = 'F', 
  season_end_year = 2023, 
  tier = '', 
  non_dom_league_url = 'https://fbref.com/en/comps/106/history/Womens-World-Cup-Seasons'
)

opta_shots <- fb_match_shooting(fb_match_urls) |> 
  as_tibble() |> 
  clean_names()
write_rds(opta_shots, file.path(proj_dir, 'opta_shots.rds'))
