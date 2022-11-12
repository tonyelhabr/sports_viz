library(worldfootballR)
library(dplyr)
library(tidyr)
library(purrr)
library(qs)

dir_proj <- '62-safc'

usl_matches <- load_fotmob_matches_by_date(league_id = 8972) |> 
  mutate(
    across(date, lubridate::date)
  )

safc_2022_matches <- usl_matches |> 
  filter(home_name == 'San Antonio FC' | away_name == 'San Antonio FC') |> 
  filter(date >= lubridate::ymd('2022-03-12'))

get_fotmob_match_team_stats <- function(match_id, overwrite = FALSE) {
  path <- file.path(dir_proj, 'data', sprintf('%s.qs', match_id))
  if (file.exists(path) & isFALSE(overwrite)) {
    return(qs::qread(path))
  }
  res <- worldfootballR::fotmob_get_match_team_stats(match_id)
  qs::qsave(res, path)
  res
}

safc_2022_match_team_stats <- safc_2022_matches$match_id |> 
  map_dfr(get_fotmob_match_team_stats)

poss <- safc_2022_match_team_stats |> 
  filter(stats_title == 'Ball possession')

long_poss <- poss |> 
  distinct(match_id, home_team, away_team, home_value, away_value) |> 
  pivot_longer(
    -match_id,
    names_to = c('side', 'stat'),
    names_sep = '_'
  ) |> 
  pivot_wider(
    names_from = stat,
    values_from = value
  )

long_poss |> 
  filter(team == 'San Antonio FC') |> 
  count(value < 50)
