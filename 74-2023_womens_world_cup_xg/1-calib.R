library(StatsBombR)
library(dplyr)
library(janitor, include.only = 'clean_names')
library(tibble, include.only = 'as_tibble')
free_comps <- FreeCompetitions()
wwc_comp <- filter(free_comps, competition_name == "Women's World Cup", season_name == '2023')
wwc_matches <- FreeMatches(wwc_comp)
wwc_events <- free_allevents(MatchesDF = wwc_matches) |> 
  as_tibble() |> 
  clean_names()

np_shot_xg <- wwc_events |> 
  filter(
    shot_type_name != 'Penalty',
    !is.na(shot_statsbomb_xg)
  ) |> 
  transmute(
    # starts_with('shot_'),
    xg = shot_statsbomb_xg,
    g = as.integer(shot_outcome_name == 'Goal'),
    type_name = shot_type_name
  )

library(probably)
np_shot_xg |> 
  transmute(
    g = factor(ifelse(g == 1L, 'yes', 'no')),
    .pred_yes = xg
  ) |> 
  cal_plot_breaks(
    truth = g,
    estimate = dplyr::matches('^[.]pred'),
    event_level = 'second'
  )
