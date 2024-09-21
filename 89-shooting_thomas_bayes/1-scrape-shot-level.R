## data scrape
library(worldfootballR)

## data manipulation
library(dplyr)
library(lubridate)
library(tidyr)

PROJ_DIR <- '89-shooting_thomas_bayes'
COUNTRY <- c('ENG', 'ESP', 'FRA', 'GER', 'ITA', 'USA')
GENDER <- 'M'
TIER <- '1st'
SEASON_END_YEAR <- c(2018:2024)

raw_shots <- worldfootballR::load_fb_match_shooting(
  country = COUNTRY,
  gender = GENDER,
  tier = TIER,
  season_end_year = SEASON_END_YEAR
)

## Extract the from "47880eb7" from "https://fbref.com/en/matches/47880eb7/Liverpool-Manchester-City-November-10-2019-Premier-League"
extract_fbref_match_id <- function(match_url) {
  basename(dirname(match_url))
}

fix_team <- function(x) {
  dplyr::case_match(
    x,
    'Montreal Impact' ~ 'CF Montréal',
    'Sporting Kansas City' ~ 'Sporting KC',
    .default = x
  )
}

shots <- raw_shots |> 
  dplyr::mutate(
    xg = as.double(xG),
    is_penalty = stringr::str_detect(Player, '\\(pen\\)') | 
      dplyr::coalesce(
        (Distance == '13' & round(xg, 2) == 0.79), 
        FALSE
      )
  ) |> 
  dplyr::transmute(
    season = Season_End_Year,
    country = Country,
    gender = Gender,
    tier = Tier,
    match_id = extract_fbref_match_id(MatchURL),
    period = as.integer(Match_Half),
    ## convert "45+2" to "45"
    min = ifelse(
      grepl('[+]', Minute),
      as.integer(gsub('(^[0-9]+)[+]([0-9]+$)', '\\1', Minute)), 
      as.integer(Minute)
    ),
    ## convert "45+2" to "2"
    min_added = ifelse(
      grepl('[+]', Minute), 
      as.integer(gsub('(^[0-9]+)[+]([0-9]+$)', '\\2', Minute)), 
      NA_integer_
    ),
    # is_home = Home_Away == 'Home',
    team = fix_team(Squad),
    player_href = Player_Href,
    player = stringr::str_remove(Player, ' \\(pen\\)'),
    distance = as.integer(Distance),
    g = as.integer(Outcome == 'Goal'),
    xg = as.double(xG),
    xgot = as.double(PSxG),
    is_on_target = !is.na(PSxG),
    is_penalty
  )

qs::qsave(shots, file.path(PROJ_DIR, 'shots.qs'))
