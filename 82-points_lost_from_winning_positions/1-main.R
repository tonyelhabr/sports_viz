library(worldfootballR) ## version: 0.6.4.9

## data manipulation
library(dplyr)
library(lubridate)
library(tidyr)

PROJ_DIR <- '82-points_lost_from_winning_positions'

## https://tonyelhabr.rbind.io/posts/fbref-gamestate-expected-goal-difference/
## https://www.football365.com/news/ranking-all-20-premier-league-clubs-points-lost-from-winning-positions

COUNTRY <- 'ENG'
GENDER <- 'M'
TIER <- '1st'
SEASON_END_YEAR <- 2024

raw_shots <- worldfootballR::load_fb_match_shooting(
  country = COUNTRY,
  gender = GENDER,
  tier = TIER,
  season_end_year = SEASON_END_YEAR
)

raw_match_summaries <- worldfootballR::load_fb_match_summary(
  country = COUNTRY,
  gender = GENDER,
  tier = TIER,
  season_end_year = SEASON_END_YEAR
)

extract_fbref_match_id <- function(match_url) {
  basename(dirname(match_url))
}

match_summaries <- raw_match_summaries |> 
  dplyr::transmute(
    match_id = extract_fbref_match_id(MatchURL),
    # season = Season_End_Year,
    # gender = Gender,
    # tier = Tier,
    date = lubridate::ymd(Match_Date),
    home_team = Home_Team ,
    away_team = Away_Team,
    period = as.integer(Event_Half),
    ## ensure that minutes always has a value
    minutes = dplyr::case_when(
      period == 1L & Event_Time > 45L ~ 45L, 
      period == 2L & Event_Time > 90L ~ 90L,
      .default = Event_Time
    ) |> as.integer(),
    minutes_added = dplyr::case_when(
      period == 1L & Event_Time > 45 ~ Event_Time - 45L, 
      period == 2L & Event_Time > 90 ~ Event_Time - 90L,
      .default = NA_integer_
    ),
    home_g = as.integer(gsub('[:].*$', '', Score_Progression)),
    away_g = as.integer(gsub('^.*[:]', '', Score_Progression))
  ) |> 
  dplyr::distinct() |> 
  dplyr::group_by(match_id) |> 
  dplyr::mutate(
    rn = dplyr::row_number(minutes + dplyr::coalesce(minutes_added, 0)),
    .before = 1
  ) |> 
  dplyr::ungroup()

restacked_match_summaries <- dplyr::bind_rows(
  match_summaries |> 
    dplyr::transmute(
      date,
      match_id,
      rn,
      period,
      minutes,
      minutes_added,
      is_home = TRUE,
      team = home_team,
      opponent = away_team,
      g = home_g,
      g_conceded = away_g
    ),
  match_summaries |> 
    dplyr::transmute(
      date,
      match_id,
      rn,
      period,
      minutes,
      minutes_added,
      is_home = FALSE,
      team = away_team,
      opponent = home_team,
      g = away_g,
      g_conceded = home_g
    )
) |> 
  dplyr::mutate(
    total_g = g + g_conceded
  ) |> 
  dplyr::arrange(date, match_id, is_home, rn)

final_scores <- restacked_match_summaries |> 
  dplyr::group_by(match_id, team) |> 
  dplyr::slice_max(total_g, n = 1, with_ties = FALSE) |> 
  dplyr::ungroup() |> 
  dplyr::select(
    date,
    match_id,
    team,
    final_g = g,
    final_g_conceded = g_conceded
  ) |> 
  dplyr::mutate(
    result = dplyr::case_when(
      final_g > final_g_conceded ~ 'w',
      final_g < final_g_conceded ~ 'l',
      final_g == final_g_conceded ~ 'd'
    ) |> 
      factor(c('l', 'd', 'w'))
  )

ever_winning_or_losing <- restacked_match_summaries |> 
  # dplyr::filter(match_id == '3a6836b4') |> 
  dplyr::mutate(
    winning = g > g_conceded,
    losing = g < g_conceded
  ) |> 
  dplyr::group_by(date, match_id, team) |> 
  dplyr::summarize(
    ever_winning = any(winning),
    ever_losing = any(losing)
  ) |> 
  dplyr::ungroup()

points_won_and_lost <- ever_winning_or_losing |> 
  dplyr::left_join(
    final_scores,
    by = dplyr::join_by(date, match_id, team)
  ) |> 
  dplyr::mutate(
    points_lost = dplyr::case_when(
      ever_winning & result == 'l' ~ 3L,
      ever_winning & result == 'd' ~ 2L,
      TRUE ~ 0L
    ),
    points_won = dplyr::case_when(
      ever_losing & result == 'w' ~ 3L,
      ever_losing & result == 'd' ~ 1L,
      TRUE ~ 0L
    )
  )

qs::qsave(points_won_and_lost, file.path(PROJ_DIR, 'points-won-and-lost.qs'))