## borrowing from blogs
library(dplyr)
library(purrr)
library(rlang)
library(tidyr)

library(worldfootballR)

library(qs) ## for local dev

COUNTRIES <- c('ENG') # , 'ESP', 'GER', 'ITA', 'FRA')
GENDERS <- 'M'
SEASON_END_YEARS <- 2018:2024
TIERS <- '1st'
# PROJ_DIR <- 'posts/xg-likelihood'

raw_shots <- worldfootballR::load_fb_match_shooting(
  country = COUNTRIES,
  tier = TIERS,
  gender = GENDERS,
  season_end_year = SEASON_END_YEARS
)

# raw_match_summaries <- worldfootballR::load_fb_match_summary(
#   country = COUNTRIES,
#   tier = TIERS,
#   gender = GENDERS,
#   season_end_year = SEASON_END_YEARS
# )

raw_team_summary <- worldfootballR::load_fb_advanced_match_stats(
  country = COUNTRIES,
  gender = GENDERS,
  tier = TIERS,
  season_end_year = SEASON_END_YEARS,
  stat_type = 'summary',
  team_or_player = 'team'
)

extract_fbref_id <- function(url) {
  basename(dirname(url))
}

drop_bad_matches <- function(df) {
  df |> 
    dplyr::filter(
      ## choose non-playoff/relegation weeks
      (Country != 'USA' & stringr::str_detect(Matchweek, 'Matchweek')) | (Country == 'USA' & Matchweek == 'Major League Soccer (Regular Season)')
    ) |> 
    ## Fix effectively dup records due to change in MatchURLs
    dplyr::filter(!(Country == 'GER' & Season_End_Year == 2024 & stringr::str_detect(MatchURL, 'Leverkusen') & !stringr::str_detect(MatchURL, 'Bayer-Leverkusen'))) |> 
    ##  Drop MLS 2024 since it's incomplete at time of writing
    dplyr::filter(
      !(Country == 'USA' & Season_End_Year == 2024)
    ) |> 
    ## Drop COVID-19-affected season for all leagues
    ##  (Ligue 1 is especially odd, as each team has about 7 games missing.)
    dplyr::filter(
      Season_End_Year != 2020
    )
}

np_shots <- raw_shots |> 
  ## Drop penalties
  dplyr::filter(
    !dplyr::coalesce((Distance == '13' & round(as.double(xG), 2) == 0.79), FALSE)
  ) |> 
  dplyr::transmute(
    season_end_year = Season_End_Year,
    ## Get this from team_summary
    # country = Country,
    # gender = Gender,
    # tier = Tier,
    squad = Squad,
    home_away = tolower(Home_Away),
    player_id = extract_fbref_id(Player_Href),
    player = Player,
    match_date = lubridate::ymd(Date),
    match_id = extract_fbref_id(MatchURL),
    minute = Minute,
    g = as.integer(Outcome == 'Goal'),
    xg = as.double(xG)
  ) |> 
  ## A handful of scored shots with empty xG
  dplyr::filter(!is.na(xg)) |> 
  dplyr::arrange(season_end_year, player_id, match_date, minute)

## Use the more commonly used name when a player ID is mapped to multiple names
##   (This "bug" happens because worldfootballR doesn't go back and re-scrape data
##   when fbref makes a name update.)
player_name_mapping <- np_shots |>
  dplyr::count(player_id, player) |>
  dplyr::group_by(player_id) |>
  dplyr::slice_max(n, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::distinct(player_id, player)

player_season_np_shots <- np_shots |> 
  dplyr::summarize(
    .by = c(player_id, season_end_year), 
    shots = dplyr::n(),
    dplyr::across(c(g, xg), sum)
  ) |> 
  dplyr::mutate(
    pr = g / xg
  ) |> 
  dplyr::left_join(
    player_name_mapping,
    by = dplyr::join_by(player_id)
  ) |> 
  dplyr::relocate(player, .after = player_id) |> 
  dplyr::arrange(player_id, season_end_year)
player_season_np_shots

# match_summaries <- raw_match_summaries |> 
#   dplyr::group_by(MatchURL) |> 
#   dplyr::mutate(
#     match_summary_rn = dplyr::row_number(Event_Time),
#     inv_match_summary_rn = dplyr::row_number(dplyr::desc(Event_Time)),
#     match_has_no_penalties = all(Event_Type != 'Penalty')
#   ) |> 
#   dplyr::ungroup() |> 
#   dplyr::mutate(
#     match_has_no_goals = Away_Score == 0 & Home_Score == 0
#   ) |> 
#   ## Drop non-shot events, e.g. card and substitution events. 
#   ##   Always keep the first timeline event, so that we're not accidentally dropping matches.
#   dplyr::filter(
#     Event_Type %in% c('Goal', 'Own Goal', 'Penalty') | 
#       ## don't drop games with no goals
#       (match_has_no_goals & match_has_no_penalties & inv_match_summary_rn == 1)
#   ) |> 
#   dplyr::transmute(
#     season = Season_End_Year,
#     country = Country,
#     gender = Gender,
#     tier = Tier,
#     match_week = stringr::str_extract(Matchweek, '[0-9]+') |> as.integer(),
#     match_id = extract_fbref_id(MatchURL),
#     date = lubridate::ymd(Match_Date),
#     match_summary_rn,
#     home_team = Home_Team ,
#     away_team = Away_Team,
#     period = as.integer(Event_Half),
#     inv_match_summary_rn,
#     # orig_event_time = Event_Time,
#     ## ensure that minutes always has a value
#     minutes = dplyr::case_when(
#       period == 1L & Event_Time > 45L ~ 45L, 
#       period == 2L & Event_Time > 90L ~ 90L,
#       .default = Event_Time
#     ) |> as.integer(),
#     minutes_added = dplyr::case_when(
#       period == 1L & Event_Time > 45 ~ Event_Time - 45L, 
#       period == 2L & Event_Time > 90 ~ Event_Time - 90L,
#       .default = NA_integer_
#     ),
#     home_g = as.integer(gsub('[:].*$', '', Score_Progression)), ## after event
#     away_g = as.integer(gsub('^.*[:]', '', Score_Progression)),
#     is_own_goal = Event_Type == 'Own Goal',
#     team = Team,
#     player = Event_Players
#   )
# 
# deduped_match_summaries <- match_summaries |> 
#   ## Some matches are recorded twice if they were rescheduled
#   dplyr::semi_join(
#     match_summaries |> 
#       dplyr::distinct(match_id, date) |> 
#       dplyr::group_by(match_id) |> 
#       dplyr::slice_max(date, n = 1) |> 
#       dplyr::ungroup(),
#     by = dplyr::join_by(match_id, date)
#   )
# 
# oneline_match_summaries <- deduped_match_summaries |>
#   dplyr::group_by(match_id) |> 
#   dplyr::slice_max(inv_match_summary_rn, n = 1, with_ties = FALSE) |> 
#   dplyr::ungroup()

team_summary <- raw_team_summary |>
  # drop_bad_matches() |> 
  dplyr::transmute(
    season = Season_End_Year,
    country = Country,
    gender = Gender,
    tier = Tier,
    # match_week = stringr::str_extract(Matchweek, '[0-9]+') |> as.integer(), ## won't works for MLS
    ## We'll use this to define our own notion of match week, where we order games
    ##   by date rather than use the FBref matchweek provided at face value, since a
    ##   a match could have been rescheduled, leading to situations where the labeled
    ##   matchweek 2 comes before the matchweek 1, for example.
    date = Match_Date, 
    match_id = extract_fbref_id(MatchURL),
    team = Team,
    home_away = tolower(Home_Away),
    team_g = Gls,
    team_xg = xG_Expected,
    team_shots = Sh,
    team_sot = SoT
  )

## main ---
agg_match_player_shots <- np_shots |> 
  dplyr::group_by(
    match_id, 
    match_date, 
    season_end_year, 
    squad, 
    home_away
  ) |> 
  dplyr::summarize(
    np_player_shots = n(),
    across(
      c(
        g,
        xg
      ),
      \(.x) sum(.x, na.rm = TRUE),
      .names = 'np_player_{.col}'
    ),
  ) |> 
  dplyr::ungroup()

wide_agg_match_player_shots <- agg_match_player_shots |> 
  tidyr::pivot_wider(
    names_from = home_away,
    values_from = c(squad, matches('^np_player_')),
    names_glue = '{home_away}_{.value}'
  )

wide_team_summary <- team_summary |> 
  tidyr::pivot_wider(
    names_from = home_away,
    values_from = matches('^team'),
    names_glue = '{home_away}_{.value}'
  )

wide_df <- wide_team_summary |> 
  dplyr::left_join(
    wide_agg_match_player_shots |> 
      dplyr::select(
        match_id,
        match_date,
        matches('^(home|away)_np')
      ),
    by = dplyr::join_by(match_id)
  )

wide_df |> 
  dplyr::filter(is.na(home_np_player_shots)) |> 
  glimpse()
wide_df
