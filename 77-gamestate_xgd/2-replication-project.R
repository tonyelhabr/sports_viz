## When not doing gamestate stuff, should just use higher-level raw data.
library(worldfootballR)
library(dplyr)

## Extract the from "47880eb7" from "https://fbref.com/en/matches/47880eb7/Liverpool-Manchester-City-November-10-2019-Premier-League"
extract_fbref_match_id <- function(match_url) {
  basename(dirname(match_url))
}

raw_team_summary <- load_fb_advanced_match_stats(
  country = c('ENG', 'ESP', 'FRA', 'GER', 'ITA', 'USA'),
  gender = 'M',
  tier = '1st',
  stat_type = 'summary',
  team_or_player = 'team'
)

## TODO: Drop MLS 2020 entirely? Also, drop MLS 2024 since it's incomplete
team_summary <- raw_team_summary |>
  ## TODO: If MLS, filter only for Matchweek = 'Major League Soccer (Regular Season)'
  dplyr::transmute(
    season = Season_End_Year,
    country = Country,
    gender = Gender,
    tier = Tier,
    match_week = stringr::str_extract(Matchweek, '[0-9]+') |> as.integer(), ## won't works for MLS
    date = Match_Date,
    match_id = extract_fbref_match_id(MatchURL),
    team = Team,
    is_home = Home_Away == 'Home',
    g = Gls,
    xg = xG_Expected,
    shots = Sh,
    sot = SoT
  ) |> 
  dplyr::group_by(season, team) |> 
  dplyr::mutate(
    season_game_count = dplyr::n(),
    game_idx = dplyr::row_number(date),
    inv_game_idx = dplyr::row_number(dplyr::desc(date))
  ) |> 
  dplyr::ungroup()
team_summary |> dplyr::count(season, country, season_game_count, sort = TRUE) |> tibble::view()

## todo:
## 1. ITA - 2024: no matches after 2024-03-17
## 2. GER - 2024: Bayer Leverkusen with 60 matches?
## 3. ESP,GER,FRA - 2024: no matches after 2024-05-04
## 4. ESP - 2018: 2 teams (Villarreal, Eibar) with only 37 matches

augmented_team_summary <- dplyr::left_join(
  team_summary,
  team_summary |> 
    dplyr::transmute(
      match_id,
      opponent = team,
      g_conceded = g,
      xg_conceded = xg,
      shots_conceded = shots,
      sot_conceded = sot
    ),
  by = dplyr::join_by(match_id),
  relationship = "many-to-many"
) |> 
  dplyr::filter(team != opponent) |> 
  dplyr::mutate(
    pts = dplyr::case_when(
      g > g_conceded ~ 3L,
      g < g_conceded ~ 0L,
      g == g_conceded ~ 1L
    ),
    pts_conceded = dplyr::case_when(
      g > g_conceded ~ 0L,
      g < g_conceded ~ 3L,
      g == g_conceded ~ 1L
    )
  )

accumulate_team_summary <- function(df, col, .prefix) {
  df |> 
    dplyr::arrange(team, season, {{ col }}) |> 
    dplyr::group_by(season, team) |> 
    dplyr::mutate(
      dplyr::across(
        c(
          shots, shots_conceded, sot, sot_conceded,
          g, g_conceded, xg, xg_conceded,
          pts, pts_conceded
        ),
        \(.x) cumsum(.x)
      )
    ) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      shot_ratio = shots / (shots + shots_conceded),
      sot_ratio = sot / (sot + sot_conceded),
      g_ratio = g / (g + g_conceded),
      xg_ratio = xg / (xg + xg_conceded),
      pts_per_game = pts / {{ col }}
    ) |> 
    dplyr::rename_with(
      .fn = \(.x) paste0(.prefix, '_', .x),
      .cols = c(
        shots, shots_conceded, sot, sot_conceded,
        g, g_conceded, xg, xg_conceded,
        pts, pts_conceded,
        ends_with('ratio'),
        pts_per_game
      )
    )
}
