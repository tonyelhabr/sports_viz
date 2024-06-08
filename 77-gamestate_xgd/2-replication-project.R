## When not doing gamestate stuff, should just use higher-level raw data.
## https://www.americansocceranalysis.com/home/2022/7/19/the-replication-project-is-xg-the-best-predictor-of-future-results
## http://web.archive.org/web/20181112175811/http://www.11tegen11.com/2015/01/05/the-best-predictor-for-future-performance-is-expected-goals/
## http://web.archive.org/web/20200818194545/https://jameswgrayson.wordpress.com/2011/04/10/predicting-future-performance/
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

team_summary <- raw_team_summary |>
  dplyr::filter(
    ## choose non-playoff/relegation weeks
    (Country != 'USA' & stringr::str_detect(Matchweek, 'Matchweek')) | (Country == 'USA' & Matchweek == 'Major League Soccer (Regular Season)')
  ) |> 
  ## Fix effectively dup records due to change in MatchURLs
  dplyr::filter(!(Country == 'GER' & Season_End_Year == 2024 & stringr::str_detect(MatchURL, 'Leverkusen') & !stringr::str_detect(MatchURL, 'Bayer-Leverkusen'))) |> 
  ##  Drop MLS 2020 entirely since it was heavily impacted by COVID.
  ##    Also, drop MLS 2024 since it's incomplete
  dplyr::filter(
    !(Country == 'USA' & Season_End_Year %in% c(2020, 2024))
  ) |> 
  ## Drop Ligue 1 COVID year since each team has about 7 games missing
  dplyr::filter(
    !(Country == 'FRA' & Season_End_Year == 2020)
  ) |>
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
team_summary

raw_team_summary |> filter(Team == 'Bayern Munich', Match_Date == '2023-09-15') |> glimpse()

raw_team_summary |> 
  dplyr::filter(Team == 'Bayern Munich', Match_Date == '2023-09-15') |> 
  dplyr::filter(
    (Country == 'GER' & Season_End_Year == 2024 & stringr::str_detect(MatchURL, 'Bayer-Leverkusen', negate = FALSE))
  )

team_summary |> 
  dplyr::filter(team == 'Bayern Munich', date == '2023-09-15')

team_summary |> 
  dplyr::filter(team == 'Bayer Leverkusen', season == 2024)

## todo:
## [x] ITA - 2024: no matches after 2024-03-17
## [x] GER - 2024: Bayer Leverkusen with 60 matches?
## [x] ESP,GER,FRA - 2024: no matches after 2024-05-04
## 4. ESP - 2018: 2 teams (Villarreal, Eibar) with only 37 matches
# team_summary |> dplyr::count(season, country, season_game_count, sort = TRUE) |> tibble::view()
# team_summary |> dplyr::filter(team == 'Bayer Leverkusen') |> dplyr::filter(season == 2024) # |> tibble::view()

team_summary_for_and_against <- dplyr::left_join(
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
  ) |> 
  dplyr::arrange(team, season, date)

# team_summary_for_and_against |> 
#   dplyr::count(season, country, team, date, sort = TRUE) |> 
#   dplyr::filter(n > 1L) 

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

cumu_team_summary <- team_summary_for_and_against |> 
  accumulate_team_summary(game_idx, .prefix = 'pre')

inv_cumu_team_summary <- team_summary_for_and_against |> 
  accumulate_team_summary(inv_game_idx, .prefix = 'post')

pre_post_team_summary <- dplyr::inner_join(
  cumu_team_summary,
  inv_cumu_team_summary |> dplyr::select(-c(is_home, opponent, season, country, gender, tier, match_week, date, season_game_count)),
  by = dplyr::join_by(match_id, team, game_idx, inv_game_idx)
)
