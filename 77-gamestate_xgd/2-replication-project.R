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

COUNTRY <- c('ENG', 'ESP', 'FRA', 'GER', 'ITA', 'USA')
GENDER <- 'M'
TIER <- '1st'
SEASON_END_YEAR <- c(2018:2024)

raw_team_summary <- load_fb_advanced_match_stats(
  country = COUNTRY,
  gender = GENDER,
  tier = TIER,
  season_end_year = SEASON_END_YEAR,
  stat_type = 'summary',
  team_or_player = 'team'
)

## Known, valid missing match xG
## https://fbref.com/en/matches/e0a20cfe/Hellas-Verona-Roma-September-19-2020-Serie-A: awarded to Hellas Verona
## https://fbref.com/en/matches/c34bbc21/Bochum-Monchengladbach-March-18-2022-Bundesliga: awarded to Monchengladbach
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
    !(Country == 'USA' & Season_End_Year == 2024)
  ) |> 
  ## Drop Ligue 1 COVID year since each team has about 7 games missing
  dplyr::filter(
    Season_End_Year != 2020
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
    game_idx = dplyr::row_number(date)
  ) |> 
  dplyr::ungroup()
team_summary

# raw_team_summary |> filter(Team == 'Bayern Munich', Match_Date == '2023-09-15') |> glimpse()
# 
# raw_team_summary |> 
#   dplyr::filter(Team == 'Bayern Munich', Match_Date == '2023-09-15') |> 
#   dplyr::filter(
#     (Country == 'GER' & Season_End_Year == 2024 & stringr::str_detect(MatchURL, 'Bayer-Leverkusen', negate = FALSE))
#   )
# 
# team_summary |> 
#   dplyr::filter(team == 'Bayern Munich', date == '2023-09-15')
# 
# team_summary |> 
#   dplyr::filter(team == 'Bayer Leverkusen', season == 2024)

## todo:
## [x] ITA - 2024: no matches after 2024-03-17
## [x] GER - 2024: Bayer Leverkusen with 60 matches?
## [x] ESP,GER,FRA - 2024: no matches after 2024-05-04
## 4. ESP - 2018: 2 teams (Villarreal, Eibar) with only 37 matches
# team_summary |> dplyr::count(season, country, season_game_count, sort = TRUE) |> tibble::view()
# team_summary |> dplyr::filter(team == 'Bayer Leverkusen') |> dplyr::filter(season == 2024) # |> tibble::view()

split_team_summary <- dplyr::left_join(
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
  relationship = 'many-to-many'
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

## still some games i need to match
# split_team_summary |> 
#   dplyr::filter(country == 'ITA') |> 
#   dplyr::anti_join(
#     gamestate_by_game,
#     by = dplyr::join_by(team, season, match_id)
#   ) |> 
#   dplyr::arrange(team, season, date)

split_data <- split_team_summary |> 
  dplyr::left_join(
    gamestate_by_game,
    by = dplyr::join_by(season, country, gender, tier, match_id, team)
  )

accumulate_team_summary <- function(df, op, .prefix) {
  df |> 
    dplyr::arrange(team, season, op(game_idx)) |> 
    dplyr::group_by(season, team) |> 
    dplyr::mutate(
      dplyr::across(
        c(
          shots, shots_conceded, sot, sot_conceded,
          g, g_conceded, xg, xg_conceded,
          g_tied, g_conceded_tied, xg_tied, xg_conceded_tied, xgot_tied, xgot_conceded_tied,
          duration_leading,
          pts, pts_conceded
        ),
        \(.x) cumsum(coalesce(.x, 0))
      )
    ) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      shot_ratio = shots / (shots + shots_conceded),
      sot_ratio = sot / (sot + sot_conceded),
      g_ratio = g / (g + g_conceded),
      xg_ratio = xg / (xg + xg_conceded),
      
      shot_tied_ratio = shots_tied / (shots_tied + shots_conceded_tied),
      sot_tied_ratio = sot_tied / (sot_tied + sot_conceded_tied),
      g_tied_ratio = g_tied / (g_tied + g_conceded_tied),
      xg_tied_ratio = xg_tied / (xg_tied + xg_conceded_tied),
      xgot_tied_ratio = xgot_tied / (xgot_tied + xgot_conceded_tied),
      
      duration_leading_per_game = duration_leading / game_idx,
      pts_per_game = pts / game_idx
    ) |> 
    dplyr::rename_with(
      .fn = \(.x) paste0(.prefix, '_', .x),
      .cols = c(
        shots, shots_conceded, sot, sot_conceded,
        g, g_conceded, xg, xg_conceded,
        shots_tied, shots_conceded_tied, sot_tied, sot_conceded_tied,
        g_tied, g_conceded_tied, xg_tied, xg_conceded_tied, xgot_tied, xgot_conceded_tied,
        duration_leading,
        pts, pts_conceded,
        ends_with('ratio'),
        ends_with('per_game')
      )
    )
}

calculate_nested_r2 <- function(data, col, target_col) {
  purrr::map_dbl(
    data,
    \(.x) {
      cor(
        .x[[col]],
        .x[[target_col]],
        use = 'complete.obs'
      )^2
    }
  )
}

pivot_rolling_r2s <- function(df) {
  df |> 
    tidyr::pivot_longer(
      -c(league_group, game_idx),
      names_pattern = '(^.*)__(.*$)',
      names_to = c('predictor', 'target'),
      values_to = 'r2'
    )
}

calculate_rolling_r2s <- function(df) {
  
  cumu_team_summary <- df |> 
    accumulate_team_summary(`+`, .prefix = 'past')
  
  inv_cumu_team_summary <- df |> 
    accumulate_team_summary(`-`, .prefix = 'future')
  
  split_team_summary <- dplyr::inner_join(
    cumu_team_summary |> 
      dplyr::select(
        season,
        team,
        country,
        # gender,
        # tier,
        # match_week,
        # date,
        # match_id,
        # is_home,
        game_idx,
        dplyr::starts_with('past')
      ),
    inv_cumu_team_summary |> 
      dplyr::select(
        season,
        team,
        game_idx,
        dplyr::starts_with('future')
      ),
    by = dplyr::join_by(season, team, game_idx)
  ) |> 
    dplyr::mutate(
      league_group = ifelse(country == 'USA', 'MLS', 'Big 5'),
      .keep = 'unused',
      .before = 1
    )
  
  split_team_summary |> 
    tidyr::nest(data = -c(league_group, game_idx)) |> 
    dplyr::mutate(
      past_shot_ratio__future_g_ratio = calculate_nested_r2(data, 'past_shot_ratio', 'future_g_ratio'),
      past_sot_ratio__future_g_ratio = calculate_nested_r2(data, 'past_sot_ratio', 'future_g_ratio'),
      past_g_ratio__future_g_ratio = calculate_nested_r2(data, 'past_g_ratio', 'future_g_ratio'),
      past_xg_ratio__future_g_ratio = calculate_nested_r2(data, 'past_xg_ratio', 'future_g_ratio'),
      past_shot_tied_ratio__future_g_ratio = calculate_nested_r2(data, 'past_shot_tied_ratio', 'future_g_ratio'),
      past_sot_tied_ratio__future_g_ratio = calculate_nested_r2(data, 'past_sot_tied_ratio', 'future_g_ratio'),
      past_g_tied_ratio__future_g_ratio = calculate_nested_r2(data, 'past_g_tied_ratio', 'future_g_ratio'),
      past_xg_tied_ratio__future_g_ratio = calculate_nested_r2(data, 'past_xg_tied_ratio', 'future_g_ratio'),
      past_xgot_tied_ratio__future_g_ratio = calculate_nested_r2(data, 'past_xgot_tied_ratio', 'future_g_ratio'),
      past_duration_leading_per_game__future_g_ratio = calculate_nested_r2(data, 'past_duration_leading_per_game', 'future_g_ratio'),
      past_pts_per_game__future_g_ratio = calculate_nested_r2(data, 'past_pts_per_game', 'future_g_ratio'),
      
      past_shot_ratio__future_pts_per_game = calculate_nested_r2(data, 'past_shot_ratio', 'future_pts_per_game'),
      past_sot_ratio__future_pts_per_game = calculate_nested_r2(data, 'past_sot_ratio', 'future_pts_per_game'),
      past_g_ratio__future_pts_per_game = calculate_nested_r2(data, 'past_g_ratio', 'future_pts_per_game'),
      past_xg_ratio__future_pts_per_game = calculate_nested_r2(data, 'past_xg_ratio', 'future_pts_per_game'),
      past_shot_tied_ratio__future_pts_per_game = calculate_nested_r2(data, 'past_shot_tied_ratio', 'future_pts_per_game'),
      past_sot_tied_ratio__future_pts_per_game = calculate_nested_r2(data, 'past_sot_tied_ratio', 'future_pts_per_game'),
      past_g_tied_ratio__future_pts_per_game = calculate_nested_r2(data, 'past_g_tied_ratio', 'future_pts_per_game'),
      past_xg_tied_ratio__future_pts_per_game = calculate_nested_r2(data, 'past_xg_tied_ratio', 'future_pts_per_game'),
      past_xgot_tied_ratio__future_pts_per_game = calculate_nested_r2(data, 'past_xgot_tied_ratio', 'future_pts_per_game'),
      past_duration_leading_per_game__future_pts_per_game = calculate_nested_r2(data, 'past_duration_leading_per_game', 'future_pts_per_game'),
      past_pts_per_game__future_pts_per_game = calculate_nested_r2(data, 'past_pts_per_game', 'future_pts_per_game')
    ) |> 
    dplyr::select(-data)
}

calculate_resampled_rolling_r2s <- function(df, resamples = 10) {
  resampled_split_team_summary <- purrr::map(
    1:resamples,
    \(.i) {
      message(sprintf('Resample %d.', .i))
      resampled_df <- df |> 
        dplyr::slice_sample(n = nrow(df), replace = FALSE) |> 
        dplyr::group_by(team, season) |> 
        dplyr::mutate(
          game_idx = dplyr::row_number(),
        ) |> 
        dplyr::ungroup()

      
      resampled_df |> 
        calculate_rolling_r2s()
    }
  ) |> 
    purrr::list_rbind()
  
  resampled_split_team_summary |> 
    dplyr::summarize(
      .by = c(league_group, game_idx),
      dplyr::across(
        dplyr::matches('__'),
        \(.x) mean(.x, na.rm = TRUE)
      )
    )
}

rolling_r2s <- split_data |> 
  calculate_rolling_r2s() |> 
  pivot_rolling_r2s()

resampled_rolling_r2s <- split_data |> 
  calculate_resampled_rolling_r2s(resamples = 2) |> 
  pivot_rolling_r2s()

library(ggplot2)
rolling_r2s |> 
  ggplot() +
  aes(
    x = game_idx,
    y = r2
  ) +
  geom_line(
    aes(color = predictor)
  ) +
  facet_grid(league_group~target)

resampled_rolling_r2s |> 
  ggplot() +
  aes(
    x = game_idx,
    y = r2
  ) +
  geom_line(
    aes(color = predictor)
  ) +
  facet_grid(league_group~target)

