library(worldfootballR)
library(dplyr)
library(lubridate)
library(tidyr)
library(qs)

PROJ_DIR <- '90-gamestate_xgd'
source(file.path(PROJ_DIR, 'config.R'))

raw_shots <- worldfootballR::load_fb_match_shooting(
  country = COUNTRY,
  gender = GENDER,
  tier = TIER,
  season_end_year = SEASON_END_YEAR
)
qs::qsave(raw_shots, file.path(DATA_DIR, 'raw_shots.qs'))

raw_match_summaries <- worldfootballR::load_fb_match_summary(
  country = COUNTRY,
  gender = GENDER,
  tier = TIER,
  season_end_year = SEASON_END_YEAR
)
qs::qsave(raw_match_summaries, file.path(DATA_DIR, 'raw_match_summaries.qs'))

raw_match_summaries <- worldfootballR::load_fb_match_summary(
  country = COUNTRY,
  gender = GENDER,
  tier = TIER,
  season_end_year = SEASON_END_YEAR
)
qs::qsave(raw_match_summaries, file.path(DATA_DIR, 'raw_match_summaries.qs'))

## Extract the from "47880eb7" from "https://fbref.com/en/matches/47880eb7/Liverpool-Manchester-City-November-10-2019-Premier-League"
extract_fbref_match_id <- function(match_url) {
  basename(dirname(match_url))
}

fix_team <- function(x) {
  dplyr::case_match(
    x,
    'Montreal Impact' ~ 'CF Montr�al',
    'Sporting Kansas City' ~ 'Sporting KC',
    .default = x
  )
}

match_summaries <- raw_match_summaries |>
  dplyr::group_by(MatchURL) |>
  dplyr::mutate(
    match_summary_rn = dplyr::row_number(dplyr::desc(Event_Time)),
    match_has_no_penalties = all(Event_Type != 'Penalty')
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    match_has_no_goals = Away_Score == 0 & Home_Score == 0
  ) |>
  ## Drop non-shot events, e.g. card and substitution events.
  ##   Always keep the first timeline event, so that we're not accidentally dropping matches.
  dplyr::filter(
    Event_Type %in% c('Goal', 'Own Goal', 'Penalty') |
      ## don't drop games with no goals
      (match_has_no_goals & match_has_no_penalties & match_summary_rn == 1)
  ) |>
  dplyr::transmute(
    match_id = extract_fbref_match_id(MatchURL),
    season = Season_End_Year,
    gender = Gender,
    tier = Tier,
    date = lubridate::ymd(Match_Date),
    home_team = fix_team(Home_Team) ,
    away_team = fix_team(Away_Team),
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
    home_g = as.integer(gsub('[:].*$', '', Score_Progression)), ## after event
    away_g = as.integer(gsub('^.*[:]', '', Score_Progression)),
    is_own_goal = Event_Type == 'Own Goal',
    team = fix_team(Team),
    player = Event_Players
  )
qs::qsave(match_summaries, file.path(DATA_DIR, 'match_summaries.qs'))

shots <- raw_shots |>
  dplyr::transmute(
    match_id = extract_fbref_match_id(MatchURL),
    period = as.integer(Match_Half),
    ## convert "45+2" to "45"
    minutes = ifelse(
      grepl('[+]', Minute),
      as.integer(gsub('(^[0-9]+)[+]([0-9]+$)', '\\1', Minute)),
      as.integer(Minute)
    ),
    ## convert "45+2" to "2"
    minutes_added = ifelse(
      grepl('[+]', Minute),
      as.integer(gsub('(^[0-9]+)[+]([0-9]+$)', '\\2', Minute)),
      NA_integer_
    ),
    is_home = Home_Away == 'Home',
    team = fix_team(Squad),
    player = Player,
    is_goal = Outcome == 'Goal',
    xg = as.double(xG)
  )
qs::qsave(shots, file.path(DATA_DIR, 'shots.qs'))

shots_with_own_goals <- dplyr::bind_rows(
  shots |>
    dplyr::transmute(
      match_id,
      period,
      minutes,
      minutes_added,
      is_home,
      team,
      player,
      is_goal,
      xg,
      is_own_goal = FALSE
    ),
  ## synthetic events for own goals
  match_summaries |>
    dplyr::filter(
      is_own_goal
    ) |>
    dplyr::transmute(
      match_id,
      period,
      minutes,
      minutes_added,
      is_home = team == home_team,
      team,
      player,
      is_goal = TRUE,
      xg = NA_real_,
      is_own_goal = TRUE
    )
)

clean_shots <- shots_with_own_goals |>
  ## To get meta-information about the game
  dplyr::inner_join(
    match_summaries |>
      dplyr::distinct(match_id, home_team, away_team),
    by = dplyr::join_by(match_id),
    relationship = 'many-to-many'
  ) |>
  dplyr::mutate(
    home_g = dplyr::case_when(
      ## Note that fotmob would list the away team for an own goal but fbref
      ##   lists the home team
      (is_goal | is_own_goal) & is_home ~ 1L,
      is_own_goal & is_home ~ 1L,
      TRUE ~ 0L
    ),
    away_g = dplyr::case_when(
      (is_goal | is_own_goal) & !is_home ~ 1L,
      TRUE ~ 0L
    ),
    home_xg = dplyr::case_when(
      is_home ~ dplyr::coalesce(xg, 0),
      TRUE ~ 0L ## even for own goals
    ),
    away_xg = dplyr::case_when(
      !is_home ~ dplyr::coalesce(xg, 0),
      TRUE ~ 0L
    )
  ) |>
  dplyr::group_by(match_id) |>
  ## Differentiate between shots in the same minute.
  dplyr::mutate(
    shot_idx = dplyr::row_number((minutes + dplyr::coalesce(minutes_added, 0L)))
  ) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    shot_id = sprintf('%s-%02d', match_id, shot_idx),
    match_id,
    period,
    minutes,
    minutes_added,
    is_home,
    is_goal,
    is_own_goal,
    player,
    home_team,
    away_team,
    home_g,
    away_g,
    home_xg,
    away_xg
  )
qs::qsave(match_summaries, file.path(DATA_DIR, 'clean_shots.qs'))

restacked_shots <- dplyr::bind_rows(
  clean_shots |>
    dplyr::filter(is_home) |>
    dplyr::transmute(
      shot_id,
      match_id,
      period,
      minutes,
      minutes_added,
      is_home,
      is_goal,
      is_own_goal,
      player,
      team = home_team,
      opponent = away_team,
      g = home_g,
      g_conceded = away_g,
      xg = home_xg,
      xg_conceded = away_xg
    ),
  clean_shots |>
    dplyr::filter(!is_home) |>
    dplyr::transmute(
      shot_id,
      match_id,
      period,
      minutes,
      minutes_added,
      is_home,
      is_goal,
      is_own_goal,
      player,
      team = away_team,
      opponent = home_team,
      g = away_g,
      g_conceded = home_g,
      xg = away_xg,
      xg_conceded = home_xg
    )
)

doublecounted_restacked_shots <- dplyr::bind_rows(
  restacked_shots |> dplyr::mutate(pov = 'primary', .before = 1),
  restacked_shots |>
    ## re-assign to temporary variable names first, so that way we don't accidentlaly overwrite information
    dplyr::rename(
      team1 = team,
      team2 = opponent,
      g1 = g,
      g2 = g_conceded,
      xg1 = xg,
      xg2 = xg_conceded
    ) |>
    ## then formally re-assign columns
    dplyr::rename(
      team = team2,
      opponent = team1,
      g = g2,
      g_conceded = g1,
      xg = xg2,
      xg_conceded = xg1
    ) |>
    dplyr::mutate(
      is_home = !is_home
    ) |>
    dplyr::mutate(
      pov = 'secondary',
      .before = 1
    )
) |>
  dplyr::arrange(match_id, shot_id, pov)
qs::qsave(doublecounted_restacked_shots, file.path(DATA_DIR, 'doublecounted_restacked_shots.qs'))

cumu_doublecounted_restacked_shots <- doublecounted_restacked_shots |>
  dplyr::group_by(match_id, team) |>
  dplyr::mutate(
    dplyr::across(
      c(g, g_conceded),
      list(cumu = cumsum)
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    gamestate = g_cumu - g_conceded_cumu
  )

ORDERED_GAMESTATE_LABELS <- c('Trailing', 'Tied', 'Leading')
gamestate_shots <- cumu_doublecounted_restacked_shots |>
  dplyr::inner_join(
    match_summaries |>
      dplyr::distinct(
        match_id,
        season,
        date,
        home_team,
        away_team
      ),
    by = dplyr::join_by(match_id),
    relationship = 'many-to-many'
  ) |>
  dplyr::transmute(
    pov,
    match_id,
    season,
    date,
    home_team,
    away_team,
    team,
    player,
    shot_id,
    period,
    minutes,
    minutes_added,
    time = minutes + dplyr::coalesce(minutes_added, 0L),
    xg,
    xgd = xg - xg_conceded,
    gamestate = cut(
      gamestate,
      breaks = c(-Inf, -1, 0, Inf),
      labels = ORDERED_GAMESTATE_LABELS
    )
  ) |>
  dplyr::group_by(match_id, team) |>
  dplyr::arrange(shot_id, .by_group = TRUE) |>
  dplyr::mutate(
    pre_shot_gamestate = dplyr::lag(gamestate, default = ORDERED_GAMESTATE_LABELS[2])
  ) |>
  dplyr::ungroup()
qs::qsave(gamestate_shots, file.path(DATA_DIR, 'gamestate_shots.qs'))

LAST_MIN_BUFFER <- 3
last_min_pad <- gamestate_shots |>
  dplyr::select(
    match_id,
    season,
    date,
    team,
    pre_shot_gamestate,
    period,
    time
  ) |>
  dplyr::group_by(match_id, team, period) |>
  dplyr::slice_max(time, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    xg = 0,
    xgd = 0,
    last_regular_min = ifelse(period == 1L, 45L, 90L),
    time = pmax(last_regular_min + LAST_MIN_BUFFER, time + 1)
  )

padded_gamestate_shots <- dplyr::bind_rows(
  gamestate_shots,
  last_min_pad
) |>
  dplyr::arrange(match_id, time)

gamestate_shots_and_durations <- padded_gamestate_shots |>
  dplyr::group_by(match_id, team) |>
  dplyr::mutate(
    prev_period = dplyr::lag(period),
    prev_time = dplyr::lag(time)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    duration = dplyr::case_when(
      period == 1L & is.na(prev_period) ~ time - 0L,
      period == 2L & period != prev_period ~ time - 45L,
      TRUE ~ time - prev_time
    )
  )
qs::qsave(gamestate_shots_and_durations, file.path(DATA_DIR, 'gamestate_shots_and_durations.qs'))

agg_gamestate_xgd <- gamestate_shots_and_durations |>
  dplyr::group_by(team, pre_shot_gamestate) |>
  dplyr::summarize(
    dplyr::across(
      c(
        xgd,
        duration
      ),
      \(.x) sum(.x, na.rm = TRUE)
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    xgd_p90 = xgd * 90 / duration
  ) |>
  dplyr::group_by(team) |>
  dplyr::mutate(
    prop_duration = duration / sum(duration)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    team,
    pre_shot_gamestate,
    xgd_p90,
    prop_duration
  )
qs::qsave(agg_gamestate_xgd, file.path(DATA_DIR, 'agg_gamestate_xgd.qs'))
