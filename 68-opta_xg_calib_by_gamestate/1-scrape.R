library(worldfootballR)  ## version: 0.6.4.1
library(dplyr)
library(lubridate)
library(tidyr)
library(qs)
library(rlang)

PROJ_DIR <- '68-opta_xg_calib_by_gamestate'
# source(file.path(PROJ_DIR, 'load_fb.R')) ## until these are added to worldfootballR

COUNTRIES <- 'ENG'
GENDERS <- 'M'
TIERS <- '1st'
SEASON_END_YEARS <- 2018:2022

raw_shots <- load_fb_match_shooting(
  country = COUNTRIES,
  gender = GENDERS,
  tier = TIERS,
  season_end_year = SEASON_END_YEARS
)

raw_match_summaries <- load_fb_match_summary(
  country = COUNTRIES,
  gender = GENDERS,
  tier = TIERS,
  season_end_year = SEASON_END_YEARS
)

extract_fbref_match_id <- function(match_url) {
  basename(dirname(match_url))
}

generate_time_key <- function(period, min, min_added) {
  period * (min + coalesce(min_added, 0L))
}

match_summaries <- raw_match_summaries |> 
  group_by(MatchURL) |> 
  mutate(
    match_summary_rn = row_number(desc(Event_Time)),
    match_has_no_penalties = all(Event_Type != 'Penalty')
  ) |> 
  ungroup() |> 
  mutate(
    match_has_no_goals = Away_Score == 0 & Home_Score == 0
  ) |> 
  filter(
    Event_Type %in% c('Goal', 'Own Goal', 'Penalty') | 
      ## don't drop games with no goals
      (match_has_no_goals & match_has_no_penalties & match_summary_rn == 1)
  ) |> 
  transmute(
    rn = row_number(), 
    match_id = extract_fbref_match_id(MatchURL),
    season = sprintf('%s/%s', Season_End_Year - 1, substr(Season_End_Year , 3, 4)),
    gender = Gender,
    tier = Tier,
    date = ymd(Match_Date),
    home_team = Home_Team ,
    away_team = Away_Team,
    period = as.integer(Event_Half),
    min = case_when(
      period == 1L & Event_Time > 45 ~ 45, 
      period == 2L & Event_Time > 90 ~ 90,
      .default = Event_Time
    ) |> as.integer(),
    min_added = case_when(
      period == 1L & Event_Time > 45 ~ Event_Time - 45, 
      period == 2L & Event_Time > 90 ~ Event_Time - 90,
      .default = NA_integer_
    ) |> as.integer(),
    # score_progression = Score_Progression,
    final_home_g = Home_Score,
    final_away_g = Away_Score,
    ## TODO: Identify these vars as "_cumu" vars for the rest of the script
    home_g = as.integer(gsub('[:].*$', '', Score_Progression)), ## after event
    away_g = as.integer(gsub('^.*[:]', '', Score_Progression)),
    match_has_no_goals,
    match_has_no_penalties,
    is_own_goal = Event_Type == 'Own Goal',
    is_penalty = Event_Type == 'Penalty',
    team = Team,
    player = Event_Players,
    event_type = ifelse(match_has_no_goals & match_has_no_penalties, NA_character_, Event_Type),
    time_key = generate_time_key(period, min, min_added)
  )

match_results <- match_summaries |> 
  distinct(
    match_id,
    season,
    gender,
    tier,
    date,
    home_team,
    away_team,
    home_g = final_home_g,
    away_g = final_away_g
  )
qsave(match_results, file.path(PROJ_DIR, 'match_results.qs'))

shots <- raw_shots |> 
  transmute(
    rn = row_number(),
    match_id = basename(dirname(MatchURL)),
    period = as.integer(Match_Half),
    min = ifelse(
      grepl('[+]', Minute),
      as.integer(gsub('(^[0-9]+)[+]([0-9]+$)', '\\1', Minute)), 
      as.integer(Minute)
    ),
    min_added = ifelse(
      grepl('[+]', Minute), 
      as.integer(gsub('(^[0-9]+)[+]([0-9]+$)', '\\2', Minute)), 
      NA_integer_
    ),
    is_home = Home_Away == 'Home',
    team = Squad,
    player = Player,
    is_goal = Outcome == 'Goal',
    xg = as.double(xG),
    is_penalty = coalesce((Distance == '13' & round(xg, 2) == 0.79), FALSE),
    distance = as.integer(Distance),
    body_part = `Body Part`,
    notes = Notes,
    ## seems that they started to exclusively use Take-On instead of Dribble in 2022
    sca1 = ifelse(Event_SCA_1 == 'Dribble', 'Take-On', Event_SCA_1),
    sca2 = ifelse(Event_SCA_2 == 'Dribble', 'Take-On', Event_SCA_2),
    time_key = generate_time_key(period, min, min_added)
  )

synthetic_rn_base <- 10^(ceiling(log10(max(shots$rn))))
shots_with_own_goals <- shots |> 
  mutate(
    is_own_goal = FALSE
  ) |> 
  bind_rows(
    ## synthetic events for own goals
    match_summaries |> 
      filter(
        is_own_goal
      ) |> 
      transmute(
        rn = .env$synthetic_rn_base + row_number(),
        match_id,
        period,
        min,
        min_added,
        is_home = team == home_team,
        team,
        player,
        is_goal = TRUE,
        xg = NA_real_,
        time_key,
        is_own_goal = TRUE,
        is_penalty = FALSE
      )
  )

clean_shots <- shots_with_own_goals |> 
  inner_join(
    match_summaries |>
      distinct(match_id, season, date, home_team, away_team),
    by = join_by(match_id),
    relationship = 'many-to-one'
  ) |> 
  left_join(
    match_summaries |> 
      group_by(match_id, time_key) |> 
      slice_max(home_g + away_g, n = 1, with_ties = FALSE) |> 
      ungroup() |>
      select(
        match_id, 
        time_key, 
        home_g, 
        away_g,
        is_own_goal,
        is_penalty,
        team,
        player
      ) |> 
      rename_with(
        \(.x) paste0('summary_', .x),
        -c(match_id)
      ),
    by = join_by(
      match_id,
      time_key >= summary_time_key,
      time_key <= summary_time_key
    ),
    relationship = 'many-to-many'
  ) |> 
  group_by(match_id) |> 
  fill(summary_home_g, summary_away_g, .direction = 'down') |> 
  ungroup() |> 
  mutate(
    across(c(summary_home_g, summary_away_g), \(.x) coalesce(.x, 0L))
  ) |> 
  mutate(
    home_g = case_when(
      ## note that fotmob would list the away team for an own goal but fbref lists the home team
      (is_goal | is_own_goal) & is_home ~ 1L,
      is_own_goal & is_home ~ 1L,
      TRUE ~ 0L
    ),
    away_g = case_when(
      (is_goal | is_own_goal) & !is_home ~ 1L,
      TRUE ~ 0L
    ),
    home_xg = case_when(
      is_home ~ coalesce(xg, 0),
      TRUE ~ 0L ## even for own goals
    ),
    away_xg = case_when(
      !is_home ~ coalesce(xg, 0),
      TRUE ~ 0L
    )
  ) |>
  group_by(match_id) |> 
  mutate(
    shot_idx = row_number(time_key)
  ) |> 
  ungroup() |> 
  mutate(
    shot_id = sprintf('%s-%02d', match_id, shot_idx)
  ) |> 
  arrange(season, date, shot_id)

## these should have equal counts. i belive these happen when there is a second shot immediately
##   after a saved penalty
clean_shots |> filter(!summary_is_penalty, is_penalty)
# clean_shots |> filter(summary_is_penalty, !is_penalty) |> count(grepl('saved|Miss|-$', summary_player))

restacked_shots <- bind_rows(
  clean_shots |> 
    filter(is_home) |> 
    transmute(
      shot_id,
      match_id,
      season,
      date,
      period,
      min,
      min_added,
      is_home,
      is_goal,
      is_penalty,
      is_own_goal,
      player,
      team = home_team,
      opponent = away_team,
      g = home_g,
      g_conceded = away_g,
      xg = home_xg,
      xg_conceded = away_xg,
      distance,
      body_part,
      sca1,
      sca2,
      summary_g = summary_home_g,
      summary_g_conceded = summary_away_g
    ),
  clean_shots |> 
    filter(!is_home) |> 
    transmute(
      shot_id,
      match_id,
      season,
      date,
      period,
      min,
      min_added,
      is_home,
      is_goal,
      is_penalty = summary_is_penalty,
      is_own_goal,
      player,
      team = away_team,
      opponent = home_team,
      g = away_g,
      g_conceded = home_g,
      xg = away_xg,
      xg_conceded = home_xg,
      distance,
      body_part,
      sca1,
      sca2,
      summary_g = summary_away_g,
      summary_g_conceded = summary_home_g,
    )
) |> 
  arrange(season, date, match_id, shot_id)

doublecounted_restacked_shots <- bind_rows(
  restacked_shots |> mutate(pov = 'primary', .before = is_home),
  restacked_shots |> 
    rename(
      team1 = team,
      team2 = opponent,
      g1 = g,
      g2 = g_conceded,
      xg1 = xg,
      xg2 = xg_conceded
    ) |> 
    rename(
      team = team2,
      opponent = team1,
      g = g2,
      g_conceded = g1,
      xg = xg2,
      xg_conceded = xg1
    ) |> 
    mutate(
      is_home = !is_home,
      pov = 'secondary'
    )
) |> 
  arrange(season, date, match_id, shot_id, pov)

cumu_doublecounted_restacked_shots <- doublecounted_restacked_shots |> 
  group_by(match_id, team) |> 
  mutate(
    across(
      c(g, g_conceded, xg, xg_conceded),
      list(cumu = cumsum)
    )
  ) |> 
  ungroup() |> 
  mutate(
    is_goal = factor(ifelse(g == 1L, 'yes', 'no')),
    game_state = g_cumu - g_conceded_cumu
  )
qsave(cumu_doublecounted_restacked_shots, file.path(PROJ_DIR, 'shots.qs'))

# cumu_doublecounted_restacked_shots |>
#   filter(
#     season == '2022/23',
#     pov == 'primary',
#     is_goal == 'yes',
#     summary_g != g_cumu
#   ) |>
#   arrange(desc(date))
# ## occasionally there are 1-minute disagreements between the match summary and player log minute,
# ##  e.g. Martial's 45+5/6 minute goal for https://fbref.com/en/matches/d2f2263d/Manchester-United-Chelsea-May-25-2023-Premier-League
# cumu_doublecounted_restacked_shots |> 
#   filter(match_id == 'd2f2263d', pov == 'primary', is_goal == 'yes')
# ## this can happen for non-extra time events as well. e.g. Danilo's 68/69th minute goal for https://fbref.com/en/matches/76db2de1/Nottingham-Forest-Brighton-and-Hove-Albion-April-26-2023-Premier-League
# cumu_doublecounted_restacked_shots |> 
#   filter(match_id == 'e265430e', pov == 'primary', is_goal == 'yes') 

match_dates <- match_summaries |> 
  dplyr::distinct(date) |> 
  dplyr::mutate(
    prev_date = date - lubridate::days(1)
  )

CLUBELO_DIR <- file.path(PROJ_DIR, 'clubelo')
dir.create(CLUBELO_DIR, showWarnings = FALSE)
get_clubelo_ratings <- function(date) {
  path <- file.path(CLUBELO_DIR, paste0(date, '.qs'))
  if (file.exists(path)) {
    return(qs::qread(path))
  }
  Sys.sleep(runif(1, 1, 3))
  message(sprintf('Scraping clubelo for %s.', date))
  url <- sprintf('http://api.clubelo.com/%s', date)
  resp <- httr::GET(url)
  httr::stop_for_status(resp)
  res <- httr::content(resp)
  qs::qsave(res,path)
  invisible(res)
}

possibly_get_clubelo_ratings <- purrr::possibly(
  get_clubelo_ratings,
  otherwise = tibble::tibble(),
  quiet = FALSE
)

raw_clubelo_ratings <- purrr::map2_dfr(
  match_dates$prev_date,
  match_dates$date,
  \(prev_date, date) {
    possibly_get_clubelo_ratings(prev_date) |> 
      dplyr::mutate(
        date = .env$date,
        .before = 1
      )
  }
)

## created manually
clubelo_team_mapping <- readr::read_csv(file.path(PROJ_DIR, 'clubelo-team-mapping.csv'))
raw_clubelo_ratings |> 
  dplyr::filter(
    ## don't filter to Tier 1 since that would mess with some promoted teams' first match of the season
    Country == 'ENG'
  ) |> 
  dplyr::inner_join(
    clubelo_team_mapping,
    by = dplyr::join_by(Club == team_clubelo)
  ) |> 
  dplyr::select(
    date,
    team = team_fbref,
    elo = Elo
  ) |> 
  qs::qsave(file.path(PROJ_DIR, 'clubelo-ratings.qs'))
