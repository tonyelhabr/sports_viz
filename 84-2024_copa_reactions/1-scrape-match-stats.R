library(dplyr)
library(tidyr)
library(purrr)
library(janitor)

library(fotmob) ## tonyelhabr/fotmob

library(qs)

PROJ_DIR <- '84-2024_copa_reactions'
source(file.path(PROJ_DIR, 'helpers.R'))
DATA_DIR <- file.path(PROJ_DIR, 'data')
LEAGUE_MATCHES_DATA_DIR <- file.path(DATA_DIR, 'league_matches')
MATCH_DETAILS_DATA_DIR <- file.path(DATA_DIR, 'match_details')
MATCH_SCORE_DATA_DIR <- file.path(DATA_DIR, 'match_score')
MATCH_TIME_DATA_DIR <- file.path(DATA_DIR, 'match_time')
MATCH_STATS_DATA_DIR <- file.path(DATA_DIR, 'match_stats')
purrr::walk(
  c(
    LEAGUE_MATCHES_DATA_DIR,
    MATCH_DETAILS_DATA_DIR,
    MATCH_SCORE_DATA_DIR,
    MATCH_TIME_DATA_DIR,
    MATCH_STATS_DATA_DIR
  ),
  \(.x) {
    dir.create(.x, showWarnings = FALSE, recursive = TRUE)
  }
)

get_and_save_league_matches <- function(league_id, ...) {
  manage_io_operations(
    fotmob::fotmob_get_league_matches,
    league_id = league_id,
    ...,
    # .overwrite = TRUE,
    .name = league_id,
    .data_dir = LEAGUE_MATCHES_DATA_DIR
  )
}

# get_fotmob_image_url <- function(team_id) {
#   sprintf('https://images.fotmob.com/image_resources/logo/teamlogo/%s.png', team_id)
# }

fotmob_get_single_match_details <- function(match_id) {
  main_url <- 'https://www.fotmob.com/api/'
  url <- paste0(main_url, 'matchDetails?matchId=', match_id)
  fotmob:::safely_get_content(url)$result
}

get_and_save_match_details <- function(match_id) {
  manage_io_operations(
    fotmob_get_single_match_details,
    match_id = match_id,
    .name = match_id,
    .data_dir = MATCH_DETAILS_DATA_DIR
  )
}

fotmob_get_single_match_time <- function(match_id) {
  res <- get_and_save_match_details(match_id)
  raw_df <- tibble::as_tibble(res$content$matchFacts$events$events)
  periods <- raw_df |> 
    dplyr::filter(type == 'Half') |> 
    dplyr::transmute(
      period = dplyr::row_number(),
      time,
    )
  added_time <- raw_df |> 
    dplyr::filter(type == 'AddedTime') |> 
    dplyr::select(
      time,
      minutes_added = minutesAddedInput
    )
  dplyr::full_join(
    periods,
    added_time,
    by = dplyr::join_by(time)
  )
}

get_and_save_match_time <- function(match_id) {
  manage_io_operations(
    fotmob_get_single_match_time,
    match_id = match_id,
    .name = match_id,
    .data_dir = MATCH_TIME_DATA_DIR,
    .sleep = FALSE
  )
}

fotmob_get_single_match_score <- function(match_id) {
  res <- get_and_save_match_details(match_id)
  status <- res$header$status
  tibble::tibble(
    regular_score_line = status$scoreStr,
    pen_score_line = ifelse(
      status$reason$short == 'Pen',
      gsub('Pen ', '', status$reason$long),
      NA_character_
    ),
    score_line = paste0(
      status$scoreStr,
      ifelse(!is.na(pen_score_line), paste0(' (', pen_score_line, ')'), '')
    )
  )
}

get_and_save_match_score <- function(match_id) {
  manage_io_operations(
    fotmob_get_single_match_score,
    match_id = match_id,
    .name = match_id,
    .data_dir = MATCH_SCORE_DATA_DIR,
    .sleep = FALSE,
    .overwrite = TRUE
  )
}

get_and_save_match_stats <- function(match_id) {
  manage_io_operations(
    fotmob::fotmob_get_match_team_stats,
    match_id = match_id,
    .name = match_id,
    .data_dir = MATCH_STATS_DATA_DIR
  )
}

map_fotmob_f <- function(match_ids, f, ...) {
  purrr::map(
    match_ids,
    \(.x) {
      res <- f(.x, ...)
      dplyr::mutate(
        res,
        match_id = .x,
        .before = 1
      )
    }
  ) |> 
    dplyr::bind_rows()
}

## main ----
raw_matches <- c(
  'Copa America 2024' = 44, 
  'EURO 2024' = 50
) |> 
  purrr::imap(
    \(.x, .y) {
      res <- get_and_save_league_matches(
        league_id = .x, 
        season = 2024,
        .overwrite = FALSE
      )
      dplyr::mutate(
        res,
        competition_name = .y
      )
    }
  ) |> 
  dplyr::bind_rows() |> 
  janitor::clean_names()

matches <- raw_matches |> 
  dplyr::select(competition_name, match_id = id, home, away, status) |>
  tidyr::unnest_wider(c(home, away, status), names_sep = '_')

completed_matches <- matches |> 
  dplyr::filter(status_finished)

match_time <- map_fotmob_f(
  completed_matches$match_id,
  get_and_save_match_time
)

agg_match_time <- match_time |> 
  dplyr::group_by(match_id) |> 
  dplyr::summarize(
    ## extra-time only counted as one period
    period_count = max(period, na.rm = TRUE),
    total_time = max(time, na.rm = TRUE),
    total_minutes_added = sum(minutes_added, na.rm = TRUE),
    total_duration = dplyr::coalesce(total_time, 0L) + dplyr::coalesce(total_minutes_added, 0L)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    had_extra_time = period_count > 2L
  )

match_score <- map_fotmob_f(
  completed_matches$match_id,
  get_and_save_match_score
)

match_stats <- map_fotmob_f(
  completed_matches$match_id,
  get_and_save_match_stats
)

match_passes <- match_stats |> 
  dplyr::filter(
    stats_title == 'Passes',
    stats_type == 'text'
  ) |> 
  dplyr::left_join(
    agg_match_time |> dplyr::select(match_id, had_extra_time, total_minutes_added, total_duration),
    by = dplyr::join_by(match_id)
  ) |> 
  dplyr::left_join(
    completed_matches |> dplyr::select(competition_name, match_id),
    by = dplyr::join_by(match_id)
  ) |>
  dplyr::left_join(
    match_score,
    by = dplyr::join_by(match_id)
  ) |> 
  dplyr::transmute(
    competition_name,
    match_id,
    match_time_utc = lubridate::as_datetime(as.POSIXct(match_time_utc, format="%a, %b %d, %Y, %H:%M", tz="UTC")),
    home_team_id,
    away_team_id,
    home_team,
    away_team,
    regular_score_line,
    pen_score_line,
    score_line,
    had_extra_time, total_minutes_added, total_duration,
    home_passes = home_value,
    away_passes = away_value
  ) |> 
  dplyr::mutate(
    dplyr::across(dplyr::ends_with('passes'), as.integer),
    total_passes = home_passes + away_passes,
    # total_passes_p90 = 90 * total_passes / total_duration,
    passes_per_minute = total_passes / total_duration
  ) |> 
  dplyr::arrange(dplyr::desc(passes_per_minute))

qs::qsave(match_passes, file.path(PROJ_DIR, 'match_passes.qs'))

## average added time per period
match_time |> 
  dplyr::left_join(
    completed_matches |> 
      dplyr::select(match_id, competition_name),
    by = dplyr::join_by(match_id)
  ) |> 
  dplyr::filter(period %in% c(1L, 2L)) |> 
  dplyr::group_by(competition_name, period) |> 
  dplyr::summarize(
    dplyr::across(minutes_added, \(.x) mean(.x, na.rm = TRUE))
  ) |> 
  dplyr::ungroup()
