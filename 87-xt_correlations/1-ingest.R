library(arrow)
library(arrow)
library(purrr)
library(tibble)
library(tidyr)
library(dplyr)
library(lubridate)


PROJ_DIR <- 'posts/xg-team-quality'
read_parquet_from_url <- function(url) {
  load <- curl::curl_fetch_memory(url)
  arrow::read_parquet(load$content)
}

REPO <- 'tonyelhabr/socceraction-streamlined'
read_socceraction_parquet_release <- function(name, tag) {
  url <- sprintf('https://github.com/%s/releases/download/%s/%s.parquet', REPO, tag, name)
  read_parquet_from_url(url)
}

read_socceraction_parquet_releases <- function(name, tag = 'data-processed') {
  purrr::map_dfr(
    2013:2022,
    \(season_start_year) {
      basename <- sprintf('8-%s-%s', season_start_year, name)
      message(basename)
      read_socceraction_parquet_release(basename, tag = tag)
    }
  )
}

read_socceraction_parquet <- function(name, branch = 'main') {
  url <- sprintf('https://github.com/%s/raw/%s/%s.parquet', REPO, branch, name)
  read_parquet_from_url(url)
}

position_mapping <- list(
  'F' = c('FWL', 'FW', 'FWR'),
  'M' = c('AML', 'ML', 'AMC', 'MC', 'AMR', 'MR'),
  'D' = c('DL', 'DML', 'DC', 'DMC', 'DR', 'DMR'),
  'K' = 'GK'
) |> 
  tibble::enframe('position', 'opta_position') |> 
  tidyr::unnest_longer(opta_position)


x <- read_socceraction_parquet_releases('x')
y <- read_socceraction_parquet_releases('y')
xt <- read_socceraction_parquet_releases('xt')
actions <- read_socceraction_parquet_releases('actions')
players <- read_socceraction_parquet_releases('players')
games <- read_socceraction_parquet_releases('games')
games <- dplyr::mutate(
  games,
  date = lubridate::date(game_date)
)

xy <- dplyr::left_join(
  y,
  x |>
    dplyr::filter(
      type_shot_a0 | type_shot_penalty_a0 | type_shot_freekick_a0
    ) |> 
    dplyr::transmute(
      game_id, 
      action_id, 
      is_penalty = type_shot_penalty_a0
    ),
  by = dplyr::join_by(game_id, action_id)
) |> 
  dplyr::left_join(
    actions |>
      dplyr::select(
        game_id,
        team_id,
        period_id,
        action_id
      ),
    by = dplyr::join_by(game_id, action_id)
  ) |>
  dplyr::left_join(
    xt |>
      dplyr::select(
        game_id,
        action_id,
        xt
      ),
    by = dplyr::join_by(game_id, action_id)
  ) |>
  dplyr::inner_join(
    games |> dplyr::select(competition_id, season_id, game_id),
    by = dplyr::join_by(game_id)
  ) |> 
  dplyr::mutate(
    dplyr::across(c(scores, concedes), ~ifelse(.x, 'yes', 'no'))
  )

game_agg <- xy |> 
  dplyr::group_by(competition_id, season_id, game_id, team_id) |> 
  dplyr::summarize(
    g = sum(goal_from_shot, na.rm = TRUE),
    npxg = sum(goal_from_shot & !dplyr::coalesce(is_penalty, FALSE), na.rm = TRUE),
    xt = sum(xt, na.rm = TRUE)
  ) |> 
  dplyr::ungroup()
