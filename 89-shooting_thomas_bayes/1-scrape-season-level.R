## data wrangling
library(worldfootballR)
library(dplyr)
library(tibble)

## distribution fitting and wrangling
library(MASS, include.only = 'fitdistr') ## to avoid `select` name conflict with dplyr
library(withr)
library(purrr)
library(tidyr)

## plotting
library(ggplot2)
library(forcats)
PROJ_DIR <- '89-shooting_thomas_bayes'
DATA_DIR <- file.path(PROJ_DIR, 'data')
purrr::walk(
  c(
    DATA_DIR
  ),
  \(.x) {
    dir.create(.x, showWarnings = FALSE, recursive = TRUE)
  }
)

manage_io_operations <- function(
    f, 
    ...,
    .name, 
    .data_dir,
    .overwrite = FALSE, 
    .sleep = TRUE,
    .delay_range = c(1, 2)
) {
  
  path <- file.path(.data_dir, paste0(.name, '.qs'))
  
  if (file.exists(path) & isFALSE(.overwrite)) {
    return(qs::qread(path))
  }
  if (isTRUE(.sleep)) {
    Sys.sleep(runif(1, .delay_range[1], .delay_range[2]))
  }
  message(sprintf('%s: Getting %s.', Sys.time(), .name))
  res <- f(...)
  qs::qsave(res, path)
  res
}

get_fb_league_player_shooting_stats <- function(country, season_end_year) {
  worldfootballR::fb_league_stats(
    country = country,
    gender = 'M',
    season_end_year = season_end_year,
    tier = '1st',
    stat_type = 'shooting',
    team_or_player = 'player'
  )
}

get_and_save_fb_league_player_shooting_stats <- function(country, season_end_year) {
  manage_io_operations(
    get_fb_league_player_shooting_stats,
    country = country,
    season_end_year = season_end_year,
    .name = paste0(country, '-', season_end_year),
    .data_dir = DATA_DIR
  )
}

raw <- tidyr::crossing(
  country = c('ENG', 'ESP', 'FRA', 'GER', 'ITA', 'USA'),
  season_end_year = c(2018:2024)
) |> 
  dplyr::mutate(
    data = purrr::map2(
      country, season_end_year,
      \(.x, .y) {
        res <- get_and_save_fb_league_player_shooting_stats(
          country = .x,
          season_end_year = .y
        ) |> 
          dplyr::mutate(
            Age = as.character(Age)
          )
      }
    )
  ) |> 
  tidyr::unnest(data) |> 
  janitor::clean_names()

raw |> 
  dplyr::transmute(
    g = gls_standard,
    npg = 
  )
  dplyr::group_by(player, player_href) |> 
  summarize(
    across(
      c(
        gls_
      )
    )
  )

player_shooting <- worldfootballR::fb_league_stats(
  country = c('ENG', 'USA'),
  gender = 'M',
  season_end_year = c(2018:2024),
  tier = '1st',
  stat_type = 'shooting',
  team_or_player = 'player'
)
