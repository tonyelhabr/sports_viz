library(httr)
library(tibble)
library(tidyr)
library(dplyr)
library(purrr)
library(janitor)
library(cli)
library(stringr)
library(readr)

proj_dir <- '72-2023_womens_world_cup'
data_dir <- file.path(proj_dir, 'data')
match_stats_dir <- file.path(data_dir, 'match-stats')
live_match_elements_dir <- file.path(data_dir, 'live-match-elements')
dir.create(match_stats_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(live_match_elements_dir, showWarnings = FALSE)

season_id <- 285026

matches_resp <- GET(sprintf('https://api.fifa.com/api/v3/calendar/matches?language=en&count=500&idSeason=%s', season_id))
results <- content(matches_resp) |> pluck('Results')

## for incomplete matches (anything beyond the group stage at the moment), there will be `NULL`s
##   which causes `pluck()` to throw an error. using a `.default` of `NA` fixes the issue.
pluck2_chr <- partial(pluck, .default = NA_character_, ... = )
pluck2_int <- partial(pluck, .default = NA_integer_, ... = )

map_pluck_chr <- function(x, ...) {
  map_chr(x, pluck2_chr, ...)
}

map_pluck_int <- function(x, ...) {
  map_int(x, pluck2_int, ...)
}

matches <- tibble(
  competition_id = map_pluck_chr(results, 'IdCompetition'),
  season_id = map_pluck_chr(results, 'IdSeason'),
  stage_id = map_pluck_chr(results, 'IdStage'),
  group_id = map_pluck_chr(results, 'IdGroup'),
  ## this won't join with the match stats, but it seems to be Fifa's "true" match ID
  match_id = map_pluck_chr(results, 'IdMatch'),
  match_status = map_pluck_int(results, 'MatchStatus'),
  ## use this to join with the match stats
  result_id = map_pluck_chr(results, 'Properties', 'IdIFES'),
  home_abbr = map_pluck_chr(results, 'Home', 'Abbreviation'),
  away_abbr = map_pluck_chr(results, 'Away', 'Abbreviation')
) |> 
  filter(match_status == 0L) |> 
  select(-match_status)
matches

scrape_match_stats <- function(result_id) {
  stats_resp <- GET(sprintf('https://fdh-api.fifa.com/v1/stats/match/%s/players.json', result_id))
  stop_for_status(stats_resp)
  stats_resp |> 
    content() |> 
    enframe('player_id', 'values') |> 
    unnest_longer(values)
}

scrape_and_save_match_stats <- function(result_id) {
  path <- file.path(match_stats_dir, paste0(result_id, '.rds'))
  if (file.exists(path)) {
    return(read_rds(path))
  }
  cli_inform('Scraping {result_id}.')
  res <- scrape_match_stats(result_id)
  write_rds(res, path)
  res
}

match_stats <- matches |> 
  pull(result_id) |> 
  map_dfr(
    ~{
      scrape_and_save_match_stats(.x) |> 
        mutate(result_id = !!.x, .before = 1)
    }
  )
match_stats

generate_live_match_url <- function(competition_id, season_id, stage_id, match_id) {
  sprintf(
    'https://api.fifa.com/api/v3/live/football/%s/%s/%s/%s?language=en',
    competition_id,
    season_id,
    stage_id,
    match_id
  )
}

scrape_live_match_elements <- function(url) {
  resp <- GET(url)
  elements <- content(resp) |> 
    enframe('element', 'values')
}

scrape_and_save_live_match_elements <- function(url, result_id, overwrite = FALSE) {
  path <- file.path(live_match_elements_dir, paste0(result_id, '.rds'))
  if (file.exists(path) & isFALSE(overwrite)) {
    return(read_rds(path))
  }
  cli_inform('Scraping {result_id}.')
  res <- scrape_live_match_elements(url)
  write_rds(res, path)
  res
}

live_match_elements <- matches |> 
  mutate(
    live_match_url = generate_live_match_url(
      competition_id = competition_id,
      season_id = season_id,
      stage_id = stage_id,
      match_id = match_id
    )
  ) |> 
  pull(live_match_url, result_id) |> 
  imap_dfr(
    ~{
      scrape_and_save_live_match_elements(
        url = ..1,
        result_id = ..2
      ) |> 
        mutate(
          result_id = !!..2,
          .before = 1
        )
    }
  )

live_match_teams <- live_match_elements |> 
  filter(
    element %in% c(
      'HomeTeam',
      'AwayTeam'
    )
  ) |> 
  unnest_wider(values)

players <- live_match_teams |> 
  transmute(
    country = ShortClubName, 
    country_picture_url = str_replace_all(PictureUrl, c('\\{format\\}' = 'sq', '\\{size\\}' = '4')), 
    Players
  ) |> 
  unnest_longer(Players) |> 
  unnest_wider(Players) |> 
  unnest_wider(where(is.list), names_sep = '_') |> 
  unnest_wider(where(is.list), names_sep = '_') |> 
  distinct(
    player_id = IdPlayer,
    player_name = PlayerName_1_Description,
    player_picture_url = PlayerPicture_PictureUrl,
    country,
    country_picture_url
  )
players

unnested_match_stats <- match_stats |> 
  hoist(
    values,
    'stat' = 1,
    'value' = 2
  ) |> 
  select(-values) 

player_stats <- unnested_match_stats |> 
  group_by(result_id, player_id, stat) |> 
  slice_max(value, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  pivot_wider(
    names_from = stat, 
    values_from = value,
    values_fill = 0
  ) |> 
  clean_names()
player_stats

player_stats_p90 <- player_stats |> 
  group_by(player_id) |> 
  summarize(
    n_matches = n_distinct(result_id),
    across(
      -c(result_id),
      sum
    )
  ) |> 
  ungroup() |> 
  # select(player_id, player_name, player_nationality, linebreaks_attempted_completed) |> 
  # arrange(desc(linebreaks_attempted_completed)) |> 
  mutate(
    across(
      -c(player_id, time_played),
      list(p90 = \(x) 90 * x / time_played)
    )
  ) |> 
  left_join(
    players, 
    by = join_by(player_id)
  )
player_stats_p90
write_rds(player_stats_p90, file.path(data_dir, 'player_stats_p90.rds'))
