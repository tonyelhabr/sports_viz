library(httr)
library(tibble)
library(tidyr)
library(dplyr)
library(purrr)
library(janitor)
library(cli)
library(qs)
library(stringr)

proj_dir <- '71-2023_u20_world_cup'
data_dir <- file.path(proj_dir, 'data')
dir.create(data_dir, showWarnings = FALSE)

season_id <- 284700
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

scrape_match_stats <- function(result_id) {
  stats_resp <- GET(sprintf('https://fdh-api.fifa.com/v1/stats/match/%s/players.json', result_id))
  stop_for_status(stats_resp)
  stats_resp |> 
    content() |> 
    enframe('player_id', 'values') |> 
    unnest_longer(values)
}

possibly_scrape_match_stats <- possibly(
  scrape_match_stats,
  otherwise = tibble(),
  quiet = FALSE
)

scrape_and_save_match_stats <- function(result_id, overwrite = FALSE) {
  path <- file.path(data_dir, 'match-stats', paste0(result_id, '.qs'))
  if (file.exists(path) & isFALSE(overwrite)) {
    return(qs::qread(path))
  }
  cli::cli_inform('Scraping {result_id}.')
  res <- possibly_scrape_match_stats(result_id)
  qs::qsave(res, path)
  res
}

slowly_scrape_and_save_match_stats <- slowly(
  scrape_and_save_match_stats,
  rate = rate_delay(pause = 1, max_times = 1),
  quiet = FALSE
)

match_stats <- matches |> 
  pull(result_id) |> 
  map_dfr(
    ~{
      scrape_and_save_match_stats(.x) |> 
        mutate(result_id = !!.x, .before = 1)
    }
  )

## live match elements for players ----
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

possibly_scrape_live_match_elements <- possibly(
  scrape_live_match_elements,
  otherwise = tibble(),
  quiet = FALSE
)

scrape_and_scrape_live_match_elements <- function(url, result_id, overwrite = FALSE) {
  path <- file.path(data_dir, 'live-match-elements', paste0(result_id, '.qs'))
  if (file.exists(path) & isFALSE(overwrite)) {
    return(qs::qread(path))
  }
  cli::cli_inform('Scraping {result_id}.')
  res <- possibly_scrape_live_match_elements(url)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  qs::qsave(res, path)
  res
}

slowly_scrape_and_save_match_stats <- slowly(
  scrape_and_save_match_stats,
  rate = rate_delay(pause = 1, max_times = 1),
  quiet = FALSE
)

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
      scrape_and_scrape_live_match_elements(
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
  # clean_names() |> 
  # rename_with(\(x) gsub('_1', '', x), everything()) |> 
  distinct(
    player_id = IdPlayer,
    player_name = str_to_title(PlayerName_1_Description),
    # position = Position,
    # player_picture_id = PlayerPicture_Id,
    player_picture_url = PlayerPicture_PictureUrl,
    country,
    country_picture_url
  )

## combine everything ----
unnested_match_stats <- match_stats |> 
  hoist(
    values,
    'stat' = 1,
    'value' = 2
  ) |> 
  select(-values) 
unnested_match_stats |> count(stat, sort = TRUE) |> tibble::view()
## Why are there 2 records???
## unnested_match_stats |> filter(stat %in% 'TotalDistance', result_id == 137442, player_id == 441263)
player_stats <- unnested_match_stats |> 
  # filter(
  #   stat %in% c(
  #     'TimePlayed',
  #     'TotalDistance', ## some duplicates???
  #     'AttemptedBallProgressions',
  #     'CompletedBallProgressions',
  #     'LinebreaksAttemptedAttackingLineCompleted',
  #     'LinebreaksAttempted',
  #     'LinebreaksAttemptedCompleted'
  #   )
  # ) |> 
  # distinct(result_id, player_id, stat, value) |> 
  group_by(result_id, player_id, stat) |> 
  slice_max(value, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  pivot_wider(
    names_from = stat, 
    values_from = value,
    values_fill = 0
  ) |> 
  clean_names()

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
qs::qsave(player_stats_p90, file.path(data_dir, 'player_stats_p90.qs'))

# long_player_stats_p90 <- player_stats |> 
#   group_by(player_id) |> 
#   summarize(
#     n_matches = n_distinct(result_id),
#     across(
#       -c(result_id),
#       sum
#     )
#   ) |> 
#   ungroup() |> 
#   mutate(
#     across(
#       -c(player_id, n_matches, time_played),
#       list(p90 = \(x) 90 * x / time_played)
#     )
#   ) |> 
#   pivot_longer(
#     -c(player_id, n_matches, time_played),
#     names_to = 'stat',
#     values_to = 'value'
#   ) |> 
#   filter(
#     time_played > 90,
#     str_detect(stat, '_p90$')
#   ) |> 
#   group_by(stat) |> 
#   mutate(
#     rnk = row_number(desc(value))
#   ) |> 
#   ungroup()
# long_player_stats_p90 |> 
#   left_join(
#     players |> distinct(player_id, player_name, country)
#   ) |> 
#   filter(rnk <= 15) |> 
#   count(stat, country, sort = TRUE) |> 
#   filter(country == 'USA')
