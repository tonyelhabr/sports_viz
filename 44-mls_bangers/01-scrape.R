
library(httr)
library(jsonlite)
library(dplyr)
library(janitor)
library(tibble)
library(lubridate)
library(tidyr)
library(stringr)
library(qs)
library(cli)
library(purrr)

dir_proj <- '44-mls_bangers'
dir_data <- file.path(dir_proj, 'data')
# fs::dir_create(dir_data)

base_url <- 'https://www.fotmob.com/'

get_matches_by_date <- function(date, overwrite = FALSE, path = file.path(dir_data, sprintf('matches-%s.qs', date)), delay = 1) {
  url <- paste0(base_url, 'matches?date=', str_remove_all(as.character(date), '-'))

  if(file.exists(path) & !overwrite) {
    cli::cli_alert_info(sprintf('Reading in matches from %s.', date))
    return(qs::qread(path))
  }
  Sys.sleep(delay)
  cli::cli_alert_info(sprintf('%s: Scraping matches on %s.', Sys.time(), date))
  resp <- jsonlite::fromJSON(url)
  qs::qsave(resp, path)
  resp
}

get_match <- function(match_id, overwrite = FALSE, path = file.path(dir_data, sprintf('match_id-%s.qs', match_id)), delay = 0.5) {
  url <- paste0(base_url, 'matchDetails?matchId=', match_id)
  
  if(file.exists(path) & !overwrite) {
    cli::cli_alert_info(sprintf('Reading in `match_id = %d`.', match_id))
    return(qs::qread(path))
  }
  Sys.sleep(delay)
  cli::cli_alert_info(sprintf('%s: Scraping `match_id = %d`.', Sys.time(), match_id))
  resp <- jsonlite::fromJSON(url)
  qs::qsave(resp, path)
  resp
}

pluck_matches <- function(x) {
  bind_cols(tibble(date = lubridate::ymd(x$date)), x$leagues)
}

pluck_match_data <- function(x) {
  content <- x$content
  bind_cols(
    tibble(match_id = content$matchFacts$matchId),
    content$shotmap$shots
  )
}

league_mapping <- tibble(
  league_id = c(47, 54, 87, 53, 55, 130),
  league_name = c('EPL', 'Bundesliga', 'LaLiga', 'Ligue 1', 'Serie A', 'MLS')
)
dates_v <- seq.Date(
  lubridate::ymd('2016-08-01'),
  lubridate::ymd('2021-10-31'), 
  by = 'day'
)

# main ----
match_ids_init <- dates_v %>% 
  map(get_matches_by_date) %>% 
  map_dfr(~pluck_matches(.x)) %>% 
  janitor::clean_names()

match_ids <- match_ids_init %>% 
  inner_join(
    league_mapping,
    by = c('primary_id' = 'league_id')
  ) %>% 
  hoist(
    matches,
    'match_id' = 'id'
  ) %>% 
  # `name` in matches is sort of like my `league_name`, but it also includes stuff like MLS tourneys
  # these are also indicated by `id`
  # `parent_league_id` is another possible thing to use for league id
  select(date, league_id = primary_id, league_name, match_id) %>% 
  unnest_longer(match_id)

match_data <- match_ids %>% 
  pull(match_id) %>% 
  map(get_match) %>% 
  map_dfr(pluck_match_data) %>% 
  janitor::clean_names() %>% 
  select(
    match_id,
    id,
    team_id,
    player_id,
    # player_name,
    period,
    min,
    min_added,
    event_type,
    shot_type,
    
    x,
    y,
    x_b = blocked_x,
    y_b = blocked_y,
    y_g = goal_crossed_y,
    z_g = goal_crossed_z,
    
    xg = expected_goals,
    xgot = expected_goals_on_target,
    # on_goal_shot
    is_blocked,
    is_on_target,
    is_own_goal
  )

matches_aug <- match_ids %>% 
  inner_join(match_data)
matches_aug %>% 
  filter(date < '2019-09-01') %>% 
  count(league_name, is_na = is.na(id))
matches_aug %>% 
  drop_na(id) %>% 
  distinct(league_name, match_id) %>% 
  count(league_name)

qs::qsave(matches_aug %>% drop_na(id), file.path(dir_proj, 'data.qs'))
