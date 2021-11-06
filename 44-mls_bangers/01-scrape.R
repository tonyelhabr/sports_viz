
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
  # content <- x$content
  shots <- x$content$shotmap$shots
  has_shots <- length(shots) > 0
  general <- x$general
  general_scalars <- tibble(
    matchId = general$matchId,
    matchRound = ifelse(is.null(general$matchRound), '', general$matchRound),
    leagueId = general$leagueId,
    leagueName = general$leagueName,
    leagueRoundName = general$leagueRoundName,
    parentLeagueId = general$parentLeagueId,
    parentLeagueSeason = general$parentLeagueSeason,
    matchTimeUTC = general$matchTimeUTC
  )
  general_teams <- tibble(
    homeTeamId = unlist(general$homeTeam$id),
    homeTeam = unlist(general$homeTeam$name),
    homeTeamColor = unlist(general$teamColors$home),
    awayTeamId = unlist(general$awayTeam$id),
    awayTeam = unlist(general$awayTeam$name),
    awayTeamColor = unlist(general$teamColors$away)
  )
  
  df <-
    bind_cols(
      # tibble(match_id = content$matchFacts$matchId),
      general_scalars,
      general_teams
    )
  
  if(!has_shots) {
    return(df)
  }
  
  df %>% 
    bind_cols(
      shots
    ) %>% 
    mutate(
      team = case_when(
        teamId == general$homeTeam$id ~ homeTeam,
        teamId == general$awayTeam$id ~ awayTeam,
        TRUE ~ NA_character_
      )
    )
}

league_mapping <- tibble(
  league_id = c(47, 54, 87, 53, 55, 130),
  league_name = c('EPL', 'Bundesliga', 'LaLiga', 'Ligue 1', 'Serie A', 'MLS')
)
dates_v <- seq.Date(
  lubridate::ymd('2019-02-03'),
  lubridate::ymd('2021-11-05'), 
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

match_data_init <- 
  match_ids %>%
  pull(match_id) %>%
  map(get_match) %>%   
  map_dfr(pluck_match_data)

match_data <- match_data_init %>% 
  janitor::clean_names() %>% 
  mutate(
    time = match_time_utc %>% strptime(format = '%a, %b %d, %Y, %H:%M UTC', tz = 'UTC') %>% lubridate::ymd_hms(),
    date = time %>% lubridate::date(),
  ) %>% 
  select(
    match_id,
    season = parent_league_season,
    time,
    date,
    
    home_id = home_team_id,
    away_id = away_team_id,
    home_team = home_team,
    away_team = away_team,
    id,
    team_id,
    team,
    player_id,
    player_name,
    period,
    min,
    min_added,
    situation,
    event_type,
    shot_type,
    
    x,
    y,
    x_b = blocked_x,
    y_b = blocked_y,
    y_g = goal_crossed_y,
    z_g = goal_crossed_z,
    
    xg = expected_goals,
    
    # zoom_ratio,
    xgot = expected_goals_on_target,
    on_goal_shot,
    is_blocked,
    is_on_target,
    is_own_goal
  )
match_data
# strptime(general_scalars$matchTimeUTC, format = '%a, %b %d, %Y, %H:%M UTC', tz = 'UTC') %>% lubridate::ymd_hms()
# strptime(general_scalars$matchTimeUTC, format = '%a, %b %d, %Y, %H:%M UTC', tz = 'UTC') %>% lubridate::date()
matches_aug <- match_ids %>% 
  inner_join(match_data)

matches_aug %>%
  drop_na(xgot) %>% 
  arrange(desc(on_goal_shot$x))
matches_aug %>%
  drop_na(xgot) %>% 
  arrange(on_goal_shot$x)
matches_aug %>%
  filter(!is.na(xgot)) %>% 
  filter(is.na(z_g))

qs::qsave(matches_aug %>% drop_na(id), file.path(dir_proj, 'data.qs'))
