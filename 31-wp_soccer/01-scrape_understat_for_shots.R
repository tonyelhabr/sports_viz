
library(tidyverse)
dir_proj <- '31-wp_soccer'
dir_data <- file.path(dir_proj, 'data')
fs::dir_create(dir_data)
path_matches <- file.path(dir_proj, 'matches.rds')
path_shots <- file.path(dir_proj, 'shots.rds')
path_meta <- file.path(dir_proj, 'meta.rds')
path_league_teams_stats <- file.path(dir_proj, 'league_teams_stats.rds')
path_teams_players_stats <- file.path(dir_proj, 'teams_players_stats.rds')
overwrite <- FALSE
path_spi <- file.path(dir_proj, 'spi_538.csv')

params <-
  crossing(
    league = c('EPL', 'La_liga', 'Bundesliga', 'Serie_A', 'Ligue_1'),
    season = c(2017L:2020L)
  )
params
options(readr.num_columns = 0)

# .display_info <- function (x, ..., .envir = parent.frame())  {
#   x <- glue::glue_collapse(x, '\n')
#   x <- glue::glue(x, .envir = .envir)
#   cli::cat_line(x)
# }

.display_info_early <- function(x, ..., .envir = parent.frame()) {
  x <- glue::glue_collapse(x, '\n')
  x <- glue::glue(x, .envir = .envir)
  cli::cat_line(sprintf('%s: Returning early%s.', Sys.time(), x))
}

.display_info_after <- function(x, ..., .envir = parent.frame()) {
  x <- glue::glue_collapse(x, '\n')
  x <- glue::glue(x, .envir = .envir)
  cli::cat_line(sprintf('%s: Returning data%s.', Sys.time(), x))
}

# Reference: https://gist.github.com/Torvaney/42cd82addb3ba2c4f33ec3247e66889c
extract_json <- function(html, varname) {
  # Extract a JSON variable from the raw html
  html %>% 
    rvest::html_nodes('script') %>% 
    rvest::html_text(trim = TRUE) %>% 
    keep(str_detect, varname) %>% 
    str_extract("(?<=JSON.parse\\(').*(?='\\))") %>% 
    stringi::stri_unescape_unicode() %>% 
    jsonlite::fromJSON()
}

unnest_df <- function(df, name) {
  # Hack to convert list/nested dfs to a single df while keeping sane column 
  # names
  if (!is.data.frame(df)) {
    return(tibble(!!name := df))
  }
  
  tbl <- as_tibble(df)
  colnames(tbl) <- str_c(name, '_', colnames(tbl))
  
  tbl
}

fetch_matches <- function(league = 'EPL', season = 2020) {
  # Fetch a dataframe of matches for an individual league/season's page
  # league_url %>% 
  sprintf('https://understat.com/league/%s/%s', toupper(league), season) %>% 
    xml2::read_html() %>% 
    extract_json('datesData') %>% 
    imap_dfc(unnest_df) %>% 
    rename(match_id = id) %>% 
    type_convert()
}

get_matches <- function(league = 'EPL', season = 2020, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('matches-%s-%s.rds', league, season)) 
  suffix <- glue::glue(' for `league = "{league}"`, `season = {season}`')
  if(file.exists(path) & !overwrite) {
    .display_info_early('{suffix}')
    return(read_rds(path))
  }
  res <- fetch_matches(league, season)
  .display_info_after('{suffix}')
  write_rds(res, path)
  res
}

matches <-
  params %>% 
  mutate(data = map2(league, season, get_matches)) %>% 
  unnest(data)
matches
write_rds(matches, path_matches)
matches %>% filter(league == 'EPL') %>% count(season)

get_shots <- function(match_id, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('%s.rds', match_id))
  suffix <- glue::glue(' for `match_id = {match_id}`')
  if(file.exists(path) & !overwrite) {
    .display_info_early('{suffix}')
    return(read_rds(path))
  }
  res <- understatr::get_match_shots(match_id)
  .display_info_after('{suffix}')
  write_rds(res, path)
  res
}

get_shots_slowly <- slowly(possibly(get_shots, tibble()))

match_ids <- matches %>% distinct(season, league, match_id)
get_paths_match <- function() {
  fs::dir_ls(dir_data, regexp = '[0-9]{5}[.]rds$')
}

match_ids_existing <- 
  get_paths_match() %>% 
  basename() %>%
  tools::file_path_sans_ext() %>% 
  as.integer() %>% 
  tibble(match_id = .)
match_ids_existing

match_ids %>% 
  anti_join(match_ids_existing) %>% 
  count(league, season)

shots_nested <-
  match_ids %>% 
  anti_join(match_ids_existing) %>% 
  filter(!(league == 'Ligue_1' & season == 2019)) %>% 
  select(league, season, match_id) %>% 
  mutate(data = map(match_id, get_shots_slowly))
shots_nested

shots <-
  get_paths_match() %>% 
  map_dfr(read_rds)
shots
# beepr::beep(3)
write_rds(shots, path_shots)

get_leagues_meta <- function(overwrite = FALSE) {
  path <- file.path(dir_data, 'leagues_meta.rds')
  suffix <- ''
  if(file.exists(path) & !overwrite) {
    .display_info_early('{suffix}')
    return(read_rds(path))
  }
  res <- understatr::get_leagues_meta()
  .display_info_after('{suffix}')
  write_rds(res, path)
  res
}
leagues_meta <- get_leagues_meta()

get_league_teams_stats <- function(league_name, year, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('league_teams_stats-%s-%s.rds', league_name, year))
  suffix <- glue::glue(' for league_name = "{league_name}"`, `year = {year}`')
  if(file.exists(path) & !overwrite) {
    .display_info_early('{suffix}')
    return(read_rds(path))
  }
  res <- understatr::get_league_teams_stats(league_name, year)
  .display_info_after('{suffix}')
  write_rds(res, path)
  res
}

league_teams_stats <-
  leagues_meta %>% 
  mutate(data = map2(league_name, year, get_league_teams_stats)) %>% 
  select(data) %>% 
  unnest(data)
league_teams_stats
write_rds(league_teams_stats, path_league_teams_stats)

teams <- league_teams_stats %>% distinct(league_name, year, team_name)
teams

get_team_players_stats <- function(team_name, year, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('team_players_stats-%s-%s.rds', team_name, year))
  suffix <- glue::glue(' for team_name = "{team_name}"`, `year = {year}`')
  if(file.exists(path) & !overwrite) {
    .display_info_early('{suffix}')
    return(read_rds(path))
  }
  res <- understatr::get_team_players_stats(team_name, year)
  .display_info_after('{suffix}')
  write_rds(res, path)
  res
}

team_players_stats <-
  teams %>% 
  mutate(
    data = map2(team_name, year, get_team_players_stats)
  ) %>% 
  select(-c(team_name, year)) %>% 
  unnest(data)
team_players_stats
write_rds(team_players_stats, path_teams_players_stats)

read_csv('https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv') %>% 
  write_csv(path_spi, na = '')
