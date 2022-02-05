
library(tidyverse)
library(cli)
library(glue)
library(rvest)
library(stringi)
library(jsonlite)
library(xml2)

dir_proj <- '48-202122_game_state'
dir_data <- file.path(dir_proj, 'data')
dir.create(dir_data, showWarnings = FALSE)
path_matches <- file.path(dir_proj, 'matches.rds')
path_shots <- file.path(dir_proj, 'shots.rds')
path_meta <- file.path(dir_proj, 'meta.rds')
path_league_teams_stats <- file.path(dir_proj, 'league_teams_stats.rds')
path_team_players_stats <- file.path(dir_proj, 'team_players_stats.rds')

params <- crossing(,
    league = 'EPL',
    season = 2021L:2021L
  )
params

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
  html %>% 
    rvest::html_nodes('script') %>% 
    rvest::html_text(trim = TRUE) %>% 
    keep(str_detect, varname) %>% 
    str_extract("(?<=JSON.parse\\(').*(?='\\))") %>% 
    stringi::stri_unescape_unicode() %>% 
    jsonlite::fromJSON()
}

unnest_df <- function(df, name) {
  if (!is.data.frame(df)) {
    return(tibble(!!name := df))
  }
  tbl <- as_tibble(df)
  colnames(tbl) <- str_c(name, '_', colnames(tbl))
  tbl
}

fetch_matches <- function(league = 'EPL', season = 2021) {
  sprintf('https://understat.com/league/%s/%s', toupper(league), season) %>% 
    xml2::read_html() %>% 
    extract_json('datesData') %>% 
    imap_dfc(unnest_df) %>% 
    rename(match_id = id) %>% 
    type_convert()
}

get_matches <- function(league = 'EPL', season = 2021, overwrite = FALSE) {
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

matches <- params %>% 
  mutate(data = map2(league, season, get_matches, overwrite = TRUE)) %>% 
  unnest(data)
matches
write_rds(matches, path_matches)

get_shots <- function(match_id, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('%s.rds', match_id))
  suffix <- glue::glue(' for `match_id = {match_id}`')
  if(file.exists(path) & !overwrite) {
    .display_info_early('{suffix}')
    return(read_rds(path))
  }
  .display_info_after('{suffix}')
  res <- understatr::get_match_shots(match_id)
  write_rds(res, path)
  res
}

possibly_get_shots <- possibly(get_shots, otherwise = tibble(), quiet = FALSE)

match_ids <- matches %>% 
  # filter(datetime < Sys.Date()) %>% 
  drop_na(goals_h, goals_a) %>% ## unplayed games
  distinct(season, league, match_id)

shots_nested <- match_ids %>% 
  arrange(league, season, match_id) %>% 
  select(league, season, match_id) %>% 
  mutate(data = map(match_id, possibly_get_shots))

shots <- shots_nested %>% 
  select(data) %>%  
  unnest(data)

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
  res <- understat::get_league_teams_stats(league_name, year)
  .display_info_after('{suffix}')
  write_rds(res, path)
  res
}

get_league_teams_stats_safely <- possibly(get_league_teams_stats, otherwise = tibble(), quiet = FALSE)

league_teams_stats <- leagues_meta %>% 
  semi_join(params %>% rename(league_name = league, year = season)) %>% 
  mutate(data = map2(league_name, year, get_league_teams_stats_safely)) %>% 
  select(data) %>% 
  unnest(data)
league_teams_stats
write_rds(league_teams_stats, path_league_teams_stats)

teams <- league_teams_stats %>% distinct(league_name, year, team_name)
teams

get_team_players_stats <- function(team_name, year, overwrite = TRUE) {
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

team_players_stats <- teams %>% 
  mutate(
    data = map2(team_name, year, get_team_players_stats)
  ) %>% 
  select(-c(team_name, year)) %>% 
  unnest(data)
team_players_stats %>% count(year)
write_rds(team_players_stats, path_team_players_stats)
