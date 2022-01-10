
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

# fs::dir_ls(dir_data, regexp = '2020') %>% fs::file_delete()

params <-
  crossing(
    league = c('La_liga', 'EPL', 'Bundesliga', 'Serie_A', 'Ligue_1'),
    season = c(2014L:2021L)
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

fetch_matches <- function(league = 'EPL', season = 2021) {
  # Fetch a dataframe of matches for an individual league/season's page
  # league_url %>% 
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

matches <-
  params %>% 
  mutate(data = map2(league, season, get_matches)) %>% 
  unnest(data)
matches
write_rds(matches, path_matches)
# matches %>% filter(league == 'EPL') %>% count(season)

get_shots <- function(match_id, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('%s.rds', match_id))
  suffix <- glue::glue(' for `match_id = {match_id}`')
  if(file.exists(path) & !overwrite) {
    .display_info_early('{suffix}')
    return(read_rds(path))
  }
  Sys.sleep(0.5)
  res <- understatr::get_match_shots(match_id)
  .display_info_after('{suffix}')
  write_rds(res, path)
  res
}

get_shots_slowly <- possibly(get_shots, tibble())

match_ids <- matches %>% distinct(season, league, match_id)
get_paths_match <- function() {
  fs::dir_ls(dir_data, regexp = '[0-9]{1-5}[.]rds$')
}

match_ids_existing <- get_paths_match() %>% 
  basename() %>%
  tools::file_path_sans_ext() %>% 
  as.integer() %>% 
  tibble(match_id = .)
match_ids_existing

match_ids %>% 
  anti_join(match_ids_existing) %>% 
  count(league, season)

shots_nested <- match_ids %>% 
  anti_join(match_ids_existing) %>% 
  filter(!(league == 'Ligue_1' & season == 2019)) %>% 
  select(league, season, match_id) %>% 
  mutate(data = map(match_id, get_shots_slowly))
shots_nested

match_ids_vec <- match_ids %>% 
  pull(match_id) 

shots <- get_paths_match() %>% 
  # file.path(dir_proj, 'data', sprintf('%s.rds', match_ids_vec)) %>% 
  map(~read_rds(.x))
# match_ids_existing %>% anti_join(shots %>% distinct(match_id))

# beepr::beep(3)
write_rds(shots, path_shots)

# Issue filed: https://github.com/ewenme/understatr/issues/14
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

.get_league_teams_stats <- function(league_name, year) {
  
  stopifnot(is.character(league_name))
  
  home_url <- "https://understat.com"
  # construct league url
  league_url <- URLencode(str_glue("{home_url}/league/{league_name}/{year}"))
  
  # read league page
  league_page <- rvest::read_html(league_url)
  
  # locate script tags
  teams_data <- understatr:::get_script(league_page)
  
  # isolate player data
  teams_data <- understatr:::get_data_element(teams_data, "teamsData")
  
  # pick out JSON string
  teams_data <- sub(".*?\\'(.*)\\'.*", "\\1", teams_data)
  
  # parse JSON
  teams_data <- jsonlite::fromJSON(teams_data, simplifyDataFrame = TRUE,
                         flatten = TRUE)
  
  # get teams data
  teams_data <- lapply(
    teams_data, function(x) {
      df <- x$history
      df$team_id <- x$id
      df$team_name <- x$title
      df
    })
  
  # convert to df
  teams_df <- do.call("rbind", teams_data)
  
  # add reference fields
  teams_df$league_name <- league_name
  teams_df$year <- as.numeric(year)
  
  # fix col classes
  teams_df$date <- as.Date(teams_df$date, "%Y-%m-%d")
  
  as_tibble(teams_df)
  
}

get_league_teams_stats <- function(league_name, year, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('league_teams_stats-%s-%s.rds', league_name, year))
  suffix <- glue::glue(' for league_name = "{league_name}"`, `year = {year}`')
  if(file.exists(path) & !overwrite) {
    .display_info_early('{suffix}')
    return(read_rds(path))
  }
  res <- .get_league_teams_stats(league_name, year)
  .display_info_after('{suffix}')
  write_rds(res, path)
  res
}

get_league_teams_stats_safely <- possibly(get_league_teams_stats, otherwise = tibble(), quiet = FALSE)

league_teams_stats <- leagues_meta %>% 
  mutate(data = map2(league_name, year, get_league_teams_stats_safely)) %>% 
  select(data) %>% 
  unnest(data)
league_teams_stats
# fs::dir_ls(dir_data, regexp = '[-]2020[.]rds')
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
