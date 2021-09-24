
library(tidyverse)
library(worldfootballR)
# pak::pak('JaseZiv/worldfootballR')


team_urls <- fb_teams_urls('https://fbref.com/en/comps/9/Premier-League-Stats')
player_urls <- team_urls %>% map_dfr(fb_player_urls) %>% as_tibble() %>% janitor::clean_names(

team_results <- team_urls %>% get_team_match_results()
team_results %>% get_season_team_stats()

# function to extract season teams stats
mp <-
  get_season_team_stats(
    country = 'ENG',
    gender = 'M',
    season_end_year = '2021',
    tier = '1st',
    stat_type = 'playing_time'
  )
mp %>% as_tibble() %>% janitor::clean_names()
mp


# understatr ----
library(understatr)

dir_proj <- '42-202122_new_players'
dir_data <- file.path(dir_proj, 'data')
fs::dir_create(dir_data)

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

# Issue filed: https://github.com/ewenme/understatr/issues/14
do_get_leagues_meta <- function(overwrite = FALSE) {
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
leagues_meta <- do_get_leagues_meta()
leagues_meta


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

do_get_league_teams_stats <- function(league_name, year, overwrite = FALSE) {
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

do_get_league_teams_stats_safely <- possibly(do_get_league_teams_stats, otherwise = tibble(), quiet = FALSE)

league_teams_stats <-
  leagues_meta %>% 
  filter(league_name == 'EPL' & year >= 2020) %>% 
  mutate(data = map2(league_name, year, do_get_league_teams_stats_safely)) %>% 
  select(data) %>% 
  unnest(data)
league_teams_stats

teams <- league_teams_stats %>% distinct(league_name, year, team_name)
teams

do_get_team_players_stats <- function(team_name, year, overwrite = FALSE) {
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
    data = map2(team_name, year, do_get_team_players_stats)
  ) %>% 
  select(-c(team_name, year)) %>% 
  unnest(data)
team_players_stats
path_teams_players_stats <- file.path(dir_proj, 'teams_players_stats.rds')
write_rds(team_players_stats, path_teams_players_stats)

