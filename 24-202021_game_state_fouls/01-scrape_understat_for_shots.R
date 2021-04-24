
library(tidyverse)
dir_proj <- '24-202021_game_state_fouls'
dir_data <- file.path(dir_proj, 'data')
fs::dir_create(dir_data)
path_matches <- file.path(dir_proj, 'matches.rds')
path_shots <- file.path(dir_proj, 'shots.rds')
overwrite <- FALSE

params <-
  crossing(
    league = c('EPL', 'La_liga', 'Bundesliga', 'Serie_A', 'Ligue_1'),
    season = c(2019, 2020)
  )
options(readr.num_columns = 0)
if(!file.exists(path_shots) & overwrite) {
  if(!file.exists(path_matches) & overwrite) {
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
      if(file.exists(path) & !overwrite) {
        cat(glue::glue('{Sys.time()}: Returning early for `league = {league}`, `season = {season}`'), sep = '\n')
        return(read_rds(path))
      }
      res <- fetch_matches(league, season)
      cat(glue::glue('{Sys.time()}: Retrieved data for `league = {league}`, `season = {season}`'), sep = '\n')
      write_rds(res, path)
      res
    }

    matches <-
      params %>% 
      mutate(data = map2(league, season, get_maches)) %>% 
      unnest(data)
    matches
    
    write_rds(matches, path_matches)
  } else {
    matches <- path_matches %>% read_rds()
  }
  
  get_shots <- function(match_id, overwrite = FALSE) {
    path <- file.path(dir_data, sprintf('%s.rds', match_id))
    if(file.exists(path) & !overwrite) {
      cat(glue::glue('{Sys.time()}: Returning early for `match_id = {match_id}`'), sep = '\n')
      return(read_rds(path))
    }
    res <- understatr::get_match_shots(match_id)
    cat(glue::glue('{Sys.time()}: Retrieved data for `match_id = {match_id}`'), sep = '\n')
    write_rds(res, path)
    res
  }
  get_shots_slowly <- slowly(possibly(get_shots, tibble()))
  
  shots_nested <-
    matches %>% 
    select(league, season, match_id) %>% 
    mutate(data = map(match_id, get_shots_slowly))
  shots_nested
  
  shots <-
    shots_nested %>% 
    select(-c(season, match_id)) %>% 
    unnest(data)
  shots
  beepr::beep(3)
  write_rds(shots, path_shots)
} else {
  shots <- path_shots %>% read_rds()
}
