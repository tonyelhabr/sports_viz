
if(!file.exists(path_shots) & overwrite) {
  if(!file.exists(path_matches) & overwrite) {
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
    
    matches <- fetch_matches(season = season)
    write_rds(matches, path_matches)
  } else {
    matches <- path_matches %>% read_rds()
  }
  
  stats <-
    matches %>%
    filter(isResult) %>% 
    pull(match_id) %>% 
    .[[1]] %>% 
    map_dfr(slowly(possibly(understatr::get_match_stats, tibble())))
  stats
  
  shots <-
    matches %>%
    filter(isResult) %>% 
    pull(match_id) %>% 
    map_dfr(slowly(possibly(understatr::get_match_shots, tibble())))
  
  write_rds(shots, path_shots)
} else {
  shots <- path_shots %>% read_rds()
}
