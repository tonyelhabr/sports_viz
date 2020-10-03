
library(tidyverse)
library(RSelenium)

.dir_proj <- '999-soccer_refs'
.dir_data_raw <- fs::path(.dir_proj, 'data-raw')
.dir_data <- fs::path(.dir_proj, 'data')
fs::dir_create(.dir_data)
# shell('docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0')
# state <- shell('docker ps', intern = TRUE)
# state[2]
path_pages_seasons <- fs::path('data-raw', 'links_seasons.csv')
.url_base <- 'https://www.whoscored.com'

.initialize_driver <- function(address = '192.168.99.100', port = 4445L) {
  dr <-
    remoteDriver(
      remoteServerAddr = address,
      port = port
    )
  dr$open()
  dr$navigate(.url_base)
  dr$screenshot(display = TRUE)
  dr
}

.get_league_info <- memoise::memoise({function() {
  tribble(
    ~league, ~league_long, ~region,
    'EPL', 'England-Premier-League', 252L
  )
}})

.get_leagues_valid <- function() {
  league_info <- .get_league_info()
  res <- league_info %>% pull(league)
  res
}

.validate_league <- function(x = .get_leagues_valid()) {
  match.arg(x)
}

.pull_league_info <- function(league, col, league_info = .get_league_info()) {
  col_sym <- sym(col)
  res <- league_info %>% filter(league == !!league) %>% pull(!!col_sym)
  res
}

.retrieve_seasons_links <-
  function(dr,
           league = .get_leagues_valid(),
           overwrite = FALSE,
           path = fs::path(.dir_data, glue::glue('{league}_season_links.csv'))) {
    league = 'EPL'
    .validate_league(league)
  path_exists <- path %>% fs::file_exists()
  if (path_exists & !overwrite) {
    res <- path %>% read_csv()
    return(res)
  }
  league_long <- .pull_league_info(league, 'league_long')
  region <- .pull_league_info(league, 'region')
  url <- glue::glue('{.url_base}/Regions/{region}/Tournaments/2/{league_long}')
  dr$navigate(url)
  dr$screenshot(display = TRUE)
  html <- dr$getPageSource()[[1]] %>% xml2::read_html()
  node <-
    html %>% 
    rvest::html_nodes('body') %>% 
    rvest::html_nodes(xpath = '//*[@id="seasons"]') %>% 
    rvest::html_children()
  
  links <- node %>% rvest::html_attr('value')
  text <- node %>% rvest::html_text()
  res <- links %>% set_names(text) %>% enframe('season', 'link')
  write_csv(res, path)
  # list(dr = dr, links = res)
  res
}

.get_suffixes_valid <- function() {
  c('Overall', 'Home')
}

.validate_suffix <- function(x = .get_suffixes_valid()) {
  match.arg(x)
}

.import_json <- function(path) {
  res <-
    path %>% 
    jsonlite::read_json(simplifyVector = TRUE) %>% 
    pluck(1) %>% 
    as_tibble() %>%
    janitor::clean_names()
  res
}

.scrape_json <-
  function(stage,
           suffix = .get_suffixes_valid(),
           league = 'EPL',
           season,
           dir = .dir_data,
           path = fs::path(dir, glue::glue('{league}_{season}_{stage}_{suffix}.json')),
           overwrite = FALSE) {
    path_exists <- path %>% fs::file_exists()
    if (path_exists & !overwrite) {
      res <- path %>% .import_json()
      return(res)
    }
    url <-
      glue::glue(
        'https://www.whoscored.com/RefereesFeed/{stage_num}/TournamentStatsByReferee?fieldString={suffix}'
      )
    dr$navigate(url)
    # dr$screenshot(display = TRUE)
    html <- dr$getPageSource()[[1]] %>% xml2::read_html()
    body <- html %>% rvest::html_nodes('body')
    text <- body %>% rvest::html_text()
    text %>% write_lines(path)
    # Need to import with the custom function. Can't just return `text`.
    res <- path %>% .import_json()
    res
  }

.scrape1 <- function(season, links) {
  # link <- links[1]
  url <- glue::glue('{.url_base}{link}')
  # url_season <- glue::glue('{.url_base}/Regions/252/Tournaments')
  dr$navigate(url)
  # html <- xml2::read_html(dr$getPageSource()[[1]])
  # text <- html %>% rvest::html_nodes('body') %>% rvest::html_text()
  src <- dr$getPageSource()[[1]]
  html <- src %>% xml2::read_html()
  link_ref <- html %>% rvest::html_node(xpath = '//*[@id="sub-navigation"]/ul/li[5]/a') %>% rvest::html_attr('href')
  url_ref <- glue::glue('{.url_base}{link_ref}')
  dr$navigate(url_ref)
  rgx <- '(^.*Stages)\\/([0-9]+)(\\/.*League-)([0-9-]+$)'
  stage <- url_ref %>% str_replace_all(rgx, '\\2')
  # season <- url_ref %>% str_replace_all(rgx, '\\4')
  # assertthat::are_equal(season_num, season)
  # url_api <- glue::glue('{.url_base}/RefereesFeed/{sub_num}/18685/RefereeStatistics/England-Premier-League-{season_num}')
  # dr$navigate(url_ref)
  # dr$screenshot(display = TRUE)
  suffixes <- .get_suffix_valid()
  res <-
    suffixes %>% 
    set_names(suffixes) %>% 
    map_dfr(
      ~.scrape_json(
        stage = stage,
        suffix = .x,
        league = !!league,
        season = !!season
      )
    )
  res
  
}
