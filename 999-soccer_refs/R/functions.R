
library(tidyverse)
library(RSelenium)

.dir_data_raw <- fs::path('data-raw')
.dir_data <- fs::path('data')
fs::dir_create(.dir_data)
# shell('docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0')
# state <- shell('docker ps', intern = TRUE)
# state[2]
path_pages_seasons <- fs::path('data-raw', 'links_seasons.csv')
.url_base <- 'https://www.whoscored.com'

.initialize_driver <- function(address = '192.168.99.100', port = 4445L) {
  dr <-
    remoteDriver(
      browserName = 'chrome',
      remoteServerAddr = address,
      port = port
    )
  dr$open()
  dr$navigate(.url_base)
  dr$screenshot(display = TRUE)
  dr
}

.get_league_info <- # memoise::memoise({
  function() {
    tribble(
      ~league, ~league_url, ~region, ~tournament, ~recent_season,
      'EPL', 'England-Premier-League', 252L, 2L, 8228L,
      'La Liga', 'Spain-La-Liga', 206L, 4L, 8321L,
      'Bundesliga', 'Germany-Bundesliga', 81L, 3L, 8279L,
      'Seria A', 'Italy-Serie-A', 108L, 5L, 8330L,
      'Ligue 1', 'France-Ligue-1', 74L, 22L, 8185L
    )
  }
# })

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
    # league = 'EPL'
    .validate_league(league)
    path_exists <- path %>% fs::file_exists()
    if (path_exists & !overwrite) {
      res <- path %>% read_csv()
      return(res)
    }
    f <- partial(.pull_league_info, league = league, ... = )
    league_url <- f('league_url')
    region <- f('region')
    tournament <- f('tournament')
    # Every league other than the Premier League failed if the season part of the url wasn't included. I figured it would just default to the most recent season (not included in the url, as it is when you actually go to the page in the browser.
    # Also, couldn't figure out a programmatic way to identify the most recent season for each league from the home page, so just hard coded it in `.get_league_info()`.
    recent_season <- f('recent_season')
    url <- glue::glue('{.url_base}/Regions/{region}/Tournaments/{tournament}/Seasons/{recent_season}/{league_url}')

    dr$navigate(url)
    # dr$screenshot(display = TRUE)
    # Sys.sleep(3)
    src <- dr$getPageSource()[[1]]
    html <- src %>% xml2::read_html()
    node <-
      html %>% 
      rvest::html_nodes('body') %>% 
      rvest::html_nodes(xpath = '//*[@id="seasons"]') %>% 
      rvest::html_children()
    
    links <- node %>% rvest::html_attr('value')
    text <- node %>% rvest::html_text()
    res <- links %>% set_names(text) %>% enframe('season', 'link')
    write_csv(res, path)
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

.scrape_season_json <-
  function(dr,
           url_parent,
           stage,
           suffix = .get_suffixes_valid(),
           league = 'EPL',
           season,
           dir = .dir_data,
           path = fs::path(dir, glue::glue('{league}_{season}_{stage}_{suffix}.json')),
           overwrite = FALSE) {
    # browser()
    path_exists <- path %>% fs::file_exists()
    if (path_exists & !overwrite) {
      res <- path %>% .import_json()
      return(res)
    }
    # Have to refresh this every time.
    dr$navigate(url_parent)
    # browser()
    url <-
      glue::glue(
        'https://www.whoscored.com/RefereesFeed/{stage}/TournamentStatsByReferee?fieldString={suffix}'
      )
    cat(cli::cli_text(glue::glue('Scraping {url} (for {season}).')))
    dr$navigate(url)
    # dr$screenshot(display = TRUE)
    # browser()
    src <- dr$getPageSource()[[1]]
    html <- src %>% xml2::read_html()
    body <- html %>% rvest::html_nodes('body')
    text <- body %>% rvest::html_text()
    if(text == '"\n    \n        The page you requested does not exist in WhoScored.com\n    \n\n\n"') {
      cat(cli::cli_alert_warning(glue::glue('WARNING: Could not load the page at {url} (for {season}).')))
      return(tibble())
    }
    text %>% write_lines(path)
    # Need to import with the custom function. Can't just return `text`.
    res <- path %>% .import_json()
    res
  }

.scrape_season <- function(dr, league = .get_leagues_valid(), link, suffix = .get_suffixes_valid(), ...) {
  .validate_league(league)
  .validate_suffix(suffix)
  # season <- seasons_links$season[1]
  # link <- rev(seasons_links$link)[1]
  url <- glue::glue('{.url_base}{link}')
  # url_season <- glue::glue('{.url_base}/Regions/252/Tournaments')
  dr$navigate(url)
  # dr$screenshot(display = TRUE)
  # html <- xml2::read_html(dr$getPageSource()[[1]])
  # text <- html %>% rvest::html_nodes('body') %>% rvest::html_text()
  src <- dr$getPageSource()[[1]]
  html <- src %>% xml2::read_html()
  link_ref <- html %>% rvest::html_node(xpath = '//*[@id="sub-navigation"]/ul/li[5]/a') %>% rvest::html_attr('href')
  url_ref <- glue::glue('{.url_base}{link_ref}')
  dr$navigate(url_ref)
  # dr$screenshot(display = TRUE)
  rgx <- '(^.*Stages)\\/([0-9]+)(\\/.*)([0-9]{4}-[0-4]{4}$)'
  stage <- url_ref %>% str_replace_all(rgx, '\\2')
  season <- url_ref %>% str_replace_all(rgx, '\\4')
  # assertthat::are_equal(season_num, season)
  # url_api <- glue::glue('{.url_base}/RefereesFeed/{sub_num}/18685/RefereeStatistics/England-Premier-League-{season_num}')
  # dr$navigate(url_ref)
  # dr$screenshot(display = TRUE)
  # suffixes <- .get_suffixes_valid()
  # suffixes <- 'Home'
  res <-
    .scrape_season_json(
      dr,
      url_parent = url_ref,
      stage = stage,
      suffix = suffix,
      league = league,
      season = season,
      ...
    )
  res
  
}

scrape_season_possibly <- possibly(.scrape_season, otherwise = tibble())

