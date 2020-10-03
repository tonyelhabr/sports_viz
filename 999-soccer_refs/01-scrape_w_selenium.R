
library(tidyverse)
library(RSelenium)

# system('docker run -d -p 4445:4444 selenium/standalon-firefox:2.53.0')
# t <- system('docker ps', intern = TRUE)
# t[2]
url_base <- 'https://www.whoscored.com'
dr <-
  remoteDriver(
    remoteServerAddr = '192.168.99.100',
    port = 4445L
  )
dr$open()

url_epl <- glue::glue('{url_base}/Regions/252/Tournaments/2/England-Premier-League')

html <- xml2::read_html(dr$getPageSource()[[1]])
html
node_seasons <-
  html %>% 
  rvest::html_nodes('body') %>% 
  rvest::html_nodes(xpath = '//*[@id="seasons"]') %>% 
  rvest::html_children()

links_seasons <- node_seasons %>% rvest::html_attr('value')
text_seasons <- node_seasons %>% rvest::html_text()
pages_seasons <- links_seasons %>% set_names(text_seasons)
pages_seasons

dr$navigate('https://www.whoscored.com/Regions/252/Tournaments/2/Seasons/7811/England-Premier-League')
dr$getTitle()
dr$navigate('https://www.whoscored.com/Regions/252/Tournaments/2/Seasons/7811/Stages/17590/RefereeStatistics/England-Premier-League-2019-2020')
dr$screenshot(display = TRUE)
dr$navigate('https://www.whoscored.com/RefereesFeed/17590/TournamentStatsByReferee?fieldString=Overall')
dr$screenshot(display = TRUE)
html <- xml2::read_html(dr$getPageSource()[[1]])
html
text <- html %>% rvest::html_nodes('body') %>% rvest::html_text() # %>% jsonlite::read_json()
.dir_proj <- '999-soccer_refs'
.dir_data_raw <- fs::path(.dir_proj, 'data-raw')
fs::dir_create(.dir_data_raw)
text %>% jsonlite::write_json(fs::path(.dir_data_raw, 'temp.json'))
text %>% write_lines(fs::path(.dir_data_raw, 'temp.json'))
json <- jsonlite::read_json(dr$getPageSource()[[1]])
json
xml2::write_html(html, "india.html"