
library(tidyverse)

# library(xml2)
library(rvest)
library(polite)
# body > div.wrapper > div.page-wrapper > div:nth-child(3) > div.block-content > div > script

# https://jsonformatter.curiousconcept.com/
path_shots_node <- here::here('data-raw', 'match12021.json')
shots_node <- 
  path_shots_node %>% 
  read_lines() %>% 
  # str_replace_all('\\\\x22', ' ') %>% 
  # str_remove_all('\\\\x[0-9A-F]{2}')
  # str_remove_all('\\\\x') %>% 
  str_replace_all(c(
    '\\\\x20' = ' ',
    '\\\\x22' = '"',
    '\\\\x3A' = ':',
    '\\\\x7B' = '{',
    '\\\\x7D' = '}',
    '\\\\x2D' = '-',
    '\\\\x5B' = '[',
    '\\\\x5D' = ']'
  ))
# temp <- tempfile()
# write_lines(shots_node, temp)
# shots_node %>% clipr::write_clip()
shots_json <-
  # temp %>% 
  shots_node %>% 
  # str_replace_all('\\,([A-Za-z_0-9]+)[:]', ',"\\1":')
  # jsonify::as.json()
  # rjson::toJSON()
  jsonlite::toJSON()
shots_json

shots_parsed <- shots_json %>% jsonlite::fromJSON(simplifyVector = TRUE)

shots <- 'data-raw/match12021_pretty.json' %>% jsonlite::read_json(simplifyDataFrame = TRUE)

shots_unnest <-
  shots %>% 
  enframe('side', 'data') %>% mutate(data = map(data, as_tibble)) %>% 
  unnest(data)
shots_unnest
shots_parsed
jsonlite::flatten(shots_parsed)

library(jsonlite)
jsoncars <- toJSON(mtcars, pretty=TRUE)
cat(jsoncars)

# Parse it back
fromJSON(jsoncars)

session <- 'https://understat.com/match/12021' %>% polite::bow()
session

page <- session %>% polite::scrape()
page

nodes <- page %>% rvest::html_nodes('script')
nodes

nodes %>% pluck(4)

shots_raw <- nodes %>% pluck(2)
shots_raw

nodes[[2]] %>% rvest::script

seq(1, nchar(s), by = 2)


