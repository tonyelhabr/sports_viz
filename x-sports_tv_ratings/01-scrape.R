
library(tidyverse)
library(rvest)
url <- 'https://en.wikipedia.org/wiki/NBA_Finals_television_ratings'
df_nba <-
  url %>% 
  read_html() %>% 
  html_table() %>% 
  pluck(3)
df_nba

clenan
df2 <-
  df_nba %>% 
  janitor::clean_names() %>% 
  select(year, avg, matches('game_')) %>% 
  mutate(
    across(
      avg,
      list(
        share = ~str_replace_all(.x, '(^.*)\\(.*$', '\\1'),
        viewers = ~str_replace_all(.x, '\\((.*)Viewers\\)', '\\1') %>% parse_number()
      )
    )
  ) %>% 
  pivot_longer(-c(year, avg), names_to = 'game', values_to = 'value') %>% 
  mutate(
    across(
      value,,
      ~str_replace_all(.x, '\\((.*)Viewers\\)', '\\1') %>% parse_number()
    )
  )
df2
gsub('[\\(\\)]', '', regmatches(df2$value, gregexpr('\\(.*?\\)', df2$value))) %>% parse_number()

regmatches
url_nfl <-
  'https://en.wikipedia.org/wiki/Super_Bowl_television_ratings'
df_nfl <-
  url_nfl %>%
  read_html() %>%
  html_table(header = FALSE) %>%
  pluck(1) %>%
  slice(3:n()) %>%
  set_names(
    c(
      'i',
      'date',
      'network',
      'viewers_avg',
      'viewers_total',
      'household_rating',
      'household_share',
      'x18to49_rating',
      'x18to49_share',
      'cost_30s_ad_avg'
    )
  ) %>% 
  mutate(
    across(matches('^(viewers|household|cost)'), ~ str_remove_all(.x, '\\[.*$')),
    across(cost_30s_ad_avg, ~str_remove_all(.x, '\\$|\\,')),
    across(matches('(avg|total|rating)$'), as.double),
    across(matches('share$'), as.integer),
    across(date, ~ parse_date(.x, '%B %d, %Y'))
  )
df_nfl
df_nfl %>% mutate(across(date, readr::parse_date))
parse_date('January 15, 1967', format = '%B %d, %Y')

df_nfl %>% slice(2:n())
df_nfl %>% names()
