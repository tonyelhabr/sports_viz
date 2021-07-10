# table.png from https://www.zeileis.org/news/euro2020paper/
library(tidyverse)
dir_proj <- '34-2020_euros_review'
txt <-
  file.path(dir_proj, 'the-analyst.txt') %>% 
  read_lines() %>% 
  # tibble(line = .) %>% 
  str_split('\t') %>% 
  unlist() %>% 
  tibble(line = .)
txt
nms <- txt %>% slice(c(2:10)) %>% mutate(idx = row_number()) %>% rename(round = line)
nms

df <-
  txt %>% 
  slice(c(12:n())) %>% 
  mutate(
    country = line %>% str_remove_all('[0-9.%]') %>% str_trim() %>% na_if(''),
    value = line %>% str_remove_all('[A-Z\\s]') %>% str_remove('[%]') %>% parse_number() %>% {. / 100}
  ) %>% 
  fill(country) %>% 
  drop_na(value) %>% 
  select(country, value) %>% 
  group_by(country) %>% 
  mutate(idx = row_number()) %>% 
  ungroup() %>% 
  # left_join(nms) %>% 
  filter(idx %in% c(5L:9L)) %>% 
  left_join(
    tibble(
      idx = 5L:9L,
      round = c('r16', 'qf', 'sf', 'finals', 'champ') # c('R16', 'Quarters', 'Semis', 'Final', 'Champ')
    )
  ) %>% 
  select(-idx) %>% 
  mutate(
    # across(idx, ~.x - 4L),
    across(country, stringr::str_to_title)
  )
df
df %>% write_csv(file.path(dir_proj, 'the-analyst.csv'))
df %>% count(country)
