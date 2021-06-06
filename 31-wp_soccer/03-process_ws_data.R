
library(tidyverse)
dir_proj <- '31-wp_soccer'

leagues_mapping <-
  tibble(
    league = c('epl'),
    league_understat = c('EPL'),
    # league_whoscored = c('England-Premier-League'),
    league_538 = c('Barclays Premier League')
  )

df <- file.path(dir_proj, 'model_data.parquet') %>% arrow::read_parquet()
df %>% filter(is_shot)

shots <-
  events %>% 
  # filter(type != 'SavedShot') %>% 
  filter(season_id >= 2019L)
  filter(type %>% str_detect('Shot|Goal'))
shots

shots %>% filter(match_id == 1375927L) %>% arrange(time)
df %>% filter(match_id == 11643L) %>% filter(is_shot) %>% arrange(minute)
# events %>% count(type, sort = TRUE) -> x
# events %>% filter(match_id == 1375927L) %>% filter(expanded_minute == 6L)

subs <-
  events %>% 
  filter(type %>% str_detect('Substitution'))
subs

cards <-
  events %>%
  filter(type == 'Card')
cards

cards_agg <-
  cards %>% 
  group_by(match_id, player) %>% 
  summarize(n = n(), time = max(time)) %>% 
  ungroup()
cards_agg
