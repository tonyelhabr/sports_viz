
library(tidyverse)
df_raw <- 'https://raw.githubusercontent.com/mrcaseb/nfl-data/master/data/nfl_lines_odds.csv' %>% read_csv()
df_raw
df_raw %>% skimr::skim()
df_raw %>% filter(!is.na(opening_lines)) %>% tail()

str_replace_game_id <- function(x, i) {
  str_replace(x, '([0-9]{4})_([0-9]{2})_([A-Z]+)_([A-Z]+)$', sprintf('\\%s', i))
}
spreads <-
  df_raw %>%
  filter(!is.na(opening_lines), market_type == 'spread') %>%
  mutate(across(
    game_id,
    list(
      yr = ~ str_replace_game_id(.x, 1) %>% as.integer(),
      wk = ~ str_replace_game_id(.x, 2) %>% as.integer(),
      tm_away = ~ str_replace_game_id(.x, 3),
      tm_home = ~ str_replace_game_id(.x, 4)
    ),
    .names = '{fn}'
  ),
  line_change = opening_lines - lines) %>%
  mutate(across(line_change, list(abs = abs))) %>%
  select(
    game_id,
    yr,
    wk,
    tm_home,
    tm_away,
    tm_fav = abbr,
    book,
    line_close = lines,
    line_open = opening_lines,
    line_change,
    line_change_abs,
    odds_close = odds,
    odds_open = opening_odds,
  ) %>% 
  arrange(yr, wk)
spreads
spreads_one <- spreads %>% filter(tm_home == tm_fav) #  | tm_away == tm_fav)
spreads_one
spreads %>% count(yr)
spreads %>% filter(is.na(yr))
n_game <- 256 + 11
yrs_valid <- 
  spreads %>% 
  group_by(game_id) %>% 
  filter(row_number() == 1L) %>% 
  ungroup() %>% 
  count(yr) %>% 
  filter(n >= !!n_game) %>% 
  select(-n)
yrs_valid

books_valid <-
  spreads %>% 
  inner_join(yrs_valid) %>% 
  group_by(game_id, book) %>% 
  filter(row_number() == 1L) %>% 
  ungroup() %>% 
  count(yr, book) %>% 
  filter(n >= !!n_game) %>% 
  select(-n) %>% 
  count(book, sort = T) %>% 
  filter(n == nrow(yrs_valid)) %>% 
  select(-n)
books_valid
spreads %>% filter(game_id %>% str_detect('2019_08_GB_KC'))
spreads %>% filter(line_change_abs > 10) %>% count(book, sort = T) %>% inner_join(books_valid)
spreads %>% inner_join(books_valid) %>% filter(line_change_abs > 10)
spreads %>% count(season, book, sort = T)
spreads %>% arrange(desc(line_change_abs)) %>% filter(line_change_abs > 14) -> z
spreads %>% 
  filter(line_change_abs <= 10) %>% 
  ggplot() +
  aes(x = line_change) +
  geom_histogram(binwidth = 1)


spreads_aug <-
  spreads %>% 
  filter(line_change_abs > 10) %>% 
  transmute(
    across(
      game_id, 
      list(
        season = ~str_replace_game_id(.x, 1),
        wk = ~str_replace_game_id(.x, 2),
        tm_home = ~str_replace_game_id(.x, 3),
        tm_away = ~str_replace_game_id(.x, 4)
      )
    )
  )
spreads_aug