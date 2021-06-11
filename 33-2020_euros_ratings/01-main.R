
library(tidyverse)

dir_proj <- '33-2020_euros_ratings'
f_read <- function(x) {
  file.path(dir_proj, sprintf('%s.csv', x)) %>% 
    read_csv()
}
leagues <- f_read('leagues') %>% rename(league_name = name)
teams <- f_read('teams')
players <- f_read('players') %>% mutate(across(dob, lubridate::date))

# TODO: Empirical bayes?
player_ratings <-
  f_read('player_ratings') %>% 
  mutate(v = 90 * (offensive_value + defensive_value) / minutes)
player_ratings

chosen_leagues <-
  tibble(
    league_name = c('Premier League', 'LaLiga', 'Ligue 1', '1. Bundesliga', 'Serie A', 'Primeira Liga'),
    country = c('England', 'Spain', 'France', 'Germany', 'Italy', 'Portugal')
  )

# i <- seq.int(18, by = 3, length.out = 6)
age_grps <-
  tibble(
    from = c(18L, 24L, 27L, 30L),
    to = c(24L, 27L, 30L, 36L)
  )
age_grps

df <-
  list(
    leagues %>% semi_join(chosen_leagues),
    teams,
    player_ratings,
    players %>% 
      drop_na(position, dob) %>% 
      select(player_id = id, player_name = name, position, dob) %>% 
      mutate(across(position, ~str_remove_all(.x, '\\(.*\\)') %>% str_remove_all('\\,.*$')))
  ) %>% 
  reduce(inner_join) %>%  
  filter(minutes > 1000) %>% 
  mutate(age = lubridate::time_length(lubridate::ymd(sprintf('%s-08-01', season)) - dob, 'year') %>% floor() %>% as.integer()) %>% 
  filter(age >= 18 & age <= 35)
df

df <-
  data.table::as.data.table(df %>% mutate(age2 = age, age3 = age))[
    data.table::as.data.table(age_grps), 
    on=.(age2 >= from, age3 < to)
  ] %>% 
  as_tibble() %>% 
  unite('age_grp', age2, age3, sep = '<=x<')
df
df %>% count(age_grp, sort = TRUE)

v_min <- 
  df %>% 
  drop_na(v) %>% 
  filter(!is.infinite(v)) %>% 
  summarize(`_` = min(v)) %>% 
  pull(`_`)
v_min

df <- df %>% mutate(across(v, ~log(.x + abs(!!v_min) + 0.001))) 

agg <-
  df %>% 
  group_by(league_id, league_name, country, season, position, age_grp) %>% 
  summarize(
    n = n(),
    across(
      v,
      list(mean = mean, sd = sd), na.rm = TRUE, .names = '{.fn}'
    )
  ) %>% 
  ungroup() %>% 
  filter(n > 1L)
agg

df <-
  df %>% 
  inner_join(agg) %>%
  mutate(z = (v - mean) / sd)
df

df %>% count(age_grp, sort = TRUE)


df %>% 
  mutate(across(age_grp, factor)) %>% 
  ggplot() +
  aes(x = z, color = age_grp) +
  geom_density() +
  coord_cartesian(xlim = c(-3, 3))
df

df %>% 
  ggplot() +
  aes(x = z) +
  geom_histogram(binwidth = 0.1) +
  coord_cartesian(xlim = c(-3, 3))

ids <-
  df %>% 
  distinct(player_id, league_id)
ids

ids_gt1 <-
  ids %>% 
  count(player_id, sort = TRUE) %>% 
  filter(n > 1L) %>% 
  select(-n) %>% 
  inner_join(ids)
ids_gt1

ids_gt1_meta <-
  ids_gt1 %>% 
  left_join(
    df %>% 
      select(player_id, league_id, league_name, season, z)
  )
ids_gt1_meta

f_rename <- function(suffix) {
  ids_gt1_meta %>% 
    mutate(dummy = 0) %>% 
    select(player_id, dummy, league_name, matches('^(season|z)')) %>% 
    rename_with(~sprintf('%s_%s', .x, suffix), c(league_name, matches('^(season|z)')))
}

res_init <-
  full_join(
    f_rename(1),
    f_rename(2)
  ) %>% 
  select(-dummy) %>% 
  filter(league_name_1 != league_name_2) %>% 
  mutate(z_diff = z_1 - z_2)
res_init

res_init_filt <-
  res_init %>% 
  filter((season_2 == (season_1 + 1)) | (season_2 == season_1))
res_init_filt

dummies <-
  res_init_filt %>% 
  select(matches('league'), z_diff) %>% 
  mutate(idx = row_number()) %>% 
  pivot_longer(-c(idx, z_diff)) %>% 
  mutate(across(name, ~str_remove(.x, 'league_name_') %>% as.integer())) %>% 
  mutate(across(name, ~if_else(.x == 1L, -1L, 1L))) %>% 
  pivot_wider(names_from = value, values_from = name, values_fill = 0L) %>% 
  select(-idx)
dummies %>% filter(`Primeira Liga` != 0L)

fit_dummy <- dummies %>% lm(formula(z_diff ~ .), data = .)
fit_dummy

coefs_dummy <-
  fit_dummy %>% 
  broom::tidy() %>% 
  select(league = term, estimate) %>% 
  arrange(desc(estimate)) %>% 
  drop_na(estimate)
coefs_dummy

# other models ----
res_init_filt %>% 
  select(league_name_1, z_diff) %>% 
  mutate(dummy = -1L) %>% 
  pivot_wider(names_from = league_name_1, values_from = dummy)

res_init_filt %>% 
  select(matches('^league'), z_diff) %>% 
  mutate(dummy_1 = -1L, dummy_2 = 1L) %>% 
  pivot_wider(names_from = c(league_name_1, league_name_2), values_from = c(dummy_1, dummy_2))

res <-
  bind_rows(
    res_init %>% select(league_name = league_name_1, z_diff),
    res_init %>% mutate(z_diff = -z_diff) %>% select(league_name = league_name_2, z_diff)
  )
res

fit_bt <-
  BradleyTerry2::BTm(
    outcome = result, 
    player1 = league_name_1, 
    player2 = league_name_2, 
    id = 'league_name', 
    data = res_init %>% mutate(result = if_else(z_diff < 0, 1L, 0L)) %>% mutate(across(matches('^league_name'), factor))
  )
fit_bt

coefs_bt <-
  fit_bt %>% 
  broom::tidy() %>% 
  mutate(across(term, ~str_remove(.x, 'league_name'))) %>% 
  select(league = term, estimate) %>% 
  arrange(-estimate)
coefs_bt

fit <- res %>% lm(formula(z_diff ~ league_name + 0), data = .)
fit

coefs <- 
  fit %>% 
  broom::tidy() %>% 
  mutate(across(term, ~str_remove(.x, 'league_name'))) %>% 
  select(league = term, estimate) %>% 
  arrange(estimate)
coefs
