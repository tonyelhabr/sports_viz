
library(tidyverse)
library(nbastatR)

players_tables()
df_dict_nba_players

players <- 
  df_dict_nba_players %>% 
  janitor::clean_names() %>%
  filter(count_seasons >= 7 | (is_active)) # %>% filter(is_active)
players

dir_data <- here::here('data-raw', '06')
fs::dir_create(dir_data)
path_data <- fs::path(dir_data, 'careers.rds')
dir_data_sub <- fs::path(dir_data, 'players')
fs::dir_create(dir_data_sub)

if(!fs::file_exists(path_data)) {
  
  # Have to do all this extra stuff (and restart the RStudio session) after around ~300 scrapes. Can't tell if it's an NBA web site limitation or something else. Oh well, do this ugly stuff and restart the session (which seems to re-set the possible rate limitation) until iterating through all players. Probably should 
  players_vec <- players_todo %>% pull(name_player)
  i_init <- 1
  n_player <- min(290, length(players_vec) - i_init)
  players_vec_filt <- players_vec[i_init:(i_init + n_player)]
  i <- 1
  scrape_player <- function(player) {
    cat(glue::glue('Pulling data for {player} at {Sys.time()} ({i} of {n_player}).'), sep = '\n')
    i <<- i + 1
    res <- player %>% players_careers(modes = c('Totals', 'PerGame'))
    write_rds(res, fs::path(dir_data_sub, sprintf('%s.rds', player)))
    res
  }
  
  scrape_player_possibly <- possibly(scrape_player, otherwise = tibble())
  careers <- players_vec_filt %>% map_dfr(scrape_player_possibly)

  careers
  write_rds(df, path_data)
} else {
  careers <- read_rds(path_data)
}

careers_proc <-
  careers %>% 
  filter(name_table == 'CareerTotalsRegularSeason') %>% 
  select(name_table, mode_search, name_player, data_table) %>% 
  pivot_wider(names_from = 'mode_search', values_from = 'data_table') %>% 
  janitor::clean_names() %>% 
  mutate_at(vars(totals), ~map(., ~rename_all(.x, paste0, '_total'))) %>% 
  mutate_at(vars(per_game), ~map(., ~rename_all(.x, paste0, '_pg'))) %>% 
  unnest(c(totals, per_game)) %>% 
  # unnest_auto(data_table) %>% 
  janitor::clean_names() %>% 
  rename(player = name_player) %>% 
  select(player, matches('gp'), matches('pts'), matches('pct_fg'), matches('pct_fg3'), matches('pct_ft'), matches('treb'), matches('ast'), matches('stl'), matches('blk')) %>% 
  select(-gp_pg)
careers_proc

# careers_proc %>% count(name_table)
# careers_filt <-
#   careers_proc %>% 
#   # filter(name_table == 'CareerTotalsRegularSeason') %>% 
#   # group_by(name_table, name_player) %>% 
#   # filter(gp_total == max(gp_total)) %>% 
#   # ungroup()
# careers_filt
# careers_filt %>% count(player)

player_y <- 'Will Barton'

df_trn <- 
  careers_proc %>%
  mutate(is_player = case_when(player == player_y ~ TRUE, TRUE ~ FALSE)) %>% 
  mutate(across(where(is.numeric), ~as.double(.x) %>% coalesce(0))) %>% 
  mutate(idx = row_number()) %>% 
  mutate(across(is_player, factor))
df_trn

df_trn_long <-
  careers_proc %>%
  mutate(is_player = case_when(player == player_y ~ TRUE, TRUE ~ FALSE)) %>% 
  # relocate(is_player) %>%
  # mutate(
  #   n_upsample = case_when(is_player ~ floor(n() * 0.5) - 1, TRUE ~ 1)
  # ) %>%
  # uncount(n_upsample) %>%
  mutate(across(where(is.numeric), ~as.double(.x) %>% coalesce(0))) %>%
  # rowwise() %>%
  # mutate(across(where(is.double), ~case_when(is_player ~ .x + rnorm(1, mean = mean(.x, na.rm = TRUE), sd = sd(.x, na.rm = TRUE)), TRUE ~ .x))) %>%
  # ungroup()
  mutate(idx = row_number()) %>%
  pivot_longer(
    -c(player, is_player, idx)
  )

df_trn_long_fp <- df_trn_long %>% filter(!is_player)
df_trn_long_tp <- df_trn_long %>% filter(is_player)

# df_trn_long_fp_agg <-
#   df_trn_long_fp %>%
#   group_by(name) %>%
#   summarize(across(value, list(mu = mean, sigma = sd))) %>%
#   ungroup()
# df_trn_long_fp_agg
# 
# df_trn_long_tp_perturb <-
#   df_trn_long_tp %>%
#   inner_join(df_trn_long_fp_agg) %>%
#   rowwise() %>%
#   mutate(across(value, list(perturb = {~.x + rnorm(1, value_mu, value_sigma)}))) %>%
#   ungroup()
# df_trn_long_tp_perturb %>% filter(name == 'gp_total')

df_trn <-
  bind_rows(
    # df_trn_long_tp_perturb %>% select(idx, is_player, player, name, value = value_perturb),
    df_trn_long_tp,
    df_trn_long_fp
  ) %>%
  pivot_wider(names_from = 'name', values_from = 'value') %>%
  mutate(across(is_player, factor))
df_trn

# fit <- rpart::rpart(formula(is_player ~ . - idx - player), data = df_trn)
library(rpart)
fit <- 
  df_trn %>% 
  select(-matches('_total$')) %>% 
  as.data.frame() %>% 
  rpart::rpart(formula(is_player ~ . - player - idx), data = ., minsplit = 1) # , control = rpart.control(cp = 0.05))
fit %>% summary()
fit %>% broomstick::tidy()
# fit %>% predict()
# fit %>% broomstick::augment()
fit
# fit %>% plot(compress = TRUE)
# fit %>% text(use.n = TRUE)
viz <- fit %>% rpart.plot::rpart.plot(roundint = FALSE)
viz$snipped.nodes
viz$branch.x
viz$obj$splits
?rpart.plot::rpart.plot
df_trn %>% arrange(-pts_pg)
df_trn %>% arrange(desc(treb_total))
df_trn %>% arrange(desc(gp_pg))

library(rpart)
fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
fit
kyphosis %>% as_tibble()
rpart.plot::rpart.plot(fit)
