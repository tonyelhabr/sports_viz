
library(tidyverse)
dir_proj <- '39-cmsac'
dir_data <- file.path(dir_proj, 'data')

stats <-
  fs::dir_ls(dir_data, regexp = 'rds$') %>% 
  tibble(path = .) %>% 
  # slice(1) %>% 
  mutate(
    data = map(path, ~read_rds(.x)) #  %>% mutate(across(Wk, as.integer), across(matches('xG$'), as.numeric)))
  ) %>% 
  select(-path) %>% 
  unnest(data) %>% 
  janitor::clean_names()
stats
stats_slim <- stats %>% select(player_name, comp, season, mp, min, g = gls, npg = g_minus_pk, xg = x_g_expected, npxg = npx_g_expected, npxgp90 = npx_g_per)
stats_mini <- stats_slim %>% filter(!is.na(npxg))
stats_mini
stats_mini %>% filter(min > 1000)
90 * 3.2/1258
