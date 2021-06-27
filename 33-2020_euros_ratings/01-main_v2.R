
library(tidyverse)
dir_proj <- '33-2020_euros_ratings'
source(file.path(dir_proj, 'helpers_v2.R'))

df <- do_import()
df_direct <- df %>% mutate(idx = row_number(), z = vaep)
res_direct_paired <- df_direct %>% do_get_data()

c(df_direct_paired, agg_direct_paired) %<-% res_direct_paired
df_direct_paired

df_direct_paired_z <-
  df_direct_paired %>% 
  group_by(position, age_grp, season = season_1, league_1, league_2) %>% 
  summarize(
    n = n(),
    across(matches('^(minutes|z)'), sum)
  ) %>% 
  ungroup() %>% 
  mutate(
    # z_diff = (z_1 - z_2) / ()
    across(z_1, ~90 * .x / minutes_1),
    across(z_2, ~90 * .x / minutes_2)
  ) %>% 
  # mutate(z_diff = z_1 - z_2)
  # filter(z_1 == min(z_1))
  filter(z_1 > 0) %>% 
  mutate(
    z_diff = (z_1 - z_2) / z_1,
    minutes = minutes_1 + minutes_2
  )
df_direct_paired_z %>% skimr::skim(z_diff)
df_direct_paired_z %>% ggplot() + aes(x = z_diff) + geom_histogram()
df_direct_paired_z %>% arrange(z_diff)
rgx <- '^(season|minutes)$'
res <-
  df_direct_paired_z %>% 
  select(matches(rgx), matches('league_[12]$'), z_diff) %>% 
  pivot_longer(-c(matches(rgx), z_diff)) %>% 
  mutate(across(name, ~str_remove(.x, 'league_') %>% as.integer())) %>% 
  mutate(across(name, ~if_else(.x == 1L, -1L, 1L))) %>% 
  pivot_wider(names_from = value, values_from = name, values_fill = 0L) %>% 
  # Make this the NA coefficient
  relocate(matches('Eredivisie'), .after = last_col()) %>% 
  mutate(wt = minutes / sum(minutes))
res

fit <- lm(formula(z_diff ~ . - 1), data = res %>% select(z_diff, matches('\\(')), weights = res$wt)

coefs <- fit %>% extract_coefs()
coefs

# new ----
ids <-
  data %>% 
  distinct(player_id, league_id)
ids

ids_gt1 <-
  ids %>% 
  count(player_id, sort = TRUE) %>% 
  filter(n > 1L) %>% 
  select(-n) %>% 
  inner_join(ids)
ids_gt1

rgx_rename <- '^(season|idx|team_name|league|minutes|z)'
ids_gt1_meta <-
  ids_gt1 %>% 
  left_join(
    data %>% 
      select(player_id, player_name, matches('position'), age_grp, country, matches(rgx_rename)) %>% 
      unite('league', country, league_name, sep = '_') %>% 
      mutate(across(league, ~str_replace_all(.x, '\\s|[.]', '_') %>% str_replace_all('[_]+', '_'))) %>% 
      left_join(league_mapping %>% select(-path)) %>% 
      select(-league) %>% 
      rename(league = league_lab)
  )

ids_lead1 <-
  ids_gt1_meta %>% 
  group_by(player_id, player_name) %>% 
  arrange(season, .by_group = TRUE) %>% 
  mutate(
    across(c(league, season, minutes, z), list(lead1 = lead))
  ) %>% 
  ungroup() %>% 
  filter(league != league_lead1)
ids_lead1

df_lead1 <-
  ids_lead1 %>% 
  mutate(
    across(z, ~90 * .x / minutes),
    across(z_lead1, ~90 * .x / minutes_lead1)
  ) %>% 
  mutate(
    z_diff = z_lead1 - z
  ) %>% 
  select(league, league_lead1, position, age_grp, matches('z'))
df_lead1

fit_lead1 <- 
  df_lead1 %>% 
  select(matches('league'), z_diff) %>% 
  lm(formula(z_diff ~ league + league_lead1), data = .)
fit_lead1

fit_lead1 %>% 
  broom::tidy() %>% 
  arrange(estimate)

coefs_lead1 <-
  fit_lead1 %>% 
  broom::tidy() %>% 
  arrange(estimate) %>% 
  mutate(
    across(term, ~str_replace_all(.x, c(':league_lead1' = ' -> ', 'league' = '')))
  )
coefs_lead1
