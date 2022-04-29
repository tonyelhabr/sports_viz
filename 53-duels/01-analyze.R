library(tidyverse)
library(qs)
library(ebbr)
dir_proj <- '53-duels'

duels <- file.path(dir_proj, 'duels.qs') %>% qs::qread()

count_duels <- function(df) {
  df %>% 
    count(season_id, player_name, is_successful) %>% 
    group_by(season_id, player_name) %>% 
    mutate(
      across(is_successful, ~ifelse(.x, 'won', 'lost')),
      prop = n / sum(n)
    ) %>% 
    ungroup() %>% 
    pivot_wider(
      names_from = is_successful,
      values_from = c(n, prop)
    ) %>% 
    mutate(n = coalesce(n_lost, 0L) + coalesce(n_won, 0L))
}

total_split <- duels %>% count_duels()
splits <- duels %>% 
  count(is_aerial, season_id, player_name, is_successful) %>% 
  group_by(is_aerial, season_id, player_name) %>% 
  mutate(
    across(is_successful, ~ifelse(.x, 'won', 'lost')),
    prop = n / sum(n)
  ) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = is_successful,
    values_from = c(n, prop)
  ) %>% 
  drop_na(n_lost, n_won) %>% 
  group_by(is_aerial) %>% 
  # mutate(n = coalesce(n_lost, 0L) + coalesce(n_won, 0L)) %>% 
  mutate(n = n_lost + n_won) %>% 
  ungroup()

q25s <- splits %>%
  group_by(is_aerial) %>% 
  summarize(
    q = round(quantile(n, 0.25))
  )

## tidy eval doesn't work with ebbr
# splits_adj <- splits %>% 
#   nest(data = -c(is_aerial)) %>% 
#   inner_join(q25s)
#   mutate(
#     data_adj = map2(data, q, ~ebbr::add_ebb_estimate(.x, n_won, n, prior_subset = n >= .y))
#   )
# splits_adj
ground_splits <- splits %>% filter(!is_aerial)
aerial_splits <- splits %>% filter(is_aerial)

postprocess_ebbr_split <- function(df, suffix, ...) {
  df %>% 
    select(
      season_id, player_name, n_lost, n_won, prop_lost, prop_won, prop_won_adj = .fitted
    ) %>% 
    rename_with(~sprintf('%s_%s', .x, suffix), -c(season_id, player_name))
}

splits_adj <- inner_join(
  aerial_splits %>% 
    ebbr::add_ebb_estimate(n_won, n, prior_subset = n >= 20) %>% 
    postprocess_ebbr_split('aerial'),
  ground_splits %>% 
    ebbr::add_ebb_estimate(n_won, n, prior_subset = n >= 40) %>% 
    postprocess_ebbr_split('ground')
) %>% 
  mutate(
    prop_won_diff = prop_won_ground - prop_won_aerial,
    prop_won_adj_diff = prop_won_adj_ground - prop_won_adj_aerial
  ) %>% 
  arrange(desc(prop_won_adj_diff))
splits_adj %>% arrange(prop_won_adj_diff)
