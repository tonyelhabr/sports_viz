
library(tidyverse)
dir_proj <- '35-probabilistic_clustering'
source(file.path(dir_proj, 'helpers.R'))

do_similarity <- function(.player = 'Jadon Sancho', .n = 2, .k = 6, .f = 'umap', .g = 'mclust') {
  res <- do_uncertainty(.n = .n, .k = .k, .f = .f, .g = .g)

  df_tidy <-
    res$uncertainty %>% 
    select(where(is.character), matches('^comp_')) %>% 
    pivot_longer(matches('^comp_')) %>% 
    mutate(across(name, ~str_remove(.x, '^comp_') %>% as.integer()))
  df_tidy
  
  .rename <- function(data, suffix) {
    data %>% 
      rename_with(~sprintf('%s_%s', .x, suffix), -c(pos_pred)) %>% 
      mutate(dummy = 0)
  }
  
  df_tidy_filt <- df_tidy %>% filter(player == .player)
  df_tidy_filt
    
  sims_init <-
    full_join(df_tidy_filt %>% .rename(1), df_tidy %>% .rename(2)) %>% 
    filter(name_1 == name_2) %>% 
    filter(player_1 != player_2)
  sims_init
  
  sims <-
    sims_init %>% 
    group_by_if(is.character) %>% 
    summarize(
      d = sqrt(sum((value_1 - value_2)^2))
    ) %>% 
    ungroup() %>% 
    group_by_at(vars(matches('_1'))) %>% 
    mutate(score = 1 - ((d - 0) / (max(d) - 0))) %>% 
    ungroup()
  sims

  sims %>% 
    # filter(player_1 == .player) %>% 
    arrange(d) %>% 
    select(-matches('_1')) %>% 
    rename_all(~str_remove(.x, '_2$'))
}

scores <-
  crossing(
    .f = fs,
    .n = c(2, 6, 12)
  ) %>% 
  mutate(scores = map2(.f, .n, ~do_similarity(.player = 'Jadon Sancho', .f = ..1, .n = ..2))) %>% 
  unnest(scores)
scores

options(tibble.print_max = 30)
scores_filt <-
  scores %>% 
  group_by(.f, .n) %>% 
  slice_head(n = 5) %>% 
  ungroup() %>% 
  select(.f, .n, player, squad, score)
scores_filt

scores_filt %>% 
  ggplot() +
  aes(x = ordered(.n), y = score, color = .f) +
  geom_point()
  
