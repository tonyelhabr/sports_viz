
library(tidyverse)
path_shots_join <- here::here('data-raw', 'sportvu', 'joined_shots_2013.csv')
shots_join <- path_shots_join %>% read_csv() %>% janitor::clean_names()
shots_join
shots <- 
  c(2013:2014) %>% 
  set_names(., .) %>% 
  map_dfr(
    ~here::here('data-raw', 'sportvu', glue::glue('joined_shots_{..1}.csv')) %>% 
      read_csv(), 
    .id = 'year'
  ) %>% 
  janitor::clean_names()
shots
# path_chart <- here::here('data-raw', 'sportvu', 'all_chart_2013.csv')
# chart <- path_chart %>% read_csv() %>% janitor::clean_names()
# chart

shots_mini <-
  shots %>% 
  filter(pts_type == 3) %>% 
  rename(player = player_name) %>% 
  arrange(year, player, shot_number) %>% 
  group_by(player) %>% 
  mutate(idx = row_number()) %>% 
  ungroup() %>% 
  select(player, idx, fgm)
shots_mini  

shots_n <- shots_mini %>% count(player, sort = TRUE)
shots_n %>% skimr::skim()

# shots_mini %>% group_by(player_id) %>% summarize(n = n()) %>% arrange(desc(n))
kr20 <- function(x) {
  # x <- x %>% select(-matches('player'))
  # x <- data.matrix(x)
  k <- ncol(x)
  rs <- rowSums(x)
  sigma2 <- var(rs)
  p_i <- colMeans(x)
  q_i <- 1 - p_i
  scaler <- k / (k - 1)
  num <- sigma2 - sum(p_i * q_i)
  res <- scaler * num / sigma2
  res
}

do_kr20_at <- function(data, k, col) {
  # cat(sprintf('k: %s', k), sep = '\n')
  col_sym <- sym(col)
  
  data_k <-
    data %>% 
    group_by(player) %>% 
    filter(n() >= k) %>% 
    # slice(c(1:k)) %>% 
    ungroup() %>% 
    filter(idx <= k)
  # browser()
  
  data_wide <-
    data_k %>%
    select(idx, player, !!col_sym) %>%
    pivot_wider(
      names_from = idx,
      values_from = !!col_sym
    )
  res <- data_wide %>% select(-player) %>% kr20()
  res
}

do_kr20_fgm <- partial(do_kr20_at, shots_mini %>% filter(player != 'Kyle Korver'), col = 'fgm', ... = )

k_max <- shots_mini %>% filter(idx == max(idx)) %>% pull(idx)
k_max
krs_fgm <-
  tibble(k = seq.int(50, k_max, by = 10)) %>% 
  mutate(
    n = map_int(k, ~filter(shots_mini, idx >= .x) %>% distinct(player) %>% nrow()),
    kr20 = map_dbl(k, do_kr20_fgm)
  ) %>% 
  # Replace the infinite first value.
  mutate(across(kr20, ~if_else(k == 1L, 0, .x))) %>% 
  mutate(kr20_2 = kr20^2)
krs_fgm

pull_kr20_threshhold <- function(data) {
  res <-
    data %>% 
    filter(kr20_2 >= 0.5) %>% 
    filter(k != 1L) %>% 
    slice(1) %>% 
    pull(k)
  res
}

threshold_fgm <- krs_fgm %>% pull_kr20_threshhold()
threshold_fgm

krs_fgm_long <-
  krs_fgm %>% 
  pivot_longer(
    matches('^kr|^n$'),
    names_to = 'metric',
    values_to = 'value'
  ) %>% 
  filter(metric %in% c('kr20_2', 'n'))
krs_fgm_long

viz_fgm <-
  krs_fgm_long %>% 
  ggplot() +
  aes(x = k, y = value) +
  geom_step() +
  geom_vline(aes(xintercept = threshold_fgm)) +
  # geom_line(aes(y = n), color = 'blue') +
  facet_wrap(~metric, scales = 'free', ncol = 1L) +
  # scale_y_continuous(sec.axis =
  theme_minimal()
viz_fgm
plotly::ggplotly(viz_fgm)

z <-
  shots_mini %>% 
  group_by(player) %>% 
  filter(idx == max(idx)) %>% 
  ungroup() %>% 
  arrange(desc(idx)) # %>% 
  filter(idx >= 830L & idx < 850L)
