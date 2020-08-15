
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
  select(player, idx, fgm) %>%
  group_by(player) %>% 
  mutate(fgm_cumu = cumsum(fgm)) %>% 
  mutate(fgm_rate_cumu = fgm_cumu / idx) %>% 
  ungroup()
shots_mini

shots_n <- shots_mini %>% count(player, sort = TRUE)
shots_n %>% skimr::skim()
shots_mini_filt <- shots_mini %>% inner_join(shots_n %>% filter(n >= 100L))
shots_mini_filt

# shots_mini %>% group_by(player_id) %>% summarize(n = n()) %>% arrange(desc(n))
kr20 <- function(data) {
  # data <- data %>% select(-matches('player'))
  # data <- data.matridata(data)
  k <- ncol(data)
  rs <- rowSums(data)
  sigma2 <- var(rs)
  p_i <- colMeans(data)
  q_i <- 1 - p_i
  scaler <- k / (k - 1)
  num <- sigma2 - sum(p_i * q_i)
  res <- scaler * num / sigma2
  res
}

splithalf <- function(data) {
  # browser()
  m <- data %>% data.matrix()
  score_e <- rowMeans(m[, c(TRUE, FALSE)])  # with even items
  score_o <- rowMeans(m[, c(FALSE, TRUE)])  # with odd items
  
  # Correlating scores from even and odd items
  r <- cor(score_e, score_o)
  r
  #> [1] 0.7681034
  
  # Adjusting with the Spearman-Brown prophecy formula
  res <- (2 * r) / (1 + r)
  # browser()
  res
}

cralpha <- function(data) {
  # browser()
  res <- data %>% psych::alpha() %>% pluck('total') %>% pluck('std.alpha')
  res
}

do_f_at <- function(data, k, col, f) {
  # cat(sprintf('k: %s', k), sep = '\n')
  # browser()
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
  
  # browser()
  res <- data_wide %>% select(-player) %>% f()
  res
}

# do_cralpha_fgm <- partial(do_f_at, shots_mini_filt, f = cralpha, col = 'fgm_rate_cumu', ... = )
# do_splithalf_fgm_rate_cumu <- partial(do_f_at, shots_mini_filt, f = splithalf, col = 'fgm_rate_cumu', ... = )
res <-
  shots_mini_filt %>% 
  # mutate(k = 1000L) %>%
  # filter(n >= 10L) %>% 
  filter(idx <= 30) %>% 
  do_f_at(col = 'fgm_rate_cumu', f = splithalf, k = 30L)
res

# do_kr20_at <- partial(do_f_at, f = kr20, ... = )
# do_cralpha_at <- partial(do_f_at, f = cralpha, ... = )
# do_kr20_fgm <- partial(do_kr20_at, shots_min_filt %>% filter(player != 'Kyle Korver'), col = 'fgm', ... = )
do_kr20_fgm <- partial(do_f_at, shots_mini_filt, f = kr20, col = 'fgm', ... = )
do_splithalf_fgm_rate <- partial(do_f_at, shots_mini_filt, f = splithalf, col = 'fgm', ... = )

k_max <- shots_mini_filt %>% filter(idx == max(idx)) %>% pull(idx)
k_max

krs_fgm <-
  tibble(k = seq.int(100, k_max, by = 10)) %>% 
  mutate(
    n = map_int(k, ~filter(shots_mini_filt, idx >= .x) %>% distinct(player) %>% nrow())
  ) %>% 
  mutate(
    value = map_dbl(k, do_kr20_fgm)
  ) %>% 
  mutate(value_2 = kr20^2)
krs_fgm

do_splithalf_fgm_rate_s <- safely(do_splithalf_fgm_rate, otherwise = NA_real_)
splithalfs_fgm <-
  tibble(k = seq.int(50, k_max - 50, by = 50)) %>% 
  # tibble(k = 1050) %>% 
  mutate(
    n = map_int(k, ~filter(shots_mini_filt, idx >= .x) %>% distinct(player) %>% nrow())
  ) %>% 
  filter(n > 1L) %>% 
  mutate(
    value = map_dbl(k, do_splithalf_fgm_rate)
  ) %>% 
  # mutate(across(value, ~if_else(k == 1L, 0, .x))) %>% 
  mutate(value_2 = value^2)
splithalfs_fgm
splithalfs_fgm %>% filter(is.na(value))

pull_threshhold <- function(data) {
  res <-
    data %>% 
    filter(value_2 >= 0.5) %>% 
    filter(k != 1L) %>% 
    slice(1) %>% 
    pull(k)
  res
}

threshold_fgm <- krs_fgm %>% pull_threshhold()
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

# z <-
#   shots_mini %>% 
#   group_by(player) %>% 
#   filter(idx == max(idx)) %>% 
#   ungroup() %>% 
#   arrange(desc(idx)) # %>% 
#   filter(idx >= 830L & idx < 850L)


temp <- tempfile()
download.file("http://personality-testing.info/_rawdata/BIG5.zip", temp, mode="wb")
d <- read.table(unz(temp, "BIG5/data.csv"), header = TRUE, sep="\t")
unlink(temp); rm(temp)
d

d <- d[1:500, paste0("E", 1:10)]
str(d)

d[, paste0("E", c(2, 4, 6, 8, 10))] <- 6 - d[, paste0("E", c(2, 4, 6, 8, 10))]
# d$score <- rowMeans(d)
head(d)

psych::alpha(d)
d
psych::alpha(d)$total$std.alpha

d2 <- d / 10
# d2[1:10, c(TRUE, FALSE)] %>% rowMeans()
splithalf(d2)
psych::alpha(d2)$total$std.alpha

d2 %>% as_tibble()
d3 <-
  d %>% 
  as_tibble() %>% 
  mutate(rn = row_number()) %>% 
  pivot_longer(
    matches('^E')
  ) %>% 
  group_by(rn) %>%
  mutate(across(value, cumsum)) %>% 
  ungroup() %>% 
  # mutate(across(value, ~{.x * 100} %>% as.integer())) %>% 
  pivot_wider(
    names_from = 'name',
    values_from = 'value'
  ) %>% 
  select(-rn)
d3
# d2
d3 %>% splithalf()
# d3 %>% splithalf()
d %>% as_tibble() %>% splithalf()
d2 %>% as_tibble() %>% splithalf()
d %>% kr20()
d2 %>% kr20()
d3 %>% kr20()
