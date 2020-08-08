

# https://fansided.com/2014/08/29/long-take-three-point-shooting-stabilize/
# https://www.baseballprospectus.com/news/article/17659/baseball-therapy-its-a-small-sample-size-after-all/
extrafont::loadfonts(device = 'win', quiet = TRUE)
library(tidyverse)
options(readr.num_columns = 0)

path_stats <- here::here('data', 'stats-tm-understat.rds')
path_shots <- here::here('data', 'shots-player-understat.rds')

stats_exists <- path_stats %>% fs::file_exists()
shots_exists <- path_shots %>% fs::file_exists()

if(!stats_exists | !shots_exists) {
  
  require(understatr)
  
  f_robustly <- function(f, pb, ..., sleep = 1L) {
    assertthat::assert_that(!missing(pb))
    f_safely <- safely(f, otherwise = tibble())
    res <- f_safely(...)$result
    Sys.sleep(sleep)
    pb$tick()
    res
  }
  
  initialize_pb <-
    function(data,
             total = nrow(data),
             format = '[:bar] :percent eta :eta\n',
             width = 80L,
             ...) {
      res <-
        progress::progress_bar$new(
          total = !!total,
          format = format,
          width = width,
          ...
        )
      res
    }

if(!stats_exists) {

  get_stats_robustly <-
    partial(
      f_robustly,
      understatr::get_team_players_stats,
      pb = tms %>% initialize_pb(),
      ... =
    )
  
  lgs_meta <- understatr::get_leagues_meta()
  # years <- lgs_meta %>% pull(year)
  # years <- 2018L:2019L
  
  get_tm_stats_robustly <-
    partial(
      f_robustly,
      understatr::get_league_teams_stats,
      pb = lgs_meta %>% initialize_pb(),
      ... =
    )
  
  tm_stats <- 
    lgs_meta %>% 
    mutate(res = map2(league_name, year, get_tm_stats_robustly)) %>% 
    select(res) %>% 
    unnest(res)
  
  tms <- tm_stats %>% distinct(league_name, year, team_name)

  stats_nested <-
    tms %>% 
    mutate(
      res = map2(team_name, year, get_stats_robustly)
    ) %>% 
    select(-team_name, -year) %>% 
    unnest(res)
  stats
  
  write_rds(stats, path_stats)
  
} else {
  stats <- read_rds(path_stats)
}

if(!shot_exists) {

  shots_by_player <-
    stats %>%
    group_by(player_name, player_id) %>% 
    summarize(shots = sum(shots)) %>% 
    ungroup() %>% 
    arrange(desc(shots))

  get_shots_robustly <-
    partial(
      f_robustly,
      understatr::get_player_shots,
      pb = shots_by_player %>% initialize_pb(),
      ... =
    )
  
  shots <-
    shots_by_player %>% 
    select(player_id, player_name) %>% 
    mutate(
      res = map(player_id, get_shots_robustly)
    ) %>% 
    select(res) %>% 
    unnest(res)

  write_rds(shots, path_shots)
} else {
  shots <- read_rds(path_shots)
}

shots_slim_filt <-
  shots %>% 
  # janitor::clean_names() %>% 
  mutate(g = if_else(result == 'Goal', 1L, 0L)) %>% 
  select(player, match_id, year, date, minute, situation, result, g, xg = xG) %>% 
  filter(situation != 'Penalty') %>% 
  group_by(player) %>% 
  arrange(date, minute, .by_group = TRUE) %>% 
  mutate(idx = row_number()) %>% 
  ungroup() %>% 
  relocate(idx) %>% 
  arrange(player, date, minute)
shots_slim_filt

shots_slim_filt_agg <-
  shots_slim_filt %>% 
  group_by(player, match_id) %>% 
  arrange(date, minute, .by_group = TRUE) %>% 
  summarize(across(c(xg, g), sum)) %>% 
  mutate(idx = row_number()) %>% 
  ungroup() %>% 
  mutate(overperform = ifelse(xg < g, 1L, 0L)) %>% 
  relocate(idx) 
shots_slim_filt_agg

shots_slim_filt_agg_n <-
  shots_slim_filt_agg %>% 
  count(player, overperform, sort = T) %>% 
  # pivot_wider(names_from = overperform, values_from = n)
  group_by(player) %>% 
  mutate(total = sum(n)) %>% 
  mutate(frac = n / total) %>% 
  ungroup() %>% 
  arrange(desc(frac))
shots_slim_filt_agg_n %>% filter(overperform == 1L, total > 20L)

kr20 <- function(x){
  x <- x %>% select(-matches('player'))
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
  res <- data_wide %>% kr20()
  res
}

do_kr20_g_at <- partial(do_kr20_at, shots_slim_filt, col = 'g', ... = )

krs_g <-
  tibble(k = seq.int(10, 220, by = 10)) %>% 
  mutate(
    n = map_int(k, ~filter(shots_slim_filt, idx >= .x) %>% distinct(player) %>% nrow()),
    kr20 = map_dbl(k, do_kr20_g_at)
  )
krs_g %>% filter(kr20 >= 0.7)

viz_krs_g <-
  krs_g %>% 
  ggplot() +
  aes(x = k, y = kr20) +
  geom_point() +
  geom_line() +
  # geom_line(aes(y = n), color = 'blue') +
  # scale_y_continuous(sec.axis = 
  theme_minimal()
viz_krs_g

shots_slim_filt %>% 
  group_by(player, match_id) %>% 
  arrange(date, minute, .by_group = TRUE) %>% 
  summarize(across(c(g), cumsum)) %>% 
  mutate(idx = row_number()) %>% 
  ungroup() %>% 
  mutate(overperform = ifelse(xg < g, 1L, 0L)) %>% 
  relocate(idx) 

do_kr20_xg_diff_at <- partial(do_kr20_at, shots_slim_filt_agg, col = 'overperform', ... = )

krs_xg_diff <-
  tibble(k = seq.int(10, 40 * 3, by = 5)) %>% 
  mutate(
    n = map_int(k, ~filter(shots_slim_filt_agg, idx >= .x) %>% distinct(player) %>% nrow()),
    kr20 = map_dbl(k, do_kr20_xg_diff_at)
  )
krs_xg_diff %>% filter(kr20 >= 0.7)


