

# https://fansided.com/2014/08/29/long-take-three-point-shooting-stabilize/
# https://www.baseballprospectus.com/news/article/17659/baseball-therapy-its-a-small-sample-size-after-all/
extrafont::loadfonts(device = 'win', quiet = TRUE)
library(tidyverse)
library(fifadb) # remotes::install_github('RobWHickman/fifadb')
options(readr.num_columns = 0)

# data retrieval

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

if(!shots_exists) {
  
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

shots_players <- shots %>% distinct(player, player_id)
stats_players <- stats %>% distinct(player = player_name, player_id)
# There are no mismatches (which is to be expected given that they come from the same data source)
shots_stats_players <-
  shots_players %>% 
  inner_join(stats_players) 
shots_stats_players

players_fifa <- fifadb::male_players %>% as_tibble()
players_fifa

# data verification ----
players_fifa_latest <-
  players_fifa %>% 
  group_by(name) %>% 
  filter(version == max(version)) %>% 
  ungroup()
players_fifa_latest

# library(lubridate)
stats_fifa <-
  stats %>% 
  distinct(player = player_name, year) %>% 
  inner_join(players_fifa_latest %>% select(player = name, dob)) %>% 
  mutate(age_2019 = lubridate::interval(dob, lubridate::ymd(sprintf('%s-01-01', year)))  / lubridate::years(1))
stats_fifa

stats_fifa_anti <- 
  stats %>% 
  anti_join(players_fifa_latest %>% select(player_name = name))
stats_fifa_anti
# players_anti <- stats_fifa_anti %>% count(league_name, player_name, sort = T)
# players_anti %>% count(n, sort = T)

shots_players_fifa <-
  shots %>% 
  distinct(player) %>% 
  inner_join(players_fifa_latest %>% select(player = name, dob))
shots_players_fifa

shots_players_fifa_anti <-
  shots %>% 
  distinct(player, player_id) %>% 
  inner_join(stats %>% select(player_id, year_stat = year, position)) %>% 
  anti_join(players_fifa_latest %>% select(player = name, dob))
shots_players_fifa_anti

# data prep ----
shots_mini <-
  shots %>% 
  inner_join(shots_players_fifa) %>% 
  # mutate(age_by_year = lubridate::interval(dob, lubridate::ymd(sprintf('%s-01-01', year)))  / lubridate::years(1)) %>% 
  # janitor::clean_names() %>% 
  mutate(g = if_else(result == 'Goal', 1L, 0L)) %>% 
  select(player, dob, match_id, year, date, minute, situation, result, g, xg = xG) %>% 
  mutate(age = lubridate::interval(dob, date)  / lubridate::years(1)) %>% 
  select(-dob) %>% 
  filter(situation != 'Penalty') %>% 
  group_by(player) %>% 
  arrange(date, minute, .by_group = TRUE) %>% 
  mutate(
    idx = row_number(),
    n_shot = n(),
    n_match = n_distinct(match_id),
    g_cumu = cumsum(g)
  ) %>% 
  mutate(g_cumu_rate = g_cumu / idx) %>% 
  ungroup() %>% 
  relocate(idx, player, age)
shots_mini

shots_first <- shots_mini %>% filter(idx == 1L)
shots_first

age_min <- 21
shots_first_last_gm <-
  shots_first %>% 
  filter(age <= age_min) %>% 
  select(player) %>% 
  inner_join(shots_mini %>% group_by(player) %>% filter(idx == max(idx)) %>% ungroup())
shots_first_last_gm
# shots_first_last_gm %>% arrange(desc(n_match))
# shots_first_last_gm %>% ggplot() + aes(x = n_match, y = g_cumu) + geom_point()

players_young <-
  shots_first_last_gm %>% 
  filter(n_match >= 10 & n_shot >= 10) %>%
  distinct(player) %>% 
  inner_join(shots_mini) %>% 
  group_by(player) %>% 
  summarise(across(age, list(min = min, max = max))) %>% 
  ungroup()
players_young

# TODO: Did I need to make this a function?
aggregate_shots_by_player <- function(data) {
  res <-
    data %>% 
    group_by(player, match_id) %>% 
    arrange(date, minute, .by_group = TRUE) %>% 
    summarize(across(c(xg, g), sum)) %>%
    ungroup() %>% 
    group_by(player) %>% 
    mutate(
      idx = row_number(),
      n_match = n(), 
      overperform = ifelse(xg > g, 0L, 1L),
      # overperform = ifelse(round(xg) > g, 0L, 1L),
      g_cumu = cumsum(g),
      xg_cumu = cumsum(xg)
    ) %>% 
    mutate(overperform_cumu = cumsum(overperform)) %>% 
    mutate(overperform_cumu_rate = overperform_cumu / idx) %>% 
    ungroup() %>% 
    relocate(idx)
  res
}

shots_agg_all <- shots_mini %>% aggregate_shots_by_player()
shots_agg_young <- shots_agg_all %>% inner_join(players_young)
shots_agg_young

# main calcs ----
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

do_kr20_g_at <- partial(do_kr20_at, shots_mini, col = 'g', ... = )

# krs_g <-
#   tibble(k = seq.int(1, 200, by = 10)) %>% 
#   mutate(
#     n = map_int(k, ~filter(shots_mini, idx >= .x) %>% distinct(player) %>% nrow()),
#     kr20 = map_dbl(k, do_kr20_g_at)
#   )
# krs_g %>% filter(kr20 >= 0.7)
# 
# viz_krs_g <-
#   krs_g %>% 
#   ggplot() +
#   aes(x = k, y = kr20) +
#   geom_point() +
#   geom_line() +
#   # geom_line(aes(y = n), color = 'blue') +
#   # scale_y_continuous(sec.axis = 
#   theme_minimal()
# viz_krs_g

do_kr20_overperform <- partial(do_kr20_at, shots_agg_all, col = 'overperform', ... = )

do_kr20_overperform_wrapper <- function(data) {
  k_max <- shots_agg_all %>% filter(idx == max(idx)) %>% pull(idx)
  res <-
    tibble(k = seq.int(1, k_max, by = 1)) %>% 
    mutate(
      n = map_int(k, ~filter(data, idx >= .x) %>% distinct(player) %>% nrow()),
      kr20 = map_dbl(k, do_kr20_overperform)
    ) %>% 
    # Replace the infinite first value.
    mutate(across(kr20, ~if_else(k == 1L, 0, .x))) %>% 
    mutate(kr20_2 = kr20^2)
  res
}

krs_overperform_all <- shots_agg_all %>% do_kr20_overperform_wrapper()
krs_overperform_all
krs_overperform_young <- shots_agg_young %>% do_kr20_overperform_wrapper()
krs_overperform_young

pull_kr20_threshhold <- function(data) {
  res <-
    data %>% 
    filter(kr20_2 >= 0.5) %>% 
    filter(k != 1L) %>% 
    slice(1) %>% 
    pull(k)
  res
}

threshold_overperform_all <- krs_overperform_all %>% pull_kr20_threshhold()
threshold_overperform_all
threshold_overperform_young <- krs_overperform_young %>% pull_kr20_threshhold()
threshold_overperform_young

krs_overperform_young_long <-
  krs_overperform_young %>% 
  pivot_longer(
    matches('^kr|^n$'),
    names_to = 'metric',
    values_to = 'value'
  ) %>% 
  filter(metric %in% c('kr20_2', 'n'))
krs_overperform_young_long

viz_overperform_young <-
  krs_overperform_young_long %>% 
  ggplot() +
  aes(x = k, y = value) +
  geom_step() +
  geom_vline(aes(xintercept = threshold_overperform_all)) +
  # geom_line(aes(y = n), color = 'blue') +
  facet_wrap(~metric, scales = 'free', ncol = 1L) +
  # scale_y_continuous(sec.axis =
  theme_minimal()
viz_overperform_young

# viz ----
shots_agg_young_n <-
  # shots_agg_all %>% 
  shots_agg_young %>% 
  count(player, overperform, n_match, sort = T) %>% 
  # pivot_wider(names_from = overperform, values_from = n)
  group_by(player, n_match) %>% 
  mutate(frac = n / n_match) %>% 
  ungroup() %>% 
  arrange(desc(frac))
shots_agg_young_n
shots_agg_young_n %>% filter(overperform == 1L) %>% filter(n_match >= 20) %>% arrange(desc(frac)) %>% head(20)

library(gganimate)

viz_overperform_cumu_rate <-
  shots_agg_young %>% 
  # inner_join(
  #   shots_agg_young_n %>% 
  #     # filter(n_match >= 38L) %>% 
  #     # sample_frac(0.1, weight = n_match) %>% 
  #     arrange(-n_match) %>% 
  #     head(10) %>% 
  #     select(player)
  # ) %>% 
  ggplot() +
  aes(x = idx, y = overperform_cumu_rate, group = player) +
  geom_step(alpha = 0.1) +
  # geom_step(
  #   data = shots_agg_all %>% filter(player %in% players_top),
  #   aes(color = player),
  #   show.legend = FALSE
  # ) +
  # gganimate::transition_reveal(along = idx) +
  # geom_vline(
  #   # data = tibble(x = 60),
  #   # xintercept = 60,
  #   data = tibble(idx = rep(threshold_overperform_young, 200 - 60)),
  #   aes(xintercept = idx)
  # ) +
  theme_minimal() +
  labs(
    x = 'Match #',
    y = 'Cumualitve Over-performance Rate'
  )
viz_overperform_cumu_rate
# gganimate::animate(viz_overperform_cumu_rate)

