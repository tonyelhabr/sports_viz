

# https://fansided.com/2014/08/29/long-take-three-point-shooting-stabilize/
# https://www.baseballprospectus.com/news/article/17659/baseball-therapy-its-a-small-sample-size-after-all/
extrafont::loadfonts(device = 'win', quiet = TRUE)
library(tidyverse)
options(readr.num_columns = 0)
require(understatr)

f_robustly <- function(f, pb, ..., sleep = 1L) {
  assertthat::assert_that(!missing(pb))
  f_safely <- safely(f, otherwise = tibble())
  res <- f_safely(...)$result
  Sys.sleep(sleep)
  pb$tick()
  res
}

initialize_pb <- function(data, total = nrow(data), format = '[:bar] :percent eta :eta\n', width = 80L, ...) {
  res <-
    progress::progress_bar$new(
      total = !!total,
      format = format,
      width = width,
      ...
    )
  res
}

get_stats_robustly <- partial(f_robustly, understatr::get_team_players_stats, pb = tms %>% initialize_pb(), ... =)

lgs_meta <- understatr::get_leagues_meta()
# years <- lgs_meta %>% pull(year)
# years <- 2018L:2019L

get_tm_stats_robustly <- partial(f_robustly, understatr::get_league_teams_stats, pb = lgs_meta %>% initialize_pb(), ... =)

tm_stats <- 
  lgs_meta %>% 
  mutate(res = map2(league_name, year, get_tm_stats_robustly)) %>% 
  select(res) %>% 
  unnest(res)

tms <- tm_stats %>% distinct(league_name, year, team_name)

get_tm_meta_robustly <- safely(understatr::get_team_meta, otherwise = tibble())
 # get_tm_stats_robustly <- partial(f_robustly, understatr::get_league_teams_stats, pb = lgs_meta %>% initialize_pb(), ... =)
# tm_meta <-
#   tms %>%
#   # filter(team_name %in% tms_filt) %>% 
#   mutate(res = map(team_name, get_tm_meta_robustly)) %>% 
#   select(-team_name, -year) %>% 
#   unnest(res)
# tm_meta

tm_meta <- tm_meta_nested %>% select(-team_name, -year) %>% unnest(res)
tm_meta

stats_nested <-
  tms %>% 
  # filter(team_name %in% tms_filt) %>% 
  mutate(
    res = map2(team_name, year, get_stats_robustly)
  ) %>% 
  select(-team_name, -year) %>% 
  unnest(res)
stats

write_rds(stats, here::here('data', 'stats-tm-understat.rds'))

shots_by_player <-
  stats %>%
  group_by(player_name, player_id) %>% 
  summarize(shots = sum(shots)) %>% 
  ungroup() %>% 
  arrange(desc(shots))
shots_by_player

get_shots_robustly <- partial(f_robustly, understatr::get_player_shots, pb = shots_by_player %>% initialize_pb(), ... = )

shots <-
  shots_by_player %>% 
  select(player_id, player_name) %>% 
  mutate(
    res = map(player_id, get_shots_robustly)
  ) %>% 
  select(res) %>% 
  unnest(res)
shots

write_rds(shots, here::here('data', 'shots-player-understat.rds'))

shots_bin <-
  shots %>% 
  mutate(is_goal = if_else(result == 'Goal', 1L, 0L)) %>% 
  select(player, match_id, year, date, minute, situation, result, is_goal, xG) %>% 
  group_by(player) %>% 
  arrange(date, minute, .by_group = TRUE) %>% 
  mutate(idx = row_number()) %>% 
  ungroup() %>% 
  arrange(player, date, minute)
shots_bin

# shots_bin %>% count(situation)
shots_bin_np <- shots_bin %>% filter(situation != 'Penalty')

set.seed(42)
df <- ifelse(runif(25 * 100) > 0.7, 1L, 0L)
dim(df) <- c(100, 25)
df <- as_tibble(df) %>% mutate(idx = row_number())
df
# shots -> # of items -> ncol -> k 
# players -> # of people -> nrow
kr20 <- function(x){
  # x <- df
  # x = shots_filt_wide
  x <- x %>% select(-matches('idx|player'))
  # x <- data.matrix(x)
  k <- ncol(x)
  
  # Person total score variances
  rs <- rowSums(x)
  sigma2 <- var(rs)
  
  # item means
  p_i <- colMeans(x)
  q_i <- 1 - p_i
  
  scaler <- k / (k - 1)
  num <- sigma2 - sum(p_i * q_i)
  den <- sigma2
  res <- scaler * num / den
  res
}
kr20(df)

# p_i
df %>% 
  # group_by(idx) %>% 
  select(-idx) %>% 
  summarize_all(mean)

# sigma2
df %>% 
  pivot_longer(
    -idx
  ) %>% 
  group_by(idx) %>% 
  summarize(across(value, ~sum(.x))) %>% 
  ungroup() %>% 
  pull(value) %>% 
  var()

do_kr20 <- function(k) {
  # k <- 20L
  # k <- 20L
  shots_filt <-
    shots_bin_np %>% 
    group_by(player) %>% 
    filter(n() >= k) %>% 
    # slice(c(1:k)) %>% 
    ungroup() %>% 
    filter(idx <= k)
  shots_filt
  
  shots_filt_wide <-
    shots_filt %>% 
    select(idx, player, is_goal) %>% 
    pivot_wider(names_from = idx, values_from = is_goal, values_fill = list(is_goal = 0L))
  # shots_filt_wide
  shots_filt_wide
  
  res <- shots_filt_wide %>% kr20()
  res
}

krs <-
  tibble(k = seq.int(10, 300, by = 10)) %>% 
  mutate(kr20 = map_dbl(k, do_kr20))
krs %>% 
  filter(kr20 >= 0.7)
krs %>% 
  ggplot() +
  aes(x = k, y = kr20) +
  geom_point() +
  geom_line()

sigma2 <-
  shots_filt %>% 
  group_by(idx) %>% 
  summarize(g = sum(is_goal)) %>% 
  ungroup() %>% 
  pull(g) %>% 
  var()
sigma2

p_i <-
  shots_filt %>% 
  group_by(player) %>% 
  summarize(g_mean = mean(is_goal)) %>% 
  ungroup() %>%
  arrange(desc(g_mean)) %>% 
  pull(g_mean)
p_i

q_i <- 1 - p_i
scaler <- k / (k - 1)
num <- sigma2 - sum(p_i * q_i)
den <- sigma2
res <- scaler * num / den
res


shots %>% filter(player_name == 'Allan Saint-Maximin')
shots

match_id_n <-
  shots %>% 
  count(match_id, sort = T)

match_id_n %>% 
  filter(n == 1L) %>% 
  slice(1) %>% 
  inner_join(shots) %>% 
  count(player_name, name = 'nn', sort = T) # %>% 
# inner_join(shots)

shots %>% 
  count(match_id, sort = T) %>% 
  ggplot() +
  aes(x = n) +
  geom_bar()
