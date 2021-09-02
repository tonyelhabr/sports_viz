
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
stats %>% filter(player_name == 'Baptiste Santamaria')

stats_slim <-
  stats %>% 
  distinct(
    player_name,
    comp,
    season,
    mp = mp_time,
    mins = min_time,
    minp90 = mins_per_90_time,
    g = gls,
    npg = g_minus_pk,
    xg = x_g_expected,
    npxg = npx_g_expected,
    xgp90 = x_g_per_minutes,
    npxgp90 = npx_g_per_minutes
  )
stats_slim
stats_mini <- stats_slim %>% drop_na(mins, npxg)

mo <- stats_slim %>% filter(player_name == 'Mohamed Salah', comp %in% c('1. Premier League', '1. La Liga', '1. Serie A', '1. Ligue 1', '1. Bundesliga'))
mo
# stats_slim %>% count(comp, sort = TRUE) %>% slice(c(2:6)) %>% pull(comp) %>% datapasta::vector_paste()

stats_agg <-
  stats_mini %>% 
  filter(comp %in% c('1. Premier League', '1. La Liga', '1. Serie A', '1. Ligue 1', '1. Bundesliga')) %>% 
  # weird
  filter(player_name %>% str_detect('tefan Radu', negate = TRUE)) %>% 
  # account for transfers amongst the big 5
  group_by(player_name, season) %>% 
  summarize(
    n = n(),
    across(c(mp, mins, g, npg, xg, npxg), sum)
  ) %>% 
  ungroup() %>% 
  filter(g > 0, mins > 500) %>% 
  mutate(
    # across(c(g, npg, xg, npxg), ~90 * .x / mp),
    # g_gt_xg = ifelse(g > xg, TRUE, FALSE),
    npxgd = npxg - npg,
    npxgd_frac = npxgd / npxg,
    npg_gt_npxg = ifelse(npxgd < 0, 1L, 0L)
  ) %>% 
  arrange(player_name, season)
stats_agg

stats_agg_lag <-
  stats_agg %>% 
  group_by(player_name) %>% 
  mutate(
    across(
      npg_gt_npxg,
      list(
        lag1 = ~lag(.x, 1),
        lag2 = ~lag(.x, 2),
        lag3 = ~lag(.x, 3)
      ),
      .names = '{fn}'
    )
  ) %>% 
  ungroup() %>% 
  mutate(
    npxgd_frac = npxgd / npxg
  )
stats_agg_lag

stats_agg_agg <-
  stats_agg_lag %>% 
  group_by(player_name) %>% 
  summarize(
    across(c(n:npg_gt_npxg), sum)
  ) %>% 
  ungroup()
stats_agg_agg

stats_agg_lag_filt <-
  stats_agg_lag %>% 
  filter(season == '2020-2021')
stats_agg_lag_filt

info <-
  tibble(
    lag1 = c(0L,0L,0L,0L,0L,0L,0L,1L,1L,1L,1L,1L,1L,1L,NA),
    lag2 = c(0L,0L,0L,1L,1L,1L,NA,0L,0L,0L,1L,1L,1L,NA,NA),
    lag3 = c(0L,1L,NA,0L,1L,NA,NA,0L,1L,NA,0L,1L,NA,NA,NA),
    grp = c('0x3', '0x2,1x1', '0x2', '0x1,1x1,0x1', '0x1,1x2', '0x1,0x1', '0x1', '1x1,0x2', '1x1,0x1,1x1', '1x1,0x1','1x2,0x1', '1x3', '1x2', '1x1', NA_character_)
  )
info

grps <-
  stats_agg_lag_filt %>% 
  count(lag1, lag2, lag3) %>% 
  left_join(info) %>% 
  select(grp, n) %>% 
  separate(grp, into = c('grp1', 'grp2'), sep = ',') %>% 
  mutate(idx = row_number()) %>% 
  pivot_longer(-c(idx, n)) %>% 
  drop_na(value) %>% 
  select(-name) %>% 
  group_by(grp = value) %>% 
  summarize(across(n, sum)) %>% 
  ungroup() %>% 
  arrange(desc(n))
grps

flops <-
  stats_agg_lag_filt %>% 
  left_join(info) %>% 
  filter(grp == '0x3') %>% 
  select(player_name, grp) %>% 
  left_join(
    stats_agg_agg
  )
flops %>% arrange(desc(npxgd_frac))
stats_agg_lag %>% 
  filter(player_name == 'Álvaro Morata')
stats_agg_lag %>%
  select(player)
  left_join(stats_agg_lag_filt)

stats_agg_lag %>% filter(player_name %>% str_detect('Son'))
stats_agg_lag %>% 
  filter(lag1 == 1L, lag2 == 1L, lag3 == 1L) %>% 
  arrange(desc(npxg))
stats_agg_lag %>% skimr::skim()

stats_agg %>% 
  select(player_name, season, npxg_gt_npg) %>% 
  pivot_wider(
    names_from = season,
    values_from = npxg_gt_npg
  )


group_by(player_name) %>% 
  summarize(
    n = n(),
  )
  ungroup()
stats_agg
stats_agg %>% arrange(desc(npxg_gt_npg))
stats_mini %>% filter(player_name == 'Baptiste Santamaria')
stats_agg %>% count(npxg_gt_npg)

# is this really valid since xG comes from a poisson binomial distribution, not a true binomial distribution
library(ebbr)
stats_mini %>% skimr::skim()
prior <-
  stats_mini %>% 
  # filter(mins > 1000) %>% 
  drop_na(npxg, mins) %>% 
  mutate(across(npxg, list(round = round))) %>% 
  ebbr::ebb_fit_prior(npxg_round, mins)
prior

stats_mini %>% 
  # drop_na(mins)
  mutate(across(npxg, list(round = round))) %>% 
  ebbr::add_ebb_estimate(npxg_round, mins, prior_subset = mins > 1000)
prior


stats_mini %>% 
  filter(mins > 1000) %>% 
  mutate(across(npxg, round))

stats_mini_adj <-
  stats_mini %>% 
  mutate(npxgp90_adj = 90 * (npxg + !!alpha) / (mins + !!alpha + !!beta)) 
stats_mini_adj
stats_mini_adj %>% arrange(desc(npxgp90))

stats_mini_adj %>% 
  # sample_frac(0.1) %>% 
  ggplot() +
  aes(x = npxgp90, y = npxgp90_adj) +
  geom_point(
    data = . %>% filter(mins < 1500),
    color = 'red',
    show.legend = FALSE,
    aes(size = mins),
    alpha = 0.2
  ) +
  geom_point(
    data = . %>% filter(mins >= 1500),
    alpha = 0.25, 
    show.legend = FALSE
  ) +
  coord_cartesian(c(0, 1), c(0, 1)) +
  theme(
    plot.caption = ggtext::element_markdown(size = 9),
  )

stats_mini %>% count(comp)
stats_mini %>% filter(min > 1000)
90 * 3.2 / 1258
