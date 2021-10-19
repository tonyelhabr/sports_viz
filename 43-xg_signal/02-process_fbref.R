
library(tidyverse)

library(sqldf)
# library(tonythemes)
# tonythemes::theme_set_tony()
dir_proj <- '43-xg_signal'
dir_data <- file.path(dir_proj, 'data')

results <-
  fs::dir_ls(dir_data, regexp = 'results-') %>% 
  tibble(path = .) %>% 
  mutate(
    data = map(path, ~read_rds(.x) %>% mutate(across(Wk, as.integer), across(matches('xG$'), as.numeric)))
  ) %>% 
  select(-path) %>% 
  unnest(data) %>% 
  janitor::clean_names() %>% 
  select(-matches('_2$'))
results

results_slim <-
  results %>% 
  filter(country == 'ENG') %>% 
  drop_na(home_x_g) %>% 
  select(
    league = country,
    season = season_end_year,
    wk,
    date,
    time,
    tm_h = home,
    tm_a = away,
    g_h = home_goals,
    g_a = away_goals,
    xg_h = home_x_g,
    xg_a = away_x_g
  )
results_slim

select_side <- function(.side) {
  suffix <- sprintf('_%s$', .side)
  side_opp <- ifelse(.side == 'h', 'a', 'h')
  suffix_opp <- sprintf('_%s$', side_opp)
  results_slim %>% 
    rename_with(~str_replace(.x, suffix, ''), c(matches(suffix), -matches('^team_'))) %>% 
    rename_with(~str_replace(.x, suffix_opp, '_opp'), c(matches(suffix_opp), -matches('^team_')))
}

results_redux <-
  bind_rows(select_side('h'), select_side('a')) %>% 
  arrange(season, league, tm, date) %>% 
  mutate(
    w = case_when(
      g > g_opp ~ 1L,
      TRUE ~ 0L
    ),
    d = case_when(
      g == g_opp ~ 1L,
      TRUE ~ 0L
    ),
    l = case_when(
      g < g_opp ~ 1L,
      TRUE ~ 0L
    ),
    pts = w * 3L + d * 1L + l * 0L,
    gd = g - g_opp,
    xgd = xg - xg_opp
  ) %>% 
  group_by(season, league, tm) %>% 
  mutate(
    mw = row_number(date),
    across(c(w, d, l, pts, g, gd, xg, xgd), list(cumu = cumsum))
  ) %>% 
  ungroup() %>% 
  group_by(league, tm) %>% 
  mutate(mw_total = row_number(season + mw/100)) %>% 
  ungroup() %>% 
  mutate(
    gdpm_cumu = gd_cumu / mw,
    xgpm_cumu = xg_cumu / mw,
    xgdpm_cumu = xgd_cumu / mw
  ) %>% 
  group_by(season, league, mw) %>% 
  mutate(
    rnk_gd = dense_rank(-gdpm_cumu),
    rnk_xg = dense_rank(-xgpm_cumu),
    rnk_xgd = dense_rank(-xgpm_cumu),
    rnk1 = dense_rank(-pts_cumu),
  ) %>% 
  ungroup() %>% 
  group_by(season, league, mw, rnk1) %>% 
  mutate(
    rnk2 = dense_rank(-gd_cumu) - 1L,
  ) %>% 
  ungroup() %>% 
  group_by(season, league, mw, rnk1, rnk2) %>% 
  mutate(
    rnk3 = dense_rank(-g_cumu) - 1L,
  ) %>% 
  ungroup() %>% 
  mutate(
    rnk_cumu = rnk1 + rnk2 + rnk3
  ) %>% 
  select(-matches('^rnk[123]|^[wdl]$|^[wdl]_cumu$'))
results_redux
results_redux %>% filter(league == 'ENG') %>% count(league, season, mw) %>% count(n, name = 'nn')

eoy_standings <- 
  results_redux %>% 
  group_by(league, season) %>% 
  slice_max(mw) %>%
  ungroup() %>%
  select(
    league,
    season,
    tm,
    gdpm_eoy = gdpm_cumu,
    rnk_gd_eoy = rnk_gd,
    xgpm_eoy = xgpm_cumu,
    rnk_xg_eoy = rnk_xg,
    xgdpm_eoy = xgdpm_cumu,
    rnk_xgd_eoy = rnk_xgd,
    pts_eoy = pts_cumu,
    rnk_eoy = rnk_cumu
  ) %>%
  arrange(league, season, rnk_eoy)
eoy_standings

cors <-
  results_redux %>% 
  left_join(eoy_standings) %>% 
  # head(1000) %>% 
  select(all_of(names(eoy_standings)), -tm, mw, gdpm_cumu, xgpm_cumu, xgdpm_cumu, rnk_cumu) %>%  
  group_nest(league, mw) %>% 
  mutate(
    pearson = map(data, corrr::correlate, method = 'pearson', quiet = TRUE),
    spearman = map(data, corrr::correlate, method = 'spearman', quiet = TRUE)
    # stretch = map(cors, corrr::stretch)
  ) %>% 
  select(-c(data))
cors

cors_long <-
  cors %>% 
  pivot_longer(
    -c(league, mw),
    names_to = 'method',
    values_to = 'cors'
  ) %>% 
  unnest(cors) %>% 
  rename(col1 = term) %>% 
  pivot_longer(
    -c(league, mw, method, col1),
    names_to = 'col2',
    values_to = 'cor'
  ) %>% 
  filter(col1 != col2)
cors_long

cors_long %>% 
  # filter(col1 == 'rnk_eoy') %>% 
  filter(col1 == 'xgdpm_eoy') %>% 
  filter(col2 %>% str_detect('cumu')) %>% 
  ggplot() +
  aes(x = mw, y = cor^2, color = method) +
  # geom_line(aes(color = factor(league))) +
  geom_hline(aes(yintercept = 0.5)) +
  geom_point() +
  geom_line() +
  facet_wrap(~col2, scales = 'fixed')

df <-
  sqldf::sqldf(
    'select a.league, a.season, a.tm, a.mw, b.mw as mw_post, a.mw_total, b.mw_total as b.mw_total_post, a.g, b.g as g_post, a.gd, b.gd as gd_post, a.xg, b.xg as xg_post, a.xgd, b.xgd as xgd_post
     from results_redux a
     inner join results_redux b
     on a.league = b.league and a.season = b.season and a.tm = b.tm and a.mw < (b.mw + 20)
     order by a.league, a.season, a.tm, a.mw, b.mw'
  ) %>% 
  as_tibble()
df %>% count(mw, sort = TRUE)
df %>% 
  filter(tm == 'Manchester City') %>% 
  # count(mw, sort = TRUE)
  filter(mw == 1, season == 2021)

agg_pre <- 
  results_redux %>% 
  arrange(league, season, tm, mw) %>% 
  select(league, season, tm, mw, g, gd, xg, xgd) %>% 
  rename_with(~sprintf('%s_pre', .x), c(g, gd, xg, xgd)) %>% 
  group_by(league, season, tm) %>% 
  mutate(
    across(matches('_pre$'), cumsum)
  ) %>% 
  ungroup() %>% 
  mutate(
    across(matches('_pre$'), ~.x / mw)
  )
agg_pre

agg_post <-
  df %>% 
  select(-mw_post) %>% 
  group_by(league, season, tm, mw) %>% 
  summarize(
    n = n(),
    across(matches('_post$'), sum)
  ) %>% 
  ungroup() %>% 
  mutate(
    # across(matches('^[x]?g[d]?$'), ~.x / mw),
    across(matches('_post$'), ~.x / n)
  ) %>% 
  arrange(league, season, tm, mw)
agg_post

agg <-
  full_join(
    agg_pre %>% filter(mw != 38L),
    agg_post %>% select(-c(n))
  )
agg

cors <-
  agg %>% 
  select(-tm) %>%  
  group_nest(league, season, mw) %>% 
  mutate(
    cors = map(data, corrr::correlate, method = 'pearson', quiet = TRUE)
  ) %>% 
  select(-c(data))
cors

cors_long <-
  cors %>% 
  unnest(cors) %>% 
  rename(col1 = term) %>% 
  pivot_longer(
    -c(league, season, mw, col1),
    names_to = 'col2',
    values_to = 'cor'
  ) %>% 
  filter(col1 != col2) %>% 
  separate(
    col1, into = c('stat1', 'state1'), sep = '_'
  ) %>% 
  separate(
    col2, into = c('stat2', 'state2'), sep = '_'
  ) %>% 
  filter(state1 == 'pre' & state2 == 'post') %>% 
  mutate(r2 = cor^2)
cors_long

cors_long %>% 
  filter(stat1 == 'xg') %>% 
  filter(mw <= 20) %>% 
  ggplot() +
  geom_hline(aes(yintercept = 0.5)) +
  aes(x = mw, y = r2, color = stat2) +
  geom_line() +
  geom_point() +
  facet_wrap(~season)


# stats <-
#   fs::dir_ls(dir_data, regexp = 'misc-team*rds$') %>%
#   tibble(path = .) %>% 
#   mutate(
#     data = map(path, ~read_rds(.x))
#   ) %>% 
#   select(-path) %>% 
#   unnest(data) %>% 
#   janitor::clean_names() 