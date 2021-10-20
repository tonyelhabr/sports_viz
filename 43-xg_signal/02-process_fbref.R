
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
  # filter(country == 'ENG') %>% 
  filter(country != 'USA') %>% 
  drop_na(home_x_g) %>% 
  drop_na(wk) %>% 
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
    gr = g / g_opp,
    xgd = xg - xg_opp,
    xgr = xg / xg_opp
  ) %>% 
  group_by(season, league, tm) %>% 
  mutate(
    mw = row_number(date),
    across(c(w, d, l, pts, matches('^x?g[dr]?')), list(cumu = cumsum))
  ) %>% 
  ungroup() %>% 
  group_by(league, tm) %>% 
  mutate(mw_total = row_number(season + mw / 100)) %>% 
  ungroup() %>% 
  group_by(season, league, mw) %>% 
  mutate(
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

# results_redux %>% 
#   filter(league == 'GER') %>% 
#   filter(!is.na(wk)) %>% 
#   # filter(league == 'GER') %>% 
#   # filter(season == 2021, mw == 2)
#   count(league, season, mw) %>%
#   count(n, name = 'nn')

df <-
  sqldf::sqldf(
    'select a.league, a.season, a.tm, a.mw, b.mw as mw_post, a.mw_total, b.mw_total as mw_total_post, a.g, b.g as g_post, a.gd, b.gd as gd_post, a.xg, b.xg as xg_post, a.xgd, b.xgd as xgd_post
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
  filter(mw == 38, season == 2021)

w <- 20
results_slide <-
  results_redux %>% 
  group_by(league, tm) %>% 
  mutate(
    across(c(g, gd, xg, xgd, pts), list(w = ~slider::slide_dbl(.x, sum, .after = !!w, .complete = TRUE)))
  ) %>% 
  ungroup() %>% 
  # filter(is.na(g_w)) %>% 
  drop_na(g_w) %>% 
  mutate(
    across(matches('_cumu$'), ~.x / mw),
    across(matches('_w'), ~.x / !!w)
  )

cors <-
  results_slide %>% 
  select(league, season, mw, matches('(^x?gd?|pts)_(cumu|w)')) %>% 
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
    -c(league, mw, col1),
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
  filter(state1 == 'cumu' & state2 == 'w') %>% 
  mutate(r2 = cor^2)
cors_long

cors_long %>% 
  filter(stat1 == 'xg') %>% 
  ggplot() +
  geom_hline(aes(yintercept = 0.5)) +
  aes(x = mw, y = r2, color = league) +
  geom_line() +
  geom_point() +
  facet_wrap(~stat2)


cors_long %>% 
  filter(stat1 == 'xg', stat2 == 'xgd') %>% 
  ggplot() +
  geom_hline(aes(yintercept = 0.5)) +
  aes(x = mw, y = r2, color = league) +
  geom_line() +
  geom_point()


cors_long %>% 
  filter(stat1 == 'xg', stat2 == 'xgd') %>% 
  group_by(mw) %>% 
  summarize(
    across(r2, mean)
  ) %>% 
  ungroup() %>% 
  ggplot() +
  geom_hline(aes(yintercept = 0.5)) +
  aes(x = mw, y = r2, group = stat2, color = state2) +
  # geom_line() +
  # geom_point() +
  geom_smooth(formula = y ~ log(x), method = 'lm')

# stats <-
#   fs::dir_ls(dir_data, regexp = 'misc-team*rds$') %>%
#   tibble(path = .) %>% 
#   mutate(
#     data = map(path, ~read_rds(.x))
#   ) %>% 
#   select(-path) %>% 
#   unnest(data) %>% 
#   janitor::clean_names() 