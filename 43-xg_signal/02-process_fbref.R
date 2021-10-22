
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

rgx_g <- '^x?g[dr]?'
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
    across(gr, ~.x %>% na_if(Inf)), #  %>% replace_na(1)),
    xgd = xg - xg_opp,
    xgr = xg / xg_opp,
    across(xgr, ~.x %>% na_if(Inf)) #  %>% replace_na(1))
  ) %>% 
  group_by(season, league, tm) %>% 
  mutate(
    mw = row_number(date),
    across(c(w, d, l, pts, matches(rgx_g)), list(cumu = cumsum))
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

results_redux <-
  inner_join(
    results_redux,
    results_redux %>% 
      select(league, season, date, time, tm_z = tm, tm = tm_opp, pts_opp = pts) %>% 
      rename(tm_opp = tm_z)
  ) %>% 
  mutate(ptsd = pts - pts_opp) %>% 
  arrange(season, league, tm, date) %>% 
  group_by(season, league, tm) %>% 
  mutate(
    mw = row_number(date),
    across(c(ptsd), list(cumu = cumsum))
  ) %>% 
  ungroup()
results_redux

# results_redux %>% 
#   filter(league == 'GER') %>% 
#   filter(!is.na(wk)) %>% 
#   # filter(league == 'GER') %>% 
#   # filter(season == 2021, mw == 2)
#   count(league, season, mw) %>%
#   count(n, name = 'nn')

w <- 20
results_slide <-
  results_redux %>% 
  group_by(league, tm) %>% 
  mutate(
    across(
      c(matches(sprintf('%s$', rgx_g)), matches('ptsd?$')), 
      list(w = ~slider::slide_dbl(.x, sum, .after = !!w, .complete = TRUE, na.rm = TRUE))
    )
  ) %>% 
  ungroup() %>% 
  # filter(is.na(g_w)) %>% 
  drop_na(g_w) %>% 
  mutate(
    across(matches('_cumu$'), ~.x / mw),
    across(matches('_w'), ~.x / !!w)
  )
results_slide

cors <-
  results_slide %>% 
  select(league, season, mw, matches('^(x?g[dr]?|ptsd?)_(cumu|w)')) %>% 
  group_nest(league, season, mw) %>% 
  mutate(
    cors = map(data, corrr::correlate, method = 'pearson', quiet = TRUE)
  ) %>% 
  select(-c(data))
cors

cors_long_init <-
  cors %>% 
  unnest(cors) %>% 
  rename(col1 = term) %>% 
  pivot_longer(
    -c(league, season, mw, col1),
    names_to = 'col2',
    values_to = 'cor'
  ) %>% 
  filter(col1 != col2)

cors_long <-
  cors_long_init %>% 
  separate(
    col1, into = c('stat1', 'state1'), sep = '_'
  ) %>% 
  separate(
    col2, into = c('stat2', 'state2'), sep = '_'
  ) %>% 
  filter(state1 == 'cumu' & state2 == 'w') %>% 
  mutate(r2 = cor^2)
cors_long

at_half <- function(fit, b2 = 0.5, m2 = 0) {
  b1 <- fit$coefficient[1]
  m1 <- fit$coefficient[2]
  (b2 - b1) / (m1 - m2)
}

gd_fits <-
  cors_long %>% 
  filter(stat2 == 'gd') %>% 
  # filter(stat1 != stat2) %>% 
  filter(stat1 %>% str_detect('r$', negate = TRUE)) %>% 
  group_nest(stat1, stat2) %>% 
  mutate(
    fit = map(data, ~lm(formula(r2 ~ log(mw)), data = .x)),
    int = map_dbl(fit, ~at_half(.x) %>% exp())
  ) %>% 
  select(-fit) %>% 
  arrange(int)
gd_fits

gd_fits_agg <-
  cors_long %>% 
  filter(stat2 == 'gd') %>% 
  # filter(stat1 != stat2) %>% 
  filter(stat1 %>% str_detect('r$', negate = TRUE)) %>% 
  group_by(mw, stat1, stat2) %>% 
  summarize(
    across(r2, mean)
  ) %>% 
  ungroup() %>% 
  group_nest(stat1, stat2) %>% 
  mutate(
    fit = map(data, ~lm(formula(r2 ~ log(mw)), data = .x)),
    int = map_dbl(fit, ~at_half(.x) %>% exp())
  ) %>% 
  select(-fit) %>% 
  arrange(int)
gd_fits_agg

cors_long %>% 
  # filter(league != 'FRA') %>% 
  filter(stat2 == 'gd') %>% 
  filter(stat1 != stat2) %>% 
  # filter(!stat1 %in% c('gr')) %>% 
  # group_by(mw) %>% 
  # summarize(
  #   across(r2, mean)
  # ) %>% 
  # ungroup() %>% 
  ggplot() +
  geom_hline(aes(yintercept = 0.5)) +
  aes(x = mw, y = r2, group = stat1, color = stat1) +
  # geom_line() +
  # geom_point() +
  # facet_grid(season~league) +
  geom_smooth(formula = y ~ log(x), method = 'lm', se = FALSE) +
  scale_y_continuous(breaks = seq(0, 0.7, by = 0.1)) +
  # scale_x_continuous(breaks = seq(1, 38, by = 1)) +
  coord_cartesian(ylim = c(0, 0.7))

cors_long %>% 
  filter(stat1 == 'xg') %>% 
  ggplot() +
  geom_hline(aes(yintercept = 0.5)) +
  aes(x = mw, y = r2, color = league) +
  geom_line() +
  geom_point() +
  facet_wrap(~stat2)

# stats ----
stats <-
  fs::dir_ls(dir_data, regexp = 'stats.*summary[-]team[.]rds$') %>%
  tibble(path = .) %>% 
  mutate(
    data = map(path, ~read_rds(.x))
  ) %>% 
  select(-path) %>% 
  unnest(data) %>% 
  janitor::clean_names() 
stats
