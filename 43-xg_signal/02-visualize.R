
library(tidyverse)
library(sqldf)

# library(tonythemes)
# tonythemes::theme_set_tony()
dir_proj <- '43-xg_signal'
dir_data <- file.path(dir_proj, 'data')
gray_wv <- rgb(24, 24, 24, maxColorValue = 255)
gray_grid_wv <- rgb(64, 64, 64, maxColorValue = 255)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla', color = 'white'),
  title = element_text('Karla', size = 14, color = 'white'),
  plot.title = element_text('Karla', face = 'bold', size = 24, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', size = 14, color = '#f1f1f1'),
  axis.text = element_text('Karla', color = 'white', size = 14),
  axis.title = element_text('Karla', size = 14, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = gray_grid_wv),
  panel.grid.minor = element_line(color = gray_grid_wv),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = gray_wv, color = gray_wv),
  plot.caption = element_text('Karla', color = 'white', hjust = 1, size = 11, face = 'italic'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'white', hjust = 0),
  plot.tag.position = c(.01, 0.01),
  strip.text = element_text('Karla', color = 'white', face = 'bold', size = 12, hjust = 0),
  panel.background = element_rect(fill = gray_wv, color = gray_wv)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

color_gd <- '#00FAD8' # hcl(h = 180, c = 150, l = 80)
color_xgd <- '#FF76B3' # hcl(h = 360, c = 150, l = 80)
color_xgd_future <- '#f8de7f'
lab_mapping <- tibble(
  stat1 = c('xgd', 'gd'),
  label = sprintf('%s Difference', c('xG', 'Goal')),
  color = c(color_xgd, color_gd)
)

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

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
    gg = g + g_opp,
    gr = ifelse(gg == 0, 0.5, g / gg),
    xgd = xg - xg_opp,
    xgr = xg / (xg + xg_opp)
  ) %>% 
  group_by(season, league, tm) %>% 
  mutate(
    mw = row_number(date),
    across(c(w, d, l, pts, matches('^x?g[dr]?')), list(cumu = cumsum))
  ) %>% 
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
    rnk = rnk1 + rnk2 + rnk3
  ) %>% 
  select(-matches('^rnk[123]|^[wdl]$|^[wdl]_cumu$'))
results_redux

results_redux <-
  inner_join(
    results_redux,
    results_redux %>% 
      select(league, season, date, time, tm_z = tm, tm = tm_opp, pts_cumu_opp = pts_cumu) %>% 
      rename(tm_opp = tm_z)
  ) %>% 
  mutate(ptsd = pts_cumu - pts_cumu_opp)
results_redux

df <-
  sqldf::sqldf(
    'select 
    a.league
    , a.season
    , a.tm
    , a.mw
    , b.mw as mw_post
    , a.g
    , b.g as g_post
    , a.gd
    , b.gd as gd_post
    , a.gr
    , b.gr as gr_post
    , a.xg
    , b.xg as xg_post
    , a.xgd
    , b.xgd as xgd_post
    , a.xgr
    , b.xgr as xgr_post
    , a.ptsd
    , b.ptsd as ptsd_post
    , a.rnk
    , b.rnk as rnk_post
     from results_redux a
     inner join results_redux b
     on a.league = b.league and a.season = b.season and a.tm = b.tm and a.mw < b.mw
     order by a.league, a.season, a.tm, a.mw, b.mw'
  ) %>% 
  as_tibble()
df

agg_pre <- 
  results_redux %>% 
  arrange(league, season, tm, mw) %>% 
  select(league, season, tm, mw, rnk, matches('^x?g[dr]?$')) %>% 
  rename_with(~sprintf('%s_pre', .x), -c(league, season, tm, mw)) %>% 
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
    across(matches('_post$'), ~.x / n)
  ) %>% 
  arrange(league, season, tm, mw)
agg_post
# agg_post %>% filter(mw == 37)
# results_redux %>% filter(mw >= 35, season == 2020, tm == 'Manchester City')
# df %>% filter(mw == 35, season == 2020, tm == 'Manchester City')
# agg_post %>% filter(mw == 35, season == 2020, tm == 'Manchester City')

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

cors_init <-
  cors %>% 
  unnest(cors) %>% 
  rename(col1 = term) %>% 
  pivot_longer(
    -c(league, season, mw, col1),
    names_to = 'col2',
    values_to = 'cor'
  ) # %>% 
  # filter(col1 != col2)
cors_init

cors_long <-
  cors_init %>% 
  separate(
    col1, into = c('stat1', 'state1'), sep = '_'
  ) %>% 
  separate(
    col2, into = c('stat2', 'state2'), sep = '_'
  ) %>% 
  filter(state1 == 'pre' & state2 == 'post') %>% 
  mutate(r2 = cor^2)
cors_long

at_half <- function(fit, b2 = 0.5, m2 = 0) {
  b1 <- fit$coefficient[1]
  m1 <- fit$coefficient[2]
  (b2 - b1) / (m1 - m2)
}

mw_cutoff <- 21
cors_long_filt <-
  cors_long %>% 
  filter(stat2 == 'gr') %>% 
  filter(stat1 %in% c('gd', 'xgd')) %>% 
  filter(mw <= mw_cutoff) 

gd_fits <-
  cors_long_filt %>% 
  group_nest(stat1, stat2) %>% 
  mutate(
    fit = map(data, ~lm(formula(r2 ~ log(mw)), data = .x)),
    b = map_dbl(fit, ~.x$coefficients[1]),
    m = map_dbl(fit, ~.x$coefficients[2]),
    y_at_x20 = b + m * log(mw_cutoff),
    x_at_y_half = map_dbl(fit, ~at_half(.x) %>% exp())
  ) %>% 
  select(-c(data, fit)) %>% 
  arrange(x_at_y_half)

x_int_xgd <- gd_fits %>% filter(stat1 == 'xgd') %>% pull(x_at_y_half)
x_int_gd <- gd_fits %>% filter(stat1 == 'gd') %>% pull(x_at_y_half)
arw_annotate <- arrow(length = unit(5, 'pt'), type = 'closed')
p <-
  cors_long_filt %>% 
  ggplot() +
  geom_hline(aes(yintercept = 0.5), color = 'white', linetype = 1, size = 1.2) +
  aes(x = mw, y = r2, color = stat1, group = stat1) +
  guides(color = 'none') +
  geom_text(
    data = gd_fits %>% left_join(lab_mapping),
    aes(label = label, x = mw_cutoff + 0.1, y = y_at_x20),
    size = pts(16),
    hjust = 0
  ) +
  coord_cartesian(ylim = c(0, 1), xlim = c(1, 25)) +
  scale_color_manual(
    values = lab_mapping %>% select(stat1, color) %>% deframe()
  ) +
  scale_y_continuous(
    breaks = c(0.5, 1)
  ) +
  scale_x_continuous(
    breaks = c(1, 6, 11, 16, mw_cutoff)
  ) +
  geom_smooth(
    formula = formula(y ~ log(x)),
    method = 'lm', 
    se = FALSE
  ) +
  geom_segment(
    inherit.aes = FALSE,
    color = 'white',
    linetype = 2,
    size = 1,
    data = tibble(),
    aes(y = 0, yend = 0.5, x = x_int_xgd, xend = x_int_xgd)
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.subtitle = ggtext::element_markdown(),
    axis.title.y = ggtext::element_markdown(),
    plot.caption = ggtext::element_markdown(),
  ) +
  labs(
    title = 'When can we start trusting the xG table?',
    subtitle = glue::glue('With <b>true</b> goal and xG difference (not a ratio)')
    x = '# of Prior Matches Used for Correlation',
    y = glue::glue('R-squared with <span style="color:{color_gd_future}">Rest-of-Season Goal Ratio</span>'),
    tag = '**Viz**: Tony ElHabr | **Data**: fbref via {worldfootballR}',
    caption = 'Data from Big 5 leagues, 2018 - 2021.'
  ) +
  geom_curve(
    inherit.aes = FALSE,
    aes(x = x_int_xgd + 1.5, xend = x_int_xgd, y = 0.81, yend = 0.51),
    linetype = 1,
    curvature = 0.2,
    color = 'white',
    arrow = arw_annotate
  ) +
  ggtext::geom_richtext(
    inherit.aes = FALSE,
    data = tibble(
      lab = glue::glue('<span style="color:white">After about 10 matches, there is more<br/>signal than noise in forecasting a team\'s<br/><b><span style="color:{color_gd_future}">rest-of-season goal ratio</span></b> using<br/><b><span style="color:{color_xgd}">xG difference</span></span> up to that point in the season.')
    )
    ,
    aes(
      x = x_int_xgd + 1.5,
      y = 0.82,
      label = lab
    ),
    fill = NA_character_,
    label.color = NA_character_,
    hjust = 0,
    color = 'white',
    family = 'Karla',
    size = pts(12)
  )

base_size <- 7
asp <- 1.5
path_viz <- file.path(dir_proj, 'xg_signal_true.png')
ggsave(
  filename = path_viz, 
  plot = p,
  height = base_size, 
  width = base_size * asp
)

