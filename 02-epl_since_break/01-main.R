
# extrafont::font_import(prompt = FALSE, pattern = 'Fira')
extrafont::loadfonts(device = 'win')
library(tidyverse)
library(ggtext)
library(ggimage)

# Reference: https://github.com/isabellabenabaye/ggplot2-reference/blob/master/ggplot2-theme-elements-reference.R
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Fira Code'),
  # title = element_text('Karla', size = 22, color = 'gray20'),
  title = element_text('IBM Plex Mono SemiBold', size = 14, color = 'gray20'),
  # plot.title = element_text('IBM Plex Mono SemiBold', face = 'bold', size = 22, color = 'gray20'),
  plot.title = element_text('IBM Plex Mono SemiBold', face = 'bold', size = 18, color = 'gray20'),
  plot.title.position = 'plot',
  # plot.subtitle = element_text('IBM Plex Mono SemiBold', face = 'bold', size = 22, color = 'gray50'),
  plot.subtitle = element_text('Fira Code', face = 'bold', size = 14, color = 'gray50'),
  axis.text = element_text(size = 14),
  # axis.title = element_text(size = 24, face = 'bold'),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  # axis.title.x = element_text(hjust = 0.95),
  # axis.title.y = element_text(hjust = 0.95),
  # axis.line = element_line(color = 'gray80'),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  # plot.margin = margin(25, 25, 25, 25),
  plot.margin = margin(10, 10, 10, 10),
  # plot.background = element_rect(fill = '#F3F4F6', color = '#F3F4F6'),
  plot.background = element_rect(fill = '#fffaf0', color = NA),
  # plot.caption = element_text(size = 15, face = 'italic'),
  plot.caption = element_text('Fira Code', size = 14, color = 'gray50', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = element_text('Fira Code', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.02),
  # legend.text = element_text(size = 14),
  legend.text = element_text(size = 14),
  # legend.background = element_rect(fill = '#F3F4F6', color = '#F3F4F6'),
  # legend.position = c(.85, .85))
  panel.background = element_rect(fill = '#fffaf0', color = NA)
)

iris %>%
  ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 4, alpha = 0.6) +
  theme(plot.tag.position = c(1, 0.02), plot.tag = element_text(hjust = 1)) +
  labs(
    title = 'Sepal Dimensions of Iris Flowers',
    subtitle = 'by Species',
    x = 'Sepal length',
    y = 'Sepal width',
    caption = 'Data: iris dataset',
    tag = 'Tony ElHabr'
  )

dir_proj <- '02-epl_since_break'
dir_data <- fs::path(dir_proj, 'data')
fs::dir_create(dir_data)
path_tm_stats_nested <- fs::path(dir_data, 'epl_tm_stats_nested.rds')
if(!fs::file_exists(path_tm_stats_nested)) {
  
  require(understatr)
  lgs_meta <- understatr::get_leagues_meta()
  lgs_meta
  
  years <-
    lgs_meta %>% 
    filter(league_name == 'EPL') %>% 
    pull(year)
  
  tm_stats_nested <- 
    crossing(league_name = 'EPL', year = years) %>% 
    mutate(stats = map2(league_name, year, understatr::get_league_teams_stats))
  write_rds(tm_stats_nested, path_tm_stats_nested)
} else {
  tm_stats_nested <- read_rds(path_tm_stats_nested)
}

str_replace_tm <- function(x) {
  str_replace_all(x, c(' Wanderers' = ''))
}

adjust_team_name_col <- function(data, col = 'team_name') {
  res <- data %>% mutate(across(!!sym(col), str_replace_tm))
}

tm_stats <-
  tm_stats_nested %>% 
  select(stats) %>% 
  unnest(stats) %>% 
  rename_with(tolower) %>% 
  group_by(year, team_name) %>% 
  mutate(wk = row_number(date)) %>% 
  ungroup() %>% 
  adjust_team_name_col() %>% 
  select(year, team_name, everything())
tm_stats

pts_agg <-
  tm_stats %>% 
  group_by(year, team_name) %>% 
  summarise(across(c(pts, xpts), sum, .names = '{col}_total')) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(rnk = row_number(desc(pts_total)))%>% 
  ungroup() %>% 
  mutate(pts_diff = pts_total - xpts_total) 
pts_agg

pts_agg_pre <-
  tm_stats %>% 
  filter(wk <= 30) %>% 
  group_by(year, team_name) %>% 
  summarise(across(c(pts, xpts), sum, .names = '{col}_total')) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(rnk = row_number(desc(pts_total)))%>% 
  ungroup() %>% 
  mutate(pts_diff = pts_total - xpts_total) 
pts_agg_pre %>% arrange(desc(abs(pts_diff)))

tms_filt <- c('Liverpool', 'Manchester City', 'Manchester United')
# pts_agg_pre_last_filt <- pts_agg_pre_last %>% filter(team_name %in% tms_filt)

tm_info <- here::here('data-raw', 'tm_info.xlsx') %>% readxl::read_excel() %>% filter(lg == 'epl')
tm_colors <- tm_info %>% select(team_name = tm, color_pri) %>% adjust_team_name_col() %>% deframe()
tm_logos <- tm_info %>% select(team_name = tm, logo_path = logo_path_png) %>% adjust_team_name_col()

pts_agg_pre_last <- 
  pts_agg_pre %>% 
  filter(year == 2019) %>% 
  mutate(rnk_perf = row_number(desc(pts_diff))) %>% 
  # mutate(across(team_name, ~fct_reorder(.x, -pts_diff))) %>% 
  rowwise() %>% 
  mutate(
    lab =
      if_else(
        # team_name %in% tms_filt ~ glue::glue('<span style="color:{tm_colors[[team_name]]}">{team_name}</span>'),
        any(team_name %in% tms_filt), glue::glue('<b><span style="color:{tm_colors[[team_name]]}">{team_name}</span></b>') %>% as.character(), team_name
      )
  ) %>% 
  ungroup()
pts_agg_pre_last

# pts_agg_pre_last_logos <-
#   pts_agg_pre_last %>% 
#   filter(team_name %in% tms_filt) %>% 
#   inner_join(tm_logos)
# pts_agg_pre_last_logos

viz_lolli <-
  pts_agg_pre_last %>% 
  ggplot() +
  aes(x = pts_diff, y = -rnk_perf) +
  geom_vline(
    aes(xintercept = 0),
    linetype = 2
  ) +
  geom_segment(
    aes(xend = 0, yend = -rnk_perf)
  ) +
  scale_y_continuous(
    breaks = seq.int(-20, -1),
    labels = pts_agg_pre_last %>% arrange(-rnk_perf) %>% pull(lab)
  ) +
  # geom_point(
  #   # data = . %>% filter(!team_name %in% tms_filt)
  # ) +
  geom_image(
    # data = pts_agg_pre_last_logos,
    data = . %>% inner_join(tm_logos),
    aes(image = logo_path)
  ) +
  theme(
    axis.text.y = element_markdown(),
    plot.subtitle = element_markdown(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.tag.position = c(.01, 0.01),
  ) +
  labs(
    title = 'Points Above Expectation, First 30 Games, Premier League 2019/20',
    subtitle = glue::glue('While <span style="color:{tm_colors[["Liverpool"]]}">Liverpool</span> over-performed before the pandemic break, <span style="color:{tm_colors[["Manchester City"]]}">Man City</span> and <span style="color:{tm_colors[["Manchester United"]]}">Man U</span> under-performed.'),
    tag = 'Data: understat | Viz: @TonyElHabr',
    x = 'Actual Points - Expected Points', 
    y = NULL
  )
# viz_lolli
ggsave(plot = viz_lolli, filename = here::here('plots', 'epl_before_break_pts_minus_xpts.png'), width = 12, height = 10, type = 'cairo')
# ggsave(plot = viz_lolli, filename = 'viz_lolli_pdf.pdf', width = 8, height = 8, device = cairo_pdf)
# pdftools::pdf_convert('viz_lolli_pdf.pdf', filenames = 'viz_lolli_pdf2png.png', format = 'png', dpi = 300)

viz_scatter <-
  pts_agg_pre_last %>% 
  ggplot() +
  aes(x = xpts_total, y = pts_total) +
  # geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  geom_abline(
    aes(intercept = 0, slope = 1), 
    size = 1, 
    color = 'black', 
    linetype = 'dashed'
  ) +
  geom_point(
    data = pts_agg_pre %>% anti_join(pts_agg_pre_last),
    alpha = 0.5,
    shape = 21, 
    size = 4, 
    color = 'black', 
    fill = 'grey90'
  ) +
  geom_segment(
    data = pts_agg_pre_last %>% filter(team_name == 'Liverpool'),
    aes(xend = xpts_total, yend = xpts_total),
    color = tm_colors[['Liverpool']],
    linetype = 2,
    size = 1.5
  ) +
  geom_image(
    data = pts_agg_pre_last %>% inner_join(tm_logos),
    aes(image = logo_path)
  ) +
  annotate(
    geom = 'segment',
    x = 49,
    y = 72,
    xend = 60.5,
    yend = 74,
    size = 1,
    # angle = -75,
    # curvature = -0.1,
    arrow = arrow(length = unit(2, 'mm')),
    lineend = 'round'
  ) +
  geom_text(
    inherit.aes = FALSE,
    aes(x = 49, y = 72),
    size = 5,
    hjust = 1,
    # family = 'Arial',
    # fontface = 'bold',
    family = 'Fira Code',
    color = 'grey20',
    label = glue::glue('Liverpool over-performed
                       by over 20 points')
  ) +
  # ggforce::geom_mark_circle(
  #   data = pts_agg_pre_last %>% filter(team_name %in% tms_filt) %>% filter(team_name == 'Liverpool'),
  #   aes(x = 
  # )
  coord_cartesian(xlim = c(15, 90), ylim = c(15, 90), clip = 'off') +
  theme(
    # plot.margin = c(20, 20, 20, 20),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.title.x = element_text(hjust = 0.99),
    axis.title.y = element_text(hjust = 0.99),
    plot.subtitle = element_markdown(),
    plot.tag = element_markdown(size = 14, hjust = 0),
    plot.tag.position = c(.01, 0.01)
  ) +
  labs(
    title = 'Actual and Expected Points, First 30 Games, Premier League 2019/20',
    subtitle = glue::glue('<span style="color:{tm_colors[["Liverpool"]]}">Liverpool</span>\'s performance above expectation was greater than any team (2014/15 - 2019/20).'),
    tag = 'Data: understat | Viz: @TonyElHabr',
    x = 'Expected Points (xPts)', 
    y = 'Actual Points'
  )
# viz_scatter
ggsave(plot = viz_scatter, filename = here::here('plots', 'epl_before_break_pts_vs_xpts.png'), width = 10.5, height = 10.5, type = 'cairo')

sim_once <- function(x, size, replace = FALSE) {
  sum(sample(x, size = size, replace = replace))
}

sim_n_times <- function(x, size, n_sim = 10000L) {
  map_dbl(1:n_sim, ~sim_once(x, size))
}

do_sim <- function(data, col, size) {
  col_quo <- enquo(col)
  col_str <- rlang::as_name(col_quo)
  col_name <- sprintf('%s_sim', col_str)
  res <-
    data %>% 
    pull(!!col_quo) %>% 
    sim_n_times(size = size) 
  res <- tibble(!!col_name := res)
  res
}

set.seed(42L)
pts_sim <-
  tm_stats %>% 
  filter(year == 2019) %>% 
  select(year, team_name, pts) %>% 
  # left_join(pts_agg) %>% 
  # mutate(wks_post = 38L - wk) %>% 
  nest(data = c(pts)) %>% 
  mutate(
    pts_nest = map2(data, 9, ~do_sim(..1, pts, size = ..2))
  ) %>% 
  select(-data) %>% 
  unnest(c(pts_nest)) %>% 
  group_by(team_name) %>% 
  mutate(idx = row_number()) %>% 
  ungroup()
pts_sim

# pts_sim %>% count(team_name, pts_sim)
# tm_stats_split <-
#   tm_stats %>% 
#   filter(year == 2019) %>% 
#   filter(date >= lubridate::ymd('2020-06-17')) %>% 
#   group_by(year, team_name) %>% 
#   summarise(wk = min(wk)) %>% 
#   ungroup()
# tm_stats_split

pts_post <-
  tm_stats %>% 
  # fuzzyjoin::fuzzy_inner_join(
  #   tm_stats_split,
  #   by = c('year' = 'year', 'team_name' = 'team_name', 'wk' = 'wk'),
  #   match_fun = list(`==`, `==`, `>=`)
  # ) %>% 
  # rename_with(~str_remove(., '[.]x$')) %>% 
  # select(-matches('[.]y$')) %>% 
  filter(year == 2019, wk >= 30) %>% 
  group_by(year, team_name) %>% 
  summarise(across(c(pts), sum)) %>% 
  ungroup()
pts_post

pts_sim_agg <-
  pts_sim %>% 
  group_by(team_name) %>% 
  summarise(
    across(
      matches('_sim$'), 
      list(
        mean = ~mean(.x), 
        max = ~max(.x)
      ), 
      .names = '{col}_{fn}'
    ),
    across(matches('(total)$'), dplyr::first)
  ) %>% 
  ungroup() %>% 
  left_join(pts_post) %>% 
  left_join(pts_agg)
pts_sim_agg

pts_sim_agg_prnk <-
  pts_sim %>% 
  left_join(pts_sim_agg) %>% 
  group_by(year, team_name) %>% 
  arrange(pts_sim, .by_group = TRUE) %>% 
  mutate(pts_sim_prnk = percent_rank(pts_sim)) %>% 
  filter(pts_sim >= pts) %>% 
  filter(row_number() == 1L) %>% 
  ungroup() %>% 
  select(-idx, -pts_sim, -pts, -rnk) %>% 
  mutate(rnk_distr = row_number(desc(pts_sim_prnk))) %>% 
  # mutate(across(team_name, ~fct_reorder(., rnk_distr))) %>% 
  arrange(rnk_distr)
pts_sim_agg_prnk

pts_sim_agg_prnk_labs <- 
  pts_sim_agg_prnk %>% 
  filter(year == 2019) %>% 
  rowwise() %>% 
  mutate(
    lab =
      if_else(
        any(team_name %in% c('Manchester United', 'Tottenham')), glue::glue('<b><span style="color:{tm_colors[[team_name]]}">{team_name}</span></b>') %>% as.character(), team_name
      )
  ) %>% 
  ungroup()
pts_sim_agg_prnk_labs

viz_distr_init <-
  pts_sim %>% 
  left_join(pts_sim_agg_prnk) %>% 
  mutate(pts_sim_diff = pts_sim - pts_sim_mean) %>% 
  # mutate(across(team_name, ~fct_reorder(., -rnk_distr))) %>% 
  arrange(rnk_distr, pts_sim) %>% 
  ggplot() +
  aes(x = pts_sim_diff, y = -rnk_distr) +
  # aes(x = pts_sim_diff, y = team_name) +
  # facet_wrap(~grp, scales = 'free_y') +
  ggridges::geom_density_ridges2(
    aes(fill = team_name, alpha = 0.8), 
    stat = 'binline',
    binwidth = 1,
    scale = 0.9, 
    show.legend = FALSE
  ) +
  scale_fill_manual(values = tm_colors)
# viz_distr_init

# Reference: https://stackoverflow.com/questions/52527229/draw-line-on-geom-density-ridges
gb <- viz_distr_init %>% ggplot_build() %>% purrr::pluck('data', 1)

p_lines <-
  gb %>% 
  as_tibble() %>% 
  filter((row_number() %% 2) == 1L) %>% # Every other row
  group_by(group) %>%
  mutate(density_cumu = cumsum(density)) %>%
  ungroup() %>%
  select(density_cumu, everything()) %>% 
  # left_join(pts_sim_agg_prnk %>% mutate(group = 21 - rnk_distr)) %>% 
  left_join(pts_sim_agg_prnk %>% mutate(group = row_number(team_name))) %>% 
  select(pts_sim_prnk, density_cumu, everything()) %>% 
  group_by(group) %>% 
  filter(pts_sim_prnk <= density_cumu) %>% 
  # filter(ndensity == min(ndensity)) %>% 
  filter(row_number() == 1L) %>% 
  ungroup()
p_lines

viz_distr <-
  viz_distr_init +
  geom_segment(
    data = p_lines, 
    aes(x = x, y = ymin, xend = x, yend = ymin + 0.9),
    color = 'black',
    size = 2
  ) +
  geom_text(
    data = pts_sim_agg_prnk,
    aes(x = 12, label = scales::percent(pts_sim_prnk, accuracy = 1)), 
    family = 'Fira Code',
    fontface = 'bold',
    vjust = -0.2, 
    hjust = 0
  ) +
  annotate(
    geom = 'curve',
    x = 8.8,
    y = 1,
    xend = 11.4,
    yend = -0.5,
    size = 1,
    # angle = -75,
    curvature = -0.1,
    arrow = arrow(length = unit(2, 'mm')),
    lineend = 'round'
  ) +
  annotate(
    geom = 'curve',
    x = 7,
    y = 0.5,
    xend = 5.2,
    yend = -0.5,
    size = 1,
    # angle = -75,
    curvature = -0.2,
    arrow = arrow(length = unit(2, 'mm')),
    lineend = 'round'
  ) +
  geom_text(
    aes(x = 8.5, y = 1.1),
    size = 3,
    hjust = 1,
    family = 'Fira Code',
    color = 'grey20',
    label = glue::glue('Percentile of actual points
                       in simulated distribution')
  ) +
  scale_y_continuous(
    breaks = seq.int(-20, -1),
    labels = pts_sim_agg_prnk_labs %>% arrange(-rnk_distr) %>% pull(lab)
  ) +
  theme(
    plot.caption = element_text(size = 9),
    plot.tag = element_text(size = 12),
    plot.subtitle = element_markdown(),
    axis.text.y = element_markdown(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = 'Actual and Simulated Points, Last 9 Games, Premier League 2019/20',
    subtitle = glue::glue('<span style="color:{tm_colors[["Manchester United"]]}">Man U</span> and <span style="color:{tm_colors[["Tottenham"]]}">Tottenham</span> performed much above their prior form following the reset.'),
    tag = 'Data: understat\nViz: @TonyElHabr',
    caption = 'Percent labels indicate the percentile of outcomes above which the\nactual last 9 game points placed relative to a re-sampled simulation\nof 9 game outcomes for a given team. No schedule strength adjustments.',
    x = 'Simulated 9 Game Points - Average of Simulated Last 9 Game Points',
    y = NULL
  )
# viz_distr
ggsave(plot = viz_distr, filename = here::here('plots', 'epl_after_break_pts_sim.png'), width = 10, height = 10, type = 'cairo')
