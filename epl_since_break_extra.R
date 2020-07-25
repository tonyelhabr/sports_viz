


tm_lvls <-
  pts_agg %>% 
  filter(year == 2019) %>% 
  mutate(across(team_name, ~forcats::fct_reorder(.x, rnk))) %>% 
  ungroup() %>% 
  pull(team_name)
tm_lvls

tm_stats_split <-
  tm_stats %>% 
  filter(year == 2019) %>% 
  filter(date >= lubridate::ymd('2020-06-17')) %>% 
  group_by(year, team_name) %>% 
  summarise(wk = min(wk)) %>% 
  ungroup()
tm_stats_split

tm_pts_post <-
  tm_stats %>% 
  fuzzyjoin::fuzzy_inner_join(
    tm_stats_split,
    by = c('year' = 'year', 'team_name' = 'team_name', 'wk' = 'wk'),
    match_fun = list(`==`, `==`, `>=`)
  ) %>% 
  rename_with(~str_remove(., '[.]x$')) %>% 
  select(-matches('[.]y$')) %>% 
  group_by(year, team_name) %>% 
  summarise(across(c(pts, xpts), sum)) %>% 
  ungroup() %>% 
  inner_join(pts_agg)
tm_pts_post

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
  tm_stats_split %>% 
  left_join(pts_agg) %>% 
  mutate(wks_post = 38L - wk) %>% 
  left_join(tm_stats %>% select(team_name, pts, xpts)) %>% 
  nest(data = c(pts, xpts)) %>% 
  mutate(
    pts_nest = map2(data, wks_post, ~do_sim(..1, pts, ..2)),
    xpts_nest = map2(data, wks_post, ~do_sim(..1, xpts, size = ..2))
  ) %>% 
  select(-data) %>% 
  unnest(c(pts_nest, xpts_nest)) %>% 
  group_by(team_name) %>% 
  mutate(idx = row_number()) %>% 
  ungroup()
pts_sim

refactor_team_name <- function(data) {
  res <- data %>% mutate(across(team_name, ~ordered(.x, levels = tm_lvls)))
  res
}

pts_sim_agg <-
  pts_sim %>% 
  group_by(team_name, rnk) %>% 
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
  left_join(tm_pts_post) %>% 
  refactor_team_name()
pts_sim_agg

pts_sim_join <- pts_sim %>% left_join(pts_sim_agg)
pts_sim_join

prank_pts <- function(col = c('pts', 'xpts')) {
  col <- match.arg(col)
  col_other <- ifelse(col == 'xpts', 'pts', 'xpts')
  
  col_sym <- sym(col)
  col_sim_sym <- sym(sprintf('%s_sim', col))
  col_prnk_sym <- sym(sprintf('%s_prnk', col))
  
  res <-
    pts_sim_join %>% 
    select(-matches(sprintf('^%s', col_other))) %>% 
    group_by(team_name) %>% 
    arrange(!!col_sim_sym, .by_group = TRUE) %>% 
    mutate(
      !!col_prnk_sym := percent_rank(!!col_sim_sym)
    ) %>% 
    filter(!!col_sim_sym >= !!col_sym) %>% 
    filter(row_number() == 1L) %>% 
    ungroup() %>% 
    refactor_team_name()
  res
}

pts_sim_agg_prnk <- prank_pts('pts')
xpts_sim_agg_prnk <- prank_pts('xpts')

tm_info <- readxl::read_excel('tm_info.xlsx') %>% filter(lg == 'epl')
tm_info

tm_colors <-
  tm_info %>% 
  select(tm, color_pri) %>% 
  # mutate(across(tm, ~ordered(.x, levels = tm_lvls))) %>% 
  deframe()
tm_colors

# vis_scatter_v2 ----

# library(thematic)
# ggplot2::theme_set(ggplot2::theme_minimal())
# thematic::thematic_on(
#   bg = 'floralwhite'# , fg = '#ffffff', accent = '#04f5ff'
#   # , accent = '#ffffff', fg = '#04f5ff'
# )
# thematic_off()
pts_agg
fit_lm <- lm(xpts_total ~ pts_total, pts_agg)
fit_lm
coef_lm <- fit_lm %>% coef()
coef_lm[1]
coef_lm[2] * 2
str(coef_lm)
90 * coef_lm[1] + coef_lm[1]
pts_agg %>% filter(year == 2019, team_name == 'Liverpool')-> pts_liverpool
fit_lm %>% predict(newdata = pts_liverpool)

# viz_scatter ----
# library(ggtext)
# library(ggtextures)
library(ggimage)

logos <-
  tm_info %>% 
  # mutate(logo = glue::glue('<img src="{logo_path_png}"; width="20"/>')) %>% 
  # mutate(logo = map(logo_path_local, ~magick::image_read_svg(.x))) %>% 
  select(team_name = tm, logo_path = logo_path_png)
logos

pts_sim_agg_logos <- pts_sim_agg %>% left_join(logos)
pts_sim_agg_logos

pts_sim_agg %>% 
  mutate(diff = pts_total - xpts_total) %>% 
  arrange(desc(diff))


viz_scatter <-
  pts_sim_agg_logos %>% 
  ggplot() +
  aes(x = pts_total, y = xpts_total) +
  # ggimage::geom_image(aes(image = logo_path), size = 0.1)
  geom_point() +
  theme(
    legend.position = 'none',
    text = element_text(family = 'Roboto', color = 'black'),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 16, color = 'black', family = 'Roboto'),
    panel.grid = element_line(color = 'grey80'),
    plot.background = element_rect(fill = '#38003c'),
    panel.background = element_rect(fill = '#38003c'),
    axis.ticks = element_blank(),
    plot.margin = margin(40, 30, 40, 40)
  )
viz_scatter
ggsave(plot = viz_scatter, filename = 'viz_scatter.png', width = 8, height = 8)


# viz_distr ----
add_grp_col <- function(data) {
  res <- data %>% mutate(grp = case_when(team_name %in% tm_lvls[1:10] ~ '2', TRUE ~ '1'))
  res
}

pts_sim_aug <- pts_sim %>% add_grp_col() 
pts_sim_agg_prnk_aug <- pts_sim_agg_prnk %>% add_grp_col()
tm_labs <-
  tm_info %>% 
  mutate(lab = tm) %>% 
  mutate(across(lab, ~str_replace_all(.x, c('Wolverhampton ' = 'Wolverhampton<br />', 'Manchester ' = 'Manchester<br />', 'Newcastle ' = 'Newcastle<br >')))) %>% 
  select(tm, lab) %>% 
  deframe()
tm_labs

viz_distr_init <-
  pts_sim_aug %>% 
  ggplot() +
  aes(x = pts_sim, y = team_name) +
  # facet_wrap(~grp, scales = 'free_y') +
  # 0.495 selected automatically
  ggridges::geom_density_ridges2(
    aes(fill = team_name, alpha = 0.8), 
    stat = 'binline',
    binwidth = 1,
    scale = 0.9, 
    show.legend = FALSE
  )

# Reference: https://stackoverflow.com/questions/52527229/draw-line-on-geom-density-ridges
gb <- viz_distr_init %>% ggplot_build() %>% purrr::pluck('data', 1)

p_lines <-
  gb %>% 
  as_tibble() %>% 
  filter((row_number() %% 2) == 1L) %>% # Every other row
  group_by(group) %>%
  mutate(density_cumu = cumsum(density)) %>%
  ungroup() %>%
  left_join(pts_sim_agg_prnk_aug %>% mutate(group = row_number())) %>% 
  select(pts_prnk, density_cumu, everything()) %>% 
  group_by(group) %>% 
  filter(pts_prnk <= density_cumu) %>% 
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
  scale_fill_manual(values = colors) +
  # scale_color_manual(values = colors) +
  scale_y_discrete(name = NULL, labels = lm_labs) +
  # geom_point(data = pts_sim_agg_prnk %>% add_grp_col(), aes(x = pts)) +
  geom_text(
    data = pts_sim_agg_prnk_aug, 
    aes(x = max(pts_sim_max), label = scales::percent(pts_prnk, accuracy = 1)), 
    fontface = 'bold',
    vjust = -2, 
    hjust = 0.5
  ) +
  # facet_wrap(~grp, scales = 'free_y') +
  # geom_image(aes(image = logo), size = 0.08) +
  ggdark::dark_theme_minimal(base_family = 'Roboto Slab') +
  # theme_void() +
  theme(
    strip.text = element_blank(),
    plot.title = element_text(face = 'bold', size = 14),
    plot.subtitle = element_markdown(size = 14),
    # axis.text.x = element_blank(),
    axis.text.y = ggtext::element_markdown(size = 12, hjust = 0, vjust = 0),
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor.y = element_blank(),
    # panel.grid.minor.x = element_line(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_markdown(size = 12, hjust = 0),
    plot.tag = element_markdown(size = 12, hjust = 1),
    plot.caption.position = 'plot',
    plot.tag.position = c(1, 0.01),
    plot.title.position = 'plot'
  ) +
  labs(
    # x = 'Team-Specific Distribution of Expected Point Outcomes Post-Break',
    x = 'Simulated Post-Break Point Distributions & Actual Post-Break Points',
    y = NULL
  )
viz_distr
ggsave(plot = viz_distr, filename = 'epl_since_break_distr.png', device = 'png', type = 'cairo', width = 8, height = 15)

viz_hist <-
  pts_sim %>% 
  ggplot() +
  aes(x = pts) +
  geom_histogram(binwidth = 1) +
  geom_vline(aes(xintercept = mean(pts))) +
  geom_vline(
    data = pts_sim %>% group_by(team_name) %>% summarise(across(pts, mean)),
    aes(xintercept = pts, color = team_name),
    show.legend = FALSE
  ) +
  facet_wrap(~team_name) +
  theme_minimal()
viz_hist

pts_sim_agg <-
  pts_sim %>% 
  group_by(team_name) %>% 
  summarise(across(pts, mean)) %>% 
  ungroup() %>% 
  rename(pts_sim_mean = pts) %>% 
  left_join(tm_pts_post %>% rename(pts_actual = pts))
pts_sim_agg


pts_sim_agg_logo <-
  pts_sim_agg %>% 
  left_join(tm_info %>% select(team_name = tm, logo = logo_path_png))
pts_sim_agg_logo


viz_scatter <-
  pts_sim_agg_logo %>% 
  # head(2) %>% 
  ggplot() +
  aes(x = pts_actual, y = pts_sim_mean) +
  geom_point(aes(color = team_name), show.legend = FALSE) +
  # geom_image(aes(image = logo), size = 0.08) +
  theme_minimal()
viz_scatter
ggsave(plot = viz_scatter, filename = 'epl_since_break.png', device = 'png', type = 'cairo', width = 10, height = 10)

