
library(tidyverse)

path_data <- fs::path('epl_since_break.rds')
if(!fs::file_exists(path_data)) {
  
  library(understatr)
  lgs_meta <- understatr::get_leagues_meta()
  lgs_meta
  
  tm_stats_nested <- 
    crossing(league_name = 'EPL', year = 2019L) %>% 
    mutate(stats = map2(league_name, year, understatr::get_league_teams_stats))
  
  tm_stats <-
    tm_stats_nested %>% 
    select(stats) %>% 
    unnest(stats) %>% 
    rename_with(tolower) %>% 
    group_by(team_name) %>% 
    mutate(wk = row_number(date)) %>% 
    ungroup()
  
  # tm_stats_cumu <-
  #   tm_stats %>% 
  #   group_by(year, team_name, team_id) %>% 
  #   mutate(across(c(xg, xga, xpts, wins, draws, loses, pts), cumsum)) %>% 
  #   ungroup()
  # tm_stats_cumu
  
  tm_stats_split <-
    tm_stats %>% 
    filter(date >= lubridate::ymd('2020-06-17')) %>% 
    group_by(team_name) %>% 
    summarise(wk = min(wk)) %>% 
    ungroup()
  tm_stats_split
  
  tm_pts_post <-
    tm_stats %>% 
    fuzzyjoin::fuzzy_inner_join(
      tm_stats_split,
      by = c('team_name' = 'team_name', 'wk' = 'wk'),
      match_fun = list(`==`, `>=`)
    ) %>% 
    rename_with(~str_remove(., '[.]x$')) %>% 
    select(-matches('[.]y$')) %>% 
    group_by(team_name) %>% 
    summarise(across(pts, sum)) %>% 
    ungroup()
  tm_pts_post
  
  sim_once <- function(x, size) {
    sum(sample(x, size = size, replace = TRUE))
  }
  sim_n_times <- function(x, size, n_sim = 10000) {
    map_dbl(1:n_sim, ~sim_once(x, size))
  }
  
  set.seed(42L)
  pts_sim_nest <-
    tm_stats_split %>% 
    mutate(wks_post = 38L - wk) %>% 
    left_join(tm_stats %>% select(team_name, pts_actual = pts)) %>% 
    # group_by(team_name) %>% 
    nest(data = c(pts_actual)) %>% 
    # nest(pts = c(pts)) %>% 
    mutate(res = map2(data, wks_post, ~pull(..1) %>% sim_n_times(size = ..2))) %>% 
    mutate(res = map(res, ~tibble(pts_sim = ..1) %>% mutate(idx = row_number()))) %>% 
    select(-data)
  pts_sim_nest
  
  pts_agg <-
    tm_stats %>% 
    group_by(team_name) %>% 
    summarise(across(pts, sum)) %>% 
    ungroup() %>% 
    mutate(rnk = row_number(desc(pts))) %>% 
    mutate(across(team_name, ~forcats::fct_reorder(.x, rnk)))
  pts_agg
  
  pts_sim <- 
    pts_sim_nest %>% 
    unnest(res)  %>% 
    left_join(tm_pts_post) %>% 
    mutate(across(team_name, ~ordered(.x, levels = rev(levels(pts_agg$team_name)))))
  pts_sim
  
  write_rds(pts_sim, path_data)
} else {
  pts_sim <- read_rds(path_data)
}

tm_lvls <- pts_sim$team_name %>% levels()

pts_sim_agg <-
  pts_sim %>% 
  group_by(team_name) %>% 
  summarise(
    across(
      pts_sim, 
      list(
        mean = ~mean(.x), 
        sd = ~sd(.x), 
        q05 = ~quantile(.x, 0.05),
        q95 = ~quantile(.x, 0.95),
        min = ~min(.x),
        max = ~max(.x)
      ), 
      .names = '{col}_{fn}'
    ),
    across(pts, dplyr::first)
  ) %>% 
  ungroup() %>% 
  mutate(p_norm = pnorm(pts, mean = pts_sim_mean, sd = pts_sim_sd, lower.tail = TRUE)) %>% 
  mutate(across(team_name, ~ordered(.x, levels = tm_lvls)))
pts_sim_agg

pts_sim_agg_prnk <-
  pts_sim %>% 
  left_join(pts_sim_agg) %>% 
  group_by(team_name) %>% 
  arrange(pts_sim, .by_group = TRUE) %>% 
  mutate(prnk = percent_rank(pts_sim)) %>% 
  filter(pts_sim >= pts) %>% 
  filter(row_number() == 1L) %>% 
  ungroup() %>% 
  mutate(p_diff = prnk - p_norm) %>% 
  mutate(across(team_name, ~ordered(.x, levels = tm_lvls)))
pts_sim_agg_prnk

# pnorm(13, mean = 12.91, sd = 3.674) # arsenal
# pnorm(21, mean = 18.93, sd = 3.941) # man city
# pnorm(14, mean = 20.71, sd = 2.691) # liverpool
library(ggtext)
# library(ggimage)
tm_info <- readxl::read_excel('tm_info.xlsx') %>% filter(lg == 'epl')
tm_info

colors <-
  tm_info %>% 
  select(tm, color_pri) %>% 
  # mutate(across(tm, ~ordered(.x, levels = tm_lvls))) %>% 
  deframe()
colors

add_grp_col <- function(data) {
  res <- data %>% mutate(grp = case_when(team_name %in% tm_lvls[1:10] ~ '2', TRUE ~ '1'))
  res
}

pts_sim_aug <- pts_sim %>% add_grp_col() 
pts_sim_agg_prnk_aug <- pts_sim_agg_prnk %>% add_grp_col()

logos <-
  tm_info %>% 
  mutate(logo = glue::glue('{tm}     <img src="{logo_path_png}"; width="20" />', .trim = FALSE)) %>% 
  mutate(logo = case_when(row_number() <= 2 ~ logo, TRUE ~ glue::glue('{tm}'))) %>% 
  # mutate(logo = case_when(row_number() <= 0 ~ logo, TRUE ~ glue::glue('{tm}'))) %>% 
  mutate(across(logo, ~str_replace_all(.x, c('Wolverhampton ' = 'Wolverhampton<br />', 'Manchester ' = 'Manchester<br />', 'Newcastle ' = 'Newcastle<br >')))) %>% 
  select(tm, logo) %>% 
  # mutate(across(tm, ~ordered(.x, levels = tm_lvls))) %>% 
  deframe()
logos

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
  select(prnk, density_cumu, everything()) %>% 
  group_by(group) %>% 
  filter(prnk <= density_cumu) %>% 
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
  scale_y_discrete(name = NULL, labels = logos) +
  # geom_point(data = pts_sim_agg_prnk %>% add_grp_col(), aes(x = pts)) +
  geom_text(
    data = pts_sim_agg_prnk_aug, 
    aes(x = max(pts_sim_max), label = scales::percent(prnk, accuracy = 1)), 
    fontface = 'bold',
    vjust = -2, 
    hjust = 0.5
  ) +
  # facet_wrap(~grp, scales = 'free_y') +
  # geom_image(aes(image = logo), size = 0.08) +
  theme_minimal(base_family = 'Arial Narrow') +
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

