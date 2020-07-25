
extrafont::loadfonts(device = 'win', quiet = TRUE)
library(tidyverse)

path_tm_stats_nested <- fs::path('epl_since_break_tm_stats_nested.rds')
if(!fs::file_exists(path_tm_stats_nested)) {
  
  library(understatr)
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

tm_stats <-
  tm_stats_nested %>% 
  select(stats) %>% 
  unnest(stats) %>% 
  rename_with(tolower) %>% 
  group_by(year, team_name) %>% 
  mutate(wk = row_number(date)) %>% 
  ungroup()
tm_stats
tm_stats <-
  tm_stats %>% 
  mutate(across(team_name, ~str_replace_all(.x, c('Wolverhampton ' = 'Wolverhampton\n'))))
# tm_stats %>% filter(team_name %>% str_detect('Wanderer')) %>% select(team_name)

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

pts_agg_pre_last <- 
  pts_agg_pre %>% 
  filter(year == 2019) %>% 
  mutate(rnk_perf = row_number(desc(pts_diff)))
pts_agg_pre_last
pts_agg_pre_last

# pts_agg_pre_last_liv <- pts_agg_pre_last %>% filter(team_name == 'Liverpool')
# pts_agg_pre_last_mc <- pts_agg_pre_last %>% filter(team_name == 'Manchester City')
# pts_agg_pre_last_mu <- pts_agg_pre_last %>% filter(team_name == 'Manchester United')
tms_filt <- c('Liverpool', 'Manchester City', 'Manchester United')
pts_agg_pre_last_filt <- pts_agg_pre_last %>% filter(team_name %in% tms_filt)

pts_agg_pre_last %>% 
  mutate(across(team_name, ~
  mutate(across(team_name, ~fct_reorder(.x, pts_diff))) %>% 
  ggplot() +
  aes(x = pts_diff, y = team_name) +
  geom_vline(
    aes(xintercept = 0),
    linetype = 2
  ) +
  geom_point() +
  geom_segment(
    aes(xend = 0, yend = team_name)
  ) +
  theme_minimal(base_family = 'IBM Plex Sans', base_size = 12) +
  labs(
    x = 'Actual Points - Expected Points', 
    y = NULL
  )

library(ggtext)
pts_agg_pre %>% 
  filter(year < 2019) %>% 
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
    shape = 21, 
    size = 5, 
    color = 'black', 
    fill = 'grey'
  ) +
  geom_point(
    data = pts_agg_pre_last,
    shape = 21,
    size = 5,
    color = 'black',
    fill = 'black',
  ) +
  geom_segment(
    data = pts_agg_pre_last_liv,
    aes(x = xpts_total, y = pts_total, xend = xpts_total, yend = xpts_total),
    linetype = 2,
    size = 1.5
  ) +
  geom_point(
    data = pts_agg_pre_last_liv,
    shape = 21,
    size = 7,
    color = 'black',
    fill = '#d00027'
  ) +
  geom_point(
    data = pts_agg_pre_last_mc,
    shape = 21,
    size = 7,
    color = 'black',
    fill = '#98c5e9'
  ) +
  geom_point(
    data = pts_agg_pre_last_mu,
    shape = 21,
    size = 7,
    color = 'black',
    fill = '#da020e'
  ) +
  # geom_curve(
  #   aes(x = 80, xend = 94, y = 70, yend = 72), 
  #   curvature = 0.2, 
  #   lineend = 'round',
  #   angle = 45,
  #   size = 1.25
  # ) +
  # geom_segment(
  #   aes(x = 89.5, xend = 90, y = 72, yend = 72),
  #   linejoin = "round", size = 1.25,
  #   arrow = arrow(length = unit(0.8, "cm"), angle = 40, type = "closed")
  # ) +
  coord_cartesian(xlim = c(10, 90), ylim = c(10, 90), clip = 'off') +
  # coord_cartesian(clip = 'off') +
  theme_minimal(base_family = 'Roboto', base_size = 12) +
  theme(
    plot.background = element_rect(fill = 'floralwhite'),
    panel.background = element_rect(fill = 'floralwhite'),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_markdown(size = 12, hjust = 0),
    plot.tag = element_markdown(size = 12, hjust = 1),
    plot.caption.position = 'plot',
    plot.tag.position = c(1, 0.01),
    plot.title.position = 'plot'
  ) +
  labs(x = 'Expected Points (xPts), Pre-Break', y = 'Actual Points, Pre-Break')
