
library(tidyverse)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 24, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown('Karla', face = 'bold', size = 18, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  # plot.caption = ggtext::element_markdown('Karla', size = 16, color = 'gray20', hjust = 1),
  plot.caption = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.016),
  # legend.text = element_text(color = 'gray20', size = 14),
  # strip.text = element_text(color = 'gray20', size = 14),
  # strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))


comm <- file.path(dir_proj, 'comm.rds') %>% read_rds()
rename_side <- function(.side) {
  # col_side <- switch(.side, 'h' = 'home', 'a' = 'away')
  .side_opp <-  switch(.side, 'h' = 'a', 'a' = 'h')
  comm %>% 
    mutate(side = .side) %>% 
    rename_with(~str_remove(.x, sprintf('_%s', .side)), matches(sprintf('_%s$', .side))) %>% 
    rename_with(~str_replace(.x, sprintf('_%s', .side_opp), '_opp'), matches(sprintf('_%s$', .side_opp))) %>% 
    select(comp, date, half, side, team, team_opp, code, code_opp, g, g_opp, stoppage_time)
}
comm_long <-
  bind_rows(rename_side('h'), rename_side('a')) %>% 
  mutate(
    g_diff = g - g_opp,
    across(g_diff, list(g_state = ~case_when(.x > 0L ~ 'ahead', .x < 0L ~ 'behind', TRUE ~ 'neutral')), .names = '{fn}')
  )
comm_long

by_team <-
  comm_long %>% 
  group_by(comp, team, code, g_state) %>% 
  summarize(
    n = n(),
    across(stoppage_time, list(max = max, mean = mean))
  ) %>% 
  ungroup() %>% 
  group_by(g_state) %>% 
  mutate(rnk_state = row_number(desc(stoppage_time_mean))) %>% 
  ungroup() %>% 
  arrange(desc(stoppage_time_mean))
by_team

iso <- worldtilegrid::wtg # $alpha.3
worldtilegrid::wtg %>% filter(name %>% str_detect('Mace')) %>% select(name)
name_fixes <-
  tibble(
    name = c('United States', 'Trinidad and Tobago', 'Curaçao', 'England', 'Wales', 'Scotland', 'Republic of Ireland'),
    code = c('USA', 'TTO', 'CUR', 'ENG', 'WAL', 'SCO', 'IRL'),
    row = c(2, 9, 8, 3, 2, 2, 3),
    col = c(1, 7, 6, 12, 11, 12, 11),
    is_extra = TRUE
  )
team_comps <-
  by_team %>% 
  distinct(team, comp)
team_comps
team_comps_gold <- team_comps %>% filter(comp == 'concacaf gold cup')
teams_drop <-
  team_comps %>% 
  semi_join(
    team_comps_gold %>% 
      select(-comp)
  ) %>% 
  # group_by(team) %>% 
  mutate(dummy = TRUE) %>% 
  pivot_wider(names_from = comp, values_from = dummy) %>% 
  filter(`concacaf gold cup` & `copa america`)

geo <- 
  geofacet::world_countries_grid1 %>% 
  select(row, col, code = code_alpha3, name) %>% 
  as_tibble() %>% 
  bind_rows(name_fixes)
geo %>% filter(name == 'United States')
by_team %>% filter(team == 'United States')

by_team_geo_init <-
  by_team %>% 
  filter(!(team %in% c('Japan', 'Qatar'))) %>% 
  filter(g_state == 'neutral') %>% 
  anti_join(
    teams_drop %>% 
      select(team) %>% 
      mutate(comp = 'copa america')
  ) %>% 
  filter(n > 2) %>% 
  rename(name = team, code_espn = code) %>% 
  full_join(geo) %>% 
  filter(col <= 9 | (between(col, 10, 18) & row <= 7)) %>% 
  # filter US for the one we added, for example
  group_by(row, col) %>% 
  mutate(rn = row_number()) %>% 
  filter(rn == 1) %>% 
  ungroup()
by_team_geo_init %>% filter(name == 'United States')
by_team_geo_init %>% filter(is_extra)

plot_grid <- function(x, ...) {
  # x$col <- factor(x$col, levels = 0:(max(x$col) + 1))
  # x$row <- factor(x$row, levels = rev(0:(max(x$row) + 1)))
  x %>% 
    ggplot() +
    aes(x = col, y = row) +
    geom_rect(
      xmin = as.numeric(x$col) - 0.5,
      xmax = as.numeric(x$col) + 0.5,
      ymin = as.numeric(x$row) - 0.5,
      ymax = as.numeric(x$row) + 0.5,
      xmin = col,
      
      ...
    ) +
    ylim(levels(x$row)) +
    xlim(levels(x$col))
}

wt <-
  by_team_geo_init %>% 
  summarize(across(stoppage_time_mean, list(min = min, max = max), na.rm = TRUE, .names = '{fn}'))
by_team_geo <-
  by_team_geo_init %>% 
  mutate(
    s = scales::rescale(stoppage_time_mean, from = c(wt$min, wt$max), to = c(11, 18)),
    lab = case_when(
      is.na(stoppage_time_mean) ~ sprintf('<b><span style="font-size:9pt">%s</span></b>', code),
      TRUE ~ sprintf('<b><span style="font-size:%.1fpt">%s</span></b><br/><span style="font-size:11pt">(%1.1f)</span>', s, code, stoppage_time_mean)
    ) 
  )
by_team_geo

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

p <-
  by_team_geo %>%
  mutate(across(row, ~-.x)) %>% 
  # plot_grid(colour = 'white', aes(fill = stoppage_time_mean), alpha = 0.6) +
  ggplot() +
  aes(x = col, y = row) +
  geom_tile(
    aes(
      # xmin = col,
      # xmax = col,
      # ymin = row,
      # ymax = row,
      x = col,
      y = row,
      fill = stoppage_time_mean
    ),
    colour = 'white', 
    alpha = 0.7
  ) +
  ggtext::geom_richtext(
    # data = by_team_geo,
    label.color = NA,
    fill = NA,
    family = 'Karla',
    aes(label = lab)
  ) +
  scale_fill_viridis_c(na.value = 'grey80', direction = 1, begin = 0.9, end = 0.1, option = 'B') +
  # paletteer::scale_fill_paletteer_c('RColorBrewer::Reds') +
  # scale_fill_distiller(palette = 'Reds', direction = 1, na.value = 'grey80') +
  geom_path(
    data = tibble(
      x = c(0, 3.5, 9, 9, 9, 9, 19),
      y = c(-10.5, -8.5, -8.5, 0, 0, -8, -8),
      grp = c(rep('a', 4), rep('b', 3))
    ),
    inherit.aes = FALSE,
    aes(x = x, y = y, group = grp)
  ) +
  annotate(
    geom = 'text',
    x = 9.1,
    y = -9,
    hjust = 0,
    label = 'Average 2nd half stoppage time in tied matches',
    family = 'Karla',
    size = pts(16),
    fontface = 'bold',
    color = 'grey20'
  ) +
  theme(
    legend.position = 'none',
    axis.text = element_blank(),
    panel.grid.major = element_blank()
  ) +
  labs(
    # title = 'Average 2nd half stoppage time in tied matches'
    y = NULL,
    x = NULL
  )
p

by_team_geo_init %>% 
  select(name, code, row, col) %>% 
  grid_preview()


require(worldtilegrid)
by_team %>% 
  filter(g_state == 'neutral') %>% 
  filter(n > 2) %>% 
  left_join(name_fixes) %>% 
  mutate(across(team, ~coalesce(name_fix, .x))) %>% 
  left_join(iso %>% select(team = name, alpha.3)) %>% 
  filter(!is.na(alpha.3)) %>% 
  ggplot() +
  aes(country = team, fill = stoppage_time_mean) +
  worldtilegrid::geom_wtg(
    border_size = 0.5
  ) +
  geom_text(
    aes(
      label = stat(alpha.3)
    ), 
    stat = 'wtg', size = 2
  ) + 
  # coord_equal() +
  scale_fill_viridis_c(na.value = NA)

by_comp <-
  by_team %>% 
  group_by(comp) %>% 
  summarize(
    stoppage_time_mean = sum(n * stoppage_time_mean) / sum(n)
  ) %>% 
  ungroup()
by_comp


by_team %>% 
  filter(g_state == 'neutral') %>% 
  filter(n > 2) %>% 
  head(20)

by_team %>% 
  filter(g_state == 'neutral') %>% 
  filter(n > 2) %>% 
  group_by(comp) %>% 
  summarize(
    n_team = n_distinct(team),
    across(n, sum)
  )

comm %>% 
  arrange(desc(date)) %>% 
  mutate(
    year = date %>% lubridate::year(),
    across(stoppage_time, list(trunc = ~ifelse(.x > 10, 10, .x)))
  ) %>% 
  ggplot() +
  aes(x = stoppage_time_trunc, y = competition_name) +
  ggbeeswarm::geom_quasirandom(
    aes(color = half),
    width = 0.2,
    groupOnX = FALSE,
    alpha = 1
  ) +
  # facet_wrap(~year) +
  theme(
    legend.position = 'top'
  )
