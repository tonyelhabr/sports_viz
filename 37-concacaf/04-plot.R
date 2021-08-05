
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
    mutate(
      across(
        comp,
        ~case_when(
          .x == 'concacaf gold cup' ~ 'North America',
          .x == 'copa america' ~ 'South America',
          TRUE ~ 'Europe'
        )
      )
    ) %>% 
    rename_with(~str_remove(.x, sprintf('_%s', .side)), matches(sprintf('_%s$', .side))) %>% 
    rename_with(~str_replace(.x, sprintf('_%s', .side_opp), '_opp'), matches(sprintf('_%s$', .side_opp))) %>% 
    select(comp, date, half, side, team, team_opp, code, code_opp, g, g_opp, stoppage_time)
}
comm_long <-
  bind_rows(rename_side('h'), rename_side('a')) %>% 
  mutate(
    g_diff = g - g_opp,
    across(g_diff, list(g_state = ~case_when(.x > 1L ~ 'ahead', .x < -1L ~ 'behind', TRUE ~ 'neutral')), .teams = '{fn}')
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
  arrange(desc(stoppage_time_mean))
by_team

iso <- worldtilegrid::wtg

team_fixes <-
  tibble(
    team = c('United States', 'Trinidad and Tobago', 'Curaçao', 'England', 'Wales', 'Scotland', 'Republic of Ireland'),
    code = c('USA', 'TTO', 'CUR', 'ENG', 'WAL', 'SCO', 'IRL'),
    row = c(2, 9, 8, 3, 2, 2, 3),
    col = c(1, 7, 6, 12, 11, 12, 11),
    is_extra = TRUE
  )

team_comps <-
  by_team %>% 
  count(team, comp)
team_comps %>% filter(team == 'Jamaica')

team_comps_gold <- team_comps %>% filter(comp == 'North America')

teams_drop <-
  team_comps %>% 
  semi_join(
    team_comps_gold %>% select(team)
  ) %>% 
  pivot_wider(names_from = comp, values_from = n) %>% 
  filter(`North America` > 0 & `South America` > 0)
teams_drop

geo <- 
  geofacet::world_countries_grid1 %>% 
  select(row, col, code = code_alpha3, team = name) %>% 
  as_tibble() %>% 
  bind_rows(team_fixes)
# geo %>% filter(team == 'United States')
# by_team %>% filter(team == 'United States')

# by_team %>% 
#   filter(!(team %in% c('Japan', 'Qatar'))) %>% 
#   filter(g_state == 'ahead') %>% 
#   anti_join(
#     teams_drop %>% 
#       select(team) %>% 
#       mutate(comp = 'copa america')
#   ) %>% 
#   filter(n > 5) 

by_team_geo_init <-
  by_team %>% 
  filter(!(team %in% c('Japan', 'Qatar'))) %>% 
  filter(g_state == 'neutral') %>% 
  anti_join(
    teams_drop %>% 
      select(team) %>% 
      mutate(comp = 'South America')
  ) %>% 
  filter(n > 5) %>% 
  rename(code_espn = code) %>% 
  mutate(rnk = row_number(desc(stoppage_time_mean))) %>% 
  inner_join(geo) %>% 
  # full_join(geo) %>% 
  # filter(col <= 9 | (between(col, 10, 18) & row <= 7)) %>% 
  # filter US for the one we added, for example
  group_by(row, col) %>% 
  mutate(rn = row_number()) %>% 
  filter(rn == 1) %>% 
  ungroup() %>% 
  select(-rn) %>% 
  arrange(desc(stoppage_time_mean))
by_team_geo_init
by_team_geo_init %>% filter(team == 'United States')
by_team_geo_init %>% filter(is_extra)

# wt <-
#   by_team_geo_init %>% 
#   summarize(across(stoppage_time_mean, list(min = min, max = max), na.rm = TRUE, .teams = '{fn}'))
# by_team_geo <-
#   by_team_geo_init %>% 
#   mutate(
#     s = scales::rescale(stoppage_time_mean, from = c(wt$min, wt$max), to = c(11, 18)),
#     lab = case_when(
#       is.na(stoppage_time_mean) ~ sprintf('<b><span style="font-size:9pt">%s</span></b>', code),
#       TRUE ~ sprintf('<b><span style="font-size:%.1fpt">%s</span></b><br/><span style="font-size:11pt">(%1.1f)</span>', s, code, stoppage_time_mean)
#     ) 
#   )
# by_team_geo

by_team_geo <-
  by_team_geo_init %>%
  mutate(
    lab = case_when(
      is.na(stoppage_time_mean) ~ sprintf('<b><span style="font-size:9pt">%s</span></b>', code),
      TRUE ~ sprintf('<b><span style="font-size:13pt">%s</span></b><br/><span style="font-size:9pt"><b>#%d</b>: %1.1f</span>', code, rnk, stoppage_time_mean)
    )
  )
by_team_geo

# # Reference: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset4=theme-code3
# .gt_theme_538 <- function(data,...) {
#   data %>%
#     gt::opt_all_caps()  %>%
#     gt::opt_table_font(
#       font = list(
#         gt::google_font('Karla'),
#         gt::default_fonts()
#       )
#     ) %>%
#     gt::tab_style(
#       style = gt::cell_borders(
#         sides = 'bottom', color = 'transparent', weight = gt::px(2)
#       ),
#       locations = gt::cells_body(
#         columns = TRUE,
#         # This is a relatively sneaky way of changing the bottom border
#         # Regardless of data size
#         rows = nrow(data$`_data`)
#       )
#     )  %>% 
#     gt::tab_options(
#       column_labels.background.color = 'white',
#       table.border.top.width = gt::px(3),
#       table.border.top.color = 'transparent',
#       table.border.bottom.color = 'transparent',
#       table.border.bottom.width = gt::px(3),
#       column_labels.border.top.width = gt::px(3),
#       column_labels.border.top.color = 'transparent',
#       column_labels.border.bottom.width = gt::px(3),
#       column_labels.border.bottom.color = 'black',
#       data_row.padding = gt::px(3),
#       footnotes.font.size = 10,
#       source_notes.font.size = 10,
#       table.font.size = 16,
#       heading.align = 'left',
#       ...
#     ) 
# }
# 
# tab <-
#   by_team_geo %>% 
#   arrange(desc(stoppage_time_mean)) %>% 
#   head(10) %>% 
#   mutate(rnk = row_number(desc(stoppage_time_mean))) %>% 
#   select(rnk, comp, team, n, stoppage_time_mean) %>% 
#   gt::gt() %>% 
#   gt::cols_label(
#     .list = 
#       list(
#         rnk = 'Rank',
#         comp = 'Competition',
#         team = 'Nation',
#         n = gt::html('# of<br/>Matches'),
#         stoppage_time_mean = gt::html('Avg.')
#       )
#   ) %>%
#   gt::fmt_number(
#     decimals = 1,
#     columns = c(stoppage_time_mean)
#   ) %>%
#   # gt::tab_spanner(
#   #   label = 'Stoppage Time',
#   #   columns = c(stoppage_time_mean, stoppage_time_max)
#   # ) %>% 
#   gt::cols_align(
#     align = 'right',
#     columns = vars(rnk)
#   ) %>%
#   # gt::tab_header(
#   #   title = 'Avergage 2nd half stoppage time',
#   #   subtitle = 'for matches ending in neutral state'
#   # ) %>% 
#   .gt_theme_538()
# tab
# path_tab <- file.path(dir_proj, 'stoppage_time_tab.png')
# gt::gtsave(tab, filename = path_tab)

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

f_text <- partial(
  ggtext::geom_richtext,
  label.color = NA,
  fill = NA,
  family = 'Karla',
  inherit.aes = FALSE,
  data = tibble(),
  ... = 
)

p <-
  by_team_geo %>%
  mutate(across(row, ~-.x)) %>% 
  ggplot() +
  aes(x = col, y = row) +
  geom_tile(
    aes(
      x = col,
      y = row,
      fill = (stoppage_time_mean)^2
    ),
    color = 'white', 
    alpha = 0.6
  ) +
  ggtext::geom_richtext(
    label.color = NA,
    fill = NA,
    family = 'Karla',
    # size = pts(14),
    aes(label = lab)
  ) +
  # scale_fill_viridis_c(na.value = 'grey80', direction = 1, begin = 0.9, end = 0.1, option = 'B', alpha = 0.6) +
  # paletteer::scale_fill_paletteer_c('RColorBrewer::Reds') +
  scale_fill_distiller(palette = 'Purples', direction = 1, na.value = 'grey80') +
  geom_path(
    data = tibble(
      x = c(0, 3.5, 9, 9, 9, 9, 19),
      y = c(-10.5, -8.5, -8.5, 0, 0, -8, -8),
      grp = c(rep('a', 4), rep('b', 3))
    ),
    inherit.aes = FALSE,
    aes(x = x, y = y, group = grp)
  ) +
  f_text(
    aes(
      x = 9.5,
      y = -8.8,
      label = 'Using longer stoppage times in tied or 1-goal matches as a<br/>heuristic for aggressiveness and gamesmanship, it\'s evident<br/>that South American teams are the most aggressive in their<br/>intra-continental tournament games. There is<br/>evidence that some North American teams playing aggressivly,<br/>living up to their CONCACAF reputation.'
    ),
    hjust = 0,
    vjust = 1,
    # lineheight = 0.95,
    size = pts(11),
    color = 'grey20'
  ) +
  f_text(
    aes(
      x = 9.5,
      y = -12,
      label = "<i>**Data**:<br/>North America: '15, '17, '19, '21 CONCACAF Gold Cup<br/>South America: '15, '16, '19, '21 CONMEBOL Copa America<br/>Europe: '16, '21 UEFA European Championship<br/><br/>Invitees (e.g. Qatar for '21 Gold Cup) not included.<br/>Mininum: 6 matches.</i>",
    ),
    hjust = 0,
    vjust = 1,
    # lineheight = 0.95,
    size = pts(10),
    color = 'grey20'
  ) +
  f_text(
    size = pts(12),
    aes(label = '**Viz**: Tony ElHabr', x = 1, y = -14.5)
  ) +
  theme(
    plot.title = ggtext::element_markdown(size = 16),
    legend.position = 'none',
    axis.text = element_blank(),
    panel.grid.major = element_blank()
  ) +
  labs(
    title = 'Average 2nd half stoppage time when matches end in a draw or with a 1-goal difference',
    y = NULL,
    x = NULL
  ) +
  ggtext::geom_richtext(
    data = tibble(
      comp = c('North America', 'South America', 'Europe'),
      x = c(5.5, 0.5, 14),
      y = c(-2, -12, -2.5),
      hjust = c(0.5, 0, -1)
    ),
    label.color = NA,
    fill = NA,
    family = 'Karla',
    fontface = 'bold',
    inherit.aes = FALSE,
    size = pts(16),
    aes(label = comp, x = x, y = y, hjust = hjust)
  )

path_map <- file.path(dir_proj, 'stoppage_time_map.png')
h <- 7
ggsave(
  p,
  filename = path_map,
  width = h * 1.5,
  height = h,
  type = 'cairo'
)

add_logo(
  path_viz = path_map,
  path_logo = file.path(dir_proj, 'gabriel-jesus.png'), # choke-cropped.png'),
  delete = FALSE,
  logo_scale = 0.12,
  path_suffix = '_w_img',
  # idx_x = 0.54,
  # idx_y = 0.72
  idx_x = 0.01,
  idx_y = 0.01
)

# path_map_w_logo <- path_map %>% str_replace('[.]png', '_w_logo.png')
# add_logo(
#   path_viz = path_map_w_logo,
#   path_logo = path_tab,
#   path_suffix = '_w_tab',
#   delete = FALSE,
#   logo_scale = 0.3,
#   idx_x = -3,
#   idx_y = 0.05
# )

by_comp <-
  by_team %>% 
  group_by(comp) %>% 
  summarize(
    stoppage_time_mean = sum(n * stoppage_time_mean) / sum(n)
  ) %>% 
  ungroup()
by_comp
