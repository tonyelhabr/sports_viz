
library(tidyverse)
# library(ggsankey)
dir_proj <- '32-2020_euros'
extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 18, color = 'gray20'),
  # plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown('Karla', face = 'bold', size = 16, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  plot.caption = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 1),
  # plot.caption = element_text('Karla', size = 12, color = 'gray20', hjust = 1),
  # plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.01),
  # legend.text = element_text(color = 'gray20', size = 14),
  # strip.text = element_text(color = 'gray20', size = 14),
  # strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

team_mapping <-
  xengagement::team_accounts_mapping %>% 
  mutate(team_short = team) %>% 
  mutate(
    across(team, ~str_replace_all(.x, c('Utd' = 'United', 'Man ' = 'Manchester ')))
  ) %>% 
  mutate(across(team, ~ifelse(.x == 'Newcastle', 'Newcastle United', .x))) %>% 
  select(team, team_short, color_pri)
team_mapping

players_filt <- 
  file.path(dir_proj, 'players.csv') %>% 
  read_csv() %>% 
  left_join(team_mapping %>% select(team, team_short)) %>% 
  select(-team) %>% 
  rename(team = team_short)
players_filt

results <-
  file.path(dir_proj, 'results.csv') %>% 
  read_csv() %>%
  mutate(across(where(is.numeric), as.integer))
results

teams_n <-
  players_filt %>% 
  count(team, sort = TRUE) %>% 
  mutate(rnk = row_number(desc(n))) %>% 
  left_join(
    team_mapping %>% 
      select(team = team_short, color_pri)
  )
teams_n

results_long <-
  results %>% 
  mutate(
    across(-c(country), ~ifelse(.x == 1L, country, NA_character_))
  ) %>% 
  select(-country) %>% 
  pivot_longer(matches('.*'), names_to = 'round', values_to = 'country') %>%
  drop_na(country)
results_long

order_rounds_at <- function(data, ...) {
  data %>% 
    mutate(
      across(!!!enquos(...), ~ordered(.x, levels = c('Start', 'Group', 'R16', 'Quarters', 'Semis', 'Title')))
    )
}

order_nodes_at <- function(data, ...) {
  data %>% 
    left_join(teams_n %>% select(node = team, rnk)) %>% 
    mutate(across(!!!enquos(...), ~fct_reorder(.x, rnk))) %>% 
    select(-rnk)
}

net <-
  results_long %>% 
  inner_join(players_filt) %>% 
  rename(node = team, x = round) %>% 
  order_rounds_at(x) %>%
  arrange(name, x) %>% 
  group_by(country, name, pos) %>% 
  mutate(
    next_x = lead(x)
  ) %>% 
  ungroup() %>% 
  order_rounds_at(next_x) %>% 
  mutate(
    next_node = ifelse(is.na(next_x), NA_character_, node)
  ) %>% 
  # left_join(teams_n %>% select(node = team, color_pri)) %>% 
  order_nodes_at(matches('node$')) 
net
teams_anti <- net %>% filter(x == 'Title') %>% distinct(node) %>% anti_join(teams_n %>% select(node = team), .)
net_filt <- net %>% anti_join(teams_anti)
net_filt
pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

pal <- teams_n %>% select(node = team, color_pri) %>% deframe()
pal
pal %>% scales::show_col()
pal %>% colorspace::lighten(amount = 0.2) %>% set_names(names(pal)) %>% scales::show_col()
p_init <-
  net_filt %>%
  # filter(x != 'Start') %>% 
  ggplot() +
  aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = node) +
  ggsankey::geom_alluvial() +
  guides(fill = 'none', colour = 'none') +
  # scale_fill_hue(values = pal, l = 90) +
  # scale_fill_identity() +
  scale_fill_manual(values = pal %>% colorspace::lighten(amount = 0.2) %>% set_names(names(pal))) +
  # scale_color_manual(values = pal) +
  scale_x_discrete(labels = c('', results %>% names() %>% setdiff(c('country', 'Start')))) +
  coord_cartesian(expand = TRUE, clip = 'off') +
  theme(
    panel.grid.major = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL
  )
p_init
gb <- p_init %>% ggplot_build()

labs <-
  gb$data[[2]] %>% 
  as_tibble() %>% 
  select(x, node, ymin, ymax, fill, colour) %>% 
  mutate(y = (ymin + ymax) / 2, diff = ymax - ymin) %>% 
  filter(x == '2')
labs

net %>% filter(x == 'Semis', node == 'Chelsea')
arw_annotate <- arrow(length = unit(3, 'pt'), type = 'closed')
p <-
  p_init + 
  geom_text(
    data = labs,
    inherit.aes = FALSE,
    hjust = 1,
    nudge_x = -0.05,
    fontface = 'bold',
    color = 'white',
    # fill = 'black',
    family = 'Karla',
    # size = pts(10),
    show.legend = FALSE,
    aes(x = x, y = y, label = node, size = diff)
  ) +
  scale_size(range = c(3, 6)) +
  ggtext::geom_richtext(
    label.color = NA,
    fill = NA,
    data = tibble(),
    inherit.aes = FALSE,
    aes(x = 'Semis', y = 60, label = 'Jorginho and Emerson (Chelsea)<br/>are the only Italian players.'),
    fontface = 'italic',
    family = 'Karla',
    # vjust = -1,
    hjust = 1,
    size = pts(11)
  ) +
  annotate(
    geom = 'curve',
    x = 'Semis',
    y = 60,
    yend = 30,
    xend = 'Title',
    size = 0.5,
    curvature = -0.25,
    arrow = arw_annotate
  ) +
  ggtext::geom_richtext(
    label.color = NA,
    fill = NA,
    data = tibble(),
    inherit.aes = FALSE,
    aes(x = 'Quarters', y = 95, label = 'Ramsdale and Johnstone (keepers) represent <br/>relegated Sheffield and West Brom.'),
    fontface = 'italic',
    family = 'Karla',
    # vjust = -1,
    hjust = 0,
    size = pts(11)
  ) +
  annotate(
    geom = 'curve',
    x = 'Quarters',
    y = 95,
    yend = 95,
    xend = 'R16',
    size = 0.5,
    curvature = 0,
    arrow = arw_annotate
  ) +
  labs(
    title = 'Premier League Representation at 2020 EUROs',
    subtitle = 'Big clubs have the most players in the tournament... who would have guessed?',
    tag = '**Viz**: Tony ElHabr',
    caption = glue::glue("<i>{glue::glue_collapse(teams_anti$node, sep = ', ', last = ' and ')}<br/>are not shownbecause they have no remaining representation.</i>")
  )
p

path_viz <- file.path(dir_proj, 'viz_sankey.png')
ggsave(
  plot = p,
  filename = path_viz,
  width = 10,
  height = 7.5,
  type = 'cairo'
)

add_logo(
  path_viz = path_viz,
  path_logo = file.path(dir_proj, 'euros-2020.png'),
  logo_scale = 0.08,
  # delete = FALSE,
  idx_x = 0.02,
  adjust_y = FALSE,
  idx_y = 0.98
)

