
library(tidyverse)
library(ggsankey)
dir_proj <- '32-2020_euros'
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
  plot.caption = element_text('Karla', size = 12, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.01),
  # legend.text = element_text(color = 'gray20', size = 14),
  # strip.text = element_text(color = 'gray20', size = 14),
  # strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

players_filt <- file.path(dir_proj, 'players.csv') %>% read_csv()
players_filt

results <-
  file.path(dir_proj, 'results.csv') %>% 
  read_csv() %>%
  mutate(across(where(is.numeric), as.integer))
results

teams_n <-
  players_filt %>% 
  count(team, sort = TRUE) %>% 
  mutate(rnk = row_number(desc(n)))
teams_n %>% pull(team)

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
  )
net

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

z <-
  net %>% 
  mutate(team = node) %>% 
  group_by(node) %>% 
  mutate(
    lab = case_when(
      name == first(name) & x == 'Start' ~ team,
      TRUE ~ NA_character_
    )
  ) %>% 
  ungroup()
z %>% filter(!is.na(lab))

net_labs <-
  net %>% 
  group_by(x, node) %>% 
  mutate(idx = row_number()) %>% 
  mutate(med = floor(median(idx)), max = max(idx)) %>% 
  ungroup() %>% 
  # filter(idx == med) %>% 
  mutate(lab = case_when(x == 'Start' & idx == med ~ node, TRUE ~ NA_character_)) %>% 
  left_join(net)
net_labs
net_labs %>% drop_na(lab)
p <-
  net_labs %>% 
  ggplot() +
  aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = node) +
  geom_alluvial(alpha = 0.5) +
  geom_alluvial_text(
    aes(label = lab),
    hjust = 0,
    family = 'Karla',
    size = pts(12)
  ) +
  guides(fill = 'none') +
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
p
