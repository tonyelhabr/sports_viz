
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

results_long <-
  results %>% 
  mutate(
    across(matches('^r'), ~ifelse(.x == 1L, country, NA_character_))
  ) %>% 
  select(-country) %>% 
  pivot_longer(matches('^r'), names_to = 'round', values_to = 'country') %>%
  # mutate(
  #   across(round, ~str_remove(.x, '^r'))
  # ) %>% 
  drop_na(country)
results_long

lvls_round <- sprintf('r%d', c(24, 16, 8, 4, 2, 1))
net <-
  results_long %>% 
  inner_join(players_filt) %>% 
  rename(node = team, x = round) %>% 
  mutate(across(x, ~ordered(.x, levels = lvls_round))) %>%
  arrange(name, x) %>% 
  group_by(country, name, pos) %>% 
  mutate(
    next_x = lead(x)
  ) %>% 
  ungroup() %>% 
  mutate(
    across(next_x, ~ordered(.x, levels = lvls_round)),
    next_node = ifelse(is.na(next_x), NA_character_, node)
  )
net
net %>% filter(x == 'r1')

labs <-
  net %>% 
  filter(x == 'r24') %>% 
  group_by(node) %>% 
  filter(name == first(name)) %>% 
  ungroup()
labs

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

z <-
  net %>% 
  mutate(team = node) %>% 
  group_by(node) %>% 
  mutate(
    lab = case_when(
      name == first(name) & x == 'r24' ~ team,
      TRUE ~ NA_character_
    )
  ) %>% 
  ungroup()
z %>% filter(!is.na(lab))

p <-
  net %>% 
  ggplot() +
  aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = node) +
  geom_alluvial() +
  # scale_y_reverse() +
  geom_alluvial_text(
    hjust = 1,
    aes(label = ifelse(x %in% c('r24'), node, NA_character_))
  )
p
geom_sankey_text(
    # data = labs,
    data = 
      net %>% 
      mutate(team = node) %>% 
      group_by(node) %>% 
      mutate(
        lab = case_when(
          name == first(name) & x == 'r24' ~ team,
          TRUE ~ NA_character_
        )
      ) %>% 
      ungroup(),
    aes(label = lab),
    hjust = 1,
    family = 'Karla',
    size = pts(10)
  ) +
  guides(color = 'none') +
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
