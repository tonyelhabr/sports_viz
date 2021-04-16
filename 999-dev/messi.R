
library(tidyverse)
extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = element_text('Karla', face = 'bold', size = 18, color = 'gray20', hjust = 0),
  # plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', face = 'bold', size = 14, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  # axis.title = element_text(size = 24, face = 'bold'),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  # panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  plot.caption = element_text('Karla', size = 14, color = 'gray20', hjust = 0),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 14, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.02, 0.01),
  legend.text = element_text(size = 14),
  strip.text = element_text(size = 14),
  strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))
df <- understatr::get_player_seasons_stats(2097) %>% janitor::clean_names()
df

viz <-
  df %>% 
  mutate(across(year, factor)) %>% 
  ggplot() +
  aes(x = year) +
  geom_segment(aes(xend = year, y = x_g, yend = goals), color = 'black') +
  geom_point(aes(y = x_g), color = 'red', size = 4) +
  geom_point(aes(y = goals), color = 'blue', size = 4) +
  ylim(0, NA) +
  theme(
    plot.caption = ggtext::element_markdown(),
    plot.title = ggtext::element_markdown()
  ) +
  labs(
    x = NULL,
    y = NULL,
    caption = '**Viz**: Tony ElHabr',
    subtitle = 'Expected goals (xG) doesn\'t account for Messi\'s individual skill.',
    title = 'Lionel Messi\'s <b><span style="color:blue">Goals Scored</span></b> Almost Always Exceeds His <b><span style="color:red">Expected Goals</span></b>'
  )
viz

ggsave(
  plot = viz,
  filename = file.path('999-dev', 'messi.png'),
  width = 10,
  height = 6,
  type = 'cairo'
)

