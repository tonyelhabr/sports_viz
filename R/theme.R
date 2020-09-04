

extrafont::loadfonts(device = 'win', quiet = TRUE)
library(tidyverse)
library(here)
library(ggtext)

theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = element_text('Karla', face = 'bold', size = 18, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', face = 'bold', size = 14, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  # axis.title = element_text(size = 24, face = 'bold'),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  # axis.text = element_text('Karla', size = 12, face = 'bold', color = 'gray20'),
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
  plot.caption = element_text('Karla', size = 14, color = 'gray50', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = element_text('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.02),
  # legend.text = element_text(size = 14),
  legend.text = element_text(size = 14),
  # legend.background = element_rect(fill = '#F3F4F6', color = '#F3F4F6'),
  # legend.position = c(.85, .85))
  panel.background = element_rect(fill = '#fffaf0', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))
