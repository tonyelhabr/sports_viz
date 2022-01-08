library(tidyverse)
library(sf)

df <- tribble(
  ~node,~y,  ~x,
  'b',   3, 1.5,
  'c',   3, 8.5,
  'd', 2.5,   3,
  'e', 2.5,   7,
  'f',   4,   2,
  'g',   4,   8,
  'h',   4,   4,
  'i',   4,   6,
  'j',   6, 3.5,
  'k',   7,   6
)

areas <- df %>% get_areas()
areas

areas$polygon_orig %>% plot(axes = TRUE, lwd = 1, lty = 2, border = 'orange')
points(areas$data_orig[[1]]$x, areas$data_orig[[1]]$y, col = 'orange', pch = 1)
points(areas$chull_orig[[1]]$x, areas$chull_orig[[1]]$y, col = 'orange', pch = 16)

areas$polygon_flip %>% plot(add = TRUE, lwd = 1, lty = 2, border = 'blue')
points(areas$data_flip[[1]]$x, areas$data_flip[[1]]$y, col = 'blue', pch = 1)
points(areas$chull_flip[[1]]$x, areas$chull_flip[[1]]$y, col = 'blue', pch = 16)

pi <- st_intersection(p1, p2)
# pd <- st_difference(p1, p2)
pd <- st_sym_difference(p1, p2)
areas$polygon_inner %>% plot(add = TRUE, col = 'green')
areas$polygon_outer %>% plot(add = TRUE, col = 'red')

areas %>% 
  transmute(
    area_inner,
    area_outer,
    area_prop = area_inner / (area_inner + area_outer)
  )

forms <- file.path('10-soccer_formations', 'formationListAndPlayerLocation.csv') %>% 
  read_csv(
    skip = 1L,
    col_names = c('row', 'player', 'x', 'y', 'formation'),
    col_types = cols(
      row = col_double(),
      player = col_integer(),
      x = col_double(),
      y = col_double(),
      formation = col_character()
    )
  ) %>% 
  select(-1) %>%
  # Coercing to integer because join after voronoi calculation is inconsistent due to ".00" decimal places.
  mutate(
    across(c(x, y), ~ (.x * 100) %>% as.integer()),
    across(formation, ~ str_replace_all(
      .x,
      c(
        'one' = '1-',
        'two' = '2-',
        'three' = '3-',
        'four' = '4-',
        'five' = '5-'
      )
    ))
  ) %>% 
  mutate(across(formation, ~str_remove(.x, '-$'))) %>% 
  mutate(idx_form = dense_rank(formation)) %>% 
  arrange(idx_form)
forms
forms %>% count(x, y, sort = TRUE)

formations <- forms %>% 
  distinct(formation) %>% 
  pull(formation)
formations

library(ggplot2)
library(ggsoccer)
p <- forms %>% 
  filter(idx_form == 1) %>% 
  ggplot() +
  aes(x = x, y = y) +
  ggsoccer::annotate_pitch(colour = 'black', fill = 'white') +
  # geom_point(
  #   shape = 21,
  #   color = 'black',
  #   size = 4,
  #   alpha = 1
  # ) +
  geom_text(
    aes(label = player),
    size = 4
  ) +
  coord_flip(xlim = c(1, 99), ylim = c(4, 96), clip = 'on') +
  theme(
    axis.title = element_text(size = 12, hjust = 0),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    y = NULL,
    x = NULL
  )
p
