library(tidyverse)
library(sf)
library(ggplot2)
library(ggsoccer)

dir_proj <- '47-formation_symmetry'
source(file.path(dir_proj, 'helpers.R'))
df <- tribble(
  ~player,~x,  ~y,
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

areas <- df %>% get_concave_hull_areas(y_center = 5)
areas

areas$polygon_orig %>% plot(axes = TRUE, lwd = 1, lty = 2, border = 'orange')
points(areas$data_orig[[1]]$x, areas$data_orig[[1]]$y, col = 'orange', pch = 1)
# points(areas$chull_orig[[1]]$x, areas$chull_orig[[1]]$y, col = 'orange', pch = 16)

areas$polygon_flip %>% plot(axes = TRUE, add = TRUE, lwd = 1, lty = 2, border = 'blue')
points(areas$data_flip[[1]]$x, areas$data_flip[[1]]$y, col = 'blue', pch = 1)
# points(areas$chull_flip[[1]]$x, areas$chull_flip[[1]]$y, col = 'blue', pch = 16)

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

set.seed(42)
jittered_forms <- forms %>% 
  mutate(
    across(x, ~.x + rnorm(n(), sd = 3)),
    across(y, ~.x + rnorm(n(), sd = 4))
  )

get_concave_hull_areas_verbosely <- function(name, ...) {
  cat(name, sep = '\n')
  get_concave_hull_areas(...)
}

do_get_areas <- function(f) {
  fv <- function(name, ...) {
    cat(name, sep = '\n')
    f(...)
  }
  jittered_forms %>% 
    filter(player != 1) %>% 
    group_nest(idx_form, formation) %>% 
    mutate(
      areas = map2(formation, data, fv),
    )
}

extract_areas <- function(df) {
  df %>% 
    select(idx_form, formation, areas) %>% 
    unnest(areas) %>% 
    transmute(
      idx_form,
      formation,
      area_inner, 
      area_outer,
      area_total = area_inner + area_outer,
      area_prop = area_inner / (area_inner + area_outer)
    )
}
concave_areas_nested <- do_get_areas(get_concave_hull_areas)
convex_areas_nested <- do_get_areas(get_convex_hull_areas)
concave_areas <- concave_areas_nested %>% extract_areas()
convex_areas <- convex_areas_nested %>% extract_areas()
agg_convex_areas <- convex_areas %>%  
  group_by(idx_form, formation) %>% 
  summarize(
    n = n(),
    area_prop = mean(area_inner / (area_inner + area_outer))
  ) %>% 
  ungroup() %>% 
  arrange(desc(area_prop))
agg_convex_areas

areas_compared <- full_join(
  concave_areas %>% select(idx_form, formation, area_prop_concave = area_prop),
  agg_convex_areas %>% select(idx_form, area_prop_convex = area_prop)
) %>% 
  mutate(
    prnk_concave = percent_rank(area_prop_concave),
    prnk_convex = percent_rank(area_prop_convex),
    prnk_diff = prnk_concave - prnk_convex
  ) %>% 
  arrange(desc(abs(prnk_diff)))
areas_compared

hulls <- nested_areas %>% 
  select(idx_form, formation, concave_areas) %>% 
  unnest(concave_areas) %>% 
  select(
    idx_form,
    formation,
    points_hull_orig
  ) %>% 
  unnest(points_hull_orig) %>% 
  group_by(idx_form) %>% 
  mutate(idx_intra = row_number()) %>% 
  ungroup()
hulls %>% filter(idx_intra == 11)

agg_area_props <- area_props %>%  
  group_by(idx_form, formation) %>% 
  summarize(
    n = n(),
    area_prop = mean(area_inner / (area_inner + area_outer))
  ) %>% 
  ungroup() %>% 
  arrange(desc(area_prop))
agg_area_props

area_props %>% 
  ggplot() +
  aes(x = area_total, y = area_prop) +
  geom_point(aes(color = factor(idx_intra)))

p <- jittered_forms %>% 
  filter(idx_form %in% c(37, 41)) %>% 
  filter(player != 1) %>% 
  ggplot() +
  aes(x = x, y = y) +
  ggsoccer::annotate_pitch(colour = 'black', fill = 'white') +
  geom_point(
    aes(color = formation),
    shape = 21,
    size = 5,
    alpha = 1
  ) +
  geom_text(
    aes(label = player, color = formation),
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
