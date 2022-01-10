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

cc <- df %>% get_concave_hull_areas(y_center = 5)
cv <- df %>% get_convex_hull_areas(y_center = 5)

cc$polygon_hull_orig %>% plot(axes = TRUE, lwd = 1, lty = 2, border = 'orange')
points(cc$data_orig[[1]]$x, cc$data_orig[[1]]$y, col = 'orange', pch = 1)
# points(cc$chull_orig[[1]]$x, cc$chull_orig[[1]]$y, col = 'orange', pch = 16)

cc$polygon_hull_flip %>% plot(axes = TRUE, add = TRUE, lwd = 1, lty = 2, border = 'blue')
points(cc$data_flip[[1]]$x, cc$data_flip[[1]]$y, col = 'blue', pch = 1)
# points(cc$chull_flip[[1]]$x, cc$chull_flip[[1]]$y, col = 'blue', pch = 16)

cc$polygon_inner %>% plot(add = TRUE, col = 'green')
cc$polygon_outer %>% plot(add = TRUE, col = 'red')

cv$polygon_hull_orig %>% plot(axes = TRUE, lwd = 1, lty = 2, border = 'orange')
points(cv$data_orig[[1]]$x, cv$data_orig[[1]]$y, col = 'orange', pch = 1)
# points(cv$chull_orig[[1]]$x, cv$chull_orig[[1]]$y, col = 'orange', pch = 16)

cv$polygon_hull_flip %>% plot(axes = TRUE, add = TRUE, lwd = 1, lty = 2, border = 'blue')
points(cv$data_flip[[1]]$x, cv$data_flip[[1]]$y, col = 'blue', pch = 1)
# points(cv$chull_flip[[1]]$x, cv$chull_flip[[1]]$y, col = 'blue', pch = 16)

cv$polygon_inner %>% plot(add = TRUE, col = 'green')
cv$polygon_outer %>% plot(add = TRUE, col = 'red')


cc %>% 
  transmute(
    area_inner,
    area_outer,
    area_prop = area_inner / (area_inner + area_outer)
  )

cv %>% 
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

do_get_areas <- function(.name) {
  f <- sprintf('get_%s_hull_areas', .name)
  fv <- function(name, ...) {
    cat(name, sep = '\n')
    exec(f, y_center = 50, ...)
  }
  path <- file.path(dir_proj, sprintf('%s_areas_nested-test.rds', .name))
  if(file.exists(path)) {
    return(read_rds(path))
  }
  areas_nested <- jittered_forms %>% 
    filter(player != 1) %>% 
    group_nest(idx_form, formation) %>% 
    mutate(
      areas = map2(formation, data, fv),
    )
  write_rds(areas_nested, path)
  areas_nested
}

concave_areas_nested <- do_get_areas('concave')
convex_areas_nested <- do_get_areas('convex')

.select_unnest <- function(df, ...) {
  df %>% 
    select(...) %>% 
    unnest(...)
}

plot_convex_area <- function(i) {
  i <- 1
  cv <- convex_areas_nested %>% filter(idx_form == i)
  cc <- concave_areas_nested %>% filter(idx_form == i)
  acv <- cv %>% .select_unnest(areas)
  acc <- cc %>% .select_unnest(areas)
  d <- cv %>% .select_unnest(data)
  
  p <- d %>% 
    ggplot() +
    aes(x = x, y = y) +
    ggsoccer::annotate_pitch(
      colour = 'black', 
      fill = 'white'
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
    ) +
    geom_point(
      data = acv$xy_hull_orig[[1]],
      aes(color = 'convex (1)')
    ) +
    geom_path(
      data = acv$xy_hull_orig[[1]],
      aes(color = 'convex (1)')
    ) +
    geom_hline(
      aes(yintercept = 50)
    ) +
    geom_point(
      data = acc$xy_hull_orig[[1]],
      aes(color = 'concave')
    ) +
    geom_path(
      data = acc$xy_hull_orig[[1]],
      aes(color = 'concave')
    ) +
    # geom_point(
    #   data = acc$xy_hull_flip[[1]],
    #   aes(color = 'concave flipped')
    # ) +
    # geom_path(
    #   data = acc$xy_hull_flip[[1]],
    #   aes(color = 'concave flipped')
    # ) + 
    geom_polygon(
      data = acc$xy_inner[[1]],
      aes(group = l2, fill = 'inner concave')
    ) +
    geom_polygon(
      data = acc$xy_outer[[1]],
      aes(group = l2, fill = 'outer concave')
    )
  
  if(nrow(acv) > 1) {
    p <- p +
      geom_point(
        data = acv$xy_hull_orig[[2]],
        aes(color = 'convex (2)')
      ) +
      geom_path(
        data = acv$xy_hull_orig[[2]],
        aes(color = 'convex (2)')
      )
  }
  
  p +
    scale_color_manual(
      values = c(
        'convex (1)' = 'magenta',
        'convex (2)' = 'yellow',
        'concave' = 'blue', 
        'concave flipped' = 'orange'
      )
    ) +
    scale_fill_manual(
      values = c(
        'inner concave' = 'green',
        'outer concave' = 'red'
      )
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
    prnk_diff = prnk_concave - prnk_convex,
    prnk_prnk = percent_rank(prnk_diff)
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
