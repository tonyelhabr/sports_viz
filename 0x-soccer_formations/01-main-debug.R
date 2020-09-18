

library(tidyverse)
forms <- 
  here::here('0x-soccer_formations', 'formationListAndPlayerLocation.csv') %>% 
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
  mutate(# Coercing to integer because join after voronoi calculation is inconsistent due to ".00" decimal places.
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
  mutate(idx_form = dense_rank(formation))
forms

forms_vs <-
  bind_rows(
    forms %>% mutate(side = 'home'),
    forms %>% mutate(side = 'away') %>% mutate(across(c(x, y), ~(-1 * .x + 100)))
  ) %>%
  mutate(across(side, ~ordered(.x, levels = c('home', 'away'))))

pal <- c('home' = 'blue', 'away' = 'red')
viz <-
  forms_vs %>% 
  filter(idx_form <= 2L) %>%
  # filter(away_form == '3-4-2-1', home_form == '4-2-3') %>% 
  # filter(side == 'away') %>% 
  ggplot() +
  aes(x = x, y = y) +
  ggsoccer::annotate_pitch(colour = 'black', fill = 'white') +
  geom_point(aes(color = side), alpha = 0.8) +
  ggrepel::geom_text_repel(
    aes(x = x + 0.02, y = y + 0.02, label = player, color = side),
    segment.color = 'black'
  ) +
  # ggnewscale::new_scale_color() +
  # facet_wrap(~formation) +
  facet_wrap(~idx_form) +
  gginnards::stat_debug_group() +
  ggforce::geom_voronoi_tile(
    aes(x = x, y = y, group = -1L, fill = side),
    color = 'black', alpha = 0.3, bound = c(0, 100, 0, 100)
  ) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal)
viz
