
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
  mutate(
    across(c(x, y), ~.x * 100),
    across(formation, ~str_replace_all(.x, c('one' = '1-', 'two' = '2-', 'three' = '3-', 'four' = '4-', 'five' = '5-')))
  ) %>% 
  mutate(idx_form = dense_rank(formation))
forms
formations <- forms %>% distinct(formation)
formations_filt <- 
  formations %>% 
  filter(formation %>% str_detect('[a-z]', negate = T)) %>% 
  mutate(across(formation, ~str_remove(.x, '-$'))) %>% 
  pull(formation)
formations_filt
pairs <-
  crossing(
    form1 = formations_filt,
    form2 = formations_filt
  ) %>% 
  filter(form1 < form2)
pairs



forms_vs <-
  bind_rows(
    forms %>% mutate(side = 'home'),
    forms %>% mutate(side = 'away') %>% mutate(across(c(x, y), ~(-1 * .x + 100)))
  ) %>% 
  mutate(across(side, ~ordered(.x, levels = c('home', 'away'))))
forms_vs

forms_vs
forms_vs %>% filter(side == 'away')

pal <- c('home' = 'blue', 'away' = 'red')
viz <-
  forms_vs %>% 
  filter(idx_form <= 1L) %>% 
  filter(player != 1) %>% 
  ggplot() +
  aes(x = x, y = y) +
  ggsoccer::annotate_pitch(colour = 'black', fill = 'white') +
  geom_point(aes(color = side), alpha = 0.8) +
  ggrepel::geom_text_repel(aes(x = x + 0.02, y = y + 0.02, label = player, color = side)) +
  # ggnewscale::new_scale_color() +
  ggforce::geom_voronoi_tile(
    inherit.aes = TRUE,
    aes(group = -1L, fill = side),
    color = 'black', alpha = 0.3, bound = c(17, 100 - 17, 0, 100)
  ) +
  # ggsoccer::annotate_pitch(colour = 'black', fill = NA) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  facet_wrap(~formation)
viz
viz$data
gb <- viz %>% ggplot_build()
gb
gb$data
