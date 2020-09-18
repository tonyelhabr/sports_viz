
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

formations <- 
  forms %>% 
  distinct(formation) %>% 
  filter(formation %>% str_detect('[a-z]', negate = T)) %>% 
  pull(formation)
formations

pairs <-
  crossing(
    away = formations,
    home = formations
  ) %>%
  # filter(away < home) %>% 
  mutate(idx_pair = row_number()) %>% 
  relocate(idx_pair)
pairs

pairs_forms <-
  pairs %>% 
  pivot_longer(-idx_pair, names_to = 'side', values_to = 'formation') %>% 
  inner_join(forms %>% select(-idx_form)) %>% 
  mutate(
    across(c(x, y), ~ifelse(side == 'away', -1L * .x + 100L, .x))
  ) %>%
  mutate(across(side, ~ordered(.x, levels = c('home', 'away'))))
pairs_forms
pairs_forms %>% filter(idx_pair == 2L, player == 5L)

bound_vor <- c(0, 100, 0, 100)
compute_vor <- function(data, bound = bound_vor, ...) {
  res_init <- deldir::deldir(data$x, data$y, rw = bound, suppressMsge = TRUE, ...)
  res_init <- res_init$summary %>% as_tibble()
  suppressMessages(res <- res_init %>% left_join(data))
  res
}

pairs_vor <-
  pairs_forms %>% 
  group_by(idx_pair) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(vor = map(data, compute_vor)) %>% 
  select(-data) %>% 
  unnest(vor)
pairs_vor

pairs_vor_agg <-
  pairs_vor %>% 
  group_by(idx_pair, side) %>% 
  summarize(across(dir.area, sum)) %>% 
  ungroup() %>% 
  rename(area = dir.area) %>% 
  pivot_wider(names_from = side, values_from = area) %>% 
  mutate(total = away + home) %>% 
  mutate(
    away_ratio = away / total,
    home_ratio = home / total
  ) %>% 
  mutate(home_diff = home_ratio - away_ratio) %>% 
  inner_join(pairs %>% rename(home_form = home, away_form = away))
pairs_vor_agg
pairs_vor_agg %>% arrange(desc(home_ratio))
pairs_vor_agg %>% arrange(desc(abs(home_diff)))

color_home <- 'blue'
color_away <- 'red'
pal <- c('home' = color_home, 'away' = color_away)
library(ggtext)
plot_pair_vor <- function(idx) {
  idx = 2L
  data <- pairs_forms %>% filter(idx_pair == idx)
  sides <- data %>% distinct(side, formation) %>% pull(formation, side)
  viz <-
    data %>% 
    ggplot() +
    aes(x = x, y = y, group = side) +
    ggsoccer::annotate_pitch(colour = 'black', fill = 'white') +
    geom_point(
      aes(color = side), 
      size = 4,
      alpha = 0.8
    ) +
    ggrepel::geom_text_repel(
      aes(x = x + 0.04, y = y + 0.04, label = player, color = side),
      fontface = 'bold',
      family = 'Karla',
      segment.color = 'black'
    ) +
    ggforce::geom_voronoi_tile(
      aes(x = x, y = y, group = -1L, fill = side),
      color = 'black', 
      alpha = 0.3, 
      bound = bound_vor
    ) +
    # ggsoccer::annotate_pitch(colour = 'black', fill = NA) +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    guides(color = FALSE, fill = FALSE) +
    coord_cartesian(xlim = c(1, 99), ylim = c(4, 96), clip = 'on') +
    theme_minimal() +
    theme(
      text = element_text(family = 'Karla'),
      title = element_text('Karla', size = 14, color = 'gray20', margin = c(1, 1, 1, 1)),
      plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 18, color = 'gray20', hjust = 0.5, lineheight = 0.1),
      # plot.title.position = 'plot',
      # plot.margin = margin(25, 25, 25, 25),
      plot.margin = margin(10, 10, 10, 10),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      # axis.title = element_blank(),
      # plot.background = element_rect(fill = '#F3F4F6', color = '#F3F4F6'),
      # plot.caption = element_text(size = 15, face = 'italic'),
      plot.caption = element_text('Karla', size = 14, color = 'gray50', hjust = 1),
      plot.caption.position = 'plot',
      panel.spacing = element_blank(),
      plot.tag = element_text('Karla', size = 12, color = 'gray20', hjust = 0),
      plot.tag.position = c(.01, 0.02)
    ) +
    labs(
      x = NULL, y = NULL,
      # subtitle = 'bleh',
      title = glue::glue('<span style="color:{color_home}">{sides[["home"]]}</span> vs. <span style="color:{color_away}">{sides[["away"]]}</span>')
    )
  
  viz
}
