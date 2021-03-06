
# config ----
library(tidyverse)
library(ggtext)
library(patchwork)

dir_proj <- '0x-soccer_formations'
path_data <- fs::path(dir_proj, 'formationListAndPlayerLocation.csv')

dir_plot <- fs::path(dir_proj, 'plots')
fs::dir_create(dir_plot)
path_gif <- fs::path(dir_proj, 'soccer_formations.gif')
width_in <- 10L
height_in <- 8L
pxpi <- 96L
width_px <- 10L * pxpi
height_px <- height_in * pxpi

size_point <- 5
color_home <- 'cyan'
color_away <- 'red'
# colors <- viridis::viridis(2)
# color_home <- colors[1]
# color_away <- colors[2]
pal <- c('home' = color_home, 'away' = color_away)
color_text <- 'white'

theme_set(ggdark::dark_theme_minimal())
theme_update(
  text = element_text(family = 'Karla', color = 'white'),
  title = element_text('Karla', size = 18, color = 'white'),
  plot.title = element_text(face = 'bold', size = 18),
  plot.margin = margin(10, 10, 10, 10),
  # panel.grid = element_blank(),
  axis.text = element_text(color = 'white'),
  plot.caption = element_text('Karla', size = 12, hjust = 0),
  plot.caption.position = 'plot',
  panel.spacing = element_blank(),
  panel.grid.major = element_line(color = 'gray30'),
  panel.grid.minor = element_line(color = 'gray30'),
  plot.background = element_rect(fill = 'gray10', color = NA),
  panel.background = element_blank()
)
update_geom_defaults('text', list(family = 'Karla', size = 4, color = 'white'))
bound_vor <- c(0, 100, 0, 100)

# data prep ----
forms <- 
  path_data %>% 
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
  mutate(idx_form = dense_rank(formation))
forms

formations <- 
  forms %>% 
  distinct(formation) %>% 
  pull(formation)
formations

pairs <-
  crossing(
    away = formations,
    home = formations
  ) %>%
  mutate(idx_pair = row_number()) %>% 
  relocate(idx_pair)
pairs

pairs_forms <-
  pairs %>% 
  pivot_longer(-idx_pair, names_to = 'side', values_to = 'formation') %>% 
  inner_join(forms %>% select(-idx_form)) %>% 
  mutate(
    across(c(x, y), ~if_else(side == 'away', -1L * .x + 100L, .x)),
  ) %>%
  mutate(across(side, ~ordered(.x, levels = c('home', 'away'))))
pairs_forms

pairs_forms_nodup <-
  pairs_forms %>% 
  group_by(idx_pair, x, y) %>% 
  mutate(rn = row_number()) %>% 
  mutate(rn_max = max(rn)) %>% 
  ungroup() %>% 
  mutate(
    across(
      c(x), 
      ~if_else(rn_max > 1L & rn == 2L, .x + 2L, .x)
    )
  )
# pairs_forms_nodup %>% count(idx_pair, x, y) %>% filter(n > 1L) 

# main calcs ----
compute_vor <- function(data, bound = bound_vor, ...) {
  res_init <- deldir::deldir(data$x, data$y, rw = bound, suppressMsge = TRUE, ...)
  res_init <- res_init$summary %>% as_tibble()
  suppressMessages(res <- res_init %>% left_join(data))
  res
}


pairs_vor <-
  pairs_forms_nodup %>% 
  group_by(idx_pair) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(vor = map(data, compute_vor)) %>% 
  select(-data) %>% 
  unnest(vor)
pairs_vor
# pairs_forms_nodup %>% filter(idx_pair == 2L) %>% count(x, y, sort = T)

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

# pairs_vor_agg %>% filter(home_form == '4-2-3-1') %>% arrange(desc(home_ratio))

pairs_vor_agg_rnks <-
  pairs_vor_agg %>% 
  group_by(home_form) %>% 
  summarize(across(home_ratio, mean)) %>% 
  ungroup() %>% 
  mutate(rnk = row_number(desc(home_ratio))) %>% 
  arrange(rnk)
pairs_vor_agg_rnks

# tangent ----
# # Extra study done in response to question about sensitivity.
# set.seed(42L)
# seeds <- sample(1L:4L, size = nrow(pairs_forms_nodup), replace = TRUE) %>% tibble(seed = .)
# seeds
# n_jitter <- 5L
# grid_params <-
#   tibble(jitter = seq.int(2L, 10L, length.out = 5L), dummy = 1L) %>% 
#   full_join(seeds %>% mutate(dummy = 1L)) %>% 
#   select(-dummy)
# grid_params
# 
# pairs_forms_redux <-
#   grid_params %>% 
#   bind_cols(replicate(n_jitter, pairs_forms_nodup, simplify = FALSE) %>% reduce(bind_rows)) %>% 
#   mutate(
#     across(x, ~case_when(seed == 1L ~ .x + jitter, seed == 2L ~ .x - jitter, TRUE ~ .x)),
#     across(y, ~case_when(seed == 3L ~ .x + jitter, seed == 4L ~ .x - jitter, TRUE ~ .x))
#   ) %>% 
#   mutate(
#     across(c(x, y), ~case_when(.x <= 1L ~ 1L, .x >= 99L ~ 99L, TRUE ~ .x))
#   )
# 
# pairs_vor_redux <-
#   pairs_forms_redux %>% 
#   select(-seed) %>% 
#   group_by(idx_pair, jitter) %>% 
#   nest() %>% 
#   ungroup() %>% 
#   mutate(vor = map(data, compute_vor)) %>% 
#   select(-data) %>% 
#   unnest(vor)
# pairs_vor_redux
# 
# pairs_vor_agg_redux <-
#   pairs_vor_redux %>% 
#   group_by(idx_pair, jitter, side) %>% 
#   summarize(across(dir.area, sum)) %>% 
#   ungroup() %>% 
#   rename(area = dir.area) %>% 
#   pivot_wider(names_from = side, values_from = area) %>% 
#   mutate(total = away + home) %>% 
#   mutate(
#     away_ratio = away / total,
#     home_ratio = home / total
#   ) %>% 
#   mutate(home_diff = home_ratio - away_ratio) %>% 
#   inner_join(pairs %>% rename(home_form = home, away_form = away))
# pairs_vor_agg_redux
# 
# pairs_vor_agg_rnks_redux <-
#   pairs_vor_agg_redux %>% 
#   group_by(jitter, home_form) %>% 
#   summarize(across(home_ratio, mean)) %>% 
#   ungroup() %>% 
#   group_by(jitter) %>% 
#   mutate(rnk = row_number(desc(home_ratio))) %>% 
#   ungroup() %>% 
#   arrange(rnk)
# pairs_vor_agg_rnks_redux

# viz ----
# Make the base swarm plot so it doesn't need to be regenerated with every formation matchup.
viz_swarm_all <-
  pairs_vor_agg %>% 
  inner_join(pairs_vor_agg_rnks %>% select(home_form, rnk)) %>% 
  mutate(across(home_form, ~fct_reorder(.x, -rnk))) %>% 
  # filter(home_form > away_form) %>% 
  ggplot() +
  aes(x = home_ratio, y = home_form) +
  ggbeeswarm::geom_quasirandom(
    alpha = 0.5,
    groupOnX = FALSE,
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(
    axis.title.x = element_text(size = 14, hjust = 1),
    axis.text = element_text(size = 9)
  ) +
  labs(
    x = '% of Pitch Ownership',
    y = NULL
  )
viz_swarm_all

plot_pair_vor <- function(idx, path, overwrite = FALSE) {

  if(fs::file_exists(path) & !overwrite) {
    return(path)
  }
  cat(sprintf('Exporting to %s', path), sep = '\n')
  pair_vor_agg <- pairs_vor_agg %>% filter(idx_pair == idx)
  
  home_form <- pair_vor_agg[['home_form']]
  
  viz2 <-
    viz_swarm_all + 
    geom_point(
      data = pair_vor_agg, 
      aes(x = away_ratio, y = away_form),
      shape = 21,
      color = 'black',
      fill = color_away, 
      # color = color_away,
      size = size_point
    ) +
    geom_point(
      data = pair_vor_agg, 
      # color = color_home,
      shape = 21,
      color = 'black',
      fill = color_home, 
      size = size_point
    )
  viz2
  
  ratios <-
    pair_vor_agg %>% 
    select(matches('ratio$')) %>% 
    rename(away = away_ratio, home = home_ratio) %>% 
    pivot_longer(matches('^.*')) %>% 
    pull(value, name)
  r_h <- ratios[['home']]
  r_a <- ratios[['away']]
  sign <- 
    case_when(
      r_h > r_a ~ '>',
      r_h < r_a ~ '<',
      TRUE ~ '==',
    )
  
  data <- pairs_forms_nodup %>% filter(idx_pair == idx)
  sides <- data %>% distinct(side, formation) %>% pull(formation, side)
  h <- sides[['home']]
  a <- sides[['away']]
  
  viz1 <-
    data %>% 
    ggplot() +
    aes(x = x, y = y, group = side) +
    ggsoccer::annotate_pitch(colour = 'black', fill = 'white') +
    geom_point(
      aes(fill = side),
      shape = 21,
      color = 'black',
      size = 4,
      alpha = 1
    ) +
    ggforce::geom_voronoi_tile(
      aes(x = x, y = y, group = -1L, fill = side),
      color = 'gray10',
      size = 1,
      alpha = 0.2, 
      bound = bound_vor
    ) +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    guides(color = FALSE, fill = FALSE) +
    coord_flip(xlim = c(1, 99), ylim = c(4, 96), clip = 'on') +
    theme(
      axis.title = element_text(size = 12, hjust = 0),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(
      y = 'Viz: @TonyElHabr | Data: @SamGoldberg1882',
      x = NULL
    )
  viz1

  res <- 
    wrap_plots(viz1, viz2, widths = c(2, 1)) +
    plot_annotation(
      title = glue::glue('<span style="font-size:12pt; color:{color_text}">({scales::percent(r_h, accuracy = 0.1)})</span> <span style="font-size:22pt; color:{color_home}">{h}</span> {sign} <span style="font-size:22pt; color:{color_away}">{a}</span> <span style="font-size:12pt; color:{color_text}">({scales::percent(r_a, accuracy = 0.1)})</span>'),
      theme = theme(
        plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 18, color = color_text, hjust = 0.5)
      )
    )
  res
  
  ggsave(plot = res, filename = path, width = width_in, height = height_in, type = 'cairo')
  path %>% as.character()
}

res_viz <-
  pairs_vor_agg %>% 
  filter(home_ratio >= away_ratio) %>% 
  mutate(path = file.path(dir_plot, sprintf('%s_v_%s.png', home_form, away_form))) %>% 
  select(idx_pair, path) %>% 
  # slice(c(2:3)) %>% 
  mutate(path = map2_chr(idx_pair, path, plot_pair_vor))
res_viz

# gif ----
paths_viz <- res_viz %>% filter(path %>% str_detect('4-3-3-falsenineattack')) %>% pull(path)
paths_viz
set.seed(42)
paths_viz_random <- paths_viz %>% sample(size = 10, replace = TRUE)
# paths_viz_random <- 
#   dir_plot %>% 
#   fs::dir_ls(regex = '*png$') %>% 
#   as.character() %>% 
#   sample(size = length(.), replace = TRUE)

gifski::gifski(
  paths_viz_random,
  gif_file = path_gif,
  width = width_px,
  height = height_px,
  delay = 1 / 20
)

# Most lopsided?
pairs_vor_agg %>% arrange(desc(home_ratio))
# Closest to 50/50?
pairs_vor_agg %>% filter(home_ratio > 0.5) %>% arrange(home_ratio)
