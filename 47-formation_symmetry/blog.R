
#- setup, include=F, echo=F, cache=F ----
knitr::opts_chunk$set(
  include = FALSE,
  echo = FALSE,
  cache = FALSE,
  eval = FALSE,
  cache.lazy = FALSE,
  fig.show = 'hide',
  fig.align = 'center',
  fig.width = 8,
  fig.asp = 0.75,
  fig.retina = 2,
  warning = FALSE,
  message = FALSE
)

#- load ----
library(readr)
library(magrittr)
library(purrr)
dir_proj <- '47-formation_symmetry'
.f_import <- function(name) {
  path <- file.path(dir_proj, sprintf('%s.rds', name))
  res <- path %>% read_rds()
  assign(value = res, x = name, envir = .GlobalEnv)
}

c(
  'nodes',
  'edges',
  'team_stats',
  'team_season_stats',
  'meta'
) %>% 
  walk(.f_import)

#- setup-ggplot ----
library(ggplot2)
library(extrafont)
library(ggtext)
library(grid)

gray_wv <- rgb(24, 24, 24, maxColorValue = 255)
gray_grid_wv <- rgb(64, 64, 64, maxColorValue = 255)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla', color = 'white'),
  title = element_text('Karla', size = 14, color = 'white'),
  plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 24, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', size = 18, color = '#f1f1f1'),
  axis.text = element_text('Karla', color = 'white', size = 14),
  axis.title = element_text('Karla', size = 14, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = gray_grid_wv),
  panel.grid.minor = element_line(color = gray_grid_wv),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = gray_wv, color = gray_wv),
  plot.caption = element_text('Karla', color = 'white', hjust = 1, size = 11, face = 'italic'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'white', hjust = 0),
  plot.tag.position = c(0.01, 0.01),
  strip.text = element_text('Karla', color = 'white', face = 'bold', size = 12, hjust = 0),
  panel.background = element_rect(fill = gray_wv, color = gray_wv)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

#- example, echo=F, include=F, eval=T ----
library(tibble)
library(dplyr)
library(tidyr)
library(sdpt3r)

df <- tibble(
  from = c('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd'),
  to   = c('b', 'c', 'd', 'a', 'c', 'd', 'a', 'b', 'd', 'a', 'b', 'c'),
  n    = c( 1L,  0L,  3L,  1L,  1L,  1L,  0L,  2L,  1L,  1L,  5L,  4L)
)

wide_df <- df %>% 
  pivot_wider(
    names_from = to,
    values_from = n,
    values_fill = 0L
  ) %>% 
  select(from, a, b, c, d) %>% 
  arrange(from) %>% 
  select(-from)
wide_df
## # A tibble: 4 x 4
##       a     b     c     d
##   <int> <int> <int> <int>
## 1     0     1     0     3
## 2     1     0     1     1
## 3     0     2     0     1
## 4     1     5     4     0

m <- as.matrix(wide_df)
symmetric_m <- m + t(m) ## must be symmetric
mc <- maxcut(symmetric_m)
max_cut <- -round(mc$pobj, 0)
max_cut
## [1] 15

#- max-cut-plot-example-prep ----
library(network)
library(ggnetwork)

upper_symmetric_m <- symmetric_m
upper_symmetric_m[lower.tri(upper_symmetric_m)] <- 0
net <- network(upper_symmetric_m)
set.edge.attribute(
  net, 
  'n',
  upper_symmetric_m[upper.tri(upper_symmetric_m) & upper_symmetric_m > 0]
)

set.seed(42)
gg_net <- net %>% 
  ggnetwork() %>% 
  as_tibble() %>% 
  mutate(
    rn = row_number()
  )
rns <- c(5, 4, 2)
filt_edges <- gg_net %>%
  filter(
    rn %in% rns
  ) %>% 
  mutate(
    across(rn, ~factor(.x, levels = rns))
  ) %>% 
  arrange(rn) %>% 
  mutate(
    x_mid = (x + xend) / 2,
    y_mid = (y + yend) / 2
  ) %>% 
  mutate(
    x_next = lead(x_mid),
    y_next = lead(y_mid)
  ) %>% 
  select(rn, vertex.names, n, x_mid, y_mid, x_next, y_next)

adj_filt_edges <- filt_edges %>% 
  add_row(
    filt_edges %>%
      slice(1) %>% 
      mutate(
        x_next = x_mid,
        y_next = y_mid,
        across(x_mid, ~.x + 0.2)
      ),
    .before = 1
  ) %>% 
  mutate(
    across(
      x_next,
      ~ifelse(
        rn == rev(rns)[1],
        x_mid - 0.2,
        .x
      )
    ),
    across(
      y_next,
      ~ifelse(
        rn == rev(rns)[1],
        y_mid,
        .x
      )
    )
  )
adj_filt_edges

#- max-cut-plot-example ----
p_ex <- gg_net %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(curvature = 0) +
  geom_nodes(color = 'black', size = pts(36)) +
  geom_nodetext(
    data = gg_net %>% filter(is.na(n)),
    aes(label = vertex.names), 
    size = pts(16), 
    color = 'white',
    fontface = 'bold', 
    family = 'Karla'
  ) +
  geom_edgetext(
    aes(label = n), 
    size = pts(16),
    fill = NA,
    hjust = 1,
    vjust = 2,
    color = 'black',
    fontface = 'bold', 
    family = 'Karla'
  ) +
  geom_curve(
    data = adj_filt_edges,
    aes(
      x = x_mid,
      y = y_mid,
      xend = x_next,
      yend = y_next
    ),
    curvature = 0.1,
    size = 2,
    linetype = 2,
    color = 'red'
  ) +
  annotate(
    geom = 'text',
    x = 0.2,
    y = 0.5,
    hjust = 0,
    size = pts(16),
    color = 'red',
    label = 'max cut: 15',
    family = 'Karla',
    fontface = 'bold'
  ) +
  theme_blank() +
  labs(
    title = 'Example max cut formulation'
  ) +
  theme(
    plot.title = element_text('Karla', face = 'bold', size = 24, color = 'black', hjust = 0.5)
  )
p_ex

ggsave(
  plot = p_ex,
  filename = file.path(dir_proj, 'example_max_cut.png'),
  width = 6,
  height = 6
)

#- example-pass-network ----
library(ggsoccer)
library(ggrepel)
library(glue)

.prep_nodes_or_edges <- function(df, meta, ...) {
  df %>% 
    inner_join(
      meta %>%
        select(
          game_id,
          home_team_id,
          home_team_name,
          away_team_name,
          home_color_pri,
          away_color_pri
        ),
      by = 'game_id'
    ) %>% 
    mutate(
      is_home = team_id == home_team_id,
      team_name = ifelse(is_home, home_team_name, away_team_name),
      color_pri = ifelse(is_home, home_color_pri, away_color_pri),
    ) %>% 
    select(all_of(colnames(df)), team_name, color_pri) %>% 
    select(
      team_name, color_pri, n, ...
    )
}

.join_select_nodes_to_edges <- function(df, suffix) {
  df %>% 
    left_join(
      n %>% 
        select(player_id, x, y) %>% 
        rename_all(~sprintf('%s_%s', .x, suffix)),
      by = sprintf('player_id_%s', suffix)
    )
}

## Reference: https://github.com/Dato-Futbol/passing-networks/blob/master/soccerPassNetEventing.R#L83-L101
.adjust_edges_for_plot <- function(df, buffer = 1.5, offset = 1){
  
  df %>% 
    mutate(
      dx = x_end - x_start,
      dy = y_end - y_start,
      dist = sqrt(dx^2 + dy^2),
      px = dx / dist,
      py = dy / dist,
      across(x_start, ~.x + px * !!buffer),
      across(y_start, ~.x + py * !!buffer),
      across(x_end, ~.x - px * !!buffer),
      across(y_end, ~.x - py * !!buffer),
      across(x_start, ~.x - py * !!offset),
      across(x_end, ~.x - py * !!offset),
      across(y_start, ~.x - px * !!offset),
      across(y_end, ~.x - px * !!offset)
    ) %>% 
    select(all_of(colnames(df)))
}

.pitch_international <- list(
  length = 105,
  width = 68,
  penalty_box_length = 16.5,
  penalty_box_width = 40.32,
  six_yard_box_length = 5.5,
  six_yard_box_width = 18.32,
  penalty_spot_distance = 11,
  goal_width = 7.32,
  origin_x = 0,
  origin_y = 0
)

.common_gg <- function(...) {
  list(
    ...,
    aes(x = x, y = y),
    ggsoccer::annotate_pitch(
      dimensions = .pitch_international,
      colour = gray_grid_wv,
      fill = gray_wv
    ),
    coord_flip(ylim = c(0, 68), xlim = c(105, 0)),
    theme(
      axis.title = element_text(size = 12, hjust = 0),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ),
    labs(
      y = NULL,
      x = NULL
    )
  )
}

.arw <- arrow(type = 'closed', length = unit(0.1, 'inches'))
.ratio <- 68 / 105
.h <- 8

plot_pass_network <- function(game_id = 1549646, min_edges = 3) {
  
  meta_filt <- meta %>% 
    filter(game_id == !!game_id)
  
  .factor_team_name <- function(df) {
    df %>% 
      mutate(
        across(team_name, ~factor(.x, levels = c(meta_filt$home_team_name, meta_filt$away_team_name)))
      )
  }
  
  n <- nodes %>% 
    .prep_nodes_or_edges(meta_filt, player_id, name, x, y) %>% 
    .factor_team_name()
  
  e <- edges %>% 
    .prep_nodes_or_edges(meta_filt, player_id_start, player_id_end) %>% 
    .join_select_nodes_to_edges('start') %>% 
    .join_select_nodes_to_edges('end') %>% 
    .factor_team_name()

  p <- n %>% 
    ggplot() +
    .common_gg() +
    geom_point(
      # color = 'white',
      aes(color = team_name),
      show.legend = FALSE
    ) +
    scale_color_manual(
      values = c(
        meta_filt$home_color_pri,
        meta_filt$away_color_pri
      ) %>% 
        setNames(c(meta_filt$home_team_name, meta_filt$away_team_name))
    ) +
    geom_curve(
      color = 'white',
      curvature = 0.1,
      inherit.aes = FALSE,
      show.legend = FALSE,
      data = e %>% filter(n >= !!min_edges) %>% .adjust_edges_for_plot(),
      aes(x = x_start, y = y_start, xend = x_end, yend = y_end, alpha = n),
      arrow = .arw
    ) +
    scale_alpha(range = c(0.1, 0.5)) +
    ggrepel::geom_text_repel(
      color = 'white',
      family = 'Karla',
      fontface = 'bold',
      # size = pts(12),
      show.legend = FALSE,
      aes(label = name, size = n)
    ) +
    theme(
      plot.title = element_markdown(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      strip.text = element_blank()
    ) +
    scale_size(range = c(pts(12), pts(16))) +
    labs(
      subtitle = sprintf('%s, through first player change', lubridate::date(meta_filt$game_date)),
      title = glue::glue("<b><span style='color:{meta_filt$home_color_pri};'>{meta_filt$home_team_name}</span></b> {meta_filt$home_score} - {meta_filt$away_score} <b><span style='color:{meta_filt$away_color_pri};'>{meta_filt$away_team_name}</span></b>"),
      caption = sprintf('Transparency of edges and size of names reflect relative number of passes.\nMinimum number of successful passes: %d.', min_edges),
      tag = '**Viz**: Tony ElHabr'
    ) +
    facet_wrap(~team_name)
  
  path <- file.path(dir_proj, sprintf('pass_network-game_id=%s.png', game_id))
  ggsave(
    plot = p,
    filename = path,
    unit = 'in',
    height = .h,
    width = 2 * .ratio * .h
  )
  
  tonythemes:::add_logo(
    path_viz = path,
    path_logo = file.path(dir_proj, 'epl-logo-white.png'),
    delete = TRUE,
    logo_scale = 0.08,
    idx_x = 0.01,
    idx_y = 0.98,
    adjust_y = FALSE
  )
}

plot_pass_network(
  game_id = 1549646
)
