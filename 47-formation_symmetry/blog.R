
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
  # 'max_cuts_weighted',
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
update_geom_defaults('point', list(color = 'white'))

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

.join_select_nodes_to_edges <- function(df, nodes, suffix) {
  df %>% 
    left_join(
      nodes %>% 
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

stat_labs <- file.path(dir_proj, 'stats.csv') %>% read_csv()
team_stats_w_labs <- team_stats %>% left_join(stat_labs)

add_white_epl_logo <- function(path) {
  tonythemes:::add_logo(
    path_viz = path,
    path_logo = file.path(dir_proj, 'epl-logo-white.png'),
    delete = TRUE,
    logo_scale = 0.1,
    idx_x = 0.01,
    idx_y = 0.99,
    adjust_y = FALSE
  )
}

plot_pass_network <- function(
  game_id = 1549604,
  team_id = NULL,
  # include_max_cuts = TRUE,
  # include_symmetry = FALSE,
  stats = c('max_cut_weighted_norm'),
  min_edges = 3
) {
  # game_id = 1549646
  # team_id = NULL
  # # include_max_cuts = TRUE
  # # include_symmetry = FALSE
  # stats = c('max_cut_weighted', 'concave_area_prop')
  # min_edges = 3
  meta_filt <- meta %>% filter(game_id == !!game_id)
  
  filename <- sprintf('network-game_id=%s', game_id)
  if(!is.null(team_id)) {
    meta_filt <- meta_filt %>% filter(team_id == !!team_id)
    filename <- sprintf('%s-team_id=%s', filename, team_id)
  }
  
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
    .join_select_nodes_to_edges(n, 'start') %>% 
    .join_select_nodes_to_edges(n, 'end') %>% 
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
      aes(label = str_replace(name, '^(.*?\\s)(.*$)', '\\2'), size = n)
    ) +
    theme(
      plot.title = element_markdown(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      strip.text = element_blank()
    ) +
    scale_size(range = c(pts(12), pts(16))) +
    labs(
      # tag = '**Viz**: Tony ElHabr',
      subtitle = sprintf('%s, through first player change', lubridate::date(meta_filt$game_date)),
      title = glue::glue("<b><span style='color:{meta_filt$home_color_pri};'>{meta_filt$home_team_name}</span></b> {meta_filt$home_score} - {meta_filt$away_score} <b><span style='color:{meta_filt$away_color_pri};'>{meta_filt$away_team_name}</span></b>"),
      caption = sprintf('Transparency of edges and size of names reflect relative number of passes.\nMinimum number of successful passes: %d.', min_edges)
    ) +
    facet_wrap(~team_name)
  
  if(!is.null(stats) && length(stats) > 0) {
    filename <- sprintf('%s-%s', filename, paste0(stats, collapse = '+'))
    extra_df <- team_stats_w_labs %>% 
      filter(stat %in% stats) %>% 
      mutate(
        across(stat, ~ordered(.x, levels = stats))
      ) %>% 
      arrange(stat) %>% 
      inner_join(
        meta_filt %>%
          select(
            game_id,
            home_team_id,
            home_team_name,
            away_team_name
          ),
        by = 'game_id'
      ) %>% 
      mutate(
        lab = sprintf('<b>%s</b>: %s%s', stat_lab, round(value, digits), ifelse(is_percent, '%', ''))
      ) %>% 
      group_by(team_name) %>% 
      summarize(
        across(lab, paste0, collapse = '<br/>')
      ) %>% 
      ungroup()
    
    p <- p +
      ggtext::geom_richtext(
        # inherit.aes = FALSE,
        # fontface = 'bold',
        fill = NA_character_,
        label.color = NA_character_,
        vjust = 1,
        hjust = 0,
        color = 'white',
        family = 'Karla',
        size = pts(14),
        data = extra_df,
        aes(
          x = 2,
          y = 2,
          label = lab
        )
      )
  }
  
  path <- file.path(dir_proj, sprintf('%s.png', filename))
  ggsave(
    plot = p,
    filename = path,
    unit = 'in',
    height = .h,
    width = 2 * .ratio * .h
  )
  add_white_epl_logo(path)
  p
}

plot_pass_network(
  game_id = 1549604,
  stats =  c('max_cut_weighted_norm'),
  min_edges = 4
)

#- cors ----
library(stringr)
library(tidyr)
library(corrr)

wide_team_stats <- team_stats %>% 
  pivot_wider(
    names_from = stat,
    values_from = value
  )

wide_team_season_stats <- team_season_stats %>% 
  pivot_wider(
    names_from = stat,
    values_from = value
  )
.str_replace_cor_col <- function(x, i) {
  str_replace_all(x, '(diff|home|away)_(.*$)', sprintf('\\%d', i))
}

do_tidy_cor <- function(data) {
  data %>% 
    corrr::correlate(quiet = TRUE) %>% 
    corrr::stretch() %>% 
    filter(!is.na(r), abs(r) != 1) %>% 
    mutate(
      across(
        c(x, y),
        list(
          prefix = ~.str_replace_cor_col(.x, 1),
          suffix = ~.str_replace_cor_col(.x, 2)
        )
      )
    ) %>% 
    filter(x_suffix != y_suffix) %>% 
    select(x, y, x_prefix, x_suffix, y_prefix, y_suffix, r) %>% 
    arrange(desc(abs(r)))
}

y_stats <- c(
  # 'n_shots_norm',
  'prop_shots',
  # 'n_passes_norm',
  'prop_passes',
  # 'n_f3_passes_norm',
  'prop_f3_passes',
  'max_cut_weighted_norm',
  'mean_distance',
  'mean_edge_betweenness',
  'mean_node_betweenness',
  'mean_node_degree_in',
  'mean_node_degree_out'
)

do_tidy_xg_cor <- function(df, label = FALSE) {
  cors <- df %>% 
    do_tidy_cor() %>% 
    # filter(x %in% c('xg_norm', 'score_norm', 'diff_xg_norm', 'diff_score')) %>% 
    filter(x %in% c('diff_xg_norm')) %>% 
    filter(
      y %in% y_stats
    ) %>% 
    select(x, y, r) %>% 
    arrange(desc(abs(r)))
  
  if(!label) {
    return(
      cors %>% 
        pivot_wider(
          names_from = x,
          values_from = r
        )
    )
  }
  
  cors %>% 
    left_join(
      stat_labs %>% 
        select(y = stat, y_lab = stat_lab)
    ) %>% 
    left_join(
      stat_labs %>% 
        select(x = stat, x_lab = stat_lab)
    ) %>% 
    select(x_lab, y_lab, r) %>% 
    pivot_wider(
      names_from = x_lab,
      values_from = r
    )
}

team_xg_cors <- wide_team_stats %>%
  select(-c(
    season_id,
    game_id,
    team_name,
    team_id,
    last_min,
    diff_last_min,
    side,
    color_pri
  )) %>% 
  do_tidy_xg_cor()

team_season_xg_cors <- wide_team_season_stats %>%
  select(-c(
    season_id, 
    team_name, 
    team_id, 
    last_min, 
    n, 
    diff_last_min
  )) %>% 
  do_tidy_xg_cor()

#- hist-mc ----
lab_subtitle <- '2017/18 - 2021/22 Boxing Day'
p_mc_hist <- wide_team_stats %>% 
  ggplot() +
  aes(x = max_cut_weighted_norm) +
  geom_histogram(binwidth = 20, fill = 'white') +
  labs(
    title = 'Game-level Weighted Max Cut',
    subtitle = lab_subtitle,
    x = 'Weighted Max Cut Per 90 Min.',
    y = NULL
  )
p_mc_hist

path_mc_hist <- file.path(dir_proj, 'game_mc_hist.png')
ggsave(
  plot = p_mc_hist,
  filename = path_mc_hist,
  width = 7,
  height = 7 / 1.5
)
add_white_epl_logo(path_mc_hist)

#- scatter-mc-vs-xg ----
pal <- c('#ef426f', '#00b2a9', '#ff8200', '#7a5195')
game_cor_color <- '#00b2a9'
season_cor_color <- '#7a5195'
p_mc_xg_scatter <- wide_team_stats %>% 
  ggplot() +
  aes(x = max_cut_weighted_norm, y = diff_xg_norm) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    size = 2,
    color = game_cor_color,
    method = 'lm',
    se = FALSE
  ) +
  ggtext::geom_richtext(
    data = team_xg_cors %>%
      filter(y == 'max_cut_weighted_norm') %>% 
      select(lab = diff_xg_norm),
    fill = NA_character_,
    label.color = NA_character_,
    hjust = 1,
    color = game_cor_color,
    family = 'Karla',
    size = pts(18),
    aes(x = 600, y = 8, label = sprintf('<b>Correlation</b>: %.0f%%', 100 * lab))
  ) +
  labs(
    title = 'Game-level Weighted Max Cut vs. xG Diff.',
    subtitle = lab_subtitle,
    caption = 'Each point represents one team in one game.',
    x = 'Weighted Max Cut Diff. Per 90 Min.',
    y = 'xG Diff. Per 90 Min.'
  )
p_mc_xg_scatter

path_mc_xg_scatter <- file.path(dir_proj, 'game_mc_xg_scatter.png')
ggsave(
  plot = p_mc_xg_scatter,
  filename = path_mc_xg_scatter,
  width = 7,
  height = 7
)
add_white_epl_logo(path_mc_xg_scatter)

# game-cors-table ----
library(gt)
library(gtExtras)

.gt_theme_538 <- function(data,...) {
  data %>%
    gt::opt_table_font(
      font = list(
        gt::google_font('Karla'),
        gt::default_fonts()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = 'bottom', color = 'transparent', weight = gt::px(2)
      ),
      locations = gt::cells_column_labels(
        columns = gt::everything()
      )
    )  %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = 'bottom', color = 'black', weight = px(1)
      ),
      locations = gt::cells_row_groups()
    ) %>%
    gt::tab_options(
      column_labels.background.color = 'white',
      heading.border.bottom.style = 'none',
      table.border.top.width = gt::px(3),
      table.border.top.style = 'none', #transparent
      table.border.bottom.style = 'none',
      # column_labels.font.weight = 'normal',
      column_labels.border.top.style = 'none',
      column_labels.border.bottom.width = gt::px(0),
      # column_labels.border.bottom.color = 'black',
      # row_group.border.top.style = 'none',
      # row_group.border.top.color = 'black',
      # row_group.border.bottom.width = px(1),
      # row_group.border.bottom.color = 'white',
      stub.border.color = 'white',
      stub.border.width = gt::px(0),
      data_row.padding = gt::px(1), # px(3),
      source_notes.font.size = 10,  # 12,
      source_notes.border.lr.style = 'none',
      table.font.size = 16,
      heading.align = 'left',
      
      footnotes.font.size = 12,
      footnotes.padding = gt::px(0),
      row_group.font.weight = 'bold',
      
      ...
    ) %>% opt_css(
      "tbody tr:last-child {
    border-bottom: 2px solid #ffffff00;
      }
    
    ",
    add = TRUE
    )
}

.highlight_gt_row <- function(gt) {
  gt %>% 
    gt::tab_style(
      location = cells_body(
        rows = y_lab == 'Weighted Max Cut / 90',
      ),
      style = list(
        gt::cell_text(weight = 'bold', style = 'italic')
      )
    )
}

.finish_gt <- function(gt) {
  gt %>% 
    .highlight_gt_row() %>% 
    .gt_theme_538()
}

.add_gt_bar <- function(gt, col, color) {
  gt %>% 
    gtExtras::gt_plt_bar(
      color = color,
      column = all_of(col),
      scaled = FALSE, 
      width = 20
    )
}

team_xg_cors_tb <- team_xg_cors %>%
  filter(
    y %in% y_stats
  ) %>% 
  left_join(
    stat_labs %>% select(y = stat, y_lab = stat_lab, stat_group_lab)
  ) %>% 
  select(stat_group_lab, y_lab, r = diff_xg_norm) %>% 
  mutate(r2 = r) %>% 
  group_by(stat_group_lab) %>% 
  gt::gt(rowname_col = 'Group') %>% 
  gt::cols_label(
    .list = list(
      y_lab = '',
      r = '',
      r2 = ''
    )
  ) %>% 
  gt::fmt_percent(
    decimals = 0,
    columns = c(r)
  ) %>% 
  .add_gt_bar(
    'r2',
    game_cor_color
  ) %>% 
  gt::tab_header(
    title = gt::html('<b>Game-level Correlation with xG Diff. / 90</b>')
  ) %>% 
  .finish_gt()
team_xg_cors_tb

gt::gtsave(
  team_xg_cors_tb,
  filename = file.path(dir_proj, 'game_xg_cors_table.png')
)

# table-2 ----
team_xg_cors_tb <- team_xg_cors %>%
  rename(
    game_r = diff_xg_norm
  ) %>% 
  left_join(
    team_season_xg_cors %>% 
      rename(season_r = diff_xg_norm)
  ) %>% 
  left_join(
    stat_labs %>% select(y = stat, y_lab = stat_lab, stat_group_lab)
  ) %>% 
  select(stat_group_lab, y_lab, game_r, season_r) %>% 
  mutate(game_r2 = game_r, season_r2 = season_r) %>% 
  group_by(stat_group_lab) %>% 
  gt::gt(rowname_col = 'Group') %>% 
  gt::cols_label(
    .list = list(
      y_lab = '',
      game_r = gt::html('<b>Game</b>'),
      game_r2 = '',
      season_r = gt::html('<b>Season</b>'),
      season_r2 = ''
    )
  ) %>% 
  gt::fmt_percent(
    decimals = 0,
    columns = c(game_r, season_r)
  ) %>% 
  gtExtras::gt_plt_bar(
    color = game_cor_color,
    column = game_r2,
    scaled = FALSE, 
    width = 20
  ) %>% 
  gtExtras::gt_plt_bar(
    color = season_cor_color,
    column = season_r2,
    scaled = FALSE, 
    width = 20
  ) %>% 
  gt::cols_move(
    columns = game_r2,
    after = game_r
  ) %>% 
  gt::cols_move(
    columns = season_r2,
    after = season_r
  ) %>% 
  gt::tab_header(
    title = gt::html('<b>Correlation with xG Diff. / 90</b>')
  ) %>% 
  .finish_gt()
team_xg_cors_tb

gt::gtsave(
  team_xg_cors_tb,
  filename = file.path(dir_proj, 'game_and_season_xg_cors_table.png')
)


# moar ----
wide_team_stats %>% 
  select(diff_xg, max_cut_weighted_norm) %>% 
  corrr::correlate()

wide_team_season_stats %>% 
  select(diff_xg_norm, max_cut_weighted_norm) %>% 
  corrr::correlate()


wide_team_season_stats %>% 
  mutate(
    across(
      c(
        # diff_xg_norm,
        diff_max_cut_unweighted_norm,
        diff_max_cut_weighted_norm,
        diff_prop_passes,
        diff_field_tilt,
        diff_prop_shots
      ),
      ~(.x - mean(.x)) / sd(.x)
    )
  ) %>% 
  # select(
  #   diff_xg_norm, diff_max_cut_weighted_norm, diff_prop_passes
  # ) %>% 
  # lm(diff_xg_norm ~ diff_max_cut_weighted_norm, data = .) %>% 
  # lm(diff_xg_norm ~ diff_field_tilt, data = .) %>% 
  lm(diff_xg_norm ~ diff_prop_passes, data = .) %>% 
  broom::tidy() %>% 
  mutate(
    across(estimate, round, 3)
  )

wide_team_season_stats %>%
  select(
    diff_xg_norm,
    diff_max_cut_weighted_norm,
    diff_max_cut_unweighted_norm,
    diff_field_tilt,
    diff_prop_passes,
    diff_prop_shots
  ) %>%
  corrr::correlate()

wide_team_stats %>% 
  ggplot() +
  aes(x = diff_prop_passes, y = diff_max_cut_unweighted_norm) +
  geom_point(aes(color = diff_xg_norm, alpha = diff_xg_norm)) +
  scale_color_viridis_c()

wide_team_stats %>% 
  ggplot() +
  aes(x = xg_norm, y = max_cut_unweighted_norm) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    color = 'magenta',
    se = FALSE
  ) +
  # coord_cartesian(
  #   x = c(-4, 4),
  #   y = c(-8, 8)
  # ) +
  labs(
    x = 'xG / 90 Min.',
    y = 'Max Cuts / Min.'
  )

wide_team_stats %>% 
  ggplot() +
  aes(x = diff_xg_norm, y = diff_max_cut_unweighted_norm) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    color = 'magenta',
    se = FALSE
  ) +
  # coord_cartesian(
  #   x = c(-4, 4),
  #   y = c(-8, 8)
  # ) +
  labs(
    x = 'xG Diff. / 90 Min.',
    y = 'Max Cuts Diff. / Min.'
  )

wide_team_stats %>% 
  select(diff_xg, z = max_cut_weighted / last_min) %>% 
  corrr::correlate()
wide_team_stats %>% 
  select(xg, z = max_cut_weighted) %>% 
  corrr::correlate()
wide_team_stats %>% 
  ggplot() +
  aes(x = diff_xg, y = max_cut_weighted / last_min) +
  geom_point()
wide_team_stats %>% 
  ggplot() +
  aes(x = last_min, y = max_cut_weighted) +
  geom_point()
wide_team_stats %>% 
  ggplot() +
  aes(x = concave_area_prop, y = max_cut_weighted) +
  geom_point()
