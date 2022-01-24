
dir_proj <- '47-formation_symmetry'
source(file.path(dir_proj, '01-process.R'))

gray_wv <- rgb(24, 24, 24, maxColorValue = 255)
gray_grid_wv <- rgb(64, 64, 64, maxColorValue = 255)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla', color = 'white'),
  title = element_text('Karla', size = 14, color = 'white'),
  plot.title = element_text('Karla', face = 'bold', size = 24, color = 'white'),
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

.prep_nodes_or_edges <- function(df, game_id, ...) {
  df %>% 
    inner_join(
      meta %>% 
        filter(game_id == !!game_id) %>% 
        select(game_id, home_team_id, home_team_name, away_team_name),
      by = 'game_id'
    ) %>% 
    mutate(
      team_name = ifelse(team_id == home_team_id, home_team_name, away_team_name)
    ) %>% 
    select(all_of(colnames(df)), team_name) %>% 
    select(
      team_name, n, ...
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
.arw <- arrow(type = 'closed', length = unit(0.1, 'inches'))
.ratio <- 68 / 105
.h <- 10

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

plot_pass_network <- function(game_id = 1549646, ...) {
  n <- nodes %>% .prep_nodes_or_edges(game_id, player_id, name, x, y)

  e <- edges %>% 
    .prep_nodes_or_edges(game_id, player_id_start, player_id_end) %>% 
    .join_select_nodes_to_edges('start') %>% 
    .join_select_nodes_to_edges('end')

  
  p <- n %>% 
    ggplot() +
    aes(x = x, y = y) +
    ggsoccer::annotate_pitch(
      dimensions = .pitch_international,
      colour = gray_grid_wv,
      fill = gray_wv
    ) +
    coord_flip(ylim = c(0, 68), xlim = c(105, 0)) +
    labs(x = NULL, y = NULL) +
    theme(
      axis.title = element_text(size = 14, hjust = 0),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    geom_point(
      color = 'white',
      show.legend = FALSE
    ) +
    geom_curve(
      color = 'white',
      curvature = 0.1,
      inherit.aes = FALSE,
      show.legend = FALSE,
      data = e %>% filter(n >= 3) %>% .adjust_edges_for_plot(),
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
    scale_size(range = c(pts(12), pts(16))) +
    labs(
      ...
    ) +
    facet_wrap(~team_name)
  ggsave(
    plot = p,
    filename = file.path(dir_proj, sprintf('pass_network-game_id=%s.png', .game_id)),
    unit = 'in',
    height = .h,
    width = 2.05 * .ratio * .h
  )

}

plot_pass_network(
  game_id = 1549646,
  title = 'Manchester United 0 - 2 Manchester City',
  subtitle = "1' - 45', 2021-11-06, Premier League",
  tag = '**Viz**: Tony ElHabr',
  caption = 'Transparency of edges and size of names reflect relative number of passes.\nMinimum number of successful passes: 3.',
)
