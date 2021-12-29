
source('46-euro_fouls/01-setup.R')

library(ggfittext)
library(grid)

rage_quits <- all_actions %>% 
  filter(
    type_name == 'foul',
    turnover_player_id == player_id,
    time_since_last_turn <= 7
  ) %>% 
  arrange(desc(game_date))
rage_quits

n_rage_quits <- rage_quits %>% 
  count(competition_id, season_id, player_id, player_name, sort = TRUE) %>% 
  left_join(players_filt)
n_rage_quits

top_n_rage_quits <- n_rage_quits %>% 
  # filter(season_id > 2017) %>% 
  filter(n == 4) %>% 
  slice_min(
    total_mp,
    n = 4,
    with_ties = FALSE
  ) %>% 
  bind_rows(
    n_rage_quits %>% filter(n == 5)
  ) %>% 
  mutate(
    lab = sprintf("%s '%s", str_replace_all(player_name, '(^.*\\s)(.*$)', '\\2'), str_sub(season_id, 3, 4))
  )
cancelo_rage_quits <- top_n_rage_quits %>% filter(player_name == 'Joao Cancelo')
top_n_rage_quits_wo_cancelo <- top_n_rage_quits %>% anti_join(cancelo_rage_quits)
n_rage_quits_minus_top <- n_rage_quits %>% anti_join(top_n_rage_quits) 
pal <- c('#ef426f', '#00b2a9', '#ff8200', '#7a5195') %>% rev()
pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

base_plot <- function(data, point.size = 1, segment.size = 0.5, ...) {
  data %>% 
    ggplot() +
    aes(x = total_mp, y = n, alpha = n, color = factor(n)) +
    coord_polar() +
    geom_point(size = point.size) +
    geom_segment(
      size = segment.size,
      aes(xend = 0, yend = 0)
    ) +
    scale_color_manual(values = pal) +
    guides(color = 'none', alpha = 'none') +
    theme(
      axis.text.y = element_blank()
    ) +
    scale_x_continuous(
      limits = c(0, 4000), 
      labels = c('0', '1,000', '2,000 minutes played', '3,000', '4,000')
    )
}

add_labels <- function(..., .data, .color = 'white') {
  list(
    ...,
    geom_point(
      data = .data,
      size = 2,
      alpha = 1,
      color = .color
    ),
    geom_segment(
      data = .data,
      # alpha = 0.5,
      size = 1,
      alpha = 0.5,
      aes(xend = 0, yend = 0),
      color = .color
    ),
    ggrepel::geom_text_repel(
      data = .data,
      family = 'Karla',
      color = .color,
      size = pts(16),
      fontface = 'bold',
      seed = 42,
      alpha = 1,
      aes(label = lab, y = n + 0.7, x = total_mp - 100)
    )
  )
}

cancelo_blue <- '#6caddf'
p_rage_quit <- n_rage_quits_minus_top %>% 
  base_plot() +
  add_labels(.data = top_n_rage_quits_wo_cancelo, .color = 'white') +
  add_labels(.data = cancelo_rage_quits, .color = cancelo_blue) +
  geom_text(
    inherit.aes = FALSE,
    color = 'white',
    fontface = 'bold',
    size = pts(14),
    data = tibble(x = 100, y = 1:5) %>% 
      mutate(lab = ifelse(y == 5, '5 "rage" fouls', y)),
    aes(x = x, y = y, label = lab),
    nudge_x = 1,
    vjust = -1,
    hjust = 0
  ) +
  labs(
    y = NULL,
    x = NULL
  ) +
  theme(
    plot.title = element_text(hjust = 0),
    plot.subtitle = ggtext::element_markdown(hjust = 0),
    plot.caption = ggtext::element_markdown(size = 14, face = 'plain')
  ) +
  labs(
    title = 'Who "rage" fouls most frequently?',
    subtitle = glue::glue('<span style="color:{cancelo_blue}"><b>Cancelo</b></span> has commited the most "rage" fouls (4)<br>in the 2021/22 Premier League season.'),
    caption = '<i>"Rage" foul: a foul within 7 seconds of own turnover</i><br/><br/><b>Viz</b>: Tony ElHabr'
  )

path_rage_quit <- file.path(dir_proj, 'rage_quit.png')
ggsave(
  plot = p_rage_quit,
  filename = path_rage_quit,
  width = 10,
  height = 10
)

arw_annotate <- arrow(length = unit(5, 'pt'), type = 'closed')
p_inset <- tibble(
  total_mp = c(2700),
  n = 1:4
) %>% 
  base_plot(point.size = 3, segment.size = 2) +
  scale_alpha_continuous(range = c(1, 1)) +
  geom_fit_text(
    inherit.aes = FALSE,
    data = tibble(
      xmin = 100,
      xmax = 1200,
      ymin = 2,
      ymax = 3,
      lab = 'more minutes'
    ),
    color = 'white',
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = lab),
    min.size = 0, grow = TRUE
  ) +
  geom_segment(
    inherit.aes = FALSE,
    aes(x = 1250, xend = 1500, y = 2.5, yend = 2.5),
    linetype = 1,
    color = 'white',
    arrow = arw_annotate
  ) +
  geom_fit_text(
    inherit.aes = FALSE,
    data = tibble(
      xmin = 2700,
      xmax = 4000,
      ymin = 0.1,
      ymax = 3,
      lab = 'more fouls'
    ),
    color = 'white',
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = lab),
    min.size = 0, grow = TRUE
  ) +
  geom_segment(
    inherit.aes = FALSE,
    aes(x = 3330, xend = 3330, y = 2, yend = 4),
    linetype = 1,
    color = 'white',
    arrow = arw_annotate
  ) +
  theme(
    plot.background = element_rect(color = 'white'),
    axis.text = element_blank()
  ) +
  labs(x = NULL, y = NULL)
p_inset

path_inset <- file.path(dir_proj, 'rage_quit_insent.png')
ggsave(
  plot = p_inset,
  filename = path_inset,
  width = 8,
  height = 8
)

path_w_inset <- add_logo(
  path_viz = path_rage_quit,
  path_logo = path_inset,
  logo_scale = 0.25,
  idx_x = 0.99,
  idx_y = 0.01,
  adjust_x = FALSE,
  adjust_y = TRUE,
  path_suffix = '_w_inset',
  delete = TRUE
)

add_logo(
  path_viz = path_w_inset,
  path_logo = file.path(dir_proj, 'cancelo-eyes.png'),
  logo_scale = 0.2,
  idx_x = 0.01,
  idx_y = 0.99,
  adjust_x = TRUE,
  adjust_y = FALSE,
  path_suffix = '_and_cancelo',
  delete = TRUE
)
