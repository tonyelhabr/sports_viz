## Reference: https://twitter.com/sonofacorner/status/1581956939151380481
## Reference: https://github.com/sonofacorner/soc-viz-of-the-week/blob/main/10172022/10172022.ipynb
library(ggplot2)
library(ggsoccer)
library(extrafont)
library(ggpattern)
library(grid)
library(cowplot)
library(magick)
library(ggforce)
library(tidyverse)

dir_proj <- '61-sonofacorner_shotmap'
sandpaper_background_color <- '#EFE9E6'
gray_points <- '#4d4d4d'
gray_text <- '#999999'
font <- 'Titillium Web'
extrafont::loadfonts(quiet = TRUE)

pal <- c(
  '#d0d6d4',
  '#c5d0cd',
  '#bbcac7',
  '#b0c3c1',
  '#a6bdbb',
  '#9bb7b5',
  '#91b1af',
  '#86aaa8',
  '#7ca4a2',
  '#719e9c',
  '#679896',
  '#5c9190',
  '#528b8a',
  '#478583',
  '#3d7f7d',
  '#327877',
  '#287271'
)

pitch_length <- ggsoccer::pitch_international$length
pitch_width <- ggsoccer::pitch_international$width
half_pitch_length <- pitch_length / 2
half_pitch_width <- pitch_width / 2

x_buffer <- 8.5
hex_width <- 4
stats <- c('goals', 'xG', 'shots', 'xG/shot')
## Reference: https://github.com/gkaramanis/tidytuesday/blob/master/2020/2020-week36/crops.R
base_hex_xy <- tibble(
  stat = stats,
  # final_y = (54 * 0:3 * 14) - (hex_width / 2),
  x = 7 + x_buffer + half_pitch_length,
  y = c(11, 23, 42, 54) - (hex_width / 2)
)

hex_xy <- base_hex_xy |> 
  rowwise() |>
  mutate(
    x = list(c(0, 0, !!hex_width, !!hex_width)),
    y = list(c(0, !!hex_width, !!hex_width, 0)),
    isox_bottom = list(x - y),
    isoy_bottom = list((x + y) / 2),
    isox_top = list(isox_bottom),
    isoy_top = list(isoy_bottom + !!hex_width),
    isox = list(c(isox_bottom[c(2, 1, 4)], isox_top[c(4, 3, 2)])),
    isoy = list(c(isoy_bottom[c(2, 1, 4)], isoy_top[c(4, 3, 2)]))
  )  |>
  ungroup() |>
  unnest(c(isox, isoy)) |>
  select(-matches('iso[xy]_'), -c(x, y)) |>
  inner_join(
    base_hex_xy,
    by = 'stat'
  )

players_of_interest <- c(
  'Andre-Pierre Gignac',
  'Juan Dinenno',
  'Ernesto Vega',
  'Nicolás Ibánez',
  'Harold Preciado',
  'Leonardo Fernandez'
)

df <- read_csv('https://raw.githubusercontent.com/sonofacorner/soc-viz-of-the-week/main/10172022/data/10172022.csv') |> 
  filter(situation != 'Penalty', !isOwnGoal) |> 
  transmute(
    player_name = playerName,
    team_id = teamId,
    team_name = teamName,
    x, y,
    xG,
    goal = ifelse(eventType == 'Goal', 1L, 0L)
  ) |> 
  filter(
    player_name %in% players_of_interest
  ) |> 
  mutate(
    across(player_name, ~ordered(toupper(.x), levels = toupper(players_of_interest))),
    ## reflect about the middle of the goal
    across(
      y, 
      ~ifelse(
        .x > half_pitch_width, 
        half_pitch_width - (.x - half_pitch_width), 
        half_pitch_width + (half_pitch_width - .x))
    )
  )

agg <- df |> 
  group_by(player_name) |> 
  summarize(
    shots = n(),
    xG = sum(xG),
    goals = sum(goal),
    median_x_yards = (18 / 16.5) * (105 - median(x))
  ) |> 
  ungroup() |> 
  mutate(
    `xG/shot` = xG / shots
  )

agg_xy <- agg |> 
  pivot_longer(
    -c(player_name),
    names_to = 'stat',
    values_to = 'value'
  ) |> 
  filter(stat %in% !!stats) |> 
  mutate(
    value_lab = ifelse(
      stat %in% c('shots', 'goals'),
      value,
      sprintf('%.2f', value)
    )
  ) |> 
  inner_join(
    base_hex_xy,
    by = 'stat'
  )

team_logos <- df |> 
  distinct(player_name, team_id) |> 
  mutate(
    team_logo_url = sprintf('https://images.fotmob.com/image_resources/logo/teamlogo/%s.png', team_id)
  )

arw <- arrow(length = unit(3, 'pt'), type = 'closed')

base <- df |> 
  ggplot() +
  aes(x = x, y = y) +
  ggsoccer::annotate_pitch(
    dimensions = ggsoccer::pitch_international,
    colour = 'black',
    fill = sandpaper_background_color
  ) +
  coord_flip(
    ylim = c(0, pitch_width),
    xlim = c(x_buffer + pitch_length / 2, pitch_length + 3) ## x buffer to cut off half line, 5 for median distance label
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = font),
    title = element_text(size = 20, color = 'black'),
    plot.title = ggtext::element_markdown(size = 18, face = 'bold', hjust = 0.5),
    plot.title.position = 'plot',
    plot.subtitle = ggtext::element_markdown(size = 14, color = '#4E616C', hjust = 0.5),
    plot.margin = margin(30, 30, 30, 30),
    plot.background = element_rect(fill = sandpaper_background_color, color = sandpaper_background_color),
    panel.background = element_rect(fill = sandpaper_background_color, color = sandpaper_background_color),
    strip.text = element_text(size = 14, color = 'black', face = 'bold'),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(2, 'lines')
  ) +
  labs(
    title = "LIGA MIX'S TOP SHOOTERS",
    subtitle = "Non-penalty shot bins for Liga MX's top 6 shooters | Apertura 2022/2023 | viz by @sonofacorner, inspired by @jonollington",
    y = NULL,
    x = NULL
  ) +
  # ggpattern::geom_polygon_pattern(
  #   data = hex_xy,
  #   aes(
  #     x = isox + x,
  #     y = isoy + y, 
  #     group = stat
  #   ),
  #   fill = sandpaper_background_color,
  #   pattern_shape = 20,
  #   pattern_density = 0.8,
  #   pattern_alpha = 0.2,
  #   pattern_spacing = 0.01,
  #   pattern_size = 0.1,
  #   pattern = 'pch',
  #   colour = 'black',
  #   size = 1.5
  # ) +
  geom_polygon(
  data = hex_xy,
  aes(
    x = isox + x,
    y = isoy + y,
    group = stat
  ),
  fill = sandpaper_background_color,
  color = 'black',
  size = 1.5
) +
  geom_text(
    # fontface = 'bold',
    family = font,
    size = 12 / .pt,
    vjust = 0.5,
    hjust = 0.5,
    data = base_hex_xy,
    aes(x = x - !!hex_width - 2, y = y + !!hex_width, label = stat)
  ) +
  geom_text(
    inherit.aes = FALSE,
    fontface = 'bold',
    family = font,
    size = 12 / .pt,
    vjust = 0.5,
    hjust = 0.5,
    data = agg_xy,
    aes(x = x, y = y + !!hex_width, label = value_lab)
  ) +
  ggforce::geom_arc(
    inherit.aes = FALSE,
    color = 'red',
    linetype = '31',
    size = 1,
    data = agg |> select(player_name, median_x_yards),
    aes(
      x0 = pitch_length, 
      y0 = half_pitch_width, 
      r = median_x_yards, 
      start = pi,
      end = 2 * pi
    )
  ) +
  geom_text(
    hjust = 1,
    color = 'red',
    size = 9 / .pt,
    family = font,
    data = agg |> select(player_name, median_x_yards),
    aes(
      x = pitch_length + 3,
      y = half_pitch_width - median_x_yards,
      label = sprintf('%.1f yds.', median_x_yards)
    )
  ) +
  geom_text(
    inherit.aes = FALSE,
    hjust = 0,
    color = 'red',
    size = 9 / .pt,
    family = font,
    data = agg |> select(player_name),
    aes(
      x = pitch_length + 3,
      y = half_pitch_width + 6,
      label = 'median distance'
    )
  ) +
  geom_segment(
    color = 'red',
    data = agg |> select(player_name, median_x_yards),
    arrow = arw,
    aes(
      x = pitch_length + 3,
      y = half_pitch_width,
      xend = pitch_length + 3,
      yend = half_pitch_width - median_x_yards + 3
    )
  ) +
  facet_wrap(
    ~player_name, 
    scales = 'fixed'
  )
base

p <- base +
  # geom_hex(
  #   aes(x = x, y = y, fill = ..density.., group = player_name),
  #   show.legend = FALSE
  # ) +
  # scale_fill_binned(pal) +
  geom_point(
    # aes(x = -(pitch_length - x), y = y)
    aes(x = x, y = y)
  )
p

## Reference: https://github.com/ajreinhard/data-viz/blob/master/ggplot/plot_SB.R
p_bld <- ggplot_gtable(ggplot_build(p))
grob_strip_index <- which(sapply(p_bld$grob, function(x) x$name) == 'strip')
facet_id <- sapply(grob_strip_index, function(grb) {
  p_bld$grobs[[grb]]$grobs[[1]]$children[[2]]$children[[1]]$label
})

for (i in 1:length(facet_id)) {
  player_name <- facet_id[i]
  team_logo_url <- team_logos |> 
    filter(player_name == !!player_name) |> 
    pull(team_logo_url)
  
  lab <- grid::textGrob(
    player_name,
    gp = grid::gpar(
      col = 'black',
      fontfamily = font,
      fontface = 'bold',
      fontsize = 11
    ),
    hjust = 0.5
  )
  
  raw_img <- magick::image_read(team_logo_url)
  bw_img <- magick::image_quantize(raw_img, colorspace = 'gray')
  img <- grid::rasterGrob(
    x = unit(0.9, 'npc'),
    vp = grid::viewport(height = 1, width = 1)
  )
  tot_tree <- grid::grobTree(lab, img)
  p_bld$grobs[[grob_strip_index[i]]] <- tot_tree
}
p2 <- cowplot::ggdraw(p_bld)

ggsave(
  plot = p2,
  filename = file.path(dir_proj, 'liga-mx.png'),
  width = 14,
  height = 7,
  units = 'in'
)
