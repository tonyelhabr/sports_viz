## Reference: https://twitter.com/sonofacorner/status/1581956939151380481
## Reference: https://github.com/sonofacorner/soc-viz-of-the-week/blob/main/10172022/10172022.ipynb
library(tidyverse)
library(ggsoccer)
library(extrafont)
library(ggpattern)
library(grid)
library(cowplot)
library(magick)
library(ggforce)
library(patchwork)

dir_proj <- '61-sonofacorner_shotmap'
sandpaper_background_color <- '#EFE9E6'
font <- 'Titillium Web'
extrafont::loadfonts(quiet = TRUE)

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
  x = 7 + x_buffer + half_pitch_length,
  y = c(11, 23, 42, 54) - (hex_width / 2)
)

hex_xy <- base_hex_xy |> 
  rowwise() |>
  mutate(
    x = list(c(0, 0, !!hex_width, !!hex_width)),
    y = list(c(0, !!hex_width, !!hex_width, 0)),
    edge_x_bottom = list(x - y),
    edge_y_bottom = list((x + y) / 2),
    edge_x_top = list(edge_x_bottom),
    edge_y_top = list(edge_y_bottom + !!hex_width),
    edge_x = list(c(edge_x_bottom[c(2, 1, 4)], edge_x_top[c(4, 3, 2)])),
    edge_y = list(c(edge_y_bottom[c(2, 1, 4)], edge_y_top[c(4, 3, 2)]))
  )  |>
  ungroup() |>
  unnest(c(edge_x, edge_y)) |>
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

arw <- arrow(length = unit(3, 'pt'), type = 'closed')

plot_player <- function(player_name) {
  # player_name <- 'JUAN DINENNO'
  d <- df |> filter(player_name == !!player_name)
  a_xy <- agg_xy |> filter(player_name == !!player_name)
  a <- agg |> 
    filter(player_name == !!player_name) |> 
    select(median_x_yards)

  base <- d |> 
    ggplot() +
    aes(x = x, y = y) +
    ## Plot hexes first so that they in the background
    geom_hex(
      aes(x = x, y = y, fill = ..density..),
      bins = c(24, 24),
      binwidth = c(pitch_length / 12, pitch_width / 24),
      show.legend = FALSE
    ) +
    ggsoccer::annotate_pitch(
      dimensions = ggsoccer::pitch_international,
      colour = 'black',
      fill = NA
    ) +
    coord_flip(
      ylim = c(0, pitch_width),
      xlim = c(x_buffer + pitch_length / 2, pitch_length + 2) ## x buffer to cut off half line, 3 for room for median distance label
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = font),
      title = element_text(color = 'black'),
      plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
      plot.title.position = 'plot',
      plot.margin = margin(10, 10, 10, 10),
      plot.background = element_rect(fill = sandpaper_background_color, color = sandpaper_background_color),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(
      title = player_name,
      y = NULL,
      x = NULL
    ) +
    ## 4 stat label hexes
    ggpattern::geom_polygon_pattern(
      data = hex_xy,
      aes(
        x = edge_x + x,
        y = edge_y + y,
        group = stat
      ),
      fill = sandpaper_background_color,
      pattern_shape = 20,
      pattern_density = 0.7,
      pattern_alpha = 0.2,
      pattern_spacing = 0.01,
      pattern_size = 0.05,
      pattern = 'pch',
      colour = 'black',
      size = 1
    ) +
    ## the 4 stat labels
    geom_text(
      family = font,
      size = 11 / .pt,
      vjust = 0.5,
      hjust = 0.5,
      data = base_hex_xy,
      ## - 2 to move below the hex
      aes(x = x - !!hex_width - 2, y = y + !!hex_width, label = stat)
    ) +
    ## the 4 stat label values
    geom_text(
      inherit.aes = FALSE,
      fontface = 'bold',
      family = font,
      size = 12 / .pt,
      vjust = 0.5,
      hjust = 0.5,
      data = a_xy,
      aes(x = x, y = y + !!hex_width, label = value_lab)
    ) +
    ggforce::geom_arc(
      inherit.aes = FALSE,
      color = 'red',
      linetype = '31',
      size = 1,
      data = a,
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
      data = a,
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
      size = 9 / .pt, ## 8 in the original viz... this becomes a victim of interpolation at 8, but not at 9
      family = font,
      data = tibble(),
      aes(
        x = pitch_length + 3,
        y = half_pitch_width + 6,
        label = 'median distance'
      )
    ) +
    geom_segment(
      color = 'red',
      data = a,
      arrow = arw,
      aes(
        x = pitch_length + 3,
        y = half_pitch_width,
        xend = pitch_length + 3,
        yend = half_pitch_width - median_x_yards + 3
      )
    ) +
    scale_fill_gradient(
      low = '#d0d6d4', 
      high = '#287271'
    )
  
  team_logo_url <- sprintf('https://images.fotmob.com/image_resources/logo/teamlogo/%s.png', d$team_id[[1]])
  raw_img <- magick::image_read(team_logo_url)
  bw_img <- magick::image_quantize(raw_img, colorspace = 'gray')
  
  logo_temp_file <- tempfile(fileext = '.png')
  magick::image_write(bw_img, logo_temp_file)
  plot_temp_file <- tempfile(fileext = '.png')
  ggsave(
    plot = base,
    filename = temp_file,
    width = 6 / 1.25,
    height = 4 / 1.25
  )
  
  path_res <- add_logo(
    path_viz = temp_file,
    path_logo = logo_temp_file,
    # path_suffix = '',
    delete = TRUE,
    logo_scale = 0.1,
    adjust_y = FALSE,
    idx_x = 0.1,
    idx_y = 0.98
  )
  
  path_res |> 
    magick::image_read() |> 
    magick::image_ggplot()
}

subplots <- toupper(players_of_interest) |> 
  map(plot_player)

p <- patchwork::wrap_plots(subplots, ncol = 3, byrow = FALSE) +
  patchwork::plot_annotation(
    title = "LIGA MIX'S TOP SHOOTERS",
    subtitle = "Non-penalty shot bins for Liga MX's top 6 shooters | Apertura 2022/2023 | viz by @sonofacorner, inspired by @jonollington",
    theme = theme(
      text = element_text(family = font),
      title = element_text(color = 'black'),
      plot.title = ggtext::element_markdown(face = 'bold', size = 40, hjust = 0.5),
      plot.title.position = 'plot',
      plot.subtitle = element_text(size = 14, color = '#4E616C', hjust = 0.5),
      plot.margin = margin(20, 20, 20, 20),
      plot.background = element_rect(fill = sandpaper_background_color, color = sandpaper_background_color),
      panel.background = element_rect(fill = sandpaper_background_color, color = sandpaper_background_color)
    )
  )

ggsave(
  plot = p,
  filename = file.path(dir_proj, 'liga-mx-subplots.png'),
  width = 14,
  height = 8,
  units = 'in'
)
