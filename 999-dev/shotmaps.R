## https://twitter.com/sonofacorner/status/1581956939151380481
## https://github.com/sonofacorner/soc-viz-of-the-week/blob/main/10172022/10172022.ipynb
library(ggplot2)
library(ggsoccer)
library(extrafont)
library(ggpattern)
library(tidyverse)
library(janitor)

blackish_background <- '#1c1c1c'
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
pitch_width <- pitch_width

x_buffer <- 8.5
hex_width <- 4
## https://github.com/gkaramanis/tidytuesday/blob/master/2020/2020-week36/crops.R
hexs <- tibble(
  entity = c('goals', 'xG', 'shots', 'xG/shot'),
  # final_y = (54 * 0:3 * 14) - (hex_width / 2)
  final_y = c(11, 23, 42, 54) - (hex_width / 2)
) |> 
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
  rename(
    y = final_y
  ) |> 
  mutate(
    x = 5 + x_buffer + pitch_length / 2
  ) |>
  group_by(entity) |>
  mutate(label_y = max(isoy)) |>
  ungroup()

players_of_interest <- c(
  'Andre-Pierre Gignac' # ,
  # 'Juan Dinenno',
  # 'Ernesto Vega',
  # 'Nicolás Ibánez',
  # 'Harold Preciado',
  # 'Leonardo Fernandez'
)


df <- read_csv('https://raw.githubusercontent.com/sonofacorner/soc-viz-of-the-week/main/10172022/data/10172022.csv', col_select = -1) |> 
  janitor::clean_names() |> 
  filter(
    player_name %in% players_of_interest
  ) |> 
  mutate(
    across(player_name, ~ordered(.x, levels = players_of_interest)),
    across(y ~ifelse(.x > 34, 34 - (y - 34), 34 + (34 - y)))
  )

base <- ggplot(df) +
  aes(x = x, y = y) +
  ggsoccer::annotate_pitch(
    dimensions = ggsoccer::pitch_international,
    colour = gray_text,
    fill = blackish_background
  ) +
  coord_flip(
    ylim = c(0, pitch_width), 
    xlim = c(x_buffer + pitch_length / 2, pitch_length)
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = font),
    title = element_text(size = 20, color = 'white'),
    plot.title = ggtext::element_markdown(size = 18, color = 'white', face = 'bold', hjust = 0.5),
    plot.title.position = 'plot',
    plot.subtitle = ggtext::element_markdown(size = 14, color = '#f1f1f1', hjust = 0.5),
    plot.margin = margin(30, 30, 30, 30),
    plot.background = element_rect(fill = blackish_background, color = blackish_background),
    panel.background = element_rect(fill = blackish_background, color = blackish_background),
    strip.text = element_text(size = 14, color = 'white', face = 'bold'),
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
  ggpattern::geom_polygon_pattern(
    data = hexs,
    aes(
      x = isox + x,
      y = isoy + y, 
      group = entity
    ),
    fill = blackish_background,
    pattern_shape = 21,
    pattern_density = 0.6,
    pattern_alpha = 0.5,
    pattern_spacing = 0.015,
    pattern = 'pch',
    colour = gray_text
  )
base

21 -> 34 + 13
45 -> 34 - 11



p <- base +
  # geom_hex(
  #   aes(x = x, y = y, fill = ..density.., group = player_name),
  #   show.legend = FALSE
  # ) +
  # scale_fill_binned(pal) +
  geom_point(
    color = 'white',
    # aes(x = -(pitch_length - x), y = y)
    aes(x = x, y = y)
  ) +
  facet_wrap(~player_name, scales = 'fixed', labeller = labeller(.default = toupper))
p


r <- (pitch_length - (x_buffer + pitch_length / 2)) / pitch_width
w <- 16
ggsave(
  filename = 'temp.png',
  width = w,
  height = (2 / 3) * r * w,
  units = 'in'
)
ggsave(
  filename = 'temp2.png',
  width = 24,
  height = 12,
  dpi = 600,
  units = 'in'
)
nflplotR::ggpreview(
  width = 4,
  height = r2 * w,
  units = 'cm'
)
