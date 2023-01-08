library(purrr)
library(dplyr)
library(ggplot2)
library(extrafont)
library(ggtext)
library(glue)
library(ggfx)

dir_proj <- '63-fbref_xg_sources'
dir_data <- file.path(dir_proj, 'data')
path_opta <- file.path(dir_data, 'big5_team_shooting_opta.rds')

blackish_background <- '#1c1c1c'
  
font <- 'Titillium Web'
extrafont::loadfonts(quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = font),
  title = element_text(size = 20, color = 'white'),
  plot.title = element_text(face = 'bold', size = 20, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = element_text(size = 16, color = '#f1f1f1'),
  axis.text = element_text(color = 'white', size = 14),
  axis.title = element_text(size = 16, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = '#4d4d4d'),
  panel.grid.minor = element_line(color = '#4d4d4d'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  # strip.text = element_text(size = 14, color = 'white', face = 'bold', hjust = 0.5),
  plot.background = element_rect(fill = blackish_background, color = blackish_background),
  plot.caption = ggtext::element_markdown(color = 'white', hjust = 0, size = 14),
  plot.caption.position = 'plot',
  panel.background = element_rect(fill = blackish_background, color = blackish_background)
)
# update_geom_defaults('text', list(color = 'white', size = 12 / .pt))

aggregate_team_xg_stats_by_season <- function(source) {
  path <- switch(
    source,
    'sb' = url('https://github.com/JaseZiv/worldfootballR_data/blob/master/data/fb_big5_advanced_statsbomb/big5_team_shooting.rds?raw=true'),
    'opta' = path_opta
  )
  
  df <- path |>
    readRDS() |>
    filter(
      Team_or_Opponent == 'team',
      Season_End_Year >= 2018,
      Season_End_Year < 2023
    )
  
  df |>
    group_by(season = Season_End_Year) |>
    summarize(
      g = sum(Gls_Standard),
      xg = sum(xG_Expected),
      npxg = sum(npxG_Expected),
      ## these are sometimes not self-consistent with g, xg, pg, and npxg
      # g_xg_d = sum(G_minus_xG_Expected),
      # npg_npxg_d = sum(`np:G_minus_xG_Expected`)
      pg = sum(PK_Standard)
    ) |>
    ungroup() |>
    mutate(
      g_xg_d = g - xg,
      npg_npxg_d = g - pg - npxg
    )
}

pretty_source_names <- c(
  'sb' = 'Old',
  'opta' = 'New'
)

source_colors <- c(
  'Old' = '#e40b27',
  'New' = '#00ADEF'
)

xg_stats_by_season <- c('sb', 'opta') |>
  set_names() |>
  map_dfr(aggregate_team_xg_stats_by_season, .id = 'source') |>
  mutate(
    across(source, ~pretty_source_names[.x])
  )

x_values <- sort(unique(xg_stats_by_season$season))
p <- xg_stats_by_season |>
  ggplot() +
  aes(
    x = season,
    y = npg_npxg_d
  ) +
  geom_hline(
    aes(yintercept = 0),
    color = 'white'
  ) +
  ggblur::geom_point_blur(
    aes(color = source),
    size = 5 * .stroke
  ) +
  geom_line(
    aes(color = source)
  ) +
  guides(
    color = 'none'
  ) +
  scale_color_manual(
    values = source_colors
  ) +
  geom_text(
    data = xg_stats_by_season |> filter(season != 2021),
    aes(
      label = sprintf('%+.1f', npg_npxg_d)
    ),
    family = font,
    fontface = 'bold',
    color = 'white',
    size = 12 / .pt
  ) +
  with_outer_glow(
    geom_text(
      data = xg_stats_by_season |> 
        filter(season == 2021) |> 
        mutate(
          x = ifelse(source == 'Old', 2021.3, 2021.2),
          y = ifelse(source == 'Old', 20, 80)
        ),
      aes(
        label = sprintf('%+.1f', npg_npxg_d),
        x = x,
        y = y,
        color = source
      ),
      family = font,
      fontface = 'bold',
      size = 12 / .pt
    ),
    colour = '#4d4d4d',
    sigma = 1,
    expand = 1
  ) +
  scale_x_continuous(
    breaks = x_values,
    labels = sprintf('%s/%s', x_values, substr(x_values + 1, 3, 4))
  ) +
  theme(
    plot.subtitle = ggtext::element_markdown(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = 'FBRef non-penalty xG - goals for Big 5 European leagues',
    subtitle = glue::glue('Comparison of <b><span style=color:{source_colors["Old"]}>old</span></b> and <b><span style=color:{source_colors["New"]}>new</span></b> data providers'),
    caption = '**Viz**: @TonyElHabr',
    x = 'Season',
    y = 'non-penalty xG - goals'
  )
p

ggsave(
  p,
  filename = file.path(dir_proj, 'fbref_xg_sources.png'),
  width = 8,
  height = 8
)
