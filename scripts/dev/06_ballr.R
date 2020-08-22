
# References:
# + https://www.owenlhjphillips.com/new-blog/2020/6/25/how-to-make-nba-shots-charts-in-r
library(tidyverse)
source(here::here("scripts", "dev", "06_ballr_helpers.R"))

df <- get_data(203081, "2019-20", "Regular Season") %>% as_tibble()
df

library(prismatic)
library(extrafont)
library(cowplot)

viz <-
  plot_court(court_themes$dark) +
  geom_polygon(
    data = df,
    aes(
      x = adj_x,
      y = adj_y,
      group = hexbin_id,
      fill = bounded_fg_diff,
      color = after_scale(clr_darken(fill, .333))
    ),
    size = .25
  ) +
  # geom_text(data = df, aes(x = adj_x, y = adj_y, group = hexbin_id, label = hexbin_id)) +
  scale_x_continuous(limits = c(-27.5, 27.5)) +
  scale_y_continuous(limits = c(0, 45)) +
  scale_fill_distiller(
    direction = -1,
    palette = "RdBu",
    limits = c(-.15, .15),
    breaks = seq(-.15, .15, .03),
    labels = c("-15%", "-12%", "-9%", "-6%", "-3%", "0%", "+3%", "+6%", "+9%", "+12%", "+15%"),
    "FG Percentage Points vs. League Average"
  ) +
  guides(fill = guide_legend(
    label.position = "bottom",
    title.position = "top",
    keywidth = .45,
    keyheight = .15,
    default.unit = "inch",
    title.hjust = .5,
    title.vjust = 0,
    label.vjust = 3,
    nrow = 1
  )) +
  theme(
    # text = element_text(size = 14, family = "Gill Sans MT"),
    legend.spacing.x = unit(0, "cm"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = rel(0.6)),
    legend.margin = margin(-10, 0, -1, 0),
    legend.position = "bottom",
    legend.box.margin = margin(-30, 0, 15, 0),
    plot.title = element_text(hjust = 0.5, vjust = -1, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9, vjust = -.5),
    plot.caption = element_text(face = "italic", size = 8),
    plot.margin = margin(0, -5, 0, -5, "cm")
  ) +
  labs(
    title = "Damian Lillard",
    subtitle = "2019-20 Regular Season"
  )

viz_redraw <-
  viz %>% 
  ggdraw() # +
  # theme(plot.background = element_rect(fill = "floralwhite", color = NA))
viz_redraw

