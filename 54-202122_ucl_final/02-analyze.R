library(tidyverse)
library(scales)
library(extrafont)
library(ggtext)
library(ggfx)
dir_proj <- '54-202122_ucl_final'
path_data <- file.path(dir_proj, 'data.rds')

gray_wv <- rgb(24, 24, 24, maxColorValue = 255)
gray_grid_wv <- rgb(64, 64, 64, maxColorValue = 255)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text(size = 20, color = 'white'),
  plot.title = ggtext::element_markdown(face = 'bold', size = 20, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown(size = 16, color = '#f1f1f1'),
  axis.text = element_text(color = 'white', size = 14),
  axis.title = element_text(size = 14, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = gray_grid_wv),
  panel.grid.minor = element_line(color = gray_grid_wv),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = gray_wv, color = gray_wv),
  plot.caption = ggtext::element_markdown(color = 'white', hjust = 1, size = 10, face = 'italic'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 12, color = 'white', hjust = 0),
  plot.tag.position = c(0.01, 0.02),
  panel.background = element_rect(fill = gray_wv, color = gray_wv)
)

pcts <- path_data |> 
  read_rds() |> 
  filter(versus != 'Goalkeepers') |> 
  filter(scouting_period == 'Last 365 Days') |> 
  filter(based_on_minutes >= 1000) |> 
  distinct(
    player,
    team_name,
    pos = versus,
    stat_group ,
    statistic,
    p = percentile,
    mp = based_on_minutes
  ) |> 
  ## npxG is also in a diff `stat_group`, so let's not have dups
  filter(!(stat_group == 'Standard' & statistic == 'npxG')) |> 
  mutate(
    group = case_when(
      statistic %in% c('Progressive Passes', 'Progressive Passes Rec', 'Progressive Carries', 'npxG') ~ 'nice',
      statistic %in% c('Pressures', 'Tackles', 'Interceptions') ~ 'dawg',
      TRUE ~ NA_character_
    )
  ) |> 
  drop_na(group)

agg <- pcts |> 
  mutate(
    across(
      p,
      ~ifelse(
        group == 'dawg',
        abs(50 - .x),
        .x
      )
    )
  ) |> 
  group_by(player, team_name, group) |> 
  summarize(
    across(mp, first),
    across(p, sum)
  ) |> 
  ungroup() |> 
  group_by(group, team_name) |> 
  mutate(
    across(p, ~scales::rescale(.x, to = c(-100, 100)))
  ) |> 
  ungroup() |> 
  pivot_wider(
    names_from = group,
    values_from = p
  )

p <- agg |> 
  ggplot() +
  aes(x = nice, y = dawg) +
  geom_vline(
    color = gray_grid_wv,
    linetype = 2,
    aes(xintercept = 0)
  ) +
  geom_hline(
    color = gray_grid_wv,
    linetype = 2,
    aes(yintercept = 0)
  ) +
  with_outer_glow(
    geom_point(
      show.legend = FALSE,
      color = '#c8102E',
      data = agg |> filter(team_name == 'Liverpool'),
      aes(size = mp)
    ),
    colour = alpha('#c8102E', 0.5)
  ) +
  with_outer_glow(
    geom_point(
      show.legend = FALSE,
      color = '#FEBE10',
      data = agg |> filter(team_name == 'Real Madrid'),
      aes(size = mp)
    ),
    colour = alpha('#FEBE10', 0.5)
  ) +
  scale_size(
    range = c(1, 6)
  ) +
  ggrepel::geom_text_repel(
    family = 'Karla',
    size = 12 / .pt,
    color = 'white',
    aes(label = player)
  ) +
  labs(
    x = "Ain't got that dawg in him",
    y = 'That boy not nice'
  ) +
  scale_x_continuous(
    sec.axis = dup_axis(name = 'Got that dawg in him')
  ) +
  scale_y_continuous(
    sec.axis = dup_axis(name = 'That boy nice')
  ) +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    plot.title = ggtext::element_markdown(vjust = 0.5, hjust = 0.5),
    axis.title.x = element_text(vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(vjust = 0.5, hjust = 0.5)
  ) +
  labs(
    caption = '<br/>Niceness based on blend of npxG and progression stats, relative to position.<br/>Dawgness based on pressures, tackles, and interceptions, relative to position.<br/>Point size based on minutes played.',
    tag = '**Viz**: Tony ElHabr<br/>**Data**: StatsBomb via fbref'
  )
p

ggsave(p, filename = file.path(dir_proj, 'nice_dawgs.png'), width = 8, height = 8)
