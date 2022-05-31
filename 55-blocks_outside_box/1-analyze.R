
# https://medium.com/@chris.summersell/are-liverpool-breaking-a-sacred-defensive-code-8c5f806a4c41
library(tidyverse)
library(qs)
library(ggsoccer)

dir_proj <- '55-blocks_outside_box'
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

df <- sprintf('%s/blocks_and_shots.qs', dir_proj) |> 
  qs::qread()|> 
  mutate(
    is_oob = case_when(
      y > (pitch_opta$width / 2 + pitch_opta$penalty_box_width / 2) ~ TRUE,
      y < (pitch_opta$width / 2 - pitch_opta$penalty_box_width / 2) ~ TRUE,
      x < (pitch_opta$length - pitch_opta$penalty_box_length) ~ TRUE,
      TRUE ~ FALSE
    )
  )

oob_blocks <- df |> 
  filter(is_blocked) |>
  count(
    season_id,
    team_id = opponent_id,
    team_name = opponent_name,
    is_oob,
    name = 'blocked_shots'
  )

oob_shots_conceded <- df |>
  filter(is_shot) |>
  count(
    season_id,
    team_id = opponent_id,
    team_name = opponent_name,
    is_oob,
    name = 'shots_conceded'
  )

oob_props <- full_join(
  oob_blocks,
  oob_shots_conceded
) |> 
  mutate(
    oob_box_prop = blocked_shots / shots_conceded
  ) |> 
  arrange(oob_box_prop)

## 2022 chart ----
latest_oob_box_props <- oob_props |> 
  filter(season_id == 2022) |> 
  filter(is_oob) |> 
  mutate(
    across(team_name, fct_reorder, -oob_box_prop)
  ) |> 
  select(-is_oob)

width <- 0.004
# https://twitter.com/CrumpledJumper/status/1529979231735672834/photo/1
tilize <- function(df, size = 0.05, min_width = width / 2) {
  init <- df |> 
    mutate(
      n = oob_box_prop %/% !!size
    ) |> 
    uncount(n) |> 
    group_by(season_id, team_id, team_name) |> 
    mutate(
      idx = row_number()
    ) |> 
    ungroup() |> 
    mutate(
      prop_idx = idx * !!size
    )
  
  bind_rows(
    init,
    init |> 
      group_by(season_id, team_id, team_name) |> 
      slice_max(idx) |> 
      ungroup() |> 
      mutate(
        across(idx, ~.x + 1L),
        prop_idx = prop_idx + oob_box_prop %% !!size
      )
  ) |> 
    arrange(season_id, team_name, idx) |> 
    group_by(season_id, team_name) |> 
    mutate(
      across(prop_idx, list(lag = dplyr::lag), default = 0)
    ) |> 
    ungroup() |> 
    filter(
      (prop_idx - prop_idx_lag) > (!!min_width)
    )
}

tiled_latest_oob_box_props <- latest_oob_box_props |> tilize()
lab_oob <- 'Blocked % of shots conceded outside of box'
height <- 0.9
p_tile <- tiled_latest_oob_box_props |> 
  ggplot() +
  geom_rect(
    data = tiled_latest_oob_box_props |>
      mutate(
        ymin = as.numeric(team_name) - height / 2,
        ymax = as.numeric(team_name) + height / 2
      ),
    aes(
      xmin = prop_idx_lag + !!width / 2,
      xmax = prop_idx - !!width / 2,
      ymin = ymin,
      ymax = ymax,
      fill = idx
    ),
    color = gray_grid_wv,
    show.legend = FALSE
  ) +
  scale_fill_viridis_c(
    option = 'A',
    begin = 0.2,
    end = 0.9
  ) +
  theme(
    panel.grid.major.y = element_blank()
  ) +
  scale_x_continuous(
    labels = scales::percent
  ) +
  scale_y_continuous(
    breaks = 20:1,
    expand = c(0.01, 0.01),
    labels = latest_oob_box_props$team_name
  ) +
  labs(
    title = 'Percentage of outside-the-box shots blocked',
    subtitle = '2021/22 Premier League',
    tag = '**Viz**: Tony ElHabr',
    caption = '<br/>',
    y = lab_oob,
    x = NULL
  )
p_tile
path_tile <- sprintf('%s/tiled_oob_blocks.png', dir_proj)
ggsave(
  p_tile,
  filename = path_tile,
  width = 10,
  height = 7.5
)

add_logo(
  path_viz = path_tile,
  path_logo = sprintf('%s/epl-logo-white.png', dir_proj),
  logo_scale = 0.13,
  idx_x = 0.01,
  idx_y = 0.98,
  adjust_y = FALSE
)

## scatter ----
all_oob_props <- oob_props |> 
  group_by(season_id, team_id, team_name) |> 
  mutate(
    total_shots_conceded = sum(shots_conceded),
    shots_prop = shots_conceded / total_shots_conceded,
    total_blocked_prop = blocked_shots / total_shots_conceded
  ) |> 
  ungroup() |> 
  filter(is_oob) |> 
  select(-is_oob)
all_oob_props |> arrange(desc(shots_conceded))
all_oob_props |> filter(team_name == 'Man City')

color_liv <- '#00B2A9' # '#c8102E'
team_name_liv <- 'Liverpool'
prior_oob_props_wo_liv <- all_oob_props |> filter(season_id != 2022, team_name != !!team_name_liv)
latest_oob_props_wo_liv <- all_oob_props |> filter(season_id == 2022, team_name != !!team_name_liv)
liv_oob_box_props <-  all_oob_props |> filter(team_name == !!team_name_liv)
pal_names <- c('Liverpool', "Other '21/22 team", "Pre '21/22 team")
pal <- setNames(c(color_liv, 'white', gray_grid_wv), pal_names)
team_mapping <- xengagement::team_accounts_mapping |> 
  select(
    team_name = team_whoscored, url_logo_espn
  )
library(nflplotR)
p_scatter <- all_oob_props |> 
  ggplot() +
  aes(x = shots_prop, y = oob_box_prop) +
  geom_jitter(
    data = prior_oob_props_wo_liv,
    size = 2,
    aes(color = pal_names[3])
  ) +
  # geom_jitter(
  #   data = latest_oob_props_wo_liv,
  #   size = 3,
  #   aes(
  #     color = pal_names[2]
  #   )
  # ) +
  # ggrepel::geom_text_repel(
  #   data = latest_oob_props_wo_liv,
  #   family = 'Karla',
  #   size = 12 / .pt,
  #   color = 'white',
  #   aes(
  #     color = pal_names[2],
  #     label = team_name
  #   )
  # ) +
  geom_from_path(
    data = latest_oob_props_wo_liv |> inner_join(team_mapping),
    aes(
      path = url_logo_espn,
      width = 0.1
    ),
    alpha = 0.5
  ) +
  geom_jitter(
    data = liv_oob_box_props,
    width = 0.002,
    height = 0.002,
    size = 3,
    aes(color = !!team_name_liv)
  ) +
  ggrepel::geom_text_repel(
    data = liv_oob_box_props,
    family = 'Karla',
    fontface = 'bold',
    size = 12 / .pt,
    color = color_liv,
    force = 10,
    aes(
      color = !!team_name_liv,
      label = sprintf("%s '%s/%s", team_name, as.integer(str_sub(season_id, 3)) - 1, str_sub(season_id, 3))
    )
  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    name = '',
    values = pal
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 4))
  ) +
  theme(
    plot.title = ggtext::element_markdown(size = 16),
    plot.subtitle = ggtext::element_markdown(size = 16),
    legend.position = 'top',
    legend.text = element_text(size = 12, color = 'white', face = 'bold')
  ) +
  labs(
    title = "<span style={color_liv}>Liverpool</span>not only block shots from outside-the-box at historically low rates,<br/>but they've also conceded a lower percentage of their shots from outside the box.",
    subtitle = '2017/18 - 2021/22 Premier League',
    x = '% of Shots Conceded Outside of Box',
    tag = '**Viz**: Tony ElHabr',
    caption = '<br/>',
    y = lab_oob
  )
p_scatter

path_scatter <- sprintf('%s/oob_blocks_and_shots.png', dir_proj)
ggsave(
  p_scatter,
  filename = path_scatter,
  width = 10,
  height = 9
)

add_logo(
  path_viz = path_scatter,
  path_logo = sprintf('%s/epl-logo-white.png', dir_proj),
  logo_scale = 0.1,
  idx_x = 0.01,
  idx_y = 0.98,
  adjust_y = FALSE
)

