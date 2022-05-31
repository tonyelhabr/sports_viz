
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
    is_outside_box = case_when(
      y > (pitch_opta$width / 2 + pitch_opta$penalty_box_width / 2) ~ TRUE,
      y < (pitch_opta$width / 2 - pitch_opta$penalty_box_width / 2) ~ TRUE,
      x < (pitch_opta$length - pitch_opta$penalty_box_length) ~ TRUE,
      TRUE ~ FALSE
    )
  )

# df |> 
#   ggplot() +
#   aes(
#     x = x, y = y
#   ) +
#   geom_point(aes(color = is_outside_box))

outside_box_blocks <- df |> 
  filter(is_blocked) |> 
  count(season_id, team_id = opponent_id, team_name = opponent_name, is_outside_box, name = 'blocked_shots')

outside_box_shots_conceded <- df |> 
  filter(is_shot) |> 
  count(season_id, team_id = opponent_id,team_name = opponent_name, is_outside_box, name = 'shots_conceded')

props <- full_join(
  outside_box_blocks,
  outside_box_shots_conceded
) |> 
  mutate(
    prop = blocked_shots / shots_conceded
  ) |> 
  arrange(prop)
props

props_outside_box <- props |> 
  filter(season_id == 2022) |> 
  filter(is_outside_box) |> 
  mutate(
    across(team_name, fct_reorder, -prop)
  ) |> 
  select(-is_outside_box)

props_outside_box <- props |> 
  # filter(season_id == 2022) |> 
  group_by(season_id, team_id, team_name) |> 
  mutate(
    shots_prop = shots_conceded / sum(shots_conceded)
  ) |> 
  ungroup() |> 
  filter(is_outside_box) |> 
  # mutate(
  #   across(team_name, fct_reorder, -prop)
  # ) |> 
  select(-is_outside_box)
props_outside_box |> arrange(desc(shots_conceded))
props_outside_box |> filter(team_name == 'Man City')
props_outside_box |> 
  ggplot() +
  aes(x = shots_prop, y = prop) +
  geom_jitter(
    data = props_outside_box |> filter(season_id != 2022),
    color = gray_grid_wv
  ) +
  geom_jitter(
    data = props_outside_box |> filter(season_id == 2022, team_name != 'Liverpool'),
    color = 'white'
  ) +
  ggrepel::geom_text_repel(
    data = props_outside_box |> filter(season_id == 2022, team_name != 'Liverpool'),
    family = 'Karla',
    size = 12 / .pt,
    color = 'white',
    aes(
      label = team_name
    )
  ) +
  geom_jitter(
    data = props_outside_box |> filter(team_name == 'Liverpool'),
    width = 0.002,
    height = 0.002,
    color = '#c8102E'
  ) +
  ggrepel::geom_text_repel(
    data = props_outside_box |> filter(team_name == 'Liverpool'),
    family = 'Karla',
    fontface = 'bold',
    size = 12 / .pt,
    color = '#c8102E',
    aes(
      label = sprintf("%s '%s", team_name, str_sub(season_id, 3))
    )
  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = '% of Shots Conceded Outside of Box',
    y = 'Blocked % of shots conceded outside of box'
  ) -> p2
p2
ggsave(p2, filename = sprintf('%s/temp2.png', dir_proj), width = 8, height = 8)
size <- 0.05
init <- props_outside_box |> 
  mutate(
    n = prop %/% !!size
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

res <- bind_rows(
  init,
  init |> 
    group_by(season_id, team_id, team_name) |> 
    slice_max(idx) |> 
    ungroup() |> 
    mutate(
      across(idx, ~.x + 1L),
      prop_idx = prop_idx + prop %% !!size
    )
) |> 
  arrange(season_id, team_name, idx) |> 
  group_by(season_id, team_name) |> 
  mutate(
    across(prop_idx, list(lag = dplyr::lag), default = 0)
  ) |> 
  ungroup()


# https://twitter.com/CrumpledJumper/status/1529979231735672834/photo/1
res2 <- res |> 
  filter(
    (prop_idx - prop_idx_lag) > 0.002
  )
res


p <- res2 |> 
  ggplot() +
  geom_rect(
    aes(
      xmin = prop_idx_lag + 0.002,
      xmax = prop_idx - 0.002,
      ymin = as.numeric(res2$team_name) - 0.45,
      ymax = as.numeric(res2$team_name) + 0.45,
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
    labels = props_outside_box$team_name
  ) +
  labs(
    title = 'Percentage of outside-the-box shots blocked',
    subtitle = '2021/22 Premier League',
    y = 'Blocked % of shots conceded outside of box',
    x = NULL
  )
p
ggsave(p, filename = sprintf('%s/temp.png', dir_proj), width = 8, height = 6)
res |> 
  ggplot() +
  aes(
  ) +
  geom_chicklet(
    aes(
      x = team_name,
      y = prop_idx - prop_idx_lag,
      group = team_name,
      fill = idx
    ),
    width = 0.8,
    radius = unit(10, 'pt'),
    color = 'white',
    show.legend = FALSE
  ) +
  scale_fill_viridis_c(
    option = 'G',
    begin = 0.2,
    end = 0.9
  ) +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank()
  ) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  labs(
    title = 'Percentage of outside-the-box shots blocked',
    y = NULL, #  '% of outside-the-box shots blocked',
    x = NULL
  )

res |> 
  mutate(
    x = (prop_idx + prop_idx_lag) / 2,
    w = (prop_idx - prop_idx_lag) * 0.9
  ) |> 
  # filter(team_name == 'Liverpool')
  ggplot() +
  aes(
    y = team_name,
    x = x
  ) +
  geom_tile(
    aes(width = w, fill = x),
    height = 0.8,
    color = 'white',
    show.legend = FALSE,
    linejoin = 'round'
  ) +
  scale_fill_viridis_c(
    option = 'G',
    begin = 0.2
  ) +
  theme(
    panel.grid.major.y = element_blank()
  ) +
  scale_x_continuous(
    breaks = c(0.2, 0.4),
    labels = scales::percent
  ) +
  labs(
    title = 'Percentage of outside-the-box shots blocked',
    x = '% of outside-the-box shots blocked',
    y = NULL
  )

props |> 
  filter(is_outside_box) |> 
  mutate(
    across(team_name, fct_reorder, -prop)
  ) |> 
  ggplot() +
  aes(
    y = team_name,
    x = prop
  ) +
  geom_col(
    width = 0.8
  ) +
  theme(
    panel.grid.major.y = element_blank()
  ) +
  scale_x_continuous(
    labels = scales::percent
  ) +
  labs(
    title = 'Percentage of outside-the-box shots blocked',
    x = '% of outside-the-box shots blocked',
    y = NULL
  )
