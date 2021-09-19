
library(tidyverse)
library(arrow)

# Brief font issues resolved with this: https://github.com/wch/extrafont/issues/88
gray_wv <- rgb(24, 24, 24, maxColorValue = 255)
gray_grid_wv <- rgb(64, 64, 64, maxColorValue = 255)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla', color = 'white'),
  title = element_text('Karla', size = 14, color = 'white'),
  plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 24, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', face = 'bold', size = 18, color = '#f1f1f1'),
  axis.text = element_text('Karla', color = 'white', size = 14),
  axis.title = element_text('Karla', size = 14, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = gray_grid_wv),
  panel.grid.minor = element_line(color = gray_grid_wv),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = gray_wv, color = gray_wv),
  # plot.caption = ggtext::element_markdown('Karla', size = 16, color = 'gray20', hjust = 1),
  plot.caption = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0),
  plot.tag.position = c(.01, 0.016),
  # legend.text = element_text(color = 'gray20', size = 14),
  strip.text = element_text('Karla', color = 'white', face = 'bold', size = 12, hjust = 0),
  # strip.background = element_blank(),
  panel.background = element_rect(fill = gray_wv, color = gray_wv)
  
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

# mtcars %>% ggplot() + aes(x = mpg, y = carb) + geom_point(aes(color = cyl)) + labs(title = 'a title', subtitle = 'a subtitle extra long') + theme(plot.title = element_text('Jet Brains Mono'))

dir_proj <- '40-xg_overperformance'
df <- file.path(dir_proj, '2010-2022_xg.parquet') %>% arrow::read_parquet()
mp <- file.path(dir_proj, '2010-2022_mp.parquet') %>% arrow::read_parquet()
players <-
  df %>%
  count(player_id, player_name, position, sort = TRUE, name = 'total_shots') %>%
  mutate(rnk_player = row_number(desc(total_shots)))
players
players %>% filter(player_name == 'Papu Gómez')

position_mapping <-
  tibble(
    position = c('AM', 'FW', 'M', 'DM', 'D'),
    position_grp = c('A', 'A', 'M', 'M', 'D')
  )
position_mapping

players_filt <-
  players %>%
  drop_na(position) %>%
  mutate(across(position, ~str_remove_all(.x, '\\(.*\\)') %>% str_replace_all('(^.*\\,)([A-Z]+$)', '\\2'))) %>%
  inner_join(position_mapping) %>%
  left_join(mp %>% group_by(player_id) %>% summarize(total_mp = sum(minutes_played, na.rm = TRUE)))
players_filt

shots <-
  df %>%
  # drop own goals
  filter(g != 3L) %>%
  inner_join(
    players_filt %>%
      select(rnk_player, player_id, position_grp, total_shots, total_mp)
  ) %>%
  left_join(
    mp
  ) %>%
  select(
    rnk_player,
    total_shots,
    player_id,
    player_name,
    season,
    league_name,
    game_id,
    atomic_id,
    team_name,
    start_time_utc,
    mp = minutes_played,
    time_seconds,
    g,
    xg
  ) %>%
  arrange(rnk_player, start_time_utc, time_seconds) %>%
  mutate(idx = row_number()) %>%
  group_by(player_id) %>%
  mutate(idx_shot = row_number()) %>%
  ungroup() %>%
  relocate(idx, idx_shot)
shots

shots_top100 <- shots %>% inner_join(players_filt %>% filter(rnk_player <= 100L))

shots_by_game <-
  shots_top100 %>%
  group_by(rnk_player, player_id, player_name, position, game_id, start_time_utc, total_shots, mp) %>%
  summarize(
    shots = n(),
    across(c(g, xg), sum)
  ) %>%
  ungroup()
shots_by_game

.w <- 10
.p <- 0.9
shots_by_game_w <-
  shots_by_game %>%
  arrange(rnk_player, start_time_utc) %>%
  group_by(player_id) %>%
  mutate(
    across(c(g, shots, mp), list(w = ~slider::slide_int(.x, sum, .before = (!!.w - 1)))),
    across(xg, list(w = ~slider::slide_dbl(.x, sum, .before = (!!.w - 1))))
  ) %>%
  ungroup() %>%
  mutate(
    g_p90_w = 90 * g_w / mp_w,
    xg_p90_w = 90 * xg_w / mp_w,
    xgd_p90_w = 90 * (g_w - xg_w) / mp_w,
    xgd_w = g_w - xg_w,
    g_ps_w = g_w / shots_w,
    xg_ps_w = xg_w / shots_w,
    xgd_ps_w = g_ps_w - xg_ps_w
  ) %>%
  mutate(
    z_o = (g_ps_w - xg_ps_w) / sqrt((xg_ps_w * (1 - xg_ps_w)) / shots_w),
    z_u = (xg_ps_w - g_ps_w) / sqrt((g_ps_w * (1 - g_ps_w)) / shots_w),
    p_o = pnorm(z_o),
    p_u = pnorm(z_u),
    is_signif_o = p_o > !!.p,
    is_signif_u = p_u > !!.p
  )
shots_by_game_w

player_ids_filt <-
  players_filt %>%
  filter(player_name %in% c('Harry Kane', 'Romelu Lukaku')) %>%
  distinct(player_id, player_name)
player_ids_filt

shots_by_game_w_filt <-
  shots_by_game_w %>%
  semi_join(player_ids_filt) %>%
  group_by(player_name) %>%
  slice_max(start_time_utc, n = 50) %>%
  ungroup() %>%
  group_by(player_name) %>%
  mutate(idx_gp = row_number(start_time_utc)) %>%
  ungroup() %>% 
  left_join(shots %>% distinct(game_id, player_id, league_name)) %>% 
  mutate(is_epl = league_name == 'Premier League')
shots_by_game_w_filt

color_g <- hcl(h = 180, c = 150, l = 80)
color_xg <- hcl(h = 360, c = 150, l = 80)

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

shots_by_game_w_filt %>%
  ggplot() +
  aes(x = idx_gp) +
  facet_wrap(~player_name, ncol = 1) +
  geom_segment(
    aes(xend = idx_gp, y = xg_p90_w, yend = g_p90_w, color = is_epl)
  )

color_signif <- '#3ec9ff'
p <-
  shots_by_game_w_filt %>%
  ggplot() +
  aes(x = idx_gp) +
  facet_wrap(~player_name, ncol = 1) +
  geom_segment(
    data = shots_by_game_w_filt %>% filter(xgd_w > 0 & !is_signif_o),
    aes(xend = idx_gp, y = xg_p90_w, yend = g_p90_w),
    linetype = '11',
    alpha = 1,
    size = 1,
    # arrow = arw,
    color = color_g
  ) +
  geom_segment(
    data = shots_by_game_w_filt %>% filter(xgd_w < 0 & !is_signif_o),
    aes(xend = idx_gp, y = xg_p90_w, yend = g_p90_w),
    linetype = '11',
    alpha = 1,
    size = 1,
    # arrow = arw,
    color = color_xg
  ) +
  geom_segment(
    data = shots_by_game_w_filt %>% filter(is_signif_o),
    aes(xend = idx_gp, y = xg_p90_w, yend = g_p90_w),
    size = 2,
    # arrow = arw,
    color = color_signif
  ) +
  geom_segment(
    data = shots_by_game_w_filt %>% filter(is_signif_u),
    aes(xend = idx_gp, y = xg_p90_w, yend = g_p90_w),
    size = 2,
    # arrow = arw,
    color = color_signif
  ) +
  geom_point(
    data = shots_by_game_w_filt,
    aes(y = g_p90_w),
    size = 1.5,
    color = color_g
  ) +
  geom_point(
    data = shots_by_game_w_filt,
    aes(y = xg_p90_w),
    size = 1.5,
    color = color_xg
  ) +
  # geom_point(
  #   data = shots_by_game_w_filt %>% filter(is_signif_o),
  #   aes(y = g_p90_w),
  #   size = 3,
  #   color = color_g
  # ) +
  # geom_point(
  #   data = shots_by_game_w_filt %>% filter(is_signif_u),
  #   aes(y = xg_p90_w),
  #   size = 3,
  #   color = color_xg
  # ) +
  theme(
    plot.title = ggtext::element_markdown(size = 16),
    plot.subtitle = ggtext::element_markdown(size = 14),
    # axis.text = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  scale_x_continuous(
    labels = c('50', '25', 'Prior\ngame'),
    # expand = c(0.1, 0.1)
    breaks = c(1, 25, 50),
    limits = c(1, 50)
  ) +
  labs(
    title = glue::glue('When do elite scorers like Kane and Lukaku <span style="color:{color_signif}">significantly</span> overperform their xG?'),
    subtitle = glue::glue('<i>Even when they are scoring at a higher rate than their xG implies (10-game rolling <span style="color:{color_g}">G/90</span> > <span style="color:{color_xg}">xG/90</span>),<br/>they are rarely doing so at a rate that we can say is <span style="color:{color_signif}">statistically significantly</span>.</i>'),
    x = 'Games Ago',
    y = 'Goals/90'
  )
p

x1 <- shots_by_game_w_filt %>% slice_max(g_p90_w)
x2 <- shots_by_game_w_filt %>% slice_min(xg_p90_w)
x3 <- shots_by_game_w_filt %>% slice_max(xgd_p90_w)
arw_annotate <- arrow(length = unit(5, 'pt'), type = 'closed')

p2 <-
  p + 
  geom_segment(
    data = tibble(player_name = 'Romelu Lukaku'),
    aes(x = 2, xend = 10, y = 0.1, yend = 0.1),
    linetype = 1,
    size = 1.2,
    color = gray_grid_wv,
    arrow = arw_annotate
  ) +
  geom_text(
    data = tibble(player_name = 'Romelu Lukaku', lab = 'Longer ago to more recent'),
    aes(x = 2, y = 0.15, label = lab),
    hjust = 0,
    size = pts(12),
    color = 'white'
  ) +
  geom_segment(
    data = x1,
    aes(
      x = idx_gp - 5,
      y = g_p90_w + 0,
      xend = idx_gp - 1,
      yend = g_p90_w
    ),
    size = 0.5,
    # curvature = -0.25,
    color = 'white',
    arrow = arw_annotate
  ) +
  # geom_curve(
  #   data = x2,
  #   aes(
  #     x = idx_gp + 5,
  #     y = xg_p90_w - 0.1,
  #     xend = idx_gp + 0.1,
  #     yend = xg_p90_w - 0.02
  #   ),
  #   # size = 0.5,
  #   curvature = -0.25,
  #   color = 'white',
  #   arrow = arw_annotate
  # ) +
  geom_text(
    data = x3,
    aes(y = g_p90_w + 0.05, label = sprintf('%0.02f', g_p90_w)),
    size = pts(12),
    color = color_g
  ) +
  geom_curve(
    data = x3,
    aes(
      x = idx_gp + 3,
      y = g_p90_w + 0.05,
      xend = idx_gp + 1,
      yend = g_p90_w + 0.05
    ),
    color = 'white',
    curvature = 0.2,
    arrow = arw_annotate
  ) +
  geom_text(
    data = x3,
    aes(x = idx_gp + 3.5, y = g_p90_w + 0.05, label = 'xG/90'),
    nudge_x = 0.7,
    size = pts(12),
    color = color_g
  ) +
  geom_text(
    data = x3,
    aes(y = xg_p90_w - 0.2, label = sprintf('%0.02f', xg_p90_w)),
    size = pts(12),
    color = color_xg
  ) +
  geom_curve(
    data = x3,
    aes(
      x = idx_gp - 3,
      y = xg_p90_w - 0.2,
      xend = idx_gp - 1,
      yend = xg_p90_w - 0.2
    ),
    color = 'white',
    curvature = -0.2 
    arrow = arw_annotate
  ) +
  geom_text(
    data = x3,
    aes(x = idx_gp - 3.5, y = xg_p90_w - 0.2, label = 'G/90'),
    size = pts(12),
    nudge_x = -0.5,
    color = color_xg
  ) +
  geom_curve(
    data = x3,
    aes(
      x = idx_gp - 5,
      y = g_p90_w + 0.2,
      xend = idx_gp - 0.5,
      yend = g_p90_w + -0.05
    ),
    color = 'white',
    # curvature = 0.1,
    arrow = arw_annotate
  ) +
  ggtext::geom_richtext(
    data = 
      x3 %>% 
      mutate(lab = glue::glue('<span style="color:white">10-game xG/90 - G/90 = <b>{scales::number(xgd_p90_w, accuracy = 0.01)}</b></span>')),
    aes(x = idx_gp - 5, y = g_p90_w + 0.2, label = lab),
    fill = NA_character_,
    label.color = NA_character_,
    hjust = 1,
    family = 'Karla'
  )
p2

ggsave(
  filename = file.path(dir_proj, 'overperformance.png'),
  p2,
  width = 10,
  height = 10
)
