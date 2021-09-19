
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
  plot.caption = ggtext::element_markdown('Karla', size = 10, color = 'white', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'white', hjust = 0),
  plot.tag.position = c(.01, 0.01),
  # legend.text = element_text(color = 'gray20', size = 14),
  strip.text = element_text('Karla', color = 'white', face = 'bold', size = 12, hjust = 0),
  # strip.background = element_blank(),
  panel.background = element_rect(fill = gray_wv, color = gray_wv)
  
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

dir_proj <- '40-xg_overperformance'
import <- function(x) {
  file.path(dir_proj, sprintf('2010-2022_%s.parquet', x)) %>% 
    arrow::read_parquet()
}
df <- import('xg')
mp <- import('mp')

players <-
  df %>%
  count(player_id, player_name, position, sort = TRUE, name = 'total_shots') %>%
  mutate(rnk_player = row_number(desc(total_shots)))
players

shots <-
  df %>%
  # drop own goals
  filter(g != 3L) %>%
  inner_join(
    players %>%
      left_join(mp %>% group_by(player_id) %>% summarize(total_mp = sum(minutes_played, na.rm = TRUE))) %>% 
      select(rnk_player, player_id, total_shots, total_mp)
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
  filter(player_name %in% c('Harry Kane', 'Romelu Lukaku', 'Timo Werner')) %>% #, 'Son Heung-Min')) %>%
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
  left_join(shots %>% distinct(game_id, player_id, league_name))
shots_by_game_w_filt

color_g <- hcl(h = 180, c = 150, l = 80)
color_xg <- hcl(h = 360, c = 150, l = 80)
# scales::show_col(color_signif_o)
color_signif_o <- '#f8de7f'
color_signif_u <- '#abadff'

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}
arw_annotate <- arrow(length = unit(5, 'pt'), type = 'closed')

lab_caption <- 'Statistically significant: p-value < 0.1 where the null hypothesis is G/shot = xG/shot over a 10-game window.<br/>Non-EPL matches included.'
plot_base <- function(df) {
  df %>%
    ggplot() +
    aes(x = idx_gp) +
    geom_segment(
      data = df %>% filter(xgd_w > 0 & !is_signif_o),
      aes(xend = idx_gp, y = xg_p90_w, yend = g_p90_w),
      linetype = '11',
      alpha = 1,
      size = 1,
      color = color_g
    ) +
    geom_segment(
      data = df %>% filter(xgd_w < 0 & !is_signif_o),
      aes(xend = idx_gp, y = xg_p90_w, yend = g_p90_w),
      linetype = '11',
      alpha = 1,
      size = 1,
      color = color_xg
    ) +
    geom_segment(
      data = df %>% filter(is_signif_o),
      aes(xend = idx_gp, y = xg_p90_w, yend = g_p90_w),
      size = 2,
      color = color_signif_o
    ) +
    geom_segment(
      data = df %>% filter(is_signif_u),
      aes(xend = idx_gp, y = xg_p90_w, yend = g_p90_w),
      size = 2,,
      color = color_signif_u
    ) +
    geom_point(
      data = df,
      aes(y = g_p90_w),
      size = 1.5,
      color = color_g
    ) +
    geom_point(
      data = df,
      aes(y = xg_p90_w),
      size = 1.5,
      color = color_xg
    ) +
    scale_x_continuous(
      labels = c('50', '25', 'Last'),
      breaks = c(1, 25, 50),
      limits = c(1, 50)
    ) +
    coord_cartesian(clip = 'off') +
    theme(
      plot.title = ggtext::element_markdown(size = 18),
      plot.subtitle = ggtext::element_markdown(size = 16),
      panel.grid.major.y = element_blank()
    ) +
    labs(
      tag = '**Viz**: Tony ElHabr',
      caption = lab_caption,
      x = 'Games Ago',
      y = 'Goals/90'
    )
}

kane_luk <- shots_by_game_w_filt %>% filter(player_name %in% c('Harry Kane', 'Romelu Lukaku'))
kane_luk_lab <- kane_luk %>% slice_max(xgd_p90_w)

p_kane_luk <-
  kane_luk %>%
  plot_base() +
  facet_wrap(~player_name, ncol = 1) +
  theme(
    plot.title = ggtext::element_markdown(size = 18),
    plot.subtitle = ggtext::element_markdown(size = 16),
    # axis.text = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  # ylim(0, 1.15) +
  ggtext::geom_richtext(
    data = tibble(lab = glue::glue('<b><span style="color:white">Even when they are scoring at a higher rate than their xG implies (10-game rolling <span style="color:{color_g}">G/90</span> > <span style="color:{color_xg}">xG/90</span>),<br/>they are rarely doing so at a rate that we can say is <span style="color:{color_signif_o}">statistically significantly</span>.</span></b>'), player_name = 'Harry Kane'),
    aes(x = 50, y = 1.2, label = lab),
    family = 'Karla',
    size = pts(11.5),
    fill = NA_character_,
    label.color = NA_character_,
    hjust = 1,
    vjust = 0
  ) +
  scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1.3), expand = c(0, 0.05), oob = function(x, ...) x) +
  coord_cartesian(clip = 'off') +
  labs(
    title = glue::glue('When do elite scorers like Kane and Lukaku <span style="color:{color_signif_o}">significantly</span> overperform their xG?')
  ) + 
  # bottom left arrow for x-axis
  geom_segment(
    data = tibble(player_name = 'Romelu Lukaku'),
    aes(x = 2, xend = 10, y = 0.01, yend = 0.01),
    linetype = 1,
    size = 1.2,
    color = gray_grid_wv,
    arrow = arw_annotate
  ) +
  # the arrow for x-axis
  geom_text(
    data = tibble(player_name = 'Romelu Lukaku', lab = 'Longer ago to more recent'),
    aes(x = 2, y = 0.06, label = lab),
    hjust = 0,
    size = pts(12),
    color = 'white'
  ) +
  # arrows for kane and lukaku at the end
  geom_curve(
    data = tibble(player_name = 'Harry Kane'),
    aes(x = 49, xend = 50, y = 0, yend = 0.3),
    linetype = 1,
    curvature = -0.2,
    color = 'white',
    arrow = arw_annotate
  ) +
  geom_curve(
    data = tibble(player_name = 'Romelu Lukaku'),
    aes(x = 49, xend = 50, y = 1.35, yend = 1.05),
    linetype = 1,
    curvature = 0.2,
    color = 'white',
    arrow = arw_annotate
  ) +
  # label pointing out recent performance
  ggtext::geom_richtext(
    data = 
      tibble(player_name = 'Romelu Lukaku', lab = glue::glue('<span style="color:white">Both are currently <b><span style="color:{color_signif_o}">significantly</span></b> over-performing.</span>')),
    aes(x = 49.5, y = 1.45, label = lab),
    fill = NA_character_,
    label.color = NA_character_,
    hjust = 1,
    family = 'Karla'
  ) +
  # g/90 value
  geom_text(
    data = kane_luk_lab,
    aes(y = g_p90_w + 0.05, label = sprintf('%0.02f', g_p90_w)),
    size = pts(12),
    color = color_g
  ) +
  # g/90 value
  geom_text(
    data = kane_luk_lab,
    aes(x = idx_gp - 3.5, y = xg_p90_w - 0.2, label = 'G/90'),
    size = pts(12),
    nudge_x = -0.5,
    color = color_xg
  ) +
  # g/90 arrow
  geom_curve(
    data = kane_luk_lab,
    aes(
      x = idx_gp + 2.5,
      y = g_p90_w + 0.05,
      xend = idx_gp + 1.5,
      yend = g_p90_w + 0.05
    ),
    color = 'white',
    curvature = 0.2,
    arrow = arw_annotate
  ) +
  # xg/90 label
  geom_text(
    data = kane_luk_lab,
    aes(x = idx_gp + 3.5, y = g_p90_w + 0.05, label = 'xG/90'),
    nudge_x = 0.7,
    size = pts(12),
    color = color_g
  ) +
  # xg/90 value
  geom_text(
    data = kane_luk_lab,
    aes(y = xg_p90_w - 0.2, label = sprintf('%0.02f', xg_p90_w)),
    size = pts(12),
    color = color_xg
  ) +
  # xg/90 arrow
  geom_curve(
    data = kane_luk_lab,
    aes(
      x = idx_gp - 2.5,
      y = xg_p90_w - 0.2,
      xend = idx_gp - 1.5,
      yend = xg_p90_w - 0.2
    ),
    color = 'white',
    curvature = 0.2,
    arrow = arw_annotate
  ) +
  # arrow pointing up from below xg/90 for specified interval
  geom_segment(
    data = kane_luk_lab,
    aes(
      x = idx_gp,
      y = xg_p90_w - 0.18,
      xend = idx_gp,
      yend = xg_p90_w - 0.02
    ),
    color = 'white',
    arrow = arw_annotate
  ) +
  # arrow pointing to desciption of labeled interval
  geom_curve(
    data = kane_luk_lab,
    aes(
      x = idx_gp - 5,
      y = g_p90_w + 0.2,
      xend = idx_gp - 0.5,
      yend = g_p90_w + -0.05
    ),
    color = 'white',,
    arrow = arw_annotate
  ) +
  # label for specified interval
  ggtext::geom_richtext(
    data = 
      kane_luk_lab %>% 
      mutate(lab = glue::glue('<span style="color:white">10-game G/90 - xG/90 = <b>{scales::number(xgd_p90_w, accuracy = 0.01)}</b></span>')),
    aes(x = idx_gp - 5, y = g_p90_w + 0.2, label = lab),
    fill = NA_character_,
    label.color = NA_character_,
    hjust = 1,
    family = 'Karla'
  )
path1_kane_luk <- file.path(dir_proj, 'overperformance.png')

ggsave(
  filename = path1_kane_luk,
  plot = p_kane_luk,
  width = 10,
  height = 10
)

generate_logo_path <- function(x) {
  file.path(dir_proj, sprintf('%s.png', x))
}

path2_kane_luk <- 
  add_logo(
    path1_kane_luk, 
    path_logo = generate_logo_path('harry-kane'), 
    idx_x = 0.78, 
    idx_y = 0.80, 
    # delete = FALSE, 
    path_suffix = '_w_kane'
  )

path3_kane_luk <- 
  add_logo(
    path2_kane_luk, 
    path_logo = file.path(dir_proj, 'romelu-lukaku.png'), 
    idx_x = 0.78, 
    idx_y = 0.37, 
    # delete = FALSE,
    path_suffix = '_lukaku'
  )

# werner ----
werner <- shots_by_game_w_filt %>% filter(player_name == 'Timo Werner')
werner_lab <- werner %>% filter(idx_gp == 20L)
p_werner <-
  werner %>%
  plot_base() +
  geom_segment(
    data = tibble(),
    aes(x = 2, xend = 10, y = 0.01, yend = 0.01),
    linetype = 1,
    size = 1,
    color = gray_grid_wv,
    arrow = arw_annotate
  ) +
  geom_text(
    data = tibble(lab = 'Longer ago to more recent'),
    aes(x = 2, y = 0.04, label = lab),
    hjust = 0,
    size = pts(10),
    color = 'white'
  ) +
  # description arrow
  geom_curve(
    data = werner_lab,
    aes(
      x = idx_gp + 6,
      y = xg_p90_w + 0.2,
      xend = idx_gp + 1,
      yend = xg_p90_w + 0.04
    ),
    color = 'white',
    curvature = -0.1,
    arrow = arw_annotate
  ) +
  # description
  ggtext::geom_richtext(
    data = tibble(),
    aes(x = 26.5, y = 0.55, label = glue::glue('<b><span style="color:{color_signif_u}">y i k e s</span></b>')),
    size = pts(14),
    fill = NA_character_,
    label.color = NA_character_,
    hjust = 0,
    family = 'Karla'
  ) +
  scale_y_continuous(breaks = c(0, 0.4, 0.8), limits = c(0, 0.8), expand = c(0, 0.05), oob = function(x, ...) x) +
  theme(
    plot.caption = ggtext::element_markdown(size = 8),
    plot.tag = ggtext::element_markdown(size = 10)
  ) +
  labs(
    title = glue::glue('What does <span style="color:{color_signif_u}">significant</span> xG under-performance look like?'),
    caption = lab_caption %>% str_replace('hypothesis', 'hypothesis<br/>')
  ) +
  # xg/90 arrow
  geom_curve(
    data = werner_lab,
    aes(
      x = idx_gp + 4,
      y = xg_p90_w + 0.02,
      xend = idx_gp + 2,
      yend = xg_p90_w + 0.02
    ),
    color = 'white',
    curvature = -0.2,
    arrow = arw_annotate
  ) +
  # xg/90 value
  geom_text(
    data = werner_lab,
    aes(x = idx_gp + 0.5, y = xg_p90_w + 0.02, label = sprintf('%0.02f', xg_p90_w)),
    size = pts(10),
    color = color_xg
  ) +
  # xg/90 label
  geom_text(
    data = werner_lab,
    aes(x = idx_gp + 5.5, y = xg_p90_w + 0.02, label = 'xG/90'),
    nudge_x = 0.7,
    size = pts(10),
    color = color_xg
  ) +
  # g/90 arrow
  geom_curve(
    data = werner_lab,
    aes(
      x = idx_gp - 5,
      y = g_p90_w - 0.0,
      xend = idx_gp - 3.5,
      yend = g_p90_w - 0.0
    ),
    color = 'white',
    curvature = 0.2,
    arrow = arw_annotate
  ) +
  # g/90 value
  geom_text(
    data = werner_lab,
    aes(x = idx_gp - 1, y = g_p90_w, label = sprintf('%0.02f', g_p90_w)),
    size = pts(10),
    hjust = 1,
    color = color_g
  ) +
  # g/90 label
  geom_text(
    data = werner_lab,
    aes(x = idx_gp - 6, y = g_p90_w + 0.0, label = 'G/90'),
    nudge_x = -0.7,
    size = pts(10),
    color = color_g
  )
p_werner
path1_werner <- file.path(dir_proj, 'underperformance.png')

ggsave(
  filename = path1_werner,
  p_werner,
  width = 7,
  height = 7
)

add_logo(
  path1_werner, 
  path_logo = generate_logo_path('timo-werner'), 
  idx_x = 0.06,
  idx_y = 0.72, 
  # delete = FALSE,
  path_suffix = '_w_werner',
  logo_scale = 0.2
)

