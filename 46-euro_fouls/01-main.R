
library(tidyverse)
library(arrow)
# library(ebbr)
# library(rPref)
library(ggfittext)

gray_wv <- rgb(24, 24, 24, maxColorValue = 255)
gray_grid_wv <- rgb(64, 64, 64, maxColorValue = 255)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla', color = 'white'),
  title = element_text('Karla', size = 14, color = 'white'),
  plot.title = element_text('Karla', face = 'bold', size = 24, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', size = 18, color = '#f1f1f1'),
  axis.text = element_text('Karla', color = 'white', size = 14),
  axis.title = element_text('Karla', size = 14, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = gray_grid_wv),
  panel.grid.minor = element_line(color = gray_grid_wv),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = gray_wv, color = gray_wv),
  plot.caption = element_text('Karla', color = 'white', hjust = 1, size = 11, face = 'italic'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'white', hjust = 0),
  plot.tag.position = c(0.01, 0.01),
  strip.text = element_text('Karla', color = 'white', face = 'bold', size = 12, hjust = 0),
  panel.background = element_rect(fill = gray_wv, color = gray_wv)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

dir_proj <- '46-euro_fouls'
f_import <- function(name) {
  path <- file.path(dir_proj, sprintf('%s.parquet', name))
  res <- path %>% arrow::read_parquet()
  assign(value = res, x = name, envir = .GlobalEnv)
}

c(
  'all_actions',
  'players_filt',
  'games_by_team'
) %>% 
  walk(f_import)

rage_quits <- all_actions %>% 
  filter(
    type_name == 'foul',
    turnover_player_id == player_id,
    time_since_last_turn <= 7
  ) %>% 
  arrange(desc(game_date))
rage_quits

n_rage_quits <- rage_quits %>% 
  count(competition_id, season_id, player_id, player_name, sort = TRUE) %>% 
  left_join(players_filt)
n_rage_quits

top_n_rage_quits <- n_rage_quits %>% 
  # filter(season_id > 2017) %>% 
  filter(n == 4) %>% 
  slice_min(
    total_mp,
    n = 4,
    with_ties = FALSE
  ) %>% 
  bind_rows(
    n_rage_quits %>% filter(n == 5)
  ) %>% 
  mutate(
    lab = sprintf("%s '%s", str_replace_all(player_name, '(^.*\\s)(.*$)', '\\2'), str_sub(season_id, 3, 4))
  )
cancelo_rage_quits <- top_n_rage_quits %>% filter(player_name == 'Joao Cancelo')
top_n_rage_quits_wo_cancelo <- top_n_rage_quits %>% anti_join(cancelo_rage_quits)
n_rage_quits_minus_top <- n_rage_quits %>% anti_join(top_n_rage_quits) 
pal <- c('#ef426f', '#00b2a9', '#ff8200', '#7a5195') %>% rev() # spurs fiesta + purple
pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

y_labs <- c('0', '1,000', '2,000 minutes played', '3,000', '4,000')
base_plot <- function(data, point.size = 1, segment.size = 0.5, ...) {
  data %>% 
    ggplot() +
    aes(x = total_mp, y = n, alpha = n, color = factor(n)) +
    coord_polar() +
    geom_point(size = point.size) +
    geom_segment(
      size = segment.size,
      aes(xend = 0, yend = 0)
    ) +
    scale_color_manual(values = pal) +
    guides(color = 'none', alpha = 'none') +
    theme(
      axis.text.y = element_blank()
    ) +
    scale_x_continuous(
      limits = c(0, 4000), 
      labels = y_labs
    )
}

add_labels <- function(..., .data, .color = 'white') {
  list(
    ...,
    geom_point(
      data = .data,
      size = 2,
      alpha = 1,
      color = .color
    ),
    geom_segment(
      data = .data,
      # alpha = 0.5,
      size = 1,
      alpha = 0.5,
      aes(xend = 0, yend = 0),
      color = .color
    ),
    ggrepel::geom_text_repel(
      data = .data,
      family = 'Karla',
      color = .color,
      size = pts(16),
      fontface = 'bold',
      seed = 42,
      # vjust = 1.5,
      # hjust = -.5,
      alpha = 1,
      aes(label = lab, y = n + 0.7, x = total_mp - 100)
    )
  )
}

cancelo_blue <- '#6caddf'
p <- n_rage_quits_minus_top %>% 
  base_plot() +
  add_labels(.data = top_n_rage_quits_wo_cancelo, .color = 'white') +
  add_labels(.data = cancelo_rage_quits, .color = cancelo_blue) +
  geom_text(
    inherit.aes = FALSE,
    color = 'white',
    fontface = 'bold',
    size = pts(14),
    data = tibble(x = 100, y = 1:5) %>% 
      mutate(lab = ifelse(y == 5, '5 "rage" fouls', y)),
    aes(x = x, y = y, label = lab),
    nudge_x = 1,
    # nudge_y = 1,
    vjust = -1,
    hjust = 0
  ) +
  labs(
    y = NULL,
    x = NULL
  ) +
  theme(
    plot.title = element_text(hjust = 0),
    plot.subtitle = ggtext::element_markdown(hjust = 0),
    plot.caption = ggtext::element_markdown(size = 14, face = 'plain')
  ) +
  labs(
    title = 'Who "rage" fouls most frequently?',
    subtitle = glue::glue('<span style="color:{cancelo_blue}"><b>Cancelo</b></span> has commited the most "rage" fouls (4)<br>in the 2021/22 Premier League season.'),
    caption = '<i>"Rage" foul: a foul within 7 seconds of own turnover</i><br/><br/><b>Viz</b>: Tony ElHabr'
  )

path_rage_quit <- file.path(dir_proj, 'rage_quit.png')
ggsave(
  plot = p,
  filename = path_rage_quit,
  w = 10,
  h = 10
)

arw_annotate <- arrow(length = unit(5, 'pt'), type = 'closed')
p_inset <- tibble::tibble(
  total_mp = c(2700),
  n = 1:4
) %>% 
  base_plot(point.size = 3, segment.size = 2) +
  scale_alpha_continuous(range = c(1, 1)) +
  geom_fit_text(
    inherit.aes = FALSE,
    data = tibble(
      xmin = c(100),
      xmax = c(1200),
      ymin = c(2),
      ymax = c(3),
      lab = 'more minutes'
    ),
    color = 'white',
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = lab),
    min.size = 0, grow = TRUE
  ) +
  geom_segment(
    inherit.aes = FALSE,
    aes(x = 1250, xend = 1500, y = 2.5, yend = 2.5),
    linetype = 1,
    color = 'white',
    arrow = arw_annotate
  ) +
  geom_fit_text(
  inherit.aes = FALSE,
  data = tibble(
    xmin = c(2700),
    xmax = c(4000),
    ymin = c(0.1),
    ymax = c(3),
    lab = 'more fouls'
  ),
  color = 'white',
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = lab),
  min.size = 0, grow = TRUE
) +
  geom_segment(
    inherit.aes = FALSE,
    aes(x = 3330, xend = 3330, y = 2, yend = 4),
    linetype = 1,
    color = 'white',
    arrow = arw_annotate
  ) +
  theme(
    plot.background = element_rect(color = 'white'),
    axis.text = element_blank()
  ) +
  labs(x = NULL, y = NULL)
p_inset

path_inset <- file.path(dir_proj, 'rage_quit_insent.png')
ggsave(
  plot = p_inset,
  filename = path_inset,
  w = 8,
  h = 8
)

path_w_inset <- add_logo(
  path_viz = path_rage_quit,
  path_logo = path_inset,
  logo_scale = 0.25,
  idx_x = 0.99,
  idx_y = 0.01,
  adjust_x = FALSE,
  adjust_y = TRUE,
  path_suffix = '_w_inset',
  delete = FALSE
)

add_logo(
  path_viz = path_w_inset,
  path_logo = file.path(dir_proj, 'cancelo-eyes.png'),
  logo_scale = 0.2,
  idx_x = 0.01,
  idx_y = 0.99,
  adjust_x = TRUE,
  adjust_y = FALSE,
  path_suffix = '_and_cancelo',
  delete = FALSE
)

# euro ---

fouls <- all_actions %>% filter(type_name == 'foul')
turnovers <- all_actions %>% filter(turnover)
euro_fouls <- all_actions %>%
  filter(
    type_name == 'foul',
    time_between_poss <= 5,
    time_since_poss_start <= 3
  )
euro_fouls


do_count_euro_fouls <- function(cols) {
  col_syms <- syms(cols)
  n_euro_fouls_by_x <- euro_fouls %>%
    count(
      competition_id,
      season_id,
      !!!col_syms,
      name = 'n_euro_fouls',
      sort = TRUE
    )
  
  n_fouls_by_x <- fouls %>%
    count(
      competition_id,
      season_id,
      !!!col_syms,
      name = 'total_fouls',
      sort = TRUE
    )
  
  n_euro_fouls_by_x %>%
    left_join(
      n_fouls_by_x
    ) %>% 
    mutate(
      prop_fouls = n_euro_fouls / total_fouls
    )
}

euro_fouls_by_player <- do_count_euro_fouls(
  c('player_id')
) %>% 
  left_join(players_filt) %>%
  left_join(games_by_team)


df <- euro_fouls_by_player %>% 
  transmute(
    season_id,
    player_id, 
    player_name,
    pos_11,
    pos_grp,
    n_euro_fouls,
    total_fouls,
    prop_fouls,
    total_mp_scaled = total_mp * inv_prop_matches,
    prop_fouls_scaled = prop_fouls * inv_prop_matches,
    n_euro_fouls_scaled = n_euro_fouls * inv_prop_matches,
    total_fouls_scaled = total_fouls * inv_prop_matches
  )
df

df_adj <- df %>% 
  filter(pos_grp %in% c('D', 'M')) %>% 
  add_ebb_estimate(
    n_euro_fouls,
    total_fouls,
    method = 'mle',
    prior_subset = total_fouls >= 10
  ) %>% 
  mutate(
    prnk1 = percent_rank(.fitted),
    prnk2 = percent_rank(-total_fouls_scaled),
    prnk3 = percent_rank(total_mp_scaled),
    prnk = prnk1 + prnk2 + prnk3,
    size = (1 - (1 - prnk1))^2 * (1 - (1 - prnk2))^2
  )
df_adj %>% 
  group_by(player_id, player_name, pos_grp, pos_11) %>% 
  summarize(
    n = n(),
    across(prnk, sum)
  ) %>% 
  ungroup() %>% 
  filter(n >= 4) %>% 
  mutate(prnk_avg = prnk / n) %>% 
  group_by(pos_11) %>% 
  slice_max(prnk_avg, n = 3, with_ties = FALSE) %>% 
  ungroup()


# f1 <- df_adj %>% 
#   lm(total_fouls_scaled ~ total_mp_scaled, data = .) %>% 
#   broom::augment()
# 
# f1 %>% 
#   ggplot() +
#   # aes(x = .fitted, y = total_fouls_scaled) +
#   aes(x = total_mp_scaled, y = total_fouls_scaled, color = -.resid) +
#   geom_abline(
#     data = tibble(intercept = rep(0, 3), slope = c(0.5, 1, 1.5)/90),
#     aes(intercept = intercept, slope = slope)
#   ) +
#   # scale_radius(range = c(0.1, 3)) +
#   scale_x_continuous(
#     limits = c(0, 3800),
#     breaks = 1:4 * 10 * 90,
#     labels =  1:4 * 10,
#     expand = c(0, 0.05)
#   ) +
#   # scale_color_viridis_c(option = 'D', direction = -1) +
#   scale_color_gradient2(midpoint = median(f1$.resid), mid = 'green', high = 'cyan', low = 'red') +
#   # scale_color
#   geom_point(show.legend = FALSE)

df_adj_pref <- df_adj %>% 
  psel(
    low(total_mp_scaled) | high(total_fouls_scaled), top = nrow(.)
  ) %>% 
  rename(z = .level)

player_names <- c('James Ward-Prowse', 'Declan Rice', 'Conor Coady', 'Aaron Cresswell')
df_adj_pref_filt <- df_adj_pref %>% 
  filter(player_name %in% player_names) %>% 
  mutate(
    lab = sprintf("%s '%s", str_replace_all(player_name, '(^.*\\s)(.*$)', '\\2'), season_id)
  )
add_players <- function(...) {
  list(
    ...,
    geom_point(
      data = df_adj_pref_filt,
      shape = 19,
      size = 4,
      color = 'black'
    ),
    ggrepel::geom_text_repel(
      data = df_adj_pref_filt,
      aes(label = lab)
    )
  )
}

p1 <- df_adj_pref %>% 
  ggplot() +
  aes(x = total_mp_scaled, y = total_fouls_scaled) +
  scale_x_continuous(
    limits = c(0, 4500),
    breaks = 1:4 * 10 * 90,
    labels = 1:4 * 10,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    expand = c(0.02, 0)
  ) +
  geom_abline(
    data = tibble(intercept = rep(0, 3), slope = c(0.5, 1, 1.5)/90),
    aes(intercept = intercept, slope = slope)
  ) +
  geom_point(
    aes(fill = z),
    shape = 21,
    size = 1.5, 
    show.legend = FALSE
  ) +
  scale_fill_viridis_c(option = 'H') +
  add_players() +
  theme(
    # panel.background = element_rect(),
    panel.border = element_rect(fill = NA),
    panel.grid.major = element_blank()
  ) +
  labs(
    x = "90's played",
    y = 'Total Fouls'
  )
p1


res <- df_adj %>% psel(low(.fitted) | high(total_fouls_scaled), top = nrow(df_adj))
res
res %>% 
  ggplot() +
  # aes(x = .fitted, y = total_fouls_scaled) +
  aes(x = .fitted, y = total_fouls_scaled, color = .level) +
  # scale_size_area(max_size = 3) +
  scale_alpha_continuous(range = c(0.25, 1)) +
  geom_point()

# xi <- mean(df_adj$.fitted)
# yi <- mean(df_adj$total_fouls_scaled)
# xi; yi
# yi/xi
# fit_poly <- lm(total_fouls_scaled ~ .fitted + I(.fitted^2), df_adj)
# fit_poly
df_adj %>% 
  ggplot() +
  # aes(x = .fitted, y = total_fouls_scaled) +
  aes(x = prnk1, y = prnk2, alpha = size, size = size) +
  # scale_size_area(max_size = 3) +
  geom_point(
    data = df_adj %>% filter(prnk1 >= 0.5, prnk2 >= 0.5),
    color = 'green'
  ) +
  geom_point(
    data = df_adj %>% filter(prnk1 <= 0.5, prnk2 >= 0.5),
    color = 'red'
  ) +
  geom_point(
    data = df_adj %>% filter(prnk1 <= 0.5, prnk2 <= 0.5),
    color = 'blue'
  ) +
  geom_point(
    data = df_adj %>% filter(prnk1 >= 0.5, prnk2 <= 0.5),
    color = 'magenta'
  ) +
  # geom_smooth(
  #   method = 'lm',
  #   formula = formula(y ~ x + I(x^2))
  # ) +
  # geom_function(
  #   fun = function(x) 696-351.1*x + 275.3*(x^2)
  # ) +
  # geom_vline(
  #   aes(xintercept = mean(.fitted^2))
  # ) +
  # geom_hline(
#   aes(yintercept = mean(total_fouls_scaled))
# ) +
# geom_abline(
#   aes(slope = yi/xi, intercept = 0)
# ) +
# coord_cartesian(xlim = c(0, 1)) +
geom_point(
  data = coady,
  size = 3,
  color = 'red'
) +
  geom_point(
    data = silva,
    size = 3,
    color = 'magenta'
  )

