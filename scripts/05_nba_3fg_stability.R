
# Data:
# + 2013-4: https://github.com/hwchase17/sportvu
# + 2015: https://www.kaggle.com/dansbecker/nba-shot-logs
# + 2016(?): https://github.com/wh0801/NBA-shooting-rationality-2016-17-Regular-Season

# Idea
# + https://fansided.com/2014/08/29/long-take-three-point-shooting-stabilize/

extrafont::loadfonts(device = 'win', quiet = TRUE)
library(tidyverse)
library(here)
library(ggtext)
library(patchwork)
library(magick)

theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = element_text('Karla', face = 'bold', size = 18, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', face = 'bold', size = 14, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  # axis.title = element_text(size = 24, face = 'bold'),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  # axis.text = element_text('Karla', size = 12, face = 'bold', color = 'gray20'),
  # axis.title.x = element_text(hjust = 0.95),
  # axis.title.y = element_text(hjust = 0.95),
  # axis.line = element_line(color = 'gray80'),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  # plot.margin = margin(25, 25, 25, 25),
  plot.margin = margin(10, 10, 10, 10),
  # plot.background = element_rect(fill = '#F3F4F6', color = '#F3F4F6'),
  plot.background = element_rect(fill = '#fffaf0', color = NA),
  # plot.caption = element_text(size = 15, face = 'italic'),
  plot.caption = element_text('Karla', size = 14, color = 'gray50', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = element_text('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.02),
  # legend.text = element_text(size = 14),
  legend.text = element_text(size = 14),
  # legend.background = element_rect(fill = '#F3F4F6', color = '#F3F4F6'),
  # legend.position = c(.85, .85))
  panel.background = element_rect(fill = '#fffaf0', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

# path <- 'https://raw.githubusercontent.com/wh0801/NBA-shooting-rationality-2016-17-Regular-Season/master/Project-Team%20shoot%20rationality%20-%20github/Shot%20data/shot%20log%20ATL.csv'
# shots_atl <- path %>% read_csv() %>% janitor::clean_names()
# shots_atl
# shots_atl %>% count(shoot_player, sort = T)
# shots_atl %>% count(date)

# path_shots_2015 <- here::here('data-raw', '05', 'shot_logs.csv')
url_shots_2015 <- 'https://raw.githubusercontent.com/SergioLlana/nba-shot-analysis/master/data/shot_logs.csv'
shots_2015 <- url_shots_2015 %>% read_csv() %>% janitor::clean_names()
shots_2015

# 'all_shots' doesn't have player name
# 'all_chart' has player name, but not shot number (which is fine tbh). Also, would need to use `shot_type` to identify 3 pt shot instead of `PTS_TYPE` (from 'all_shots').
shots_2013_2014 <- 
  c(2013:2014) %>% 
  set_names(., .) %>% 
  map_dfr(
    # ~here::here('data-raw', '05', 'sportvu', glue::glue('joined_shots_{..1}.csv')) %>%
    ~glue::glue('https://raw.githubusercontent.com/hwchase17/sportvu/master/joined_shots_{..1}.csv') %>% 
    read_csv() %>% 
    # Get rid of first (index) column and duplicated columns (auto-renamed by `read_csv()`).
    select(-1, -matches('_1$')), 
    .id = 'year'
  ) %>% 
  janitor::clean_names()
shots_2013_2014

# Found these by inspection.
fix_player_names <- function(x) {
  str_replace_all(
    x,
    c(
      'nowtizski' = 'nowitzki',
      'dwyane wade' = 'dwayne wade', # doesn't take a lot of 3s, but whatever
      'mnta ellis' = 'monta ellis',
      'tim hardaway jr' = 'time hardaway jr',
      'danilo gallinari' = 'danilo gallinai'
    )
  )
}

shots <-
  bind_rows(
    shots_2013_2014,
    shots_2015 %>% mutate(year = '2015')
  ) %>% 
  mutate(
    across(year, as.integer),
    across(player_name, ~tolower(.x) %>% str_remove_all('[.-]'))
  ) %>% 
  mutate(across(player_name, fix_player_names))
shots

# # Inspecting player names to fix... Could do more, but oh well.
# shots_n_by_year <-
#   shots %>% 
#   # group_by(player_name) %>% 
#   # summarize(n = n_distinct(year)) %>% 
#   # ungroup() %>% 
#   filter(pts_type == 3) %>% 
#   count(player_name, year)
# shots_n_by_year
# 
# shots_n1_by_year <-
#   shots_n_by_year %>% 
#   count(player_name) %>% 
#   filter(n == 1L) %>% 
#   select(-n) %>% 
#   inner_join(shots_n_by_year) %>% 
#   arrange(desc(n))
# shots_n1_by_year %>% head(20)
# 
# shots_n_by_year %>% filter(player_name %>% str_detect('fisher'))

shots_mini <-
  shots %>% 
  filter(pts_type == 3) %>% 
  rename(player = player_name) %>% 
  arrange(year, player, shot_number) %>% 
  group_by(player) %>% 
  mutate(idx = row_number()) %>% 
  ungroup() %>% 
  select(player, idx, fgm) %>%
  group_by(player) %>% 
  mutate(fgm_cumu = cumsum(fgm)) %>% 
  mutate(fgm_rate_cumu = fgm_cumu / idx) %>% 
  ungroup()
shots_mini

shots_n <- shots_mini %>% count(player, sort = TRUE)
shots_n %>% skimr::skim()
shots_mini_filt <- shots_mini %>% inner_join(shots_n %>% filter(n >= 100L))
shots_mini_filt

# shots_mini %>% group_by(player_id) %>% summarize(n = n()) %>% arrange(desc(n))
kr20 <- function(data) {
  # data <- data %>% select(-matches('player'))
  # data <- data.matridata(data)
  k <- ncol(data)
  rs <- rowSums(data)
  sigma2 <- var(rs)
  p_i <- colMeans(data)
  q_i <- 1 - p_i
  scaler <- k / (k - 1)
  num <- sigma2 - sum(p_i * q_i)
  res <- scaler * num / sigma2
  res
}

# I was trying this out with other data and columns, hence the partial function.
do_f_at <- function(data, k, col, f) {
  
  col_sym <- sym(col)
  
  data_k <-
    data %>% 
    group_by(player) %>% 
    filter(n() >= k) %>% 
    ungroup() %>% 
    filter(idx <= k)
  
  data_wide <-
    data_k %>%
    select(idx, player, !!col_sym) %>%
    pivot_wider(
      names_from = idx,
      values_from = !!col_sym
    )
  
  res <- data_wide %>% select(-player) %>% f()
  res
}

do_kr20_fgm <- partial(do_f_at, shots_mini_filt, f = kr20, col = 'fgm', ... = )

k_max <- shots_mini_filt %>% filter(idx == max(idx)) %>% pull(idx)
k_max
# NOTE: This should be an integer sequence, and `k` and `idx` should be kept as integers. If it's just a double sequence, then the gif rendering may have issues to do mismtached classes!
k_seq <- seq.int(10L, k_max, by = 10L)
krs_fgm <-
  tibble(k = k_seq) %>% 
  mutate(
    n = map_int(k, ~filter(shots_mini_filt, idx >= .x) %>% distinct(player) %>% nrow())
  ) %>% 
  mutate(
    value = map_dbl(k, do_kr20_fgm)
  ) %>% 
  mutate(value_2 = value^2)
krs_fgm

cutoff_fgm <-
  krs_fgm %>% 
  filter(value_2 >= 0.5) %>% 
  filter(k != 1L) %>% 
  slice(1) %>% 
  pull(k) %>% 
  as.integer()
cutoff_fgm

shots_n_rnk <- 
  shots_mini_filt %>% 
  group_by(player) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(rnk = row_number(desc(n))) %>% 
  arrange(rnk)
shots_n_rnk

players_filt <- c('stephen curry', 'damian lillard', 'klay thompson', 'james harden','kyle lowry', 'kyle korver')
players_top <- 
  shots_n_rnk %>% 
  # filter(rnk <= 10L) %>% 
  filter(player %in% players_filt) %>% 
  mutate(player_pretty = player %>% str_to_title())
players_top

# players_top %>% pull(player) %>% clipr::write_clip()
# teamcolors::league_pal('nba')
players_colors <-
  tibble(
    player = c('stephen curry', 'damian lillard', 'klay thompson', 'james harden', 'trevor ariza', 'wesley matthews', 'gerald green', 'kyle lowry', 'kevin love', 'kyle korver'),
    color = c('#fdb927', '#000000', '#fdb927', '#ce1141', '#ce1141', '#000000', '#e56020', '#ce1141', '#860038', '#e13a3e')
  ) %>% 
  filter(player %in% players_filt) %>% 
  deframe()
# players_colors %>% scales::show_col()

n_player_viz <- 100L
shots_viz <-
  shots_mini_filt %>% 
  semi_join(shots_n_rnk %>% filter(rnk <= n_player_viz)) %>% 
  mutate(is_tenth = idx %% 10 == 0) %>% 
  filter(is_tenth) %>% 
  select(-is_tenth)
shots_viz

shots_viz_top <-
  shots_viz %>% 
  inner_join(players_top) %>% 
  mutate(lab = sprintf('%s (%.1f%%)', player_pretty, round(100 * fgm_rate_cumu)))
shots_viz_top

x_pos_lab <- 1800L
height <- 825
nframe <- 150
animate_partial <-
  partial(
    gganimate::animate,
    nframe = nframe,
    end_pause = 50,
    fps = 20,
    width = 750,
    ... = 
  )

add_common_layers <- function(...) {
  list(
    geom_vline(
      data = tibble(idx = cutoff_fgm),
      color = 'grey20',
      linetype = 1,
      aes(xintercept = idx, group = NULL),
      size = 1.1
    ),
    scale_x_continuous(labels = scales::comma_format(accuracy = 1), limits = c(0L, 2300L)),
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)),
    theme(
      panel.grid.major.y = element_blank()
    )
  )
}

# Reference: https://github.com/thomasp85/gganimate/wiki/Temperature-time-series
viz_fgm_rate_cumu <-
  shots_viz %>% 
  ggplot() +
  aes(x = idx, y = fgm_rate_cumu, group = player) +
  add_common_layers() +
  geom_step(alpha = 0.1) +
  geom_step(
    data = shots_viz_top,
    aes(color = player),
    size = 1.1,
    show.legend = FALSE
  ) +
  geom_segment(
    data = shots_viz_top,
    aes(x = idx, y = fgm_rate_cumu, xend = x_pos_lab, yend = fgm_rate_cumu),
    linetype = 2,
    size = 1
  ) +
  geom_text(
    # Shift curry up to avoid conflict with Klay near the end.
    data = 
      shots_viz_top %>% 
      mutate(y = case_when(player == 'stephen curry' ~ fgm_rate_cumu + 0.009, TRUE ~ fgm_rate_cumu)),
    # data = shots_viz_top,
    aes(x = x_pos_lab + 1L, y = y, label = lab),
    # family = 'Karla',
    hjust = 0
  ) +
  scale_color_manual(values = players_colors) +
  geom_text(
    data = tibble(idx = cutoff_fgm),
    aes(x = idx, y = 0.55, label = glue::glue('Stability point: {cutoff_fgm}'), group = NULL),
    size = 4,
    hjust = 1.05,
    color = 'grey20'
  ) +
  gganimate::transition_reveal(along = idx) +
  coord_cartesian(ylim = c(0.2, 0.6)) +
  theme(
    plot.subtitle = ggtext::element_markdown(),
    axis.text.x = element_blank()
  ) +
  labs(
    title = 'How many shots does it take for 3P% to stabilize?',
    subtitle = glue::glue('2013 - 2016 NBA shot logs indicate the answer is ~**{cutoff_fgm}** shots.'),
    x = NULL,
    y = 'Cumulative 3P% Average'
  )
# viz_fgm_rate_cumu

viz_fgm_rate_cumu_gif <- animate_partial(viz_fgm_rate_cumu, height = 0.75 * height)
# viz_fgm_rate_cumu_gif

krs_viz <- krs_fgm %>% rename(idx = k)
viz_krs <-
  krs_viz %>% 
  # Last value_2 values are NAs because n = 1, so add dummy values to make the plot line up with the prior plot.
  fill(value_2) %>% 
  ggplot() +
  aes(x = idx, y = value_2) +
  add_common_layers() +
  geom_segment(
    data = krs_viz %>% mutate(x = 0, y = value_2, yend = value_2) %>% filter(idx <= cutoff_fgm),
    aes(x = x, y = y, xend = idx, yend = yend),
    linetype = 2,
    size = 1.1,
    color = 'grey20'
  ) +
  geom_step() +
  gganimate::transition_reveal(along = idx) +
  labs(
    tag = 'Viz: @TonyElHabr | Data: NBA',
    y = 'Stability Rate',
    x = 'Shot #'
  )
# viz_krs

viz_krs_gif <- animate_partial(viz_krs, height = 0.25 * height)
# viz_krs_gif
# Feference: https://github.com/thomasp85/gganimate/wiki/Animation-Composition
viz_fgm_rate_cumu_mgif <- magick::image_read(viz_fgm_rate_cumu_gif)
viz_krs_mgif <- magick::image_read(viz_krs_gif)

res_gif <- magick::image_append(c(viz_fgm_rate_cumu_mgif[1], viz_krs_mgif[1]), stack = TRUE)
for(i in 2:nframe){
  combo_gif <- magick::image_append(c(viz_fgm_rate_cumu_mgif[i], viz_krs_mgif[i]), stack = TRUE)
  res_gif <- c(res_gif, combo_gif)
}
magick::image_write(res_gif, path = here::here('plots', 'nba_3p_stability.gif'))
