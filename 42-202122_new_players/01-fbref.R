
library(tidyverse)
library(waffle)
library(tonythemes)
library(worldfootballR)
library(grid)
library(magick)
library(cowplot)

gray_wv <- rgb(24, 24, 24, maxColorValue = 255)
gray_grid_wv <- rgb(64, 64, 64, maxColorValue = 255)
dir_proj <- '42-202122_new_players'

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla', color = 'white'),
  title = element_text('Karla', size = 14, color = 'white'),
  plot.title = element_text('Karla', face = 'bold', size = 24, color = 'white'),
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
  plot.caption = element_text('Karla', size = 10, color = 'white', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'white', hjust = 0),
  plot.tag.position = c(.01, 0.01),
  # legend.text = element_text(color = 'gray20', size = 14),
  strip.text = element_text('Karla', color = 'white', face = 'bold', size = 12, hjust = 0),
  # strip.background = element_blank(),
  panel.background = element_rect(fill = gray_wv, color = gray_wv)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

player_mapping <-
  worldfootballR::player_dictionary_mapping() %>% 
  set_names(c('player_name_fbref', 'player_url_fbref', 'player_url', 'pos_tm'))

pal <- c(
  'Forward' = '#ffbe0b',
  'Winger' = '#fb5607',
  'Midfielder' = '#ff006e',
  'Defender' = '#8338ec',
  'Goalkeeper' = '#3a86ff'
)
# nms_pal <- pal %>% names()

replace_pos <- function(x) {
  y <-
    x %>% 
    str_replace('-', ' ') %>%
    str_replace('(^.*\\s+)?(.*$)', '\\2')
  
  case_when(
    y == 'Back' ~ 'Defender',
    y == 'Striker' ~ 'Forward',
    y == 'Midfield' ~ 'Midfielder',
    TRUE ~ y
  )
}

retieve_fbref_big5 <- function(x) {
  worldfootballR::fb_big5_advanced_season_stats(x, 'playing_time', team_or_player = 'player')
}

epl_2020 <- file.path(dir_proj, '2021-efl.xlsx') %>% readxl::read_excel()

fbref_stats <-
  bind_rows(
    retieve_fbref_big5(2021),
    retieve_fbref_big5(2022)
  ) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>%
  transmute(
    start_year = season_end_year - 1L,
    comp,
    player_name = player,
    team = squad,
    gp = mp_playing_time,
    mp = min_playing_time
  )

fbref_stats_all <- bind_rows(fbref_stats, epl_2020)
players21 <-
  fbref_stats_all %>% 
  filter(start_year == 2021 & comp == 'Premier League')
players21

players20 <-
  fbref_stats_all %>% 
  filter(start_year == 2020) %>% 
  semi_join(players21 %>% distinct(player_name))
players20

team_mapping <- 'https://raw.githubusercontent.com/tonyelhabr/xengagement/master/data-raw/team_mapping.csv' %>% 
  read_csv(show_col_types = FALSE) %>% 
  filter(league == 'epl') %>% 
  select(league, team_fbref, url_logo_espn, color_pri) %>% 
  drop_na(team_fbref) %>% 
  mutate(
    across(
      team_fbref,
      list(
        team = ~case_when(
          .x == 'Norwich City' ~ 'Norwich',
          .x %>% str_detect('Utd$') ~ str_replace(.x, 'Utd', 'United'),
          TRUE ~ .x
        )
      ),
      .names = '{fn}'
    )
  )
team_mapping

players_wide <-
  bind_rows(
    players20,
    players21
  ) %>% 
  select(start_year, player_name, team, mp) %>% 
  group_by(start_year, player_name) %>% 
  slice_max(mp) %>% 
  ungroup() %>% 
  # rename(team_fbref = team) %>% 
  # anti_join(team_mapping %>% select(team_fbref, team)) %>% 
  # select(-team_fbref) %>% 
  pivot_wider(
    names_from = c(start_year),
    values_from = c(team, mp)
  ) %>% 
  semi_join(players21)
players_wide

new_players <-
  players_wide %>% 
  drop_na(mp_2021) %>% 
  filter(team_2020 != team_2021) %>% 
  # filter(team_2020 != team_2021 | is.na(team_2020)) %>% 
  arrange(desc(mp_2021)) %>% 
  # ayyyy no missing if i do anti_join here
  inner_join(player_mapping %>% select(player_name = player_name_fbref, pos = pos_tm)) %>% 
  mutate(
    across(pos, replace_pos)
  ) %>% 
  select(-matches('_2020$')) %>% 
  rename_all(~str_remove(.x, '_2021$')) %>% 
  distinct()
new_players

summarize_by <- function(...) {
  new_players %>% 
    group_by(team, ...) %>% 
    summarize(
      # across(where(is.numeric), sum),
      # n_player = n_distinct(player_name),
      # n = n()
      across(mp, sum)
    ) %>% 
    ungroup() %>% 
    mutate(mp_p90 = round(mp / 90, 0)) %>% 
    arrange(desc(mp))
}

agg_by_team <- summarize_by()
agg_by_pos <- summarize_by(pos)

agg <-
  agg_by_pos %>% 
  left_join(
    agg_by_team %>% 
      select(team, mp_p90_total = mp_p90)
  ) %>% 
  # right_join(
  #   crossing(
  #     team = team_mapping$team_fbref,
  #     pos = agg_by_pos %>% distinct(pos) %>% pull(pos)
  #   )
  # ) %>%
  # mutate(across(where(is.numeric), replace_na, 0)) %>%
  add_row(
    team = 'Newcastle Utd', pos = 'Wanker', mp = 0, mp_p90 = 1, mp_p90_total = 1
  ) %>% 
  mutate(
    # across(team, ~fct_reorder(.x, -mp_p90_total)),
    across(team, ~ordered(.x, levels = c(agg_by_team$team, 'Newcastle Utd')))
  ) %>% 
  arrange(team, -mp)
# agg$team %>% levels()
agg

p <-
  agg %>% 
  ggplot() +
  facet_wrap(~team) +
  aes(values = mp_p90) +
  theme_enhance_waffle() +
  waffle::geom_waffle(
    aes(fill = pos),
    n_rows = 5,
    color = gray_wv
  ) +
  scale_fill_manual(values = pal, na.value = gray_wv) +
  guides(
    fill = guide_legend(
      title = '',
      # keywidth = 1.5, keyheight = 1.5,
      label.theme = element_text(
        family = 'Karla',
        color = 'white',
        face = 'bold',
        size = 12
      )
    )
  ) +
  labs(
    title = 'Which squads have had the largest influx of new players this season?',
    subtitle = 'Each square represents 90 minutes played, aggregated across players on a given team.<br/>A "new" player is a player who did not play on a given team in the prior season.',
    # caption = 'A "new" player is a player who did not play on a given team in the prior season.'
    caption = ' ',
    tag = '**Viz**: Tony ElHabr | **Data**: fbref via {worldfootballR}',
  ) +
  theme( 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18),
    strip.text.x = element_text(margin = margin(10, 0, 10, 0, 'pt')),
    plot.subtitle = ggtext::element_markdown(size = 14, face = 'italic'),
    # plot.caption = ggtext::element_markdown(size = 14, face = 'italic'),
    legend.position = 'top'
  )
p

p_bld <- ggplot_gtable(ggplot_build(p))
grob_strip_index <- which(sapply(p_bld$grob, function(x) x$name)=='strip')
facet_id <- sapply(grob_strip_index, function(grb) {
  p_bld$grobs[[grb]]$grobs[[1]]$children[[2]]$children[[1]]$label
})
# p_bld$layout$z[grob_strip_index] <- 0


for (i in 1:length(facet_id)) {
  # i <- 1
  url <- team_mapping %>% filter(team_fbref == facet_id[i]) %>% pull(url_logo_espn)
  img <- grid::rasterGrob(image = magick::image_read(url), vp = grid::viewport(height = 0.8, width = 0.6))
  tot_tree <- grid::grobTree(img)
  
  p_bld$grobs[[grob_strip_index[i]]] <- tot_tree
}
p <- cowplot::ggdraw(p_bld)
base_size <- 7
asp <- 1.5
path_waffle <- file.path(dir_proj, '202122_epl_new_players.png')
ggsave(
  filename = path_waffle, 
  plot = cowplot::ggdraw(p), 
  height = base_size, 
  width = base_size * asp
)

tonythemes:::add_logo(
  path_viz = path_waffle,
  path_logo = file.path(dir_proj, 'epl-logo-white.png'),
  delete = FALSE,
  logo_scale = 0.1,
  idx_x = 0.01,
  idx_y = 0.98,
  adjust_y = FALSE
)

# logo_size <- 0.05
# asp <- 1.5
# 
# ## is image taller than wider? if so, make sure the width is at least the base_size
# if (asp < 1) {
#   base_size_rat_wid <- (5/base_size)
#   logo_size <- (5/base_size) * logo_size * asp
#   base_size <- base_size / asp
# } else {
#   base_size_rat_wid <- (5/base_size) / asp
#   logo_size <- (5/base_size) * logo_size
# }
# pts <- function(x) {
#   as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
# }
# font <- 'Karla'
# caption <-
#   gridtext::richtext_grob(
#     # expression(bold('Viz'), ': Tony ElHabr'),
#     '**Viz**: Tony ElHabr',
#     x = unit(0.01 * (base_size_rat_wid), 'npc'),
#     gp = gpar(
#       col = 'white',
#       fontfamily = font,
#       fontsize = pts(24)
#     ),
#     hjust = 0
#   )
# footer_rect <- grid::grid.rect(x = unit(seq(0.5, 1.5, length=2), 'npc'), gp=gpar(col = 'transparent', fill = rep(gray_wv, 2)), draw = F)
# footer <- grid::grobTree(footer_rect, caption)
# 
# plt.final <- gridExtra::grid.arrange(p, footer, heights = unit(c(1, 24), c('null', 'pt')))
# ggsave(
#   filename = file.path(dir_proj, 'temp2.png'), 
#   plot = cowplot::ggdraw(plt.final), 
#   height = base_size, 
#   width = base_size * asp
# )


