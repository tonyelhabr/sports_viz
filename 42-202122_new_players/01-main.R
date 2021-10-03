
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
  plot.caption = element_text('Karla', size = 10, color = 'white', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'white', hjust = 0),
  plot.tag.position = c(.01, 0.01),
  strip.text = element_text('Karla', color = 'white', face = 'bold', size = 12, hjust = 0),
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
scales::show_col(pal)

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
  path <- file.path(dir_proj, sprintf('big5-playing-time-player-%d.rds', x))
  if(file.exists(path)) {
    return(read_rds(path))
  }
  res <- worldfootballR::fb_big5_advanced_season_stats(x, 'playing_time', team_or_player = 'player')
  write_rds(res, path)
  res
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
  select(team_fbref, url_logo_espn) %>% 
  drop_na(team_fbref) %>% 
  mutate(
    across(
      team_fbref,
      list(
        team = ~case_when(
          .x == 'Norwich City' ~ 'Norwich',
          .x %>% str_detect('Utd$') ~ str_replace(.x, 'Utd', 'United'),
          TRUE ~ .x,
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
  rename(team_fbref = team) %>%
  left_join(team_mapping %>% select(team_fbref, team)) %>%
  select(-team_fbref) %>%
  pivot_wider(
    names_from = c(start_year),
    values_from = c(team, mp)
  ) %>% 
  semi_join(players21)
players_wide

new_players <-
  players_wide %>% 
  drop_na(mp_2021) %>% 
  filter(team_2020 != team_2021 | is.na(team_2020)) %>% 
  # ayyyy no missing if i do anti_join here
  inner_join(player_mapping %>% select(player_name = player_name_fbref, pos = pos_tm)) %>% 
  mutate(
    across(pos, replace_pos)
  ) %>% 
  select(-matches('_2020$')) %>% 
  rename_all(~str_remove(.x, '_2021$')) %>% 
  distinct()
new_players
new_players %>% filter(team == 'Manchester City') # %>% drop_na(team_)

summarize_by <- function(...) {
  new_players %>% 
    group_by(team, ...) %>% 
    summarize(
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
  mutate(
    across(team, ~ordered(.x, levels = c(agg_by_team$team)))
  ) %>% 
  arrange(team, -mp)
agg

p <-
  agg %>% 
  ggplot() +
  facet_wrap(~team) +
  aes(values = mp_p90) +
  theme_enhance_waffle() +
  waffle::geom_waffle(
    aes(fill = pos),
    n_rows = 6,
    color = gray_wv
  ) +
  scale_fill_manual(values = pal, na.value = gray_wv) +
  guides(
    fill = guide_legend(
      title = '',
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
    caption = 'Each block represents 90 minutes played, aggregated across players on a given team.<br/>A "new" player is any player who did not play on a given team in the prior season.<br/>Data through October 2, 2021.',
    subtitle = 'Two promoted teams, Norwich and Watford, have played lots of new players. Brentford is closer to average.',
    tag = '**Viz**: Tony ElHabr | **Data**: fbref via {worldfootballR}',
  ) +
  theme( 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18),
    strip.text.x = element_text(margin = margin(10, 0, 10, 0, 'pt')),
    plot.subtitle = ggtext::element_markdown(size = 14, face = 'plain'),
    plot.caption = ggtext::element_markdown(size = 11, face = 'italic'),
    legend.position = 'top'
  )
p

## Reference: https://github.com/ajreinhard/data-viz/blob/master/ggplot/plot_SB.R
p_bld <- ggplot_gtable(ggplot_build(p))
grob_strip_index <- which(sapply(p_bld$grob, function(x) x$name)=='strip')
facet_id <- sapply(grob_strip_index, function(grb) {
  p_bld$grobs[[grb]]$grobs[[1]]$children[[2]]$children[[1]]$label
})
# p_bld$layout$z[grob_strip_index] <- 0 ## not sure what the point of this is...

for (i in 1:length(facet_id)) {
  id <- facet_id[i]
  url <-
    team_mapping %>% filter(team == !!id) %>% pull(url_logo_espn)
  lab <-
    grid::textGrob(
      id,
      x = unit(0, 'npc'),
      gp = grid::gpar(
        col = 'white',
        fontfamily = 'Karla',
        fontface = 'bold',
        fontsize = 11
      ),
      hjust = 0
    )
  img <-
    grid::rasterGrob(
      image = magick::image_read(url),
      # just = 'right',
      hjust = 1,
      x = unit(1, 'npc'),
      ## 1 and 0.75 is also fine
      vp = grid::viewport(height = 0.8, width = 0.6)
    )
  tot_tree <- grid::grobTree(lab, img)
  p_bld$grobs[[grob_strip_index[i]]] <- tot_tree
}
p <- cowplot::ggdraw(p_bld)
base_size <- 8
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
