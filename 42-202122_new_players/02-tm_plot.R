
library(tidyverse)
library(ggwaffle)
library(tonythemes)

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


dir_proj <- '42-202122_new_players'
path_stats <- file.path(dir_proj, 'tm_stats.rds')
path_player_bios <- file.path(dir_proj, 'tm_player_bios.rds')
team_mapping <- 'https://raw.githubusercontent.com/tonyelhabr/xengagement/master/data-raw/team_mapping.csv' %>% read_csv() %>% filter(league == 'epl') %>% select(league, team_whoscored, team_tm, slug_tm, team_fbref)
stats <- 
  path_stats %>% 
  read_rds() %>% 
  janitor::clean_names() %>% 
  mutate(
    slug_tm = url %>% str_remove_all('(^.*[.]com\\/)|(\\/startseite.*$)')
  ) %>% 
  left_join(team_mapping %>% select(team_tm, slug_tm))
stats

player_bios <- 
  path_player_bios %>%
  read_rds() %>% 
  distinct(start_year, player_name, player_url, team_tm = current_club) %>% 
  left_join(team_mapping %>% select(team_tm, slug_tm))
player_bios

teams_filt <- stats %>% filter(start_year == 2021 & league == 'Premier League') %>% distinct(team_tm)
stats_filt <- stats %>% semi_join(teams_filt)

stats_w_urls <-
  stats_filt %>% 
  left_join(player_bios)
stats_w_urls

player_mapping <-
  worldfootballR::player_dictionary_mapping() %>% 
  set_names(c('player_name_fbref', 'player_url_fbref', 'player_url', 'pos'))

fbref_stats <-
  fb_big5_advanced_season_stats(2022, 'playing_time', team_or_player = 'player') %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  filter(comp == 'Premier League') %>% 
  select(player_name_fbref = player, team_fbref = squad, g = mp_playing_time, mp = min_playing_time)
fbref_stats

new_players_imputed <-
  stats_w_urls %>% 
  filter(start_year == 2021) %>% 
  anti_join(player_mapping) %>% 
  select(player_name, team_tm, appearances, minutes_played) %>% 
  arrange(desc(minutes_played)) %>% 
  left_join(
    fbref_stats %>% 
      # need to join on team_tm cuz players like Tammy Abraham have incorrect TM current_club
      inner_join(team_mapping %>% select(team_fbref, team_tm)) %>% 
      rename(player_name = player_name_fbref)
  ) %>% 
  arrange(desc(mp))
new_players_imputed

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

players <-
  stats_filt %>% 
  group_by(player_name, start_year) %>% 
  slice_max(in_squad, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(
    across(player_pos, replace_pos)
  ) %>% 
  left_join(team_mapping %>% select(team_whoscored, team_tm)) %>% 
  select(player_name, player_pos, start_year, team = team_whoscored, mp = minutes_played, appearances) %>% 
  pivot_wider(names_from = start_year, values_from = c(team, appearances, mp))
players

stats_filt %>% 
  filter(start_year == 2020) %>% 
  arrange(desc(in_squad))
  group_by(team_name) %>% 
  summarize(
    across(minutes_played, sum)
  ) %>% 
  ungroup() %>% 
  arrange(desc(minutes_played)) %>% 
  mutate(mp_p90 = round(minutes_played / 90, 0)) %>% 
  slice_max(mp_p90)

new_players <-
  players %>% 
  filter(team_2020 != team_2021 | is.na(team_2020)) %>% 
  select(-matches('_2020$')) %>% 
  rename_all(~str_remove(.x, '_2021$'))
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
agg_by_pos <- summarize_by(player_pos)

# base <-
#   agg %>% 
#   mutate(mp_p90 = round(mp / 90, 0)) %>% 
#   ggplot() +
#   aes(fill = player_pos, values = mp_p90) +
#   expand_limits(x = c(0, 0), y = c(0, 0)) +
#   coord_equal()  +
#   theme_enhance_waffle()
# base

agg <-
  agg_by_pos %>% 
  left_join(
    agg_by_team %>% select(team, mp_p90_total = mp_p90)
  ) %>% 
  mutate(
    across(team, ~fct_reorder(.x, -mp_p90_total))
agg

agg %>% 
  ggplot() +
  facet_wrap(~team) +
  aes(values = mp_p90) +
  # expand_limits(x = c(0, 0), y = c(0, 0)) +
  # coord_equal()  +
  # hrbrthemes::theme_ipsum_rc() +
  theme_enhance_waffle() +
  waffle::geom_waffle(
    aes(fill = player_pos),
    n_rows = 6,
    color = 'white'
  ) +
  scale_fill_manual(values = pal) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
