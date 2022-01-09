
library(tidyverse)
library(arrow)

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

dir_proj <- '47-formation_symmetry'
source(file.path(dir_proj, 'helpers.R'))
f_import <- function(name) {
  path <- file.path(dir_proj, sprintf('%s.parquet', name))
  res <- path %>% arrow::read_parquet()
  assign(value = res, x = name, envir = .GlobalEnv)
}

c(
  'all_actions',
  'all_actions_atomic',
  'player_games',
  'games_by_team',
  'players',
  'games',
  'teams'
) %>% 
  walk(f_import)

meta <- games %>% 
  select(game_id, home_team_id, away_team_id) %>% 
  left_join(teams %>% rename_all(~sprintf('home_%s', .x))) %>% 
  left_join(teams %>% rename_all(~sprintf('away_%s', .x)))

last_actions_by_game <- all_actions_atomic %>% 
  inner_join(
    player_games %>% filter(!is_starter)
  ) %>% 
  group_by(game_id) %>% 
  slice_min(atomic_action_id, n = 1) %>% 
  ungroup() %>% 
  select(game_id, last_atomic_action_id = atomic_action_id)
last_actions_by_game

filt_actions <- all_actions_atomic %>% 
  left_join(
    last_actions_by_game
  ) %>% 
  filter(!is.na(original_event_id)) %>% ## dribbles
  filter(atomic_action_id < last_atomic_action_id) %>% 
  left_join(all_actions) %>% 
  arrange(game_id, atomic_action_id, action_id, original_event_id) %>% 
  group_by(game_id) %>% 
  fill(action_id, result_id, result_name) %>% 
  ungroup() %>% 
  left_join(meta) %>% 
  mutate(
    is_home = team_id == home_team_id,
    side = ifelse(team_id == home_team_id, 'home', 'away'),
    across(
      x,
      ~ifelse(
        is_home,
        105 - .x,
        .x
      )
    ),
    across(
      y,
      ~ifelse(
        is_home,
        68 - .x,
        .x
      )
    )
  )

most_recent_game <- filt_actions %>% 
  slice_max(game_id)

successful_passes <- filt_actions %>% 
  filter(type_name %in% c('pass', 'receival'), result_name == 'success') %>% 
  group_by(game_id, action_id) %>% 
  filter(n() == 2) %>% 
  ungroup() %>% 
  distinct(
    game_id,
    team_id,
    period_id,
    player_id,
    action_id,
    type_name,
    x,
    y
  ) %>% 
  pivot_wider(
    names_from = c(type_name),
    values_from = c(player_id, x, y)
  ) %>% 
  rename_all(
    # ~str_replace_all(
    #   .x,
    #   '(pass|receival)$',
    #   c(
    #     'pass' = 'start',
    #     'receival' = 'end'
    #   )
    # )
    ~str_replace(
      .x,
      'pass',
      'start'
    ) %>% 
      str_replace(
        'receival',
        'end'
      )
  ) %>% 
  arrange(game_id, team_id, player_id_start)

edges <- successful_passes %>% 
  group_by(game_id, player_id_start, player_id_end) %>% 
  summarize(n = n()) %>% 
  ungroup()

touches <- filt_actions %>% 
  select(game_id, period_id, team_id, player_id, atomic_action_id, type_name, side, x, y)

touches %>% 
  filter(type_name == 'shot') %>% 
  group_by(side, period_id) %>% 
  summarize(
    across(c(x, y), mean)
  ) %>% 
  ungroup()

nodes <- filt_actions %>% 
  select(game_id, team_id, player_id, atomic_action_id, x, y) %>% 
  left_join(
    player_games
  ) %>% 
  filter(starting_position_name != 'GK') %>% 
  group_by(game_id, team_id, player_id) %>% 
  summarize(n = n(), across(c(x, y), mean)) %>% 
  ungroup()

get_concave_hull_areas_verbosely <- function(name, ...) {
  cat(name, sep = '\n')
  get_concave_hull_areas(y_center = 34, ...)
}

nested_areas <- nodes %>% 
  mutate(id = sprintf('%s-%s', game_id, team_id)) %>% 
  group_nest(id, game_id, team_id) %>% 
  mutate(
    areas = map2(id, data, get_concave_hull_areas_verbosely)
  )

nodes %>% 
  count(game_id, team_id, sort = TRUE)


nodes %>% 
  summarize(
    across(c(x, y), mean)
  )
# Reference: https://github.com/Torvaney/ggsoccer/blob/master/R/dimensions.R
.pitch_international <- list(
  length = 105,
  width = 68,
  penalty_box_length = 16.5,
  penalty_box_width = 40.32,
  six_yard_box_length = 5.5,
  six_yard_box_width = 18.32,
  penalty_spot_distance = 11,
  goal_width = 7.32,
  origin_x = 0,
  origin_y = 0
)  
