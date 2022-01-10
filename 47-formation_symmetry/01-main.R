
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
  select(game_id, home_team_id, away_team_id, season_id, game_date, away_score, home_score) %>% 
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
  select(
    game_id,
    period_id,
    team_id,
    player_id,
    atomic_action_id,
    type_name,
    result_name,
    side,
    x,
    y
  )

## how i figured out i needed to flip the home side
# touches %>% 
#   filter(type_name == 'shot') %>% 
#   group_by(side, period_id) %>% 
#   summarize(
#     across(c(x, y), mean)
#   ) %>% 
#   ungroup()

nodes <- filt_actions %>% 
  select(game_id, team_id, player_id, atomic_action_id, x, y) %>% 
  left_join(
    player_games
  ) %>% 
  filter(starting_position_name != 'GK') %>% 
  group_by(game_id, team_id, player_id) %>% 
  summarize(n = n(), across(c(x, y), mean)) %>% 
  ungroup() %>% 
  mutate(id = sprintf('%07d-%03d', game_id, team_id))

bad_ids <- nodes %>% 
  group_by(id) %>% 
  ## teams with less than 10 outfield players?!?
  filter(n() < 10) %>% 
  ungroup() %>% 
  distinct(id)
bad_ids

do_get_areas <- function(.name) {
  f <- sprintf('get_%s_hull_areas', .name)
  fv <- function(name, ...) {
    cat(name, sep = '\n')
    exec(f, y_center = 34, ...)
  }
  path <- file.path(dir_proj, sprintf('%s_areas_nested.rds', .name))
  if(file.exists(path)) {
    return(read_rds(path))
  }
  areas_nested <- nodes %>% 
    group_nest(id, game_id, team_id) %>%
    mutate(
      areas = map2(id, data, fv),
    )
  write_rds(areas_nested, path)
  areas_nested
}

concave_areas_nested <- do_get_areas('concave')
convex_areas_nested <- do_get_areas('convex')

# extract_areas <- function(df) {
#   df %>% 
#     select(id, game_id, team_id, areas) %>% 
#     unnest(areas) %>% 
#     transmute(
#       id,
#       game_id,
#       team_id,
#       area_inner, 
#       area_outer,
#       area_total = area_inner + area_outer,
#       area_prop = area_inner / (area_inner + area_outer)
#     )
# }
# concave_areas <- concave_areas_nested %>% extract_areas()
# convex_areas <- convex_areas_nested %>% extract_areas()
hoist_areas <- function(df) {
  df %>% 
    select(id, game_id, team_id, areas) %>% 
    hoist(areas, 'area_inner') %>% 
    hoist(areas, 'area_outer')
}
transmute_area <- function(df) {
  df %>% 
    mutate(
      area_total = area_inner + area_outer,
      area_prop = area_inner / (area_inner + area_outer)
    ) %>% 
    select(-c(areas))
}

concave_areas <- concave_areas_nested %>% 
  hoist_areas() %>% 
  hoist(areas, 'xy_hull_orig') %>% 
  mutate(
    n = map_int(xy_hull_orig, ~nrow(.x[[1]]))
  ) %>% 
  transmute_area() %>% 
  select(-xy_hull_orig)

concave_area_diffs <- concave_areas %>% 
  anti_join(bad_ids) %>% 
  # count(n, sort = TRUE) %>% 
  left_join(meta) %>% 
  mutate(
    side = ifelse(team_id == home_team_id, 'home', 'away')
  ) %>% 
  select(game_id, side, area_prop) %>% 
  pivot_wider(
    names_from = side,
    names_glue = '{side}_{.value}',
    values_from = area_prop
  ) %>% 
  left_join(meta) %>% 
  mutate(
    diff_area_prop = home_area_prop - away_area_prop
  ) %>% 
  arrange(desc(abs(diff_area_prop)))


convex_areas <- convex_areas_nested %>% 
  hoist_areas() %>% 
  unnest(c(area_inner, area_outer)) %>% 
  transmute_area()

agg_convex_areas <- convex_areas %>%  
  group_by(id) %>% 
  summarize(
    n = n(),
    area_prop = mean(area_inner / (area_inner + area_outer))
  ) %>% 
  ungroup()
agg_convex_areas

areas_compared <- full_join(
  concave_areas %>% select(id, game_id, team_id, area_prop_concave = area_prop),
  agg_convex_areas %>% select(id, area_prop_convex = area_prop)
) %>% 
  mutate(
    prnk_concave = percent_rank(area_prop_concave),
    prnk_convex = percent_rank(area_prop_convex),
    prnk_diff = prnk_concave - prnk_convex,
    prnk_prnk = percent_rank(prnk_diff)
  ) %>% 
  arrange(desc(prnk_prnk))
areas_compared

concave_areas

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
