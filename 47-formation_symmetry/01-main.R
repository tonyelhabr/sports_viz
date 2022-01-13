
library(tidyverse)
library(arrow)

# gray_wv <- rgb(24, 24, 24, maxColorValue = 255)
# gray_grid_wv <- rgb(64, 64, 64, maxColorValue = 255)
# 
# extrafont::loadfonts(device = 'win', quiet = TRUE)
# theme_set(theme_minimal())
# theme_update(
#   text = element_text(family = 'Karla', color = 'white'),
#   title = element_text('Karla', size = 14, color = 'white'),
#   plot.title = element_text('Karla', face = 'bold', size = 24, color = 'white'),
#   plot.title.position = 'plot',
#   plot.subtitle = element_text('Karla', size = 18, color = '#f1f1f1'),
#   axis.text = element_text('Karla', color = 'white', size = 14),
#   axis.title = element_text('Karla', size = 14, color = 'white', face = 'bold', hjust = 0.99),
#   axis.line = element_blank(),
#   panel.grid.major = element_line(color = gray_grid_wv),
#   panel.grid.minor = element_line(color = gray_grid_wv),
#   panel.grid.minor.x = element_blank(),
#   panel.grid.minor.y = element_blank(),
#   plot.margin = margin(10, 10, 10, 10),
#   plot.background = element_rect(fill = gray_wv, color = gray_wv),
#   plot.caption = element_text('Karla', color = 'white', hjust = 1, size = 11, face = 'italic'),
#   plot.caption.position = 'plot',
#   plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'white', hjust = 0),
#   plot.tag.position = c(0.01, 0.01),
#   strip.text = element_text('Karla', color = 'white', face = 'bold', size = 12, hjust = 0),
#   panel.background = element_rect(fill = gray_wv, color = gray_wv)
# )
# update_geom_defaults('text', list(family = 'Karla', size = 4))
tonythemes::theme_set_tony()
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
    # side = ifelse(team_id == home_team_id, 'home', 'away'),
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
    ),
    in_final_third = y <= 105 / 3
  )

# most_recent_game <- filt_actions %>% 
#   slice_max(game_id)

add_side_col <- function(df) {
  df %>% 
    left_join(meta) %>% 
    mutate(
      side = ifelse(team_id == home_team_id, 'home', 'away')
    )
}

scores <- filt_actions %>% 
  filter(type_name == 'shot' & result_name == 'success') %>% 
  distinct(
    game_id,
    team_id,
    period_id,
    player_id,
    action_id
  ) %>% 
  count(game_id, team_id, name = 'score') %>%
  add_side_col() %>%
  select(
    game_id,
    # final_home_score = home_score,
    # final_away_score = away_score,
    side,
    score
  ) %>%
  pivot_wider(
    names_from = side,
    names_glue = '{side}_{.value}',
    values_from = score,
    values_fill = 0L
  ) %>% 
  left_join(
    meta %>% 
      rename(
        final_home_score = home_score,
        final_away_score = away_score
      )
  )
scores %>% 
  filter(away_score > final_away_score)

field_tilt <- filt_actions %>% 
  filter(type_name == 'pass', in_final_third) %>% 
  distinct(
    game_id,
    team_id,
    period_id,
    player_id,
    action_id
  ) %>% 
  count(game_id, team_id) %>% 
  group_by(game_id) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup()

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
    in_final_third,
    type_name,
    x,
    y
  ) %>% 
  pivot_wider(
    names_from = c(type_name),
    values_from = c(player_id, in_final_third, x, y)
  ) %>% 
  rename_all(
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
successful_passes %>% 
  left_join(filt_actions %>% select(game_id, team_id, period_id, player_id, action_id, in_final_third))

## todo: use this just for visualization?
pass_network_edges <- successful_passes %>% 
  group_by(game_id, player_id_start, player_id_end) %>% 
  summarize(n = n()) %>% 
  ungroup()

team_field_tilt <- successful_passes %>% 
  count(game_id, team_id, in_final_third)
team_field_tilt

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
  group_by(id, game_id, team_id) %>% 
  ## teams with less than 10 outfield players?!?
  filter(n() < 10) %>% 
  ungroup() %>% 
  distinct(id, game_id, team_id)
bad_ids

field_tilt %>% inner_join(bad_ids)

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
    n_players = map_int(xy_hull_orig, ~nrow(.x[[1]]))
  ) %>% 
  transmute_area() %>% 
  select(-xy_hull_orig)

concave_area_diffs <- concave_areas %>% 
  anti_join(bad_ids) %>% 
  left_join(field_tilt %>% rename(n_touches = n, field_tilt = prop)) %>% 
  add_side_col() %>% 
  select(game_id, side, area_prop, field_tilt) %>% 
  pivot_wider(
    names_from = side,
    names_glue = '{side}_{.value}',
    values_from = c(area_prop, field_tilt)
  ) %>% 
  ## should reduce rows since not all games are included in scores (intentionally)
  inner_join(scores %>% select(-matches('^final_'))) %>% 
  mutate(
    diff_area_prop = home_area_prop - away_area_prop,
    diff_field_tilt = home_field_tilt - away_field_tilt,
    diff_score = home_score - away_score
  ) %>% 
  arrange(desc(abs(diff_area_prop)))

concave_area_diffs_redux <- concave_area_diffs %>% 
  transmute(
    game_date,
    season_id,
    game_id,
    is_home_stronger =home_area_prop > away_area_prop,
    stronger_area_prop = ifelse(is_home_stronger, home_area_prop, away_area_prop),
    weaker_area_prop = ifelse(!is_home_stronger, home_area_prop, away_area_prop),
    stronger_field_tilt = ifelse(is_home_stronger, home_field_tilt, away_field_tilt),
    weaker_field_tilt = ifelse(!is_home_stronger, home_field_tilt, away_field_tilt),
    stronger_score = ifelse(is_home_stronger, home_score, away_score),
    weaker_score = ifelse(!is_home_stronger, home_score, away_score),
    stronger_team = ifelse(is_home_stronger, home_team_name, away_team_name),
    weaker_team = ifelse(!is_home_stronger, home_team_name, away_team_name),
    diff_area_prop = stronger_area_prop - weaker_area_prop,
    diff_field_tilt = stronger_field_tilt - weaker_field_tilt,
    diff_score = stronger_score - weaker_score
  )

.str_replace_cor_col <- function(x, i) {
  str_replace_all(x, '(diff|stronger|weaker|home|away)_(.*$)', sprintf('\\%d', i))
}

do_tidy_cor <- function(data) {
  data %>% 
    corrr::correlate(quiet = TRUE) %>% 
    corrr::stretch() %>% 
    filter(!is.na(r)) %>% 
    filter(y > x) %>% 
    filter(abs(r) != 1) %>% 
    arrange(desc(abs(r))) %>% 
    mutate(
      across(
        c(x, y),
        list(
          prefix = ~.str_replace_cor_col(.x, 1),
          suffix = ~.str_replace_cor_col(.x, 2)
        )
      )
    ) %>% 
    filter(x_suffix != y_suffix)
}

concave_cors <- concave_area_diffs %>% 
  select(
    where(is.numeric), -matches('_id$')
  ) %>% 
  do_tidy_cor()

concave_cors_redux <- concave_area_diffs_redux %>% 
  select(
    where(is.numeric), -matches('_id$')
  ) %>% 
  do_tidy_cor()

concave_area_diffs %>% 
  ggplot() +
  aes(x = home_area_prop, y = away_area_prop) +
  geom_point()

concave_area_diffs_redux %>% 
  ggplot() +
  aes(x = stronger_area_prop, y = diff_field_tilt) +
  geom_point()

## convex ----
convex_areas <- convex_areas_nested %>% 
  hoist_areas() %>% 
  unnest(c(area_inner, area_outer)) %>% 
  transmute_area()

agg_convex_areas <- convex_areas %>%  
  group_by(id, game_id, team_id) %>% 
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

areas_compared %>% arrange(desc(area_prop_concave))

areas_compared %>% 
  ggplot() +
  aes(x = prnk_concave, y = prnk_convex) +
  geom_point()

areas_compared %>% 
  ggplot() +
  aes(x = area_prop_concave, y = area_prop_convex) +
  geom_point()

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
