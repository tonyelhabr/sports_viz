
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

agg_actions <- function(...) {
  filt_actions %>% 
    filter(...) %>% 
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
}

field_tilt <- agg_actions(type_name == 'pass', in_final_third)
n_successful_passes <- agg_actions(type_name == 'pass')
n_shots <- agg_actions(type_name == 'shot')

# full_join(
#   n_successful_passes %>% select(game_id, team_id, prop_passes = prop),
#   field_tilt %>% select(game_id, team_id, prop_field_tilt = prop)
# ) %>% 
#   ggplot() +
#   aes(x = prop_passes, y = prop_field_tilt) +
#   geom_point()

drop_keepers <- function(df) {
  df %>% 
    anti_join(
      player_games %>% 
        filter(starting_position_name == 'GK')
    )
}

successful_passes <- filt_actions %>% 
  # filter(action_id == 210, game_id == 1190174, period_id == 1, team_id == 13) %>% 
  drop_keepers() %>% 
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

add_id_col <- function(df) {
  df %>% 
    mutate(id = sprintf('%07d-%03d', game_id, team_id))
}

## network stats ----
edges <- successful_passes %>% 
  group_by(game_id, team_id, player_id_start, player_id_end) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  add_id_col()

nodes <- filt_actions %>% 
  filter(type_name == 'pass') %>% 
  select(game_id, team_id, player_id, atomic_action_id, x, y) %>% 
  drop_keepers() %>% 
  group_by(game_id, team_id, player_id) %>% 
  summarize(n = n(), across(c(x, y), mean)) %>% 
  ungroup() %>% 
  add_id_col() # %>% 
  # left_join(players %>% rename(name = player_name))

bad_ids <- nodes %>% 
  count(id, game_id, team_id, name = 'n_players') %>% 
  ## teams with less than 10 outfield players?!?
  filter(n_players < 10) %>% 
  distinct(id, game_id, team_id, n_players)

drop_bad_ids <- function(df) {
  df %>% anti_join(bad_ids)
}

do_compute_network_stats <- function(overwrite = FALSE) {
  fv <- function(name, ...) {
    cat(name, sep = '\n')
    compute_network_stats(...)
  }
  path <- file.path(dir_proj, 'network_stats.rds')
  if(file.exists(path) & !overwrite) {
    return(read_rds(path))
  }
  network_stats_nested <- 
    inner_join(
      nodes %>% 
        group_nest(id, game_id, team_id, .key = 'nodes'),
      edges %>% 
        group_nest(id, game_id, team_id, .key = 'edges')
    ) %>% 
    drop_bad_ids() %>% 
    mutate(
      network_stats = pmap(list(id, nodes, edges), fv)
    )
  write_rds(network_stats_nested, path)
  network_stats_nested
}

network_stats_nested <- do_compute_network_stats()

## nested areas ----
do_get_areas <- function(.name, overwrite = FALSE) {
  f <- sprintf('get_%s_hull_areas', .name)
  fv <- function(name, ...) {
    cat(name, sep = '\n')
    exec(f, y_center = 34, ...)
  }
  path <- file.path(dir_proj, sprintf('%s_areas_nested.rds', .name))
  if(file.exists(path) & !overwrite) {
    return(read_rds(path))
  }
  areas_nested <- nodes %>% 
    drop_bad_ids() %>% 
    group_nest(id, game_id, team_id) %>%
    mutate(
      areas = map2(id, data, fv)
    )
  write_rds(areas_nested, path)
  areas_nested
}

concave_areas_nested <- do_get_areas('concave', overwrite = TRUE)
convex_areas_nested <- do_get_areas('convex',  overwrite = TRUE)

## prep agg ----
hoist_areas <- function(df) {
  df %>% 
    select(id, game_id, team_id, areas) %>% 
    hoist(areas, 'area_inner') %>% 
    hoist(areas, 'area_outer') %>% 
    hoist(areas, 'n_players' = 'n_players_orig') %>% 
    unnest(c(n_players, area_inner, area_outer))
}

transmute_area <- function(df) {
  df %>% 
    group_by(id) %>% 
    mutate(
      layer = row_number()
    ) %>% 
    ungroup() %>% 
    mutate(
      area_total = area_inner + area_outer,
      area_prop = area_inner / (area_inner + area_outer)
    ) %>% 
    select(-c(areas))
}

aggregate_areas <- function(df) {
  df %>%  
    group_by(id, game_id, team_id) %>% 
    summarize(
      n = n(),
      across(n_players, sum, na.rm = TRUE),
      area_prop = mean(area_inner / (area_inner + area_outer))
    ) %>% 
    ungroup()
}

concave_areas <- concave_areas_nested %>% 
  hoist_areas() %>% 
  transmute_area()
agg_concave_areas <- concave_areas %>% aggregate_areas()
stopifnot(1 == (concave_areas %>% count(id) %>% count(n) %>% filter(n > 1) %>% nrow()))
convex_areas <- convex_areas_nested %>% 
  hoist_areas() %>% 
  transmute_area()
convex_areas %>% count(id) %>% count(n) ## lots have 2 layers

agg_convex_areas <- convex_areas %>% aggregate_areas()

## diffs ----
concave_area_diffs_init <- concave_areas %>% 
  select(game_id, team_id, concave_area_prop = area_prop) %>% 
  left_join(
    concave_areas %>% 
      group_by(game_id, team_id) %>% 
      slice_max(area_outer, n = 1, with_ties = FALSE) %>% 
      ungroup() %>% 
      select(game_id, team_id, convex_area_prop_first = area_prop)
  ) %>% 
  left_join(
    agg_convex_areas %>% 
      select(game_id, team_id, convex_area_prop = area_prop)
  ) %>% 
  left_join(
    field_tilt %>% 
      select(game_id, team_id, field_tilt = prop)
  ) %>% 
  left_join(
    n_successful_passes %>% 
      select(game_id, team_id, prop_passes = prop)
  ) %>% 
  left_join(
    n_shots %>% 
      select(game_id, team_id, prop_shots = prop)
  ) %>% 
  add_side_col() %>%
  select(
    game_id,
    side,
    convex_area_prop_first,
    concave_area_prop,
    convex_area_prop,
    field_tilt,
    prop_passes,
    prop_shots
  )

concave_area_diffs_init

concave_area_diffs <- concave_area_diffs_init %>% 
  pivot_wider(
    names_from = side,
    names_glue = '{side}_{.value}',
    values_from = c(
      concave_area_prop,
      convex_area_prop_first,
      convex_area_prop,
      field_tilt,
      prop_passes,
      prop_shots
    )
  ) %>% 
  ## should reduce rows since not all games are included in scores (intentionally)
  inner_join(scores %>% select(-matches('^final_'))) %>% 
  mutate(
    diff_concave_area_prop = home_concave_area_prop - away_concave_area_prop,
    diff_convex_area_prop = home_convex_area_prop - away_convex_area_prop,
    diff_convex_area_prop_first = home_convex_area_prop - away_convex_area_prop_first,
    diff_field_tilt = home_field_tilt - away_field_tilt,
    diff_prop_passes = home_prop_passes - away_prop_passes,
    diff_prop_shots = home_prop_shots - away_prop_shots,
    diff_score = home_score - away_score
  ) %>% 
  arrange(desc(abs(diff_concave_area_prop)))

concave_area_diffs %>% 
  count(home_concave_area_prop > away_concave_area_prop, home_convex_area_prop < away_concave_area_prop)

concave_area_diffs %>% 
  ggplot() +
  aes(x = diff_concave_area_prop, y = diff_convex_area_prop) +
  geom_point()

concave_area_diffs_redux <- concave_area_diffs %>% 
  transmute(
    game_date,
    season_id,
    game_id,
    is_home_stronger = home_prop_passes > away_prop_passes,
    stronger_concave_area_prop = ifelse(is_home_stronger, home_concave_area_prop, away_concave_area_prop),
    weaker_concave_area_prop = ifelse(!is_home_stronger, home_concave_area_prop, away_concave_area_prop),
    stronger_convex_area_prop_first = ifelse(is_home_stronger, home_convex_area_prop_first, away_convex_area_prop_first),
    weaker_convex_area_prop_first = ifelse(!is_home_stronger, home_convex_area_prop_first, away_convex_area_prop_first),
    stronger_convex_area_prop = ifelse(is_home_stronger, home_convex_area_prop, away_convex_area_prop),
    weaker_convex_area_prop = ifelse(!is_home_stronger, home_convex_area_prop, away_convex_area_prop),
    stronger_field_tilt = ifelse(is_home_stronger, home_field_tilt, away_field_tilt),
    weaker_field_tilt = ifelse(!is_home_stronger, home_field_tilt, away_field_tilt),
    stronger_prop_passes = ifelse(is_home_stronger, home_prop_passes, away_prop_passes),
    weaker_prop_passes = ifelse(!is_home_stronger, home_prop_passes, away_prop_passes),
    stronger_prop_shots = ifelse(is_home_stronger, home_prop_shots, away_prop_shots),
    weaker_prop_shots = ifelse(!is_home_stronger, home_prop_shots, away_prop_shots),
    stronger_score = ifelse(is_home_stronger, home_score, away_score),
    weaker_score = ifelse(!is_home_stronger, home_score, away_score),
    stronger_team = ifelse(is_home_stronger, home_team_name, away_team_name),
    weaker_team = ifelse(!is_home_stronger, home_team_name, away_team_name),
    diff_concave_area_prop = stronger_concave_area_prop - weaker_concave_area_prop,
    diff_convex_area_prop_first = stronger_convex_area_prop_first - weaker_convex_area_prop_first,
    diff_convex_area_prop = stronger_convex_area_prop - weaker_convex_area_prop,
    diff_field_tilt = stronger_field_tilt - weaker_field_tilt,
    diff_prop_passes = stronger_prop_passes - weaker_prop_passes,
    diff_prop_shots = stronger_prop_shots - weaker_prop_shots,
    diff_score = stronger_score - weaker_score
  )

## cors ----
.str_replace_cor_col <- function(x, i) {
  str_replace_all(x, '(diff|stronger|weaker|home|away)_(.*$)', sprintf('\\%d', i))
}

do_tidy_cor <- function(data) {
  data %>% 
    corrr::correlate(quiet = TRUE) %>% 
    corrr::stretch() %>% 
    filter(!is.na(r)) %>% 
    # filter(y > x) %>% 
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
    filter(x_suffix != y_suffix) %>% 
    filter(x %>% str_detect('area') | y %>% str_detect('area')) %>% 
    filter(
      !(
        (x %>% str_detect('area') & y %>% str_detect('area'))
      )
    )
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
concave_cors_redux

concave_cors %>% 
  filter(x == 'diff_concave_area_prop')
concave_cors %>% 
  filter(x == 'home_concave_area_prop')

concave_cors_redux %>% 
  filter(x == 'diff_convex_area_prop_first')

concave_area_diffs_redux %>% 
  ggplot() +
  aes(x = stronger_concave_area_prop, y = weaker_concave_area_prop) +
  geom_point(
    aes(color = stronger_field_tilt)
  ) +
  geom_abline(
    aes(slope = 1, intercept = 0)
  ) +
  coord_cartesian(
    xlim = c(0, 1),
    ylim = c(0, 1)
  )

concave_area_diffs_redux %>% 
  ggplot() +
  aes(x = stronger_concave_area_prop, y = stronger_field_tilt) +
  geom_point(
  ) +
  geom_abline(
    aes(slope = 1, intercept = 0)
  ) +
  coord_cartesian(
    xlim = c(0, 1),
    ylim = c(0, 1)
  )


## convex ----
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

## plot ----
## Reference: https://github.com/Torvaney/ggsoccer/blob/master/R/dimensions.R
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

.select_unnest <- function(df, ...) {
  df %>% 
    select(...) %>% 
    unnest(...)
}

plot_convex_area <- function(i) {
  # i <- '1549543-161'
  i <- '1549626-32' # liverpool 5 0 man utd (home)
  cv <- convex_areas_nested %>% filter(id == i)
  cc <- concave_areas_nested %>% filter(id == i)
  acv <- cv %>% .select_unnest(areas)
  acc <- cc %>% .select_unnest(areas)
  d <- cv %>% .select_unnest(data)
  
  p <- d %>% 
    ggplot() +
    aes(x = x, y = y) +
    ggsoccer::annotate_pitch(
      dimensions = .pitch_international,
      colour = 'black', 
      fill = 'white'
    ) +
    # coord_flip(xlim = c(1, 99), ylim = c(4, 96), clip = 'on') +
    theme(
      axis.title = element_text(size = 12, hjust = 0),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(
      y = NULL,
      x = NULL
    ) +
    geom_point(
      data = acv$xy_hull_orig[[1]],
      aes(color = 'convex (1)')
    ) +
    geom_path(
      data = acv$xy_hull_orig[[1]],
      aes(color = 'convex (1)')
    ) +
    geom_hline(
      aes(yintercept = 34)
    ) +
    geom_point(
      data = acc$xy_hull_orig[[1]],
      aes(color = 'concave')
    ) +
    geom_path(
      data = acc$xy_hull_orig[[1]],
      aes(color = 'concave')
    )
  
  if(nrow(acv) > 1) {
    p <- p +
      geom_point(
        data = acv$xy_hull_orig[[2]],
        aes(color = 'convex (2)')
      ) +
      geom_path(
        data = acv$xy_hull_orig[[2]],
        aes(color = 'convex (2)')
      )
  }
  
  p +
    scale_color_manual(
      values = c(
        'convex (1)' = 'magenta',
        'convex (2)' = 'yellow',
        'concave' = 'blue', 
        'concave flipped' = 'orange'
      )
    ) +
    scale_fill_manual(
      values = c(
        'inner concave' = 'green',
        'outer concave' = 'red'
      )
    )
}
