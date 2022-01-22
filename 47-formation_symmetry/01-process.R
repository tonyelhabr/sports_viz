
library(tidyverse)
library(arrow)

dir_proj <- '47-formation_symmetry'
understat_xg <- file.path(dir_proj, 'understat_xg.rds') %>% read_rds()
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
    player_games %>% filter(!is_starter, minutes_played > 0)
  ) %>% 
  group_by(game_id) %>% 
  slice_min(atomic_action_id, n = 1) %>% 
  ungroup() %>% 
  select(game_id, last_atomic_action_id = atomic_action_id, period_id, time) %>% 
  mutate(
    last_min = as.integer((period_id - 1) * 45 + time / 60)
  )

# last_actions_by_game %>% 
#   mutate(
#   ggplot(aes(last_min)) +
#   geom_histogram()

pivot_stat <- function(df, .stat) {
  df %>% 
    pivot_longer(
      -game_id,
      names_pattern = sprintf('(home|away)_(team_id|%s)', .stat),
      names_to = c('side', 'col'),
      values_to = 'value'
    ) %>% 
    pivot_wider(
      names_from = col,
      values_from = value
    ) %>% 
    select(-side)
}

xg <- last_actions_by_game %>% 
  left_join(
    meta %>% 
      transmute(
        game_id,
        date = game_date %>% lubridate::date(),
        home_team_name,
        away_team_name
      )
  ) %>% 
  left_join(
    understat_xg %>% 
      transmute(
        # understat_game_id = match_id,
        home_team_name = team_h,
        away_team_name = team_a,
        understat_time = minute,
        date,
        home_xg = xg_h,
        away_xg = xg_a
      )
  ) %>% 
  filter(understat_time < last_min) %>% 
  group_by(game_id) %>% 
  summarize(
    across(c(home_xg, away_xg), sum)
  ) %>% 
  ungroup() %>% 
  left_join(
    meta %>% 
      select(
        game_id,
        home_team_id,
        away_team_id
      )
  ) %>% 
  pivot_stat('xg')
xg
xg %>% arrange(desc(xg))

filt_actions <- all_actions_atomic %>% 
  left_join(
    last_actions_by_game %>% 
      select(game_id, last_atomic_action_id)
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

add_side_col <- function(df) {
  df %>% 
    left_join(meta) %>% 
    mutate(
      side = ifelse(team_id == home_team_id, 'home', 'away')
    ) %>% 
    select(all_of(colnames(df)), side)
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
      select(
        game_id,
        home_team_id,
        away_team_id
      )
  ) %>% 
  pivot_stat('score')

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
# 
# drop_keepers <- function(df) {
#   df %>% 
#     anti_join(
#       player_games %>% 
#         filter(starting_position_name == 'GK')
#     )
# }

successful_passes <- filt_actions %>% 
  # drop_keepers() %>% 
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
  # drop_keepers() %>% 
  group_by(game_id, team_id, player_id) %>% 
  summarize(n = n(), across(c(x, y), mean)) %>% 
  ungroup() %>% 
  add_id_col() %>% 
  left_join(players %>% rename(name = player_name))

bad_ids <- nodes %>% 
  count(id, game_id, team_id, name = 'n_players') %>% 
  ## teams with less than 11 players?!?
  filter(n_players < 11) %>% 
  distinct(id, game_id, team_id, n_players)

drop_bad_ids <- function(df) {
  df %>% anti_join(bad_ids)
}

do_compute_network_stats <- function(overwrite = F) {
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

do_compute_max_cuts <- function(weighted = TRUE, overwrite = F) {
  fv <- function(name, ...) {
    cat(name, sep = '\n')
    compute_max_cut(weighted = weighted, ...)
  }
  path <- file.path(dir_proj, sprintf('max_cuts_%sweighted.rds', ifelse(weighted, '', 'un')))
  if(file.exists(path) & !overwrite) {
    return(read_rds(path))
  }
  max_cuts_nested <- edges %>% 
    group_nest(id, game_id, team_id) %>% 
    drop_bad_ids() %>% 
    mutate(
      max_cuts = map2_dbl(id, data, fv)
    )
  write_rds(max_cuts_nested, path)
  max_cuts_nested
}

weighted_max_cuts_nested <- do_compute_max_cuts(TRUE)
unweighted_max_cuts_nested <- do_compute_max_cuts(FALSE)

max_cuts <- full_join(
  weighted_max_cuts_nested %>% select(id, game_id, team_id, max_cut_weighted = max_cuts),
  unweighted_max_cuts_nested %>% select(id, max_cut_unweighted = max_cuts)
)

## nested areas ----
do_get_areas <- function(.name, overwrite = F) {
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

concave_areas_nested <- do_get_areas('concave')
convex_areas_nested <- do_get_areas('convex')

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

convex_areas <- convex_areas_nested %>% 
  hoist_areas() %>% 
  transmute_area()
agg_convex_areas <- convex_areas %>% aggregate_areas()

## diffs ----
stat_cols <- c(
  'concave_area_prop',
  'concave_area_prop_first',
  'convex_area_prop',
  'convex_area_prop_first',
  'max_cut_weighted',
  'max_cut_unweighted',
  'field_tilt',
  'prop_passes',
  'prop_shots',
  'reciprocity',
  'transitivity',
  'mean_distance',
  'density',
  'score',
  'xg'
)

area_diffs_init <- agg_concave_areas %>% 
  select(game_id, team_id, concave_area_prop = area_prop) %>% 
  inner_join(
    concave_areas %>% 
      group_by(game_id, team_id) %>% 
      slice_max(area_outer, n = 1, with_ties = FALSE) %>% 
      ungroup() %>% 
      select(game_id, team_id, concave_area_prop_first = area_prop)
  ) %>% 
  inner_join(
    agg_convex_areas %>%
      select(game_id, team_id, convex_area_prop = area_prop)
  ) %>%
  inner_join(
    convex_areas %>%
      group_by(game_id, team_id) %>%
      slice_max(area_outer, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(game_id, team_id, convex_area_prop_first = area_prop)
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
  left_join(
    max_cuts %>% 
      select(game_id, team_id, max_cut_weighted, max_cut_unweighted)
  ) %>% 
  left_join(
    network_stats_nested %>% 
      select(game_id, team_id, network_stats) %>% 
      unnest(network_stats)
  ) %>% 
  ## should reduce rows since not all games are included in scores (intentionally)
  # inner_join(
  left_join(
    scores
  ) %>% 
  left_join(
    xg
  ) %>% 
  left_join(
    last_actions_by_game %>% 
      select(game_id, last_min)
  ) %>% 
  add_side_col() %>%
  mutate(
    across(c(prop_shots, score, xg), ~coalesce(.x, 0))
  )

do_compute_area_diffs <- function(.side) {
  area_diffs_init %>% 
    select(-team_id) %>% 
    pivot_longer(
      -c(game_id, side),
      names_to = 'stat',
      values_to = 'value'
    ) %>% 
    mutate(
      across(value, ~ifelse(side == .side, .x, -.x))
    ) %>% 
    group_by(game_id, stat) %>% 
    summarize(
      across(value, sum)
    ) %>% 
    ungroup() %>% 
    pivot_wider(
      names_from = stat,
      names_glue = 'diff_{stat}',
      values_from = value
    ) %>% 
    mutate(side = .side)
}

diffs <- bind_rows(
  do_compute_area_diffs('home'),
  do_compute_area_diffs('away')
)

team_areas <- area_diffs_init %>% 
  left_join(
    meta %>% 
      select(season_id, game_id)
  ) %>% 
  left_join(
    diffs
  ) %>% 
  pivot_longer(
    -c(season_id, team_id, game_id, side, last_min),
    names_to = 'stat',
    values_to = 'value'
  ) %>% 
  left_join(teams) %>% 
  mutate(
    across(
      value,
      ~ifelse(
        .x %in% c('xg'),
        ~.x * 90 / last_min,
        .x
      )
    )
  ) %>% 
  left_join(
    xg
  ) %>% 
  add_side_col() %>% 
  left_join(
    diffs %>% 
      select(game_id, side, diff_xg)
  )

xg_season <- xg %>% 
  add_side_col() %>% 
  left_join(
    meta %>% 
      select(season_id, game_id)
  ) %>% 
  left_join(
    diffs %>% 
      select(game_id, side, diff_xg)
  ) %>% 
  left_join(
    last_actions_by_game %>% 
      select(game_id, last_min)
  ) %>% 
  group_by(season_id, team_id) %>% 
  summarize(
    n_games = n(),
    across(last_min, list(mean = mean)),
    across(c(xg, diff_xg), list(sum = sum))
  ) %>% 
  ungroup()

team_season_areas <- team_areas %>% 
  group_by(season_id, team_id, team_name, stat) %>% 
  summarize(
    # n = n(),
    # across(last_min, mean),
    across(
    value,
    list(
      mean = mean,
      sd = sd,
      q05 = ~quantile(.x, 0.05),
      q95 = ~quantile(.x, 0.95)
    ),
    na.rm = TRUE,
    .names = '{fn}'
  )) %>% 
  ungroup() %>% 
  left_join(
    xg_season
  )
