
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
# tonythemes::theme_set_tony()
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

concave_areas_nested <- do_get_areas('concave', TRUE)
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
area_diffs_init %>% skimr::skim()

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
  # select(-c(side, game_id)) %>% 
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
  )

# team_areas %>% 
#   pivot_wider(
#     names_from = stat,
#     values_from = value
#   ) %>% 
#   ggplot() +
#   aes(x = field_tilt, y = diff_xg) +
#   geom_point(aes(color = factor(season_id))) +
#   geom_smooth(aes(color = factor(season_id)))

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
  ungroup()

wide_team_season_areas <- team_season_areas %>% 
  select(season_id, team_name, stat, mean) %>% 
  pivot_wider(
    names_from = stat,
    values_from = mean
  )

wide_team_season_areas %>% 
  select(-c(season_id:team_name)) %>% 
  corrr::correlate(quiet = TRUE) %>% 
  corrr::stretch() %>% 
  filter(!is.na(r)) %>% 
  filter(abs(r) != 1) %>% 
  filter(x == 'diff_xg') %>% 
  # mutate(
  #   y_is_diff = y %>% str_detect('^diff_')
  # ) %>% 
  filter(y %>% str_detect('^diff_')) %>% 
  select(y, r) %>% 
  arrange(desc(abs(r)))

wide_team_season_areas
  ggplot() +
  aes(x = diff_convex_area_prop, y = diff_xg, color = factor(season_id)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

team_season_areas_filt <- team_season_areas %>% 
  filter(stat == 'diff_convex_area_prop') %>% 
  mutate(grp = tidytext::reorder_within(team_name, mean, season_id))

team_season_areas_filt %>% 
  ggplot() +
  # aes(y = tidytext::reorder_within(team_name, mean, season_id), x = mean, group = grp) +
  # tidytext::scale_y_reordered() +
  aes(y = grp, x = mean, group = grp) +
  scale_y_discrete(name = '', labels = team_season_areas_filt %>% select(grp, team_name) %>% deframe()) +
  geom_errorbarh(
    # aes(xmin = mean - 2 * sd, xmax = mean + 2 * sd)
    aes(xmin = q05, xmax = q95)
  ) +
  geom_point() +
  scale_x_continuous(labels = scales::percent) +
  facet_wrap(~season_id, scales = 'free_y') +
  labs(
    x = NULL,
    y = NULL
  )

area_diffs <- inner_join(
  area_diffs_init %>% 
    select(-team_id) %>% 
    pivot_wider(
      names_from = side,
      names_glue = '{side}_{.value}',
      values_from = all_of(stat_cols)
    ),
  diffs %>% filter(side == 'home') %>% select(-side)
)

# area_diffs %>% skimr::skim()
# area_diffs %>% 
#   count(home_concave_area_prop > away_concave_area_prop, home_convex_area_prop < away_concave_area_prop)
# 
# area_diffs %>% 
#   ggplot() +
#   aes(x = diff_concave_area_prop, y = diff_convex_area_prop) +
#   geom_point()

## finding examples ----
teams %>% filter(team_name %in% c('Chelsea'))
outlier_prnks <- area_diffs %>% 
  left_join(
    meta %>% select(
      game_id,
      season_id,
      home_team_name,
      away_team_name,
      final_home_score = home_score,
      final_away_score = away_score
    )
  ) %>%
  filter(season_id >= 2020) %>% 
  left_join(
    last_actions_by_game %>% 
      select(game_id, last_min)
  ) %>% 
  select(
    game_id,
    season_id,
    home_team_name,
    away_team_name,
    final_home_score,
    final_away_score,
    home_score,
    away_score,
    last_min,
    home_xg,
    away_xg,
    diff_xg,
    home_concave_area_prop,
    away_concave_area_prop,
    diff_concave_area_prop
  ) %>% 
  mutate(
    across(matches('^diff'), percent_rank)
  ) %>% 
  mutate(
    prnk1 = diff_xg * diff_concave_area_prop,
    prnk2 = (1 - diff_xg) * diff_concave_area_prop,
    prnk3 = diff_xg * (1 - diff_concave_area_prop)
  ) %>% 
  relocate(home_team_name, away_team_name, final_home_score, final_away_score)


outlier_prnks %>% 
  filter(season_id == 2021) %>% 
  inner_join(
    scores %>% left_join(teams) %>% select(-score)
  ) %>% 
  filter(team_name == 'Arsenal')

outlier_prnks %>% 
  filter(season_id == 2021) %>% 
  inner_join(
    scores %>% left_join(teams) %>% select(-score)
  ) %>% 
  # filter(home_team_name == 'Man Utd' | away_team_name == 'Man Utd') %>% 
  mutate(is_home = home_team_name == team_name) %>% 
  ggplot() +
  aes(x = diff_xg, y = diff_concave_area_prop) +
  geom_point(aes(color = is_home)) +
  ggrepel::geom_text_repel(
    aes(
      label = sprintf('%s @ %s', away_team_name, home_team_name)
    )
  ) +
  facet_wrap(~team_name)
# Norwitch @ Arsenal, Arsenal @ West Ham
outlier_prnks %>% 
  filter(last_min > 45) %>% 
  # filter(home_xg > 1 | away_xg > 1) %>% 
  filter(home_concave_area_prop < 0.3 | away_concave_area_prop < 0.3) %>% 
  filter(home_concave_area_prop > 0.7 | away_concave_area_prop > 0.7) %>% 
  arrange(desc(prnk1))
outlier_prnks %>% 
  filter(last_min > 45) %>% 
  filter(home_concave_area_prop < 0.3 | away_concave_area_prop < 0.3) %>% 
  filter(home_concave_area_prop > 0.7 | away_concave_area_prop > 0.7) %>% 
  arrange(desc(prnk2))
outlier_prnks %>% 
  filter(last_min > 45) %>% 
  filter(season_id == 2021) %>% 
  filter(home_concave_area_prop < 0.4 | away_concave_area_prop < 0.4) %>% 
  filter(home_concave_area_prop > 0.7 | away_concave_area_prop > 0.7) %>% 
  arrange(desc(prnk3))


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
    # filter(x %>% str_detect('area') | y %>% str_detect('area')) %>% 
    # filter(
    #   !(
    #     (x %>% str_detect('area') & y %>% str_detect('area'))
    #   )
    # ) %>% 
    select(x, y, x_prefix, x_suffix, y_prefix, y_suffix, r) %>% 
    arrange(desc(abs(r)))
}

areas_cors <- area_diffs %>% 
  select(
    where(is.numeric), -matches('_id$')
  ) %>% 
  do_tidy_cor()
areas_cors %>% filter(x == 'home_xg', y_prefix == 'home')

area_diffs

area_diffs %>% 
  filter(game_id == 1549663) %>% 
  pivot_longer(-c(game_id)) %>% 
  left_join(meta)

area_diffs %>% 
  filter(game_id %in% c(1549626, 1549563)) %>% 
  glimpse()
meta %>% filter(home_team_name == 'Man Utd', away_team_name == 'Liverpool') # 1549626, 32, 26
meta %>% filter(home_team_name == 'Man City', away_team_name == 'Arsenal') # 1549563, 167, 13

areas_cors %>% 
  filter(x == 'diff_concave_area_prop')
areas_cors %>% 
  filter(x == 'diff_convex_area_prop', y_prefix == 'diff')
areas_cors %>% 
  filter(x == 'diff_xg', y_prefix == 'diff')
areas_cors %>% 
  filter(x == 'home_field_tilt', y_prefix == 'home')

# stat_combos <- full_join(
#   area_diffs %>% 
#     select(-matches('diff|first|score')) %>% 
#     pivot_longer(
#       -game_id,
#       names_to = 'stat1',
#       values_to = 'value1'
#     ),
#   area_diffs %>% 
#     select(-matches('diff|first|score')) %>% 
#     pivot_longer(
#       -game_id,
#       names_to = 'stat2',
#       values_to = 'value2'
#     )
# ) %>% 
#   filter(stat1 != stat2)
# stat_combos %>% distinct(stat1)
# stat_combos %>% count(stat1, stat2)
# set.seed(1)
# meta_sample <- meta %>% slice_sample(n = 100)
# stat_combos %>% 
#   semi_join(meta_sample) %>% 
#   ggplot() +
#   aes(value1, value2) +
#   geom_point() +
#   facet_grid(stat1~stat2, scales = 'free') -> p
# ggsave(plot = p, filename = 'temp.png', width = 30, height = 30)

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
    unnest(where(is.list))
}

plot_networks <- function(.game_id = 1549626) {
  # .game_id = 1549626 # liverpool @ man utd (2021-10-24)
  # .game_id <- 1549646 # man city @ man utd (2021-11-06)
  # .game_id <- 1549539
  # .game_id <- 1549603
  # .game_id <- 1549638
  # .game_id <- 1549539 # arsenal at brentford (arsenal dominates)
  .game_id <- 1549589 # tot at arsenal
  area_diffs %>% 
    filter(game_id == .game_id) %>% 
    glimpse()
  named_team_ids <- meta %>% 
    filter(game_id == .game_id) %>% 
    select(home_team_id, away_team_id) %>% 
    c() %>% 
    unlist()
  n <- nodes %>% filter(game_id == .game_id) %>% add_side_col()
  e <- edges %>% filter(game_id == .game_id) %>% add_side_col()
  n %>% 
    ggplot() +
    aes(x = x, y = y) +
    ggsoccer::annotate_pitch(
      dimensions = .pitch_international,
      colour = 'black', 
      fill = 'white'
    ) +
    # coord_flip(xlim = c(1, 99), ylim = c(4, 96), clip = 'on') +
    coord_flip(ylim = c(0, 68), xlim = c(105, 0)) +
    labs(x = NULL, y = NULL) +
    theme(
      axis.title = element_text(size = 12, hjust = 0),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    geom_point(
      aes(size = n)
    ) +
    ggrepel::geom_text_repel(
      aes(label = name)
    ) +
    facet_wrap(~side)
  
}

.common_gg <- function(data, ...) {
  list(
    ...,
    aes(x = x, y = y),
    ggsoccer::annotate_pitch(
      dimensions = .pitch_international,
      colour = 'black', 
      fill = 'white'
    ),
    # coord_flip(xlim = c(1, 99), ylim = c(4, 96), clip = 'on') +
    theme(
      axis.title = element_text(size = 12, hjust = 0),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ),
    labs(
      y = NULL,
      x = NULL
    ),
    coord_flip(ylim = c(0, 68), xlim = c(105, 0))
  )
}

plot_convex_area <- function(.game_id = 1549626) {
  named_team_ids <- meta %>% 
    filter(game_id == .game_id) %>% 
    select(home_team_id, away_team_id) %>% 
    c()
  # cvs <- convex_areas_nested %>% filter(game_id == .game_id)
  ccs <- convex_areas_nested %>% filter(game_id == .game_id)
  
  acc <- ccs %>% .select_unnest(team_id, areas)
  d <- ccs %>% .select_unnest(team_id, data)
  
  xy <- acc %>% 
    mutate(grp = row_number()) %>% 
    .select_unnest(grp, team_id, xy_hull_orig)
  
  p <- d %>% 
    ggplot() +
    .common_gg() +
    facet_wrap(~team_id) +
    geom_hline(
      aes(yintercept = 34)
    ) +
    geom_point(
      data = xy,
      aes(color = factor(team_id))
    ) +
    geom_path(
      data = xy,
      aes(color = factor(team_id), group = grp)
    )
  p
  
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
