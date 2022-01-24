
dir_proj <- '47-formation_symmetry'
source(file.path(dir_proj, '01-process.R'))

wide_team_season_areas <- team_season_areas %>% 
  # select(season_id, team_name, stat, value, n, n_games, last_min_mean) %>% 
  pivot_wider(
    names_from = stat,
    values_from = value
  )

team_areas %>% 
  filter(stat %>% str_detect('last_min', negate = TRUE)) %>% 
  group_by(stat) %>% 
  mutate(
    across(
      value,
      ~(.x - mean(.x)) / sd(.x)
    )
  ) %>% 
  ggplot() +
  aes(x = value) +
  geom_histogram() +
  facet_wrap(~stat, scales = 'free')

wide_team_season_areas %>% 
  lm(formula(diff_xg ~ diff_score), data = .) %>% 
  broom::tidy()

wide_team_season_areas %>% 
  mutate(
    across(
      c(matches('^diff')),
      ~(.x - mean(.x)) / sd(.x)
    )
  ) %>% 
  select(
    m
  )
  lm(formula(diff_xg ~ diff_max_cut_weighted + prop_shots), data = .) %>% 
  broom::tidy() %>% 
  ggplot() +
  aes(x = estimate, y = term) +
  geom_point() +
  geom_errorbarh(
    aes(xmin = estimate - std.error, xmax = estimate + std.error)
  ) +
  geom_vline(
    aes(xintercept = 0)
  )

wide_game_areas <- team_areas %>% 
  select(season_id, game_id, team_name, stat, value) %>% 
  pivot_wider(
    names_from = stat,
    values_from = value
  )

.str_replace_cor_col <- function(x, i) {
  str_replace_all(x, '(diff|home|away)_(.*$)', sprintf('\\%d', i))
}

do_tidy_cor <- function(data) {
  data %>% 
    corrr::correlate(quiet = TRUE) %>% 
    corrr::stretch() %>% 
    filter(!is.na(r), abs(r) != 1) %>% 
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
    select(x, y, x_prefix, x_suffix, y_prefix, y_suffix, r) %>% 
    arrange(desc(abs(r)))
}

do_tidy_xg_cor <- function(df) {
  df %>% 
    do_tidy_cor() %>% 
    filter(x == 'diff_xg') %>% 
    filter(y %>% str_detect('^diff_')) %>% 
    select(y, r) %>% 
    arrange(desc(abs(r)))
}

team_season_xg_cors <- wide_team_season_areas %>% 
  select(-c(season_id, team_name)) %>% 
  do_tidy_xg_cor()
game_xg_cors <- wide_game_areas %>% 
  select(-c(season_id, game_id, team_name)) %>% 
  do_tidy_xg_cor()

wide_team_season_areas %>% 
  ggplot() +
  aes(x = diff_convex_area_prop, y = diff_xg, color = factor(season_id)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

wide_team_season_areas %>% 
  ggplot() +
  aes(x = diff_max_cut_weighted, y = diff_xg, color = factor(season_id)) +
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

plot_convex_area <- function(.game_id = 1549626) {
  named_team_ids <- meta %>% 
    filter(game_id == .game_id) %>% 
    select(home_team_id, away_team_id) %>% 
    c()
  cs <- convex_areas_nested %>% filter(game_id == .game_id)
  
  acs <- cs %>% select_unnest(team_id, areas)
  d <- cs %>% select_unnest(team_id, data)
  
  xy <- acs %>% 
    mutate(grp = row_number()) %>% 
    select_unnest(grp, team_id, xy_hull_orig)
  
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
}
