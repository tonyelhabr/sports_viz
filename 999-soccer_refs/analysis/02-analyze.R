
stats <-
  fs::dir_ls(
    .dir_data,
    regexp = 'Overall[.]json$'
  ) %>% 
  fs::file_info() %>% 
  mutate(across(size, as.integer)) %>% 
  filter(size > 2000) %>% 
  select(path) %>% 
  mutate(basename = path %>% basename()) %>% 
  mutate(
    across(
      basename,
      list(
        league = ~str_replace_ref_path(.x, 1),
        season = ~str_replace_ref_path(.x, 2),
        suffix = ~str_replace_ref_path(.x, 3)
      ),
      .names = '{fn}'
    )
  ) %>% 
  mutate(data = map(path, .import_json)) %>% 
  select(-path, -basename) %>% 
  unnest(data)
stats

# stats_filt <- stats %>% filter(league == 'EPL')
stats_filt <-
  stats %>% 
  filter(season != max(season)) %>% 
  # group_by(official_id, first_name, last_name, name, country_id, league) %>% 
  group_by(name, league) %>% 
  mutate(num_games_tot = sum(num_games)) %>% 
  ungroup() %>% 
  relocate(num_games_tot) %>% 
  filter(num_games_tot > 38) %>% 
  arrange(desc(num_games_tot))
stats_filt

stats_agg <-
  stats_filt %>% 
  # These are always 0.
  select(-wins, -losses) %>% 
  # Tursn out refs only appear in 1 league, so it's ok to group by both name and league.
  group_by(name, league) %>% 
  summarize(
    # n_league = n_distinct(league),
    n_season = n_distinct(season),
    across(c(fouls_per_game, fouls_per_tackle, penalties_awarded_against_per_game, yellow_cds_per_game, red_cds_per_game, home_win_ratio, away_win_ratio, draw_ratio), ~sum(.x * num_games / num_games_tot)),
    across(c(num_games, num_participations, fouls, tackles, penalties, yellow_cds, red_cds, home_wins, away_wins, draws, field), ~sum(.x, na.rm = TRUE))
  ) %>% 
  ungroup() %>% 
  arrange(desc(num_participations))
stats_agg
# stats_agg %>% count(n_league)
# stats_agg %>% skimr::skim()

stats_agg_long <-
  stats_agg %>%
  pivot_longer(-c(name:league), names_to = 'stat', values_to = 'value') %>% 
  mutate(
    across(
      stat,
      list(type = ~case_when(
        .x %in% c('away_win_ratio', 'draw_ratio') ~ 'pg_exclude',
        str_detect(.x, '_(per|ratio)') ~ 'pg',
        TRUE ~ 'raw'
      )
      )
    )
  ) %>% 
  group_by(stat) %>% 
  mutate(
    frac = (value - min(value)) / (max(value) - min(value))
  ) %>% 
  ungroup() %>% 
  group_by(league, stat) %>% 
  mutate(
    frac_league = (value - min(value)) / (max(value) - min(value))
  ) %>% 
  ungroup()
stats_agg_long

stats_wl_ratio_wide <-
  stats_agg_long %>% 
  # filter(stat %in% c('num_participations', sprintf('%s_wins', c('home', 'draw')))) %>% 
  filter(stat %in% c('num_participations', 'home_wins', 'draws')) %>% 
  select(name, league, stat, value) %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  mutate(
    # away_win_ratio = away_wins / num_participations,
    draw_ratio = draws / num_participations,
    home_win_ratio = home_wins / num_participations
  )
stats_wl_ratio_wide
.refs_filt <- 'Anthony Taylor'
stats_wl_ratio_wide %>% filter(name %in% .refs_filt)
stats_wl_ratio_wide %>% filter(name == 'Adam Nunn')

viz_1 <-
  stats_wl_ratio_wide %>% 
  ggplot() +
  # aes(x = home_win_ratio, y = away_win_ratio) +
  aes(x = home_win_ratio, y = draw_ratio) +
  geom_point(aes(size = num_participations))
viz_1
stats_agg_long %>% distinct(name)

viz_2 <-
  stats_agg_long %>% 
  # filter(idx_name <= 3L) %>% 
  filter(name %in% .refs_filt) %>% 
  # mutate(
  #   name = name %>% forcats::fct_reorder(idx_name)
  # ) %>% 
  filter(stat_type %in% 'pg') %>% 
  mutate(idx_stat = dense_rank(stat)) %>% 
  ggplot() +
  aes(x = idx_stat, y = frac_league) +
  geom_col(aes(fill = stat), width = 1.0) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_polar() +
  # facet_wrap(~name) +
  theme_minimal() +
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  labs(x = NULL, y = NULL)
viz_2

stats_agg_long_aug <-
  stats_agg_long %>% 
  inner_join(
    stats_agg %>% 
      select(name, num_participations)
  )
stats_agg_long_aug

stats_agg_long_aug %>%
  filter(stat == 'red_cds_per_game') %>% 
  group_by(league) %>% 
  filter(frac_league == max(frac_league)) %>% 
  ungroup()

viz_3 <-
  stats_agg_long_aug %>% 
  filter(league == 'EPL') %>% 
  filter(stat_type == 'pg') %>% 
  group_by(stat) %>% 
  mutate(
    idx = row_number(frac)
  ) %>% 
  ggplot() +
  aes(y = idx, x = frac) +
  scale_x_continuous(values = scales::percent) +
  geom_segment(aes(yend = idx, xend = 0)) +
  geom_point() +
  geom_segment(
    data = . %>% filter(name %in% .refs_filt),
    size = 3,
    color = 'red'
  ) +
  geom_point(
    data = . %>% filter(name %in% .refs_filt),
    size = 3,
    color = 'red'
  ) +
  facet_wrap(~stat, scales = 'free') +
  labs(x = 'Percentile', y = NULL) +
  theme_minimal()
viz_3
