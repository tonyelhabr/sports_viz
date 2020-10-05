
str_replace_ref_path <- function(x, i) {
  str_replace_all(x, '(.*)_([0-9]{4}-[0-9]{4})_([0-9]+)_(Overall|Home)[.]json$', sprintf('\\%d', i))
}

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
        suffix = ~str_replace_ref_path(.x, 4)
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
  # group_by(official_id, first_name, last_name, name, country_id, league) %>% 
  group_by(name, league) %>% 
  mutate(num_games_tot = sum(num_games)) %>% 
  ungroup() %>% 
  filter(num_games_tot > 38)
stats_filt

stats_agg <-
  stats_filt %>% 
  group_by(name, league) %>% 
  summarize(
    # n_league = n_distinct(league),
    n_season = n_distinct(season),
    across(c(fouls_per_game, fouls_per_tackle, penalties_awarded_against_per_game, yellow_cds_per_game, red_cds_per_game, home_win_ratio, away_win_ratio, draw_ratio), ~sum(.x * num_games / num_games_tot)),
    across(c(num_games, num_participations, fouls, tackles, penalties, yellow_cds, red_cds, wins, losses, home_wins, away_wins, draws, field), sum)
  ) %>% 
  ungroup() %>% 
  arrange(desc(num_participations))
stats_agg
# stats_agg %>% skimr::skim()

stats_pg <-
  c(fouls_per_game, fouls_per_tackle, penalties_awarded_against_per_game, yellow_cds_per_game, red_cds_per_game, home_win_ratio, away_win_ratio, draw_ratio)
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
  mutate(idx_stat = dense_rank(stat)) %>% 
  group_by(name, stat_type) %>% 
  mutate(frac_tot = sum(frac, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    idx_name = dense_rank(desc(frac_tot))
  ) %>% 
  # select(-frac_tot) %>% 
  arrange(idx_name)
stats_agg_long

stats_wl_ratio_wide <-
  stats_agg_long %>% 
  filter(stat %in% c('num_participations', sprintf('%s_wins', c('home', 'away')))) %>% 
  select(name, league, stat, value) %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  mutate(home_win_ratio = home_wins / num_participations, away_win_ratio = away_wins / num_participations)
stats_wl_ratio_wide

viz_1 <-
  stats_wl_ratio_wide %>% 
  ggplot() +
  aes(x = home_win_ratio, y = away_win_ratio) +
  geom_point(aes(size = num_participations))
viz_1
stats_agg_long %>% distinct(name)
stats_agg_long %>% 
  filter(idx_name <= 3L) %>% 
  mutate(
    name = name %>% forcats::fct_reorder(idx_name)
  ) %>% 
  ggplot() +
  aes(x = idx_stat, y = frac) +
  geom_col(aes(fill = stat), width = 1.0) +
  coord_polar() +
  facet_wrap(~name)
