
extrafont::loadfonts(device = 'win', quiet = TRUE)
library(tidyverse)
library(ggtext)
library(patchwork)
theme_set(ggdark::dark_theme_minimal())
theme_update(
  text = element_text(family = 'Karla', color = 'white'),
  title = element_text('Karla', size = 22, color = 'white'),
  plot.title = element_text(face = 'bold', size = 22),
  plot.subtitle = element_text(face = 'bold', size = 14, color = 'white'),
  # plot.margin = margin(10, 10, 10, 10),
  # panel.grid = element_blank(),
  axis.text = element_text(family = 'Karla', color = 'white'),
  plot.caption = element_text('Karla', size = 12, hjust = 0),
  plot.caption.position = 'plot',
  panel.spacing = element_blank(),
  panel.grid.major = element_line(color = 'gray30'),
  panel.grid.minor = element_line(color = 'gray30'),
  plot.background = element_rect(fill = 'gray10', color = NA),
  plot.tag = element_markdown(size = 12, hjust = 1),
  plot.tag.position = c(1, 0.01),
  panel.background = element_blank()
)
update_geom_defaults('text', list(family = 'Karla', size = 4, color = 'white'))
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
  ungroup() %>% 
  inner_join(
    stats_agg %>% 
      select(name, num_participations)
  )
stats_agg_long

stats_wl_ratio_wide <-
  stats_agg_long %>% 
  # filter(stat %in% c('num_participations', sprintf('%s_wins', c('home', 'draw')))) %>% 
  filter(stat %in% c('home_wins', 'draws')) %>% 
  select(name, league, num_participations, stat, value) %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  mutate(
    # away_win_ratio = away_wins / num_participations,
    draw_ratio = draws / num_participations,
    home_win_ratio = home_wins / num_participations
  )
stats_wl_ratio_wide
.ref_manu <- 'Anthony Taylor'
stats_wl_ratio_wide %>% filter(name == .ref_manu)
# stats_wl_ratio_wide %>% filter(name == 'Adam Nunn')

refs_current <-
  stats %>% 
  filter(season == max(season))
refs_current
stats_agg_long %>%
  semi_join(refs_current %>% select(name)) %>% 
  filter(stat == 'red_cds_per_game') %>% 
  group_by(league) %>% 
  filter(frac_league == max(frac_league)) %>% 
  ungroup()
.ref_epl_most_red <- 'Mike Dean' # Leeds v Man City

viz_1 <-
  stats_wl_ratio_wide %>% 
  ggplot() +
  # aes(x = home_win_ratio, y = away_win_ratio) +
  aes(x = home_win_ratio, y = draw_ratio) +
  geom_point(aes(size = num_participations))
viz_1
stats_agg_long %>% distinct(name)
# ggthemr::ggthemr('flat')
# LaCroixColoR::lacroix_palette("PeachPear", type = "discrete") -> z
# str(z)

.convert_rgb_to_hex <- function(r, g, b, a = 1) {
  rgb(r / 255, g / 255, b / 255, alpha = a)
}

yellow <- .convert_rgb_to_hex(228, 211, 84)
blue <- .convert_rgb_to_hex(124, 181, 236, 1)
purple <- .convert_rgb_to_hex(148, 153, 255)
red <- .convert_rgb_to_hex(241, 92, 128)
green <- .convert_rgb_to_hex(144, 237, 125)

stats_pg_info <-
  tibble(
    stat = c('fouls_per_game', 'penalties_awarded_against_per_game', 'yellow_cds_per_game', 'red_cds_per_game', 'home_win_ratio'),
    stat_lab = c('Fouls', 'Penalties', 'Yellow Cards', 'Red Cards', 'Home Wins'),
    color = c(blue, purple, yellow, red, green),
    idx_stat = c(1L, 2L, 3L, 4L, 5L)
  )
pal <-
  stats_pg_info %>% 
  select(stat_lab, color) %>% 
  deframe()
pal

stats_agg_long_pg <-
  stats_agg_long %>% 
  inner_join(stats_pg_info) %>% 
  mutate(across(stat_lab, ~forcats::fct_reorder(.x, idx_stat)))
stats_agg_long_pg

.dir_plot <- here::here()
plot_ref_radar <-
  function(ref_name,
           league_lab = 'Premier League',
           lab_subtitle = NULL,
           dir = .dir_plot,
           file = ref_name,
           path = fs::path(dir, sprintf('%s.png', file)),
           path_logo = fs::path('data-raw', sprintf('%s cropped.jpg', ref_name)),
           path_w_logo = path %>% str_replace('[.]png', '_w_logo.png'),
           logo_scale = 7) {
    
    viz <- 
      stats_agg_long_pg %>% 
      filter(name == ref_name) %>% 
      ggplot() +
      aes(x = stat_lab, y = frac_league) +
      # aes(x = stat_lab) +
      # geom_hline(yintercept = seq(0, 1, by = 0.25), colour = 'grey30', size = 0.2) +
      geom_col(aes(fill = stat_lab), width = 1.01, show.legend = FALSE) +
      # geom_bar(aes(weight = frac_league, fill = ..count..), width = 1) +
      scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1, by = 0.2)) +
      scale_fill_manual(values = pal) +
      coord_polar() +
      geom_text(
        aes(y = frac_league + 0.1, label = scales::percent(frac_league, accuracy = 1)), 
        size = 5, 
        family = 'Karla',
        fontface = 'bold'
      ) +
      theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 18, face = 'bold'),
        legend.position = 'bottom',
        legend.title = element_blank()
      ) +
      labs(
        title = glue::glue('{ref_name} vs. All {league_lab} Referees'),
        subtitle = lab_subtitle,
        caption = glue::glue(
          'Seasons: 2009/10 - 2019/20
        Only referees participating in >38 games considered'
        ),
        tag = '**Viz:** @TonyElHabr',
        x = NULL, y = NULL
      )
    viz
    ggsave(plot = viz, filename = path, width = 10, height = 10, type = 'cairo')
    viz_w_logo <- add_logo(path, path_logo, 'top right', logo_scale = logo_scale)
    # ggsave(plot = res, filename = path_final, width = 10, height = 10, type = 'cairo')
    magick::image_write(viz_w_logo, path_w_logo)
    viz_w_logo
  }

viz_radar_manu <- plot_ref_radar(ref_name = .ref_manu, lab_subtitle = 'Officiated Matchweek 4 Man U v. Tottenham')
viz_radar_worst <- plot_ref_radar(ref_name = .ref_epl_most_red, lab_subtitle = 'Active Official with Highest Card Rate')

.dir_plot <- here::here()
path <- fs::path(.dir_plot, sprintf('%s.png', .ref_manu))
path_final <- path %>% str_replace('[.]png', '_w_logo.png')
ggsave(plot = viz_2, filename = path, width = 10, height = 10, type = 'cairo')
res <- add_logo(path, fs::path('data-raw', 'anthony-taylor-cropped.jpg'), 'top right', logo_scale = 7)
# ggsave(plot = res, filename = path_final, width = 10, height = 10, type = 'cairo')
magick::image_write(res, path_final)


viz_3 <-
  stats_agg_long %>% 
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
    data = . %>% filter(name %in% .ref_manu),
    size = 3,
    color = 'red'
  ) +
  geom_point(
    data = . %>% filter(name %in% .ref_manu),
    size = 3,
    color = 'red'
  ) +
  facet_wrap(~stat, scales = 'free') +
  labs(x = 'Percentile', y = NULL) +
  theme_minimal()
viz_3
