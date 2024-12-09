
library(tidyverse)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = element_text('Karla', face = 'bold', size = 24, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', face = 'bold', size = 16, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  plot.caption = ggtext::element_markdown('Karla', size = 14, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 14, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.01),
  legend.text = element_text(color = 'gray20', size = 14),
  strip.text = element_text(color = 'gray20', size = 14),
  strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

dir_proj <- '25-202021_soccer_refs'
dir_data <- file.path(dir_proj, 'data')

results <-
  fs::dir_ls(dir_data, regexp = 'results-') %>% 
  tibble(path = .) %>% 
  mutate(
    data = map(path, ~read_rds(.x) %>% mutate(across(Wk, as.integer), across(matches('xG$'), as.numeric)))
  ) %>% 
  select(-path) %>% 
  unnest(data) %>% 
  janitor::clean_names() %>% 
  select(-matches('_2$'))
results
countries_big5 <- c('ENG', 'ESP', 'FRA', 'GER', 'ITA')
results_filt <- results %>% filter(country %in% countries_big5)

stats <-
  fs::dir_ls(dir_data, regexp = 'misc-player.*rds$') %>%
  tibble(path = .) %>% 
  mutate(
    data = map(path, ~read_rds(.x))
  ) %>% 
  select(-path) %>% 
  unnest(data) %>% 
  janitor::clean_names() 

# teams_stats <- stats %>% count(team = home_team, stat = 'n_stats')
# teams_stats
# results_stats <- results %>% count(team = home, stat = 'n_results')
# results_stats
# teams_stats %>% 
#   full_join(results_stats) %>% 
#   filter(is.na(n_results)) %>% 
#   pull(team) %>% 
# datapasta::vector_paste()
team_corrections <-
  tibble(
    team_stats = c('Brighton & Hove Albion', 'Huddersfield Town', 'Manchester United', 'Newcastle United', 'Sheffield United', 'Tottenham Hotspur', 'West Bromwich Albion', 'West Ham United', 'Wolverhampton Wanderers'),
    team_results = c('Brighton', 'Huddersfield', 'Manchester Utd', 'Newcastle Utd', 'Sheffield Utd', 'Tottenham', 'West Brom', 'West Ham', 'Wolves')
  )

.add_ratio_foul_tackle_col <- function(data) {
  data %>% mutate(ratio_foul_tackle = n_foul / n_tackle_won)
}

misc_w_refs <-
  stats %>% 
  mutate(
    idx_stats = row_number(),
    date = strptime(match_date, '%A %B %d, %Y', tz = 'UTC') %>% lubridate::date()
  ) %>% 
  select(-matches('_goals$')) %>% 
  rename(team_home = home_team, team_away = away_team) %>% 
  left_join(team_corrections %>% rename(team_home = team_stats, team_home_alt = team_results)) %>% 
  mutate(
    across(team_home, ~coalesce(team_home_alt, .x))
  ) %>% 
  left_join(team_corrections %>% rename(team_away = team_stats, team_away_alt = team_results)) %>% 
  mutate(
    across(team_away, ~coalesce(team_away_alt, .x))
  ) %>% 
  select(
    idx_stats,
    country,
    # tier,
    gender,
    season,
    team_home,
    team_away,
    date,
    team,
    player,
    mp = min,
    n_card_y = crd_y,
    n_card_r = crd_r,
    n_card_y2 = x2crd_y,
    n_foul_committed = fls,
    n_foul_drawn = fld,
    n_offside = off,
    n_tackle_won = tkl_w,
    n_pk_won = p_kwon,
    n_pk_conceded = p_kcon
  ) %>% 
  # 2nd yellow card counts as both a yellow and a red.
  mutate(across(c(n_card_y, n_card_r), ~.x + n_card_y2)) %>% 
  select(-n_card_y2) %>% 
  # Not sure what's thje best way to handle these seemingly duplicate numbers. Just picking one and dropping the other.
  select(-n_foul_drawn) %>% 
  rename(n_foul = n_foul_committed) %>% 
  select(-n_pk_won) %>% 
  rename(n_pk = n_pk_conceded) %>% 
  # filter(idx_stats == 10204L) %>% 
  inner_join(
    results_filt %>%
      mutate(idx_results = row_number()) %>% 
      select(idx_results, wk, date, team_home = home, team_away = away, referee)
  ) %>% 
  .add_ratio_foul_tackle_col() %>% 
  select(
    idx_stats,
    idx_results,
    country,
    # tier,
    gender,
    season,
    wk,
    date,
    team_home,
    team_away,
    referee,
    team,
    player,
    everything()
  )
misc_w_refs

.do_group <- function(data, ...) {
  data %>% 
    group_by(season, country, gender, referee, date, team_home, team_away,  ...)
}

agg_by_game_team <-
  misc_w_refs %>% 
  .do_group(team) %>% 
  summarize(
    n_player = n_distinct(player),
    across(
      c(mp:last_col()),
      sum,
      na.rm = TRUE
    )
  ) %>% 
  ungroup() %>% 
  .add_ratio_foul_tackle_col()
agg_by_game_team

agg_by_game <-
  agg_by_game_team %>% 
  .do_group() %>% 
  summarize(
    across(
      c(n_player:last_col()),
      sum
    )
  ) %>% 
  ungroup()
agg_by_game

.sum <- partial(sum, na.rm = TRUE, ... =)
agg <-
  agg_by_game %>% 
  group_by(season, country, gender, referee) %>% 
  summarize(
    n_game = sum(mp != 0),
    n_player = .sum(n_player),
    mp = .sum(mp),
    n_card_y = .sum(n_card_y),
    n_card_r = .sum(n_card_r),
    n_foul = .sum(n_foul),
    n_offside = .sum(n_offside),
    n_tackle_won = .sum(n_tackle_won),
    n_pk = .sum(n_pk)
    # across(c(n_player:last_col()), sum, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  relocate(season, referee, n_game, n_player) %>% 
  .add_ratio_foul_tackle_col() %>% 
  mutate(
    across(c(n_card_y:n_pk), ~11 * 90 * .x / mp)
  ) %>% 
  arrange(desc(n_game))
agg

# agg %>% arrange(desc(n_card_r))
# agg %>% count(country)
# agg %>% count(tier)

agg %>% skimr::skim()
agg_filt <-
  agg %>% 
  select(-n_tackle_won) %>% 
  filter(n_game >= 5L)
agg_filt

stat_labs <-
  tibble(
    stat = c('n_card_y', 'n_card_r', 'n_offside', 'n_foul', 'n_pk', 'ratio_foul_tackle'),
    stat_lab = c('Yellow Cards Per 90', 'Red Cards Per 90', 'Offsides Per 90', 'Fouls Per 90', 'PKs Per 90', 'Fouls Per Successful Tackle')
  ) %>% 
  mutate(idx_stat = row_number()) %>% 
  mutate(across(stat_lab, ~fct_reorder(.x, idx_stat)))
stat_labs

pal <- scales::hue_pal()(5)
league_logos <-
  tibble(
    country = countries_big5,
    # path_logo = file.path(dir_proj, sprintf('%s-150px.png', c('epl', 'la-liga', 'ligue-1', 'bundesliga', 'serie-a'))),
    path_logo = file.path(dir_proj, sprintf('%s.png', c('epl-150px', 'la-liga-150px', 'ligue-1', 'bundesliga', 'serie-a'))),
    idx_logo = c(1L, 2L, 4L, 3L, 5L),
    color = c(pal[5], pal[2], pal[3], pal[1], pal[4])
  ) %>% 
  mutate(img = glue::glue("<img src={path_logo} width='40' height='40'/>")) %>% 
  # mutate(across(c(path_logo, img), ~fct_reorder(.x, idx_logo)))
  arrange(idx_logo)
league_logos
# league_logos$path_logo %>% map_lgl(file.exists)

agg_filt_long <-
  agg_filt %>% 
  pivot_longer(
    n_card_y:last_col(),
    names_to = 'stat',
    values_to = 'value'
  ) %>% 
  # filter(value > 0)
  left_join(stat_labs, by = 'stat') %>% 
  left_join(league_logos, by = 'country') %>% 
  mutate(grp = tidytext::reorder_within(country, value, stat_lab))

grps <-
  agg_filt_long %>% 
  distinct(grp) %>% 
  mutate(
    country = grp %>% str_sub(1, 3)
  ) %>% 
  left_join(league_logos) %>% 
  select(grp, img, color)
grps

viz <-
  agg_filt_long %>% 
  ggplot() +
  aes(x = value, y = grp, group = grp, color = country) +
  facet_wrap(~stat_lab, scales = 'free') +
  ggbeeswarm::geom_quasirandom(groupOnX = FALSE) +
  scale_y_discrete(name = '', labels = grps %>% select(grp, img) %>% deframe()) +
  scale_color_manual(values = league_logos %>% select(country, color) %>% deframe()) +
  guides(color = FALSE) +
  theme(
    axis.text.y = ggtext::element_markdown(size = 10.5),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = 'top'
  ) +
  labs(
    title = 'Referee Activity, Big 5 Leagues',
    subtitle = 'In which leagues are referees more involved in the action?',
    caption = 'Each dot represents a single referee in a single season for 2019/20 and 2020/21.<br/>Minimum 5 appearances in a given season.',
    tag = '**Viz**: Tony ElHabr<br/>**Data**: fbref',
    x = NULL,
    y = NULL
  )
# viz

# h <- 10
ggsave(
  plot = viz,
  filename = file.path(dir_proj, 'viz_big5_ref_p90.png'),
  height = 10,
  width = 12,
  type = 'cairo'
)

# La Liga and EPL titans face off today. Given how each league's referees call the game differently (EPL refs seem to allow more pHySiCaLiTy), one side is bound to be frustrated by the end of the match.
