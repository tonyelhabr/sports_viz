
library(tidyverse)
dir_proj <- '25-202021_soccer_refs'
path_results <- file.path(dir_proj, 'results.rds')
path_stats <- file.path(dir_proj, 'stats.rds')
results <- path_results %>% read_rds()
stats <- path_stats %>% read_rds()

# teams_stats <- stats %>% count(team = home_team, name = 'n_stats')
# teams_stats
# results_stats <- results %>% count(team = home, name = 'n_results')
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
  data %>% mutate(ratio_foul_tackle = n_foul_committed / n_tackle_won)
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
    idx_stats, country, tier, gender, season, team_home, team_away, home_x_g, away_x_g, date, team, player,
    mp = min,
    card_y = crd_y,
    card_r = crd_r,
    card_y2 = x2crd_y,
    n_foul_committed = fls,
    n_foul_drawn = fld,
    n_offside = off,
    n_tackle_won = tkl_w,
    pk_won = p_kwon,
    pk_conceded = p_kcon
  ) %>% 
  select(-pk_won) %>% 
  rename()
  # filter(idx_stats == 10204L) %>% 
  inner_join(
    results %>%
      mutate(idx_results = row_number()) %>% 
      select(idx_results, wk, date, team_home = home, team_away = away, referee)
  ) %>% 
  .add_ratio_foul_tackle_col() %>% 
  select(
    idx_stats,
    idx_results,
    country,
    tier,
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

agg_by_game_team <-
  misc_w_refs %>% 
  group_by(season, country, tier, gender, referee, date, team_home, team_away, team) %>% 
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
  group_by(season, country, tier, gender, referee, date, team_home, team_away) %>% 
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
  group_by(season, country, tier, gender, referee) %>% 
  summarize(
    n_game = sum(mp != 0),
    n_player = .sum(n_player),
    mp = .sum(mp),
    card_y = .sum(card_y),
    card_r = .sum(card_r),
    card_y2 = .sum(card_y2),
    n_foul_committed = .sum(n_foul_committed),
    n_foul_drawn = .sum(n_foul_drawn),
    n_offside = .sum(n_offside),
    n_tackle_won = .sum(n_tackle_won),
    pk_won = .sum(pk_won),
    pk_conceded = .sum(pk_conceded),
    # across(c(n_player:last_col()), sum, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  .add_ratio_foul_tackle_col() %>% 
  relocate(season, referee, n_game, n_player) %>% 
  mutate(
    across(c(card_y:pk_conceded), ~11 * 90 * .x / mp)
  ) %>% 
  arrange(desc(n_game))
agg

agg %>% arrange(desc(card_r))
agg %>% count(country)
agg %>% count(tier)

agg %>% skimr::skim()
agg_filt <-
  agg %>% 
  filter(n_game >= 5L) %>% 
  select(-c(card_y2, n_foul_drawn, n_tackle_won, pk_won)) %>% 
  rename(n_pk = pk_conceded)
app_filt

agg_filt_long <-
  agg_filt %>% 
  pivot_longer(card_y:last_col()) # %>% 
  # filter(value > 0)
app_filt_long

agg_filt_long %>% 
  filter(value > 0) %>% 
  ggplot() +
  aes(x = value, group = country, color = country) +
  geom_density() +
  facet_wrap(~name, scales = 'free')

agg_filt_long %>% 
  ggplot() +
  aes(x = value, y = country, group = country, color = country) +
  ggbeeswarm::geom_quasirandom(groupOnX = FALSE) +
  facet_wrap(~name, scales = 'free') +
  theme(legend.position = 'top')

agg_filt_long %>% 
  ggplot() +
  aes(x = value, y = tidytext::reorder_within(country, value, name), group = country, color = country) +
  ggbeeswarm::geom_quasirandom(groupOnX = FALSE) +
  tidytext::scale_y_reordered() +
  facet_wrap(~name, scales = 'free') +
  theme(legend.position = 'top')

agg %>% 
  pivot_longer(card_y:last_col()) %>% 
  ggplot() +
  aes(x = value, group = country, color = country) +
  geom_density() +
  facet_wrap(~name, scales = 'free')

agg %>% 
  filter(n_game >= 10) %>% 
  skimr::skim()

agg %>% 
  filter(n_game >= 10) %>% 
  filter(n_foul_drawn > n_foul_committed)

agg %>% 
  filter(n_game >= 10) %>% 
  mutate(foul_ratio = n_foul_committed / n_tackle_won) %>% 
  arrange(desc(foul_ratio))
