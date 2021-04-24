
library(tidyverse)
dir_proj <- '25-202021_soccer_refs'
dir_data <- file.path(dir_proj, 'data')
path_stats <- file.path(dir_data, 'stats.rds')
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
    idx_stats, season, team_home, team_away, home_x_g, away_x_g, date, team, player,
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
  # filter(idx_stats == 10204L) %>% 
  inner_join(
    results %>%
      mutate(idx_results = row_number()) %>% 
      select(idx_results, wk, date, team_home = home, team_away = away, referee)
  ) %>% 
  select(
    idx_stats,
    idx_results,
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

misc_w_refs_agg_by_game_team <-
  misc_w_refs %>% 
  group_by(season, referee, date, team_home, team_away, team) %>% 
  summarize(
    n_player = n_distinct(player),
    across(
      c(mp:pk_conceded),
      sum
    )
  ) %>% 
  ungroup()
misc_w_refs_agg_by_game_team

misc_w_refs_agg_by_game <-
  misc_w_refs_agg_by_game_team %>% 
  group_by(season, referee, date, team_home, team_away) %>% 
  summarize(
    across(
      c(n_player:pk_conceded),
      sum
    )
  ) %>% 
  ungroup()
misc_w_refs_agg_by_game

misc_w_refs_agg <-
  misc_w_refs_agg_by_game %>% 
  group_by(season, referee) %>% 
  summarize(
    n_game = sum(mp != 0),
    across(c(n_player:pk_conceded), sum)
  ) %>% 
  ungroup() %>% 
  relocate(season, referee, n_game, n_player) %>% 
  mutate(
    across(c(card_y:pk_conceded), ~11 * 90 * .x / mp)
  ) %>% 
  arrange(desc(n_game))
misc_w_refs_agg
misc_w_refs_agg %>% 
  arrange(desc(card_r))

misc_w_refs_agg %>% 
  filter(n_game >= 10) %>% 
  skimr::skim()

misc_w_refs_agg %>% 
  filter(n_game >= 10) %>% 
  filter(n_foul_drawn > n_foul_committed)

misc_w_refs_agg %>% 
  filter(n_game >= 10) %>% 
  mutate(foul_ratio = n_foul_committed / n_tackle_won) %>% 
  arrange(desc(foul_ratio))
