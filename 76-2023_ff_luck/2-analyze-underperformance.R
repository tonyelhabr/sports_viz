library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

PROJ_DIR <- '76-2023_ff_luck'
DATA_DIR <- file.path(PROJ_DIR, 'data')
PLOTS_DIR <- file.path(PROJ_DIR, 'plots')

## data ----
franchises <- readr::read_csv(file.path(DATA_DIR, 'franchises-all.csv'))
team_scores <- readr::read_csv(file.path(DATA_DIR, 'team-scores-all.csv'))
SEASONS <- unique(team_scores$season)
CURRENT_SEASON <- max(SEASONS)

clean_team_scores <- team_scores |> 
  dplyr::filter(!is.na(result)) |> 
  dplyr::group_by(season) |> 
  dplyr::mutate(
    max_week = max(week)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    n_games = ifelse(season < CURRENT_SEASON & week >= (max_week - 1), 2L, 1L)
  )

median_scoring <- clean_team_scores |> 
  group_by(season, week) |> 
  mutate(
    median_score = median(user_score)
  ) |> 
  ungroup() |> 
  mutate(
    median_result = ifelse(user_score > median_score, 'W', 'L')
  ) |> 
  filter(week <= max_week) |> 
  group_by(season, user_name) |> 
  summarize(
    reg_wins = sum(result == 'W'),
    median_wins = sum(median_result == 'W')
  ) |> 
  ungroup() |> 
  mutate(
    total_wins = reg_wins + median_wins
  ) |> 
  group_by(season) |> 
  mutate(
    reg_placing = row_number(desc(reg_wins)),
    placing = row_number(desc(total_wins))
  ) |> 
  ungroup() |> 
  arrange(season, reg_placing)

median_scoring |> 
  filter(season == 2024)

## data processing ----
league_avg_projected_scores <- clean_team_scores |> 
  group_by(season) |> 
  summarize(
    mean_d = mean((user_score - user_projected_score) / n_games, na.rm = TRUE),
    sd_d = sd((user_score - user_projected_score) / n_games, na.rm = TRUE),
    n_games = sum(n_games),
    across(
      matches('user.*score'),
      \(.x) sum(.x, na.rm = TRUE)
    )
  ) |> 
  ungroup() |> 
  mutate(
    user_d = user_score - user_projected_score,
    user_d_per_game = user_d / n_games
  )

agg_scores <- clean_team_scores |> 
  group_by(season, user_name) |> 
  summarize(
    n_games = sum(n_games),
    across(
      ends_with('score'),
      \(.x) sum(.x, na.rm = TRUE)
    )
  ) |> 
  ungroup() |> 
  mutate(
    user_d = user_score - user_projected_score,
    opponent_d = opponent_score - opponent_projected_score,
    user_d_per_game = user_d / n_games,
    opponent_d_per_game = opponent_d / n_games
  )

MAX_WEEK <- clean_team_scores |> 
  filter(season == CURRENT_SEASON) |> 
  pull(week) |> 
  max()

tb_projected_scores_underperformance <- agg_scores |> 
  filter(season == CURRENT_SEASON) |> 
  mutate(
    total_d_per_game = user_d_per_game - opponent_d_per_game,
    rnk = row_number(user_d_per_game - opponent_d_per_game)
  ) |> 
  inner_join(
    franchises |> 
      select(
        season,
        user_name,
        logo
      ),
    by = join_by(season, user_name)
  ) |> 
  arrange(rnk) |> 
  select(
    logo,
    user_name,
    user_d_per_game,
    opponent_d_per_game,
    total_d_per_game
  ) |> 
  gt::gt() |> 
  gt::cols_label(
    'logo' = '',
    'user_name' = 'Player',
    'user_d_per_game' = 'Team',
    'opponent_d_per_game' = 'Opponent',
    'total_d_per_game' = gt::html('Team -<br/>Opponent')
  ) |> 
  gtExtras::gt_theme_538() |> 
  gt::fmt_image(
    columns = logo
  ) |> 
  gt::tab_spanner(
    columns = ends_with('per_game'),
    label = 'Avg. Score Above Projected'
  ) |> 
  gt::fmt_number(
    columns = ends_with('per_game'),
    decimals = 1
  ) |> 
  gt::tab_header(
    title = gt::md('Under-performance compared to ESPN projected scores'),
    subtitle = gt::md(
      sprintf(
        'Through week %s, %s season', 
        MAX_WEEK, 
        CURRENT_SEASON
      )
    )
  )

gt::gtsave(
  tb_projected_scores_underperformance,
  filename = file.path(PLOTS_DIR, sprintf('%s-projected-scores-underperformance.png', format(Sys.Date(), '%Y-%m-%d'))),
  zoom = 1.5
)

adj_agg_scores <- agg_scores |> 
  left_join(
    league_avg_projected_scores |> 
      select(
        season,
        league_avg_d_per_game = user_d_per_game,
        league_avg_d = mean_d,
        league_avg_sd = sd_d
      ),
    by = join_by(season)
  ) |> 
  mutate(
    z_user_d_per_game = (user_d_per_game - league_avg_d) / league_avg_sd,
    z_opponent_d_per_game = (opponent_d_per_game - league_avg_d) / league_avg_sd,
    total_z_d_per_game = z_user_d_per_game - z_opponent_d_per_game
  )

## Most underperforming individual
adj_agg_scores |> 
  arrange(z_user_d_per_game) |> 
  select(
    season,
    user_name,
    user_score,
    user_projected_score,
    user_d_per_game,
    league_avg_d_per_game,
    league_avg_d,
    league_avg_sd,
    z_user_d_per_game
  )

## Most overperforming individual
adj_agg_scores |> 
  arrange(desc(z_user_d_per_game)) |> 
  select(
    season,
    user_name,
    user_score,
    user_projected_score,
    user_d_per_game,
    league_avg_d_per_game,
    league_avg_d,
    league_avg_sd,
    z_user_d_per_game
  )

## Most underperforming + unluckiest
adj_agg_scores |> 
  arrange(total_z_d_per_game) |> 
  select(
    season,
    user_name,
    user_score,
    user_projected_score,
    user_d_per_game,
    # league_avg_d_per_game,
    z_user_d_per_game,
    z_opponent_d_per_game,
    total_z_d_per_game
  )

## Most overperforming + luckiest
adj_agg_scores |> 
  arrange(desc(total_z_d_per_game)) |> 
  select(
    season,
    user_name,
    user_score,
    user_projected_score,
    user_d_per_game,
    # league_avg_d_per_game,
    z_user_d_per_game,
    z_opponent_d_per_game,
    total_z_d_per_game
  )

