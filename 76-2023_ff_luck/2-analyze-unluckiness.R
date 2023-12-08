library(readr)
library(dplyr)
library(tidyr)

library(gt)
library(gtExtras)

library(ggforce)
library(ggrepel)

PROJ_DIR <- '76-2023_ff_luck'
DATA_DIR <- file.path(PROJ_DIR, 'data')

## data ----
scores <- readr::read_csv(file.path(DATA_DIR, 'team-scores-all.csv'))
SEASONS <- unique(scores$season)
CURRENT_SEASON <- max(SEASONS)

clean_scores <- scores |> 
  dplyr::filter(!is.na(result)) |> 
  dplyr::mutate(
    dplyr::across(
      c(
        user_name,
        opponent_user_name
      ),
      \(.x) 
      dplyr::case_when(
        .x == 'Andrew ElHabr' ~ 'Andrew E.',
        .x == 'Andrew Lara' ~ 'Andrew L.',
        .x == 'Manuel Espinosa' ~ 'Manny',
        TRUE ~ gsub('\\s.*$', '', .x)
      )
    )
  ) |> 
  dplyr::group_by(season) |> 
  dplyr::mutate(
    max_week = max(week)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    n_games = ifelse(season < CURRENT_SEASON & week >= (max_week - 1), 2L, 1L)
  ) |> 
  dplyr::select(-max_week)

## data processing ----
league_avg_projected_scores <- clean_scores |> 
  group_by(season) |> 
  summarize(
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

clean_scores |> 
  ggplot() +
  aes(
    x = user_score - user_projected_score
  ) +
  geom_histogram() +
  facet_wrap(~season)

agg_scores <- clean_scores |> 
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
  ) |> 
  left_join(
    league_avg_projected_scores |> 
      select(
        season,
        league_avg_d_per_game = user_d_per_game
      ),
    by = join_by(season)
  ) |> 
  mutate(
    adj_user_d_per_game = user_d_per_game - league_avg_d_per_game,
    adj_opponent_d_per_game = opponent_d_per_game - league_avg_d_per_game
  )

agg_scores |> 
  arrange(adj_user_d_per_game) |> 
  select(
    season,
    user_name,
    user_score,
    user_projected_score,
    user_d_per_game,
    league_avg_d_per_game,
    adj_user_d_per_game
  )

agg_scores |> 
  arrange(desc(adj_user_d_per_game)) |> 
  select(
    season,
    user_name,
    user_score,
    user_projected_score,
    user_d_per_game,
    league_avg_d_per_game,
    adj_user_d_per_game
  )

agg_scores |> 
  arrange(opponent_d_per_game)
agg_scores |> 
  arrange(desc(opponent_d_per_game))

