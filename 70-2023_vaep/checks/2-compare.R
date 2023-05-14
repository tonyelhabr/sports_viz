library(arrow)
library(qs)

library(dplyr)
library(stringr)
library(lubridate)

input_data_dir <- '../socceraction-streamlined/data/final'
proj_dir <- '70-2023_vaep/checks'
spadl_games <- arrow::read_parquet(file.path(input_data_dir, 'games.parquet'))
# spadl_teams <- arrow::read_parquet(file.path(input_data_dir, 'teams.parquet'))
spadl_teams <- arrow::read_parquet('../socceraction-streamlined/data/processed/8/2023/teams.parquet')
fotmob_shots <- qs::qread(file.path(proj_dir, 'fotmob_shots.qs')) |>
  filter(situation != 'FreeKick') |> 
  # mutate(
  #   minutes = min + coalesce(min_added, 0L)
  # ) |> 
  group_by(match_id) |> 
  mutate(
    shot_idx = row_number()
  ) |> 
  ungroup()

team_mapping <- bind_cols(
  fotmob_shots |> 
    distinct(team_id = home_team_id, team = home_team) |> 
    arrange(str_remove(team, '^AFC ')) |> 
    rename_all(~sprintf('fotmob_%s', .x)),
  spadl_teams |> 
    arrange(team = team_name) |> 
    rename_all(~sprintf('spadl_%s', .x))
)
VAEP_COLS <- c(
  'pred_scores',
  'ovaep',
  'vaep',
  'pred_scores_atomic',
  'ovaep_atomic',
  'vaep_atomic'
)

spadl_shots <- arrow::read_parquet(file.path(input_data_dir, 'all_vaep.parquet')) |> 
  filter(
    # !is.na(action_id), ## non-atomic
    (
      lead(type_name) %in% c('shot', 'goal') |
        type_name %in% c('shot', 'goal') 
    ) &
      season_id == 2023
  ) |> 
  left_join(
    team_mapping |> 
      select(
        fotmob_home_team_id = fotmob_team_id,
        spadl_team_id
      ),
    by = join_by(home_team_id == spadl_team_id)
  ) |> 
  left_join(
    team_mapping |> 
      select(
        fotmob_away_team_id = fotmob_team_id,
        spadl_team_id
      ),
    by = join_by(away_team_id == spadl_team_id)
  ) |> 
  mutate(
    # minutes = (period_id - 1L) * 45L + ceiling(time_seconds / 60L),
    season = sprintf('%s/%s', season_id - 1, substr(season_id, 3, 4)),
    game_date = date(game_date)
  ) |> 
  select(
    season,
    game_date,
    home_team_id = fotmob_home_team_id,
    away_team_id = fotmob_away_team_id,
    action_id,
    atomic_action_id,
    period = period_id,
    time_seconds,
    team_id,
    player_id,
    type_name,
    bodypart_name,
    player_name,
    result_name,
    all_of(VAEP_COLS)
  )

n_shots_by_game_compared <- full_join(
  fotmob_shots |> 
    count(home_team_id, away_team_id, game_date, name = 'n_shots_fotmob'),
  spadl_shots |> 
    filter(!is.na(action_id), type_name == 'shot')  |> 
    count(home_team_id, away_team_id, game_date, name = 'n_shots_spadl'),
  by = join_by(home_team_id, away_team_id, game_date)
) |>
  arrange(game_date, home_team_id)

n_shots_by_game_compared |> 
  count(n_shots_fotmob == n_shots_spadl)

non_atomic_compare <- inner_join(
  fotmob_shots,
  spadl_shots |> 
    filter(!is.na(action_id)) |> 
    group_by(game_date, home_team_id) |> 
    mutate(
      across(
        pred_scores,
        list(lag = ~lag(.x)),
        .names = 'prev_{.col}'
      )
    ) |> 
    ungroup() |> 
    filter(type_name == 'shot') |> 
    group_by(game_date, home_team_id) |> 
    mutate(
      shot_idx = row_number()
    ) |> 
    ungroup() |> 
    select(
      season,
      game_date,
      home_team_id,
      away_team_id,
      shot_idx,
      all_of(VAEP_COLS),
      starts_with('prev_')
    ),
  by = join_by(season, game_date, shot_idx, home_team_id, away_team_id)
)

library(ggplot2)
non_atomic_compare |> 
  ggplot() +
  aes(
    x = g - xg,
    y = ovaep
  ) +
  geom_point(
    aes(
      color = factor(g)
    )
  )

non_atomic_compare |> 
  ggplot() +
  aes(
    x = xg,
    y = ovaep
  ) +
  geom_point(
    aes(
      color = factor(g)
    )
  )

non_atomic_compare |> 
  ggplot() +
  aes(
    x = xg,
    y = pred_scores
  ) +
  geom_point(
    aes(
      color = factor(g)
    )
  )

non_atomic_compare |> 
  ggplot() +
  aes(
    x = xg,
    y = pred_scores - prev_pred_scores
  ) +
  geom_point(
    aes(
      color = factor(g)
    )
  )

non_atomic_compare |> 
  ggplot() +
  aes(
    x = xg,
    y = prev_pred_scores
  ) +
  geom_point(
    aes(
      color = factor(g)
    )
  )

non_atomic_compare |> 
  transmute(
    g,
    xg,
    g_minus_xg = g,
    pred_scores,
    prev_pred_scores,
    diff_pred_score = pred_scores - prev_pred_scores,
    vaep
  ) |> 
  corrr::correlate() |> 
  autoplot()
