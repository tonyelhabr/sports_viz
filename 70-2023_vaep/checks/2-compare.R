library(arrow)
library(qs)

library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

library(tidyr)
library(purrr)
library(corrr)

input_data_dir <- '../socceraction-streamlined/data/final'
proj_dir <- '70-2023_vaep/checks'
spadl_games <- arrow::read_parquet(file.path(input_data_dir, 'games.parquet'))
spadl_teams <- arrow::read_parquet('../socceraction-streamlined/data/processed/8/2023/teams.parquet')
fotmob_shots <- qs::qread(file.path(proj_dir, 'fotmob_shots.qs')) |>
  filter(situation != 'FreeKick') |> 
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
  'vaep'
)
all_vaep <- arrow::read_parquet(file.path(input_data_dir, 'all_vaep.parquet')) |> 
  filter(
    season_id == 2023
  )

all_spadl_shots <- all_vaep |> filter(type_name == 'shot')
process_spadl <- function(df, ...) {
  df |> 
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
      game_date = date(game_date),
      goal = ifelse(type_name == 'shot' & result_name == 'success', 'Goal', 'No Goal')
    ) |> 
    select(
      season,
      game_date,
      game_id,
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
      goal,
      ...,
      all_of(VAEP_COLS)
    )
}

spadl_shots <- process_spadl(all_spadl_shots)
spadl_post_shots <- spadl_shots |> 
  transmute(
    game_id,
    action_id = action_id + 1L,
    shot_goal = goal
  ) |> 
  left_join(
    all_vaep,
    by = join_by(game_id, action_id)
  ) |> 
  process_spadl(shot_goal) |> 
  select(-goal) |> 
  rename(goal = shot_goal)

spadl_pre_shots <- spadl_shots |> 
  transmute(
    game_id,
    action_id = action_id - 1L,
    shot_goal = goal
  ) |> 
  left_join(
    all_vaep,
    by = join_by(game_id, action_id)
  ) |> 
  process_spadl(shot_goal) |> 
  select(-goal) |> 
  rename(goal = shot_goal)

join_fotmob_shots <- function(df) {
  inner_join(
    fotmob_shots |> 
      mutate(
        g_minus_xg = g - xg
      ),
    df |> 
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
        goal,
        all_of(VAEP_COLS),
        starts_with('prev_')
      ),
    by = join_by(season, game_date, shot_idx, home_team_id, away_team_id)
  )
}

compare_shots <- join_fotmob_shots(spadl_shots)
compare_post_shots <- join_fotmob_shots(spadl_post_shots)
compare_pre_shots <- join_fotmob_shots(spadl_pre_shots)

correlate_vars <- function(df) {
  df |> 
    select(
      goal,
      xg,
      g_minus_xg,
      pred_scores,
      vaep
    ) |>
    nest(data = -c(goal)) |> 
    mutate(
      data = map(data, corrr::correlate)
    ) |> 
    unnest(data) |> 
    rename(term1 = term) |> 
    pivot_longer(
      -c(goal, term1),
      names_to = 'term2',
      values_to = 'cor'
    ) |> 
    filter(!is.na(cor)) |> 
    filter(term1 %in% c('vaep')) |> 
    mutate(
      across(goal, ~ifelse(.x == 'Goal', 'goal', 'no_goal'))
    ) |> 
    pivot_wider(
      names_from = goal,
      values_from = cor
    )
}

compare_cors <- compare_shots |> correlate_vars()
compare_cors
compare_pre_shots |> correlate_vars()
compare_post_shots |> correlate_vars()

theme_compare <- function(...) {
  list(
    ...,
    geom_hline(
      aes(yintercept = 0)
    ),
    geom_vline(
      aes(xintercept = 0)
    ),
    geom_abline(
      aes(
        slope = 1,
        intercept = 0
      )
    ),
    guides(
      color = guide_legend(
        title = '',
        override.aes = list(size = 3)
      )
    ),
    coord_equal(),
    theme_minimal(base_size = 14),
    theme(
      legend.position = 'top',
      plot.title.position = 'plot',
      plot.background = element_rect(color = 'white', fill = 'white'),
      panel.background = element_rect(color = 'white', fill = 'white')
    ),
    labs(
      subtitle = '2022/23 EPL season (VAEP hold-out set)'
    )
  )
}

gg_compare <- function(...) {
  list(
    ...,
    geom_point(
      aes(
        color = goal
      )
    ),
    theme_compare()
  )
}

compare_shots |>
  ggplot() +
  aes(
    x = xg,
    y = vaep
  ) +
  gg_compare() +
  labs(
    title = 'xG vs. VAEP',
    x = 'xG',
    y = 'VAEP'
  ) +
  geom_text(
    data = compare_cors |> 
      filter(
        term1 == 'vaep',
        term2 == 'xg'
      ),
    color = '#00BFC4',
    size = 14 / .pt,
    fontface = 'bold',
    hjust = 0,
    aes(
      x = 0.01,
      y = 1.1,
      label = paste0('No goal cor.: ', round(no_goal, 2))
    )
  ) +
  geom_text(
    data = compare_cors |> 
      filter(
        term1 == 'vaep',
        term2 == 'xg'
      ),
    color = '#F8766D',
    size = 14 / .pt,
    fontface = 'bold',
    hjust = 0,
    aes(
      x = 0.01,
      y = 1.2,
      label = paste0('Goal cor.: ', round(goal, 2))
    )
  )

ggsave(
  filename = file.path(proj_dir, 'xg_vs_vaep.png'),
  width = 7,
  height = 7,
  units = 'in'
)


compare_post_shots |>
  ggplot() +
  aes(
    x = xg,
    y = vaep
  ) +
  gg_compare() +
  labs(
    title = 'Action after shot',
    x = 'xG',
    y = 'VAEP'
  )

compare_pre_shots |>
  ggplot() +
  aes(
    x = xg,
    y = vaep
  ) +
  gg_compare() +
  labs(
    title = 'Action before shot',
    x = 'xG',
    y = 'VAEP'
  )

compare_shots |>
  ggplot() +
  aes(
    x = g_minus_xg,
    y = vaep
  ) +
  gg_compare() +
  labs(
    title = 'Goal minus xG vs. VAEP',
    x = 'Goal minus xG',
    y = 'VAEP'
  ) +
  geom_text(
    data = compare_cors |> 
      filter(
        term1 == 'vaep',
        term2 == 'g_minus_xg'
      ),
    color = '#00BFC4',
    size = 14 / .pt,
    fontface = 'bold',
    hjust = 0,
    aes(
      x = -0.8,
      y = 0.8,
      label = paste0('No goal cor.: ', round(no_goal, 2))
    )
  ) +
  geom_text(
    data = compare_cors |> 
      filter(
        term1 == 'vaep',
        term2 == 'g_minus_xg'
      ),
    color = '#F8766D',
    size = 14 / .pt,
    fontface = 'bold',
    hjust = 0,
    aes(
      x = -0.8,
      y = 1,
      label = paste0('Goal cor.: ', round(goal, 2))
    )
  )

ggsave(
  filename = file.path(proj_dir, 'g_minus_xg_vs_vaep.png'),
  width = 7,
  height = 7,
  units = 'in'
)

compare_post_shots |>
  ggplot() +
  aes(
    x = g_minus_xg,
    y = vaep
  ) +
  gg_compare() +
  labs(
    title = 'Action after shot',
    x = 'Goal minus xG',
    y = 'VAEP'
  )

compare_pre_shots |>
  ggplot() +
  aes(
    x = g_minus_xg,
    y = vaep
  ) +
  gg_compare() +
  labs(
    title = 'Action before shot',
    x = 'Goal minus xG',
    y = 'VAEP'
  )

