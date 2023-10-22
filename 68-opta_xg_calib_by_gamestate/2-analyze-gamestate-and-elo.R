library(dplyr)
library(qs)
library(purrr)

PROJ_DIR <- '68-opta_xg_calib_by_gamestate'
raw_shots <- qs::qread(file.path(PROJ_DIR, 'shots.qs')) |> 
  dplyr::filter(
    pov == 'primary',
    # season %in% c('2020/21', '2021/22', '2022/23'),
    !is_own_goal
  )
clubelo_ratings <- qs::qread(file.path(PROJ_DIR, 'clubelo-ratings.qs'))

## uncalibrated xG calibration ----
match_teams <- raw_shots |> 
  dplyr::distinct(
    match_id,
    date,
    home_team = ifelse(is_home, team, opponent),
    away_team = ifelse(is_home, opponent, team)
  ) |>
  dplyr::left_join(
    clubelo_ratings |> 
      dplyr::select(
        date,
        home_team = team,
        home_elo = elo
      ),
    by = dplyr::join_by(date, home_team)
  ) |>
  dplyr::left_join(
    clubelo_ratings |> 
      dplyr::select(
        date,
        away_team = team,
        away_elo = elo
      ),
    by = dplyr::join_by(date, away_team)
  )

ORDERED_GAME_STATE_LABELS <- c('trailing', 'neutral', 'leading')
# ORDERED_GAME_STATE_LABELS <- c('<-1', '-1', 'neutral', '+1', '>+1')
shots <- raw_shots |> 
  dplyr::inner_join(
    match_teams,
    by = dplyr::join_by(date, match_id)
  ) |> 
  dplyr::transmute(
    match_id,
    season,
    date,
    home_team,
    away_team,
    home_elo,
    away_elo,
    team,
    player,
    shot_id,
    period,
    min,
    min_added,
    is_penalty,
    is_goal,
    g,
    g_conceded,
    g_cumu,
    g_conceded_cumu,
    # .pred_yes = xg,
    # .pred_no = 1 - xg,
    dplyr::across(xg, \(.x) ifelse(.x <= 0, 0.01, .x)),
    raw_game_state = game_state,
    game_state = cut(
      game_state,
      breaks = c(-Inf, -1, 0, Inf), 
      # breaks = c(-Inf, -2, 1, Inf), 
      labels = ORDERED_GAME_STATE_LABELS
    ),
    elo = ifelse(team == home_team, home_elo, away_elo),
    elo_d = elo - ifelse(team == home_team, away_elo, home_elo)
  ) |> 
  dplyr::group_by(match_id) |> 
  dplyr::arrange(shot_id, team, .by_group = TRUE) |> 
  dplyr::mutate(
    raw_pre_shot_game_state = dplyr::lag(raw_game_state, default = 0L),
    pre_shot_game_state = dplyr::lag(game_state, default = ORDERED_GAME_STATE_LABELS[2])
  ) |> 
  dplyr::ungroup()
shots |> filter(match_id == '0014076a', team == 'Arsenal')

glm_model <- glm(
  is_goal ~ xg + pre_shot_game_state + elo_d + elo:elo_d, # + xg:elo_d + elo:elo_d,
  data = shots,
  # offset = shots$xg
  family = binomial(link = 'logit')
)
# glm_model
broom::tidy(glm_model)

cal_shots <- shots
cal_shots$.pred_glm <- predict(
  glm_model,
  data = cal_shots,
  type = 'response'
)


library(ggplot2)
cal_shots |> 
  ggplot() +
  aes(
    x = xg,
    y = .pred_glm,
    color = elo_d
  ) +
  scale_color_viridis_c(
    option = 'H'
  ) +
  geom_point() +
  geom_abline(linetype = 2) +
  facet_wrap(~pre_shot_game_state)

cal_shots |> 
  ggplot() +
  aes(
    x = .pred_glm,
    y = xg - .pred_glm,
    color = elo_d
  ) +
  scale_color_viridis_c(
    option = 'H'
  ) +
  geom_point() +
  geom_hline(
    aes(
      yintercept = 0
    ),
    linetype = 2
  )

yardstick::roc_auc_vec(
  truth = cal_shots$is_goal,
  estimate = cal_shots$.pred_glm
)

yardstick::roc_auc_vec(
  truth = cal_shots$is_goal,
  estimate = cal_shots$xg
)

