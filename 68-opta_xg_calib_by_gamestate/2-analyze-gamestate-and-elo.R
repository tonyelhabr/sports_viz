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

elo_quantiles <- quantile(
  c(match_teams$home_elo, match_teams$away_elo), 
  c(0.25, 0.75), 
  na.rm = TRUE
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
    across(
      distance,
      ~cut(
        .x,
        breaks = c(seq(0, 18, by = 2), 20, 25, 30, 35, Inf)
      )
    ),
    body_part,
    # sca1,
    # sca2,
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
shots |> dplyr::filter(match_id == '0014076a', team == 'Arsenal')

np_shots <- dplyr::filter(shots, !is_penalty)

np_shots_std <- np_shots |> 
  dplyr::mutate(
    elo_std = scales::rescale(
      elo,
      from = c(unname(elo_quantiles)),
      to = c(-1, 1)
    )
  )
base_model <- glm(
  is_goal ~ xg, #  + elo_d + xg:elo_d + elo:elo_d,
  data = np_shots,
  # offset = np_shots$xg,
  family = binomial(link = 'logit')
)
broom::tidy(base_model)
calibrator <- probably::cal_estimate_isotonic(
  np_shots |> mutate(.pred_yes = xg, .pred_no = 1 - xg),
  truth = is_goal,
  estimate = dplyr::starts_with('.pred')
)

new_xg <- probably::cal_apply(
  np_shots |> mutate(.pred_yes = xg, .pred_no = 1 - xg),
  calibrator
)

robust_model <- glm(
  is_goal ~ 0 + distance + body_part + pre_shot_game_state + elo_d + elo:elo_d,
  data = np_shots,
  offset = np_shots$xg,
  family = binomial(link = 'logit')
)
broom::tidy(robust_model)

# glm_model <- glm(
#   is_goal ~ xg + pre_shot_game_state + elo_d + elo:elo_d, # + xg:elo_d + elo:elo_d,
#   data = shots,
#   # offset = shots$xg
#   family = binomial(link = 'logit')
# )
# # glm_model
# broom::tidy(glm_model)

cal_shots <- np_shots
cal_shots$.pred_glm <- predict(
  base_model,
  # newdata = cal_shots,
  type = 'response'
)
hist(cal_shots$.pred_glm)


library(ggplot2)
new_xg |> 
  ggplot() +
  aes(
    x = xg,
    y = .pred_yes,
    color = elo_d
  ) +
  scale_color_viridis_c(
    option = 'H'
  ) +
  geom_point() +
  geom_abline(linetype = 2)

new_xg |> 
  ggplot() +
  aes(
    x = xg,
    y = xg - .pred_yes,
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
  geom_abline(linetype = 2)


