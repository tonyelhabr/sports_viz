library(dplyr)
library(qs)
library(purrr)

PROJ_DIR <- '68-opta_xg_calib_by_gamestate'
raw_shots <- qs::qread(file.path(PROJ_DIR, 'shots.qs')) |> 
  dplyr::filter(
    pov == 'primary',
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

# library(ggplot2)
# match_teams |> 
#   ggplot() +
#   aes(
#     x = home_elo - away_elo,
#     binwidth = 10
#   ) +
#   geom_histogram()
# match_teams |> 
#   ggplot() +
#   aes(
#     x = home_elo,
#     binwidth = 10
#   ) +
#   geom_histogram()

shots <- raw_shots |> 
  dplyr::inner_join(
    match_teams,
    by = dplyr::join_by(match_id, date)
  ) |> 
  dplyr::transmute(
    match_id,
    season,
    date,
    home_team,
    away_team,
    home_elo,
    away_elo,
    home_elo_std = scales::rescale(
      home_elo,
      from = c(unname(elo_quantiles)),
      to = c(-1, 1)
    ),
    away_elo_std = scales::rescale(
      away_elo,
      from = c(unname(elo_quantiles)),
      to = c(-1, 1)
    ),
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
    dplyr::across(xg, \(.x) ifelse(.x <= 0, 0.01, .x)),
    elo = ifelse(team == home_team, home_elo, away_elo),
    elo_std = ifelse(team == home_team, home_elo_std, away_elo_std),
    elo_d = elo - ifelse(team == home_team, away_elo, home_elo),
    elo_d_std = elo_std - ifelse(team == home_team, away_elo_std, home_elo_std),
    better_elo = ifelse(
      (team == home_team & home_elo > away_elo) |
        (team == away_team & home_elo < away_elo),
      'yes',
      'no'
    ) |> factor(levels = c('no', 'yes'))
  )
shots |> filter(match_id == '0014076a', team == 'Arsenal')

np_shots <- dplyr::filter(shots, !is_penalty)

glm_model <- glm(
  is_goal ~ 0 + xg:better_elo + better_elo, # + xg:elo_d + elo:elo_d,
  data = np_shots,
  family = binomial(link = 'logit'),
  offset = np_shots$xg
)
glm_model
broom::tidy(glm_model)

cal_shots <- np_shots
cal_shots$.pred_yes<- predict(
  glm_model,
  data = cal_shots,
  type = 'response'
)
# hist(cal_shots$elo_std)
# hist(cal_shots$elo_d_std)
hist(cal_shots$.pred_yes)

# library(betareg)
## doesn't really make sense to do betaregression if we don't have access to underlying features
# beta_model <- betareg(
#   xg ~ elo*elo_d,
#   data = shots
# )
# 
# cal_shots$.pred_beta <- predict(
#   beta_model,
#   data = shots
# )

library(ggplot2)
cal_shots |> 
  ggplot() +
  aes(
    x = xg,
    y = .pred_yes,
    color = better_elo
  ) +
  geom_point() +
  geom_abline(linetype = 2)

cal_shots |> 
  ggplot() +
  aes(
    x = .pred_yes,
    y = xg - .pred_yes,
    color = better_elo
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
  estimate = cal_shots$.pred_yes
)

yardstick::roc_auc_vec(
  truth = cal_shots$is_goal,
  estimate = cal_shots$xg
)
