library(dplyr)
library(lubridate)
library(tibble)

library(rsample)
library(recipes)
library(workflows)
library(tune)
library(yardstick)

library(vip)
library(pdp)
library(ggplot2)
library(scales)
library(pdp)
library(furrr)
library(future)

read_parquet_from_url <- function(url) {
  load <- curl::curl_fetch_memory(url)
  arrow::read_parquet(load$content)
}

REPO <- 'tonyelhabr/socceraction-streamlined'
read_socceraction_parquet_release <- function(name, tag) {
  url <- sprintf('https://github.com/%s/releases/download/%s/%s.parquet', REPO, tag, name)
  read_parquet_from_url(url)
}

read_socceraction_parquet_releases <- function(name, tag = 'data-processed') {
  purrr::map_dfr(
    2013:2022,
    \(season_start_year) {
      basename <- sprintf('8-%s-%s', season_start_year, name)
      message(basename)
      read_socceraction_parquet_release(basename, tag = tag)
    }
  )
}

read_socceraction_parquet <- function(name, branch = 'main') {
  url <- sprintf('https://github.com/%s/raw/%s/%s.parquet', REPO, branch, name)
  read_parquet_from_url(url)
}

position_mapping <- list(
  'F' = c('FWL', 'FW', 'FWR'),
  'M' = c('AML', 'ML', 'AMC', 'MC', 'AMR', 'MR'),
  'D' = c('DL', 'DML', 'DC', 'DMC', 'DR', 'DMR'),
  'K' = 'GK'
) |> 
  tibble::enframe('position', 'opta_position') |> 
  tidyr::unnest_longer(opta_position)


x <- read_socceraction_parquet_releases('x')
y <- read_socceraction_parquet_releases('y')
xt <- read_socceraction_parquet_releases('xt')
actions <- read_socceraction_parquet_releases('actions')
players <- read_socceraction_parquet_releases('players')
games <- read_socceraction_parquet_releases('games')
games <- dplyr::mutate(
  games,
  date = lubridate::date(game_date)
)
team_elo <- read_socceraction_parquet('data/final/8/2013-2022/clubelo-ratings')

quantile(team_elo$elo, c(0.25, 0.5, 0.75))
hist(team_elo$elo)

open_play_shots <- games |>
  dplyr::transmute(
    season_id,
    game_id,
    date,
    home_team_id,
    away_team_id
  ) |> 
  dplyr::inner_join(
    x |> 
      dplyr::filter(type_shot_a0 == 1) |> 
      dplyr::select(
        game_id,
        action_id,
        
        ## features
        start_x_a0,
        start_y_a0,
        start_dist_to_goal_a0,
        start_angle_to_goal_a0,
        type_dribble_a1,
        type_pass_a1,
        type_cross_a1,
        type_corner_crossed_a1,
        type_shot_a1,
        type_freekick_crossed_a1,
        bodypart_foot_a0,
        bodypart_head_a0,
        bodypart_other_a0
      ) |> 
      dplyr::mutate(
        dplyr::across(-c(game_id, action_id), as.integer)
      ),
    by = dplyr::join_by(game_id),
    relationship = 'many-to-many'
  ) |> 
  dplyr::inner_join(
    y |> 
      dplyr::transmute(
        game_id, 
        action_id,
        scores = ifelse(scores, 'yes', 'no') |> factor(levels = c('yes', 'no'))
      ),
    by = dplyr::join_by(game_id, action_id)
  ) |> 
  dplyr::inner_join(
    actions |> 
      dplyr::select(
        game_id,
        action_id,
        team_id,
        player_id
      ),
    by = dplyr::join_by(game_id, action_id)
  ) |> 
  dplyr::left_join(
    xt |> 
      dplyr::select(
        game_id,
        action_id,
        xt
      ),
    by = dplyr::join_by(game_id, action_id)
  ) |> 
  dplyr::inner_join(
    players |> 
      dplyr::select(
        game_id,
        team_id,
        player_id,
        player_name,
        starting_position
      ) |> 
      dplyr::left_join(
        position_mapping,
        by = dplyr::join_by(starting_position == opta_position)
      ) |> 
      dplyr::filter(position != 'K'),
    by = dplyr::join_by(game_id, team_id, player_id)
  ) |> 
  dplyr::group_by(player_id) |> 
  dplyr::arrange(date, action_id, .by_group = TRUE) |> 
  tidyr::fill(position, .direction = 'down') |> 
  dplyr::ungroup() |> 
  dplyr::left_join(
    team_elo |> dplyr::select(date, home_team_id = team_id, home_elo = elo),
    by = dplyr::join_by(date, home_team_id)
  ) |> 
  dplyr::left_join(
    team_elo |> dplyr::select(date, away_team_id = team_id, away_elo = elo),
    by = dplyr::join_by(date, away_team_id)
  ) |> 
  dplyr::transmute(
    date,
    season_id,
    game_id,
    team_id,
    opponent_team_id = ifelse(team_id == home_team_id, away_team_id, home_team_id),
    action_id,
    player_id,
    player_name,
    position,
    set = dplyr::case_when(
      season_id %in% c(2013:2016) ~ 'train',
      season_id %in% c(2017:2019) ~ 'validation',
      season_id %in% c(2020:2022) ~ 'test'
    ),
    
    scores,
    
    xt,
    rolling_xgd = NA_real_, ## TODO
    elo = ifelse(team_id == home_team_id, home_elo, away_elo),
    opponent_elo = ifelse(team_id == home_team_id, away_elo, home_elo),
    elo_diff = elo - opponent_elo,
    
    start_x_a0,
    start_y_a0,
    start_dist_to_goal_a0,
    start_angle_to_goal_a0,
    type_dribble_a1,
    type_pass_a1,
    type_cross_a1,
    type_corner_crossed_a1,
    type_shot_a1,
    type_freekick_crossed_a1,
    bodypart_foot_a0,
    bodypart_head_a0,
    bodypart_other_a0
  )

train <- open_play_shots |> dplyr::filter(set == 'train')
val <- open_play_shots |> dplyr::filter(set == 'validation')
test <- open_play_shots |> dplyr::filter(set == 'test')

## setup base model ----
rec_full <- recipes::recipe(
  scores ~ 
    rolling_xgd +
    elo +
    elo_diff +
    start_x_a0 +
    start_y_a0 +
    start_dist_to_goal_a0 +
    start_angle_to_goal_a0 +
    type_dribble_a1 +
    type_pass_a1 +
    type_cross_a1 +
    type_corner_crossed_a1 +
    type_shot_a1 +
    type_freekick_crossed_a1 +
    bodypart_foot_a0 +
    bodypart_head_a0 +
    bodypart_other_a0,
  data = train
)

rec_elo <- rec_full |> 
  recipes::step_rm(rolling_xgd)

rec_base <- rec_elo |> 
  recipes::step_rm(elo, elo_diff)

spec_base <- parsnip::boost_tree(
  trees = 500,
  learn_rate = 0.01,
  tree_depth = 12,
  min_n = 20, 
  loss_reduction = 0.0009316,
  sample_size = 0.2373513,
  mtry = 11,
  stop_iter = 36
) |>
  parsnip::set_engine('xgboost') |> 
  parsnip::set_mode('classification')

recipe_features <- rec_elo |> prep() |> juice() |> colnames()
sign_lgl <- recipe_features %in% c('elo', 'elo_diff')
constraints <- ifelse(sign_lgl, 1, 0)

spec_elo <- parsnip::boost_tree(
  trees = 500,
  learn_rate = 0.01,
  tree_depth = 13,
  min_n = 31, 
  loss_reduction = 0.0006153,
  sample_size = 0.3222589,
  mtry = 12,
  stop_iter = 47
) |>
  parsnip::set_engine('xgboost', monotone_constraints = !!constraints) |> 
  parsnip::set_mode('classification')

wf_base <- workflows::workflow(
  preprocessor = rec_base,
  spec = spec_base
)

wf_elo <- workflows::workflow(
  preprocessor = rec_elo,
  spec = spec_elo
)

met_set <- yardstick::metric_set(
  fixed_brier_skill_score,
  yardstick::f_meas,
  yardstick::accuracy, 
  yardstick::roc_auc, 
  yardstick::sensitivity
)

val_and_test <- dplyr::bind_rows(val, test)
last_fit_base <- tune::last_fit(
  wf_base,
  split = rsample::make_splits(
    train,
    val_and_test
  ),
  metrics = met_set
)

last_fit_elo <- tune::last_fit(
  wf_elo,
  split = rsample::make_splits(
    train,
    val_and_test
  ),
  metrics = met_set
)

## diagnostics ----
tune::collect_metrics(last_fit_base)
tune::collect_metrics(last_fit_elo)

val_and_test_preds_base |> 
  inner_join(
    val_and_test |> 
      transmute(
        game_id,
        action_id,
        elo,
        elo_bucket = 100 * (elo %/% 100),
        elo_diff,
        elo_diff_bucket = 100 * (elo_diff %/% 100)
      )
  ) |> 
  group_by(elo_bucket, elo_diff_bucket) |> 
  summarize(
    shots = n(),
    g = sum(scores == 'yes'),
    xg = sum(xg_base)
  ) |> 
  ungroup() |> 
  mutate(
    xgd = g - xg
  )

sampled_val_and_test <- val_and_test |> 
  filter(season_id == 2022)

baked_elo_data <- rec_elo |>
  prep() |>
  bake(new_data = sampled_updated_val_and_test) |>
  select(-scores)

elo_model_object <- hardhat::extract_fit_engine(last_fit_elo)
pdp::partial(
  elo_model_object,
  train = baked_elo_data,
  pred.var = 'elo',
  type = 'classification',
  plot = TRUE,
  prob = TRUE,
  trim.outliers = TRUE
)

pdp::partial(
  elo_model_object,
  train = baked_elo_data,
  pred.var = 'elo_diff',
  type = 'classification',
  plot = TRUE,
  prob = TRUE,
  trim.outliers = TRUE
)


extract_n_features_from_last_fit <- function(last_fit) {
  last_fit |>
    dplyr::pull(.workflow) |> 
    purrr::pluck(1) |> 
    workflows::extract_recipe() |> 
    base::summary() |> 
    dplyr::filter(role == 'predictor') |> 
    nrow()
}

plot_var_imp_from_last_importance <- function(last_fit) {
  
  n_features <- extract_n_features_from_last_fit(last_fit)
  last_fit |> 
    workflows::extract_fit_parsnip() |>
    vip::vip(
      geom = 'point',
      include_type = TRUE, 
      num_features = n_features
    ) + 
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(Importance, accuracy = 1)),
      nudge_y = 0.02
    )
}

plot_var_imp_from_last_importance(last_fit_base)
plot_var_imp_from_last_importance(last_fit_elo)

## rolling xgd ----
train_preds <- last_fit_base |> 
  hardhat::extract_workflow() |> 
  broom::augment(train, type = 'prob') |> 
  dplyr::select(
    date, 
    action_id, 
    player_id,
    position,
    scores,
    xg_base = .pred_yes
  )

init_rolling_xgd_per_shot_by_position <- train_preds |> 
  dplyr::group_by(player_id, position) |> 
  dplyr::arrange(date, action_id, .by_group = TRUE) |> 
  dplyr::mutate(
    rn = dplyr::row_number(),
    rolling_g = base::cumsum(as.integer(scores == 'yes'))/ rn,
    rolling_xg = base::cumsum(xg_base) / rn
  ) |> 
  dplyr::ungroup() |> 
  dplyr::transmute(
    date,
    action_id,
    player_id,
    position,
    rn,
    rolling_g,
    rolling_xg,
    rolling_xgd = rolling_g - rolling_xg
  ) |> 
  dplyr::filter(rn <= 10) |> 
  dplyr::group_by(position, rn) |> 
  dplyr::summarize(
    n = dplyr::n(),
    dplyr::across(
      c(
        rolling_g,
        rolling_xg,
        rolling_xgd
      ),
      mean
    )
  ) |> 
  dplyr::ungroup()

rolling_xgd_per_shot_by_position <- dplyr::bind_rows(
  init_rolling_xgd_per_shot_by_position,
  init_rolling_xgd_per_shot_by_position |> 
    dplyr::group_by(rn) |> 
    dplyr::summarize(
      rolling_g = sum(n * rolling_g) / sum(n),
      rolling_xg = sum(n * rolling_xg) / sum(n),
      rolling_xgd = sum(n * rolling_xgd) / sum(n)
    ) |> 
    dplyr::mutate(
      position = NA_character_
    )
)

val_and_test_preds_base <- last_fit_base |> 
  hardhat::extract_workflow() |> 
  broom::augment(val_and_test, type = 'prob') |> 
  dplyr::select(
    set,
    season_id, 
    game_id, 
    date, 
    action_id, 
    team_id, 
    player_id,
    position,
    scores,
    xg_base = .pred_yes
  )

val_and_test_rolling_xgd <- val_and_test_preds_base |> 
  dplyr::group_by(player_id, position) |> 
  dplyr::arrange(date, action_id, .by_group = TRUE) |> 
  dplyr::mutate(
    rn = dplyr::row_number(),
    rolling_g = slider::slide_mean(as.integer(scores == 'yes'), before = 10L, complete = TRUE),
    rolling_xg = slider::slide_mean(xg_base, before = 10L, complete = TRUE)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::transmute(
    rn,
    position,
    set,
    date,
    game_id,
    action_id,
    player_id,
    rolling_g,
    rolling_xg,
    rolling_xgd = rolling_g - rolling_xg 
  ) |> 
  dplyr::left_join(
    rolling_xgd_per_shot_by_position |> 
      dplyr::select(
        position,
        rn,
        init_rolling_xgd = rolling_xgd
      ),
    by = dplyr::join_by(position, rn)
  ) |> 
  dplyr::transmute(
    game_id,
    action_id,
    player_id,
    rolling_xgd = dplyr::coalesce(rolling_xgd, init_rolling_xgd)
  )

updated_val_and_test <- dplyr::rows_update(
  val_and_test,
  val_and_test_rolling_xgd,
  by = c('game_id', 'action_id', 'player_id')
)

## full ----
spec_full <- parsnip::boost_tree(
  trees = 500,
  learn_rate = 0.01,
  tree_depth = 10,
  min_n = 32, 
  loss_reduction = 0.0048179,
  sample_size = 0.1744129, 
  mtry = 9,
  stop_iter = 17
) |>
  parsnip::set_engine('xgboost') |> 
  parsnip::set_mode('classification')

wf_full <- workflows::workflow(
  preprocessor = rec_full,
  spec = spec_full
)

updated_val <- updated_val_and_test |> dplyr::filter(set == 'validation')
updated_test <- updated_val_and_test |> dplyr::filter(set == 'test')

last_fit_full <- tune::last_fit(
  wf_full,
  split = rsample::make_splits(
    updated_val,
    updated_test
  ),
  metrics = met_set
)

plot_var_imp_from_last_importance(last_fit_full)

test_preds_full <- last_fit_full |> 
  hardhat::extract_workflow() |> 
  broom::augment(test, type = 'prob') |> 
  dplyr::transmute(
    set,
    season_id, 
    game_id, 
    date, 
    action_id, 
    team_id, 
    player_id,
    position,
    scores,
    xg_full = .pred_yes
  )

updated_test_preds <- dplyr::inner_join(
  val_and_test_preds_base |> 
    dplyr::filter(set == 'test') |> 
    dplyr::select(
      season_id,
      game_id,
      date,
      action_id,
      player_id,
      position,
      scores,
      xg_base
    ),
  test_preds_full |> 
    dplyr::select(
      game_id,
      action_id,
      xg_full
    ),
  by = dplyr::join_by(game_id, action_id)
) |> 
  dplyr::arrange(date, game_id, action_id)

updated_test_preds |> 
  tidyr::pivot_longer(
    c(xg_base, xg_full),
    names_to = 'xg_type',
    values_to = 'xg'
  ) |> 
  dplyr::group_by(xg_type) |> 
  yardstick::roc_curve(scores, xg) |> 
  dplyr::ungroup() |> 
  ggplot2::ggplot() +
  ggplot2::aes(
    x = 1 - specificity, 
    y = sensitivity,
    color = xg_type
  ) +
  ggplot2::geom_abline(lty = 2, linewidth = 1.5) +
  ggplot2::geom_point() +
  ggplot2::coord_equal()

sampled_updated_val_and_test <- updated_val_and_test |> 
  filter(season_id == 2022)

baked_full_data <- rec_full |>
  prep() |>
  bake(new_data = sampled_updated_val_and_test) |>
  select(-scores)

full_model_object <- hardhat::extract_fit_engine(last_fit_full)
res <- pdp::partial(
  full_model_object,
  train = baked_full_data,
  pred.var = 'rolling_xgd',
  type = 'classification',
  plot = TRUE,
  prob = TRUE,
  trim.outliers = TRUE
)
res

pdp::partial(
  full_model_object,
  train = baked_full_data,
  pred.var = 'elo',
  type = 'classification',
  plot = TRUE,
  prob = TRUE,
  trim.outliers = TRUE
)

pdp::partial(
  full_model_object,
  train = baked_full_data,
  pred.var = c('elo', 'elo_diff'),
  type = 'classification',
  plot = TRUE,
  prob = TRUE,
  trim.outliers = TRUE
)

elo_ice <- pdp::partial(
  full_model_object,
  train = baked_full_data,
  pred.var = 'elo',
  type = 'classification',
  ice = TRUE,
  plot = FALSE,
  prob = TRUE,
  trim.outliers = TRUE
)
