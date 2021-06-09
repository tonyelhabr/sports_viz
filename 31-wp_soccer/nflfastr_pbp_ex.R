# packages
library(nflfastR)
library(splitTools)
library(dials)
library(xgboost)
library(ggplot2)
library(tidyverse)

set.seed(2013) # gohawks

# this should be the default u y do this R
options(scipen = 999999)

# size of hyperparameter grid to search over
# if you don't have a potato computer, can set this to a bigger number
grid_size <- 40

pbp_data <- nflfastR::load_pbp(2019:2020) %>%
  dplyr::mutate(
    # label data with whether possession team ended up winning
    # note that NA result and ties dealt with later
    label = dplyr::case_when(
      result > 0 & posteam == home_team ~ 1,
      result < 0 & posteam == away_team ~ 1,
      TRUE ~ 0
    ),
    # create home indicator used in model
    home = ifelse(posteam == home_team, 1, 0)
  ) %>%
  # creates Diff_Time_Ratio and spread_time
  nflfastR:::prepare_wp_data() %>%
  # don't deal with NA, just drop
  dplyr::filter(
    !is.na(down),
    !is.na(game_seconds_remaining),
    !is.na(yardline_100),
    !is.na(score_differential),
    # overtime is hard
    qtr <= 4,
    !is.na(result),
    !is.na(posteam),
    # throw out ties
    result != 0
  ) %>%
  dplyr::select(
    # label and identifying info
    label,
    game_id,
    season,
    
    # features
    receive_2h_ko,
    spread_time,
    home,
    half_seconds_remaining,
    game_seconds_remaining,
    Diff_Time_Ratio,
    score_differential,
    down,
    ydstogo,
    yardline_100,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining
  )

pbp_data %>% 
  dplyr::filter(season == 2020) %>% 
  count(game_id, posteam)

test_data <- pbp_data %>%
  dplyr::filter(season >= 2020)

train_data <- pbp_data %>%
  dplyr::filter(season < 2020)

# explanation of this step below
folds <- splitTools::create_folds(
  y = train_data$game_id,
  k = 5,
  type = "grouped",
  invert = TRUE
)
folds %>% flatten_int() %>% unique()
folds


train_labels <- train_data %>%
  dplyr::select(label)

# get rid of extra columns
train_data <- train_data %>%
  dplyr::select(-season, -game_id, -label)

grid <- dials::grid_latin_hypercube(
  # this finalize thing is because mtry depends on # of columns in data
  dials::finalize(dials::mtry(), train_data),
  dials::min_n(),
  dials::tree_depth(),
  # to force learn_rate to not be crazy small like dials defaults to
  # because my computer is slow
  # if you're trying this for a different problem, expand the range here
  # by using more negative values
  dials::learn_rate(range = c(-1.5, -0.5), trans = scales::log10_trans()),
  dials::loss_reduction(),
  sample_size = dials::sample_prop(),
  size = grid_size
) %>%
  dplyr::mutate(
    # has to be between 0 and 1 for xgb
    # for some reason mtry gives the number of columns rather than proportion
    mtry = mtry / length(train_data),
    # see note below
    monotone_constraints = "(0, 0, 0, 0, 0, 1, 1, -1, -1, -1, 1, -1)"
    
    # for the monotone constraints
    # these are notes to myself to make sure the constraints are in the right order
    # the order of the constraints needs to match up with the columns in the df
    
    # receive_2h_ko, 0
    # spread_time, 0
    # home, 0
    
    # half_seconds_remaining, 0
    # game_seconds_remaining, 0
    # Diff_Time_Ratio, 1
    
    # score_differential, 1
    # down, -1
    # ydstogo, -1
    
    # yardline_100, -1
    # posteam_timeouts_remaining, 1
    # defteam_timeouts_remaining, -1
  ) %>%
  # make these the right names for xgb
  dplyr::rename(
    eta = learn_rate,
    gamma = loss_reduction,
    subsample = sample_size,
    colsample_bytree = mtry,
    max_depth = tree_depth,
    min_child_weight = min_n
  )

grid

get_row <- function(row) {
  params <-
    list(
      booster = "gbtree",
      objective = "binary:logistic",
      eval_metric = c("logloss"),
      eta = row$eta,
      gamma = row$gamma,
      subsample = row$subsample,
      colsample_bytree = row$colsample_bytree,
      max_depth = row$max_depth,
      min_child_weight = row$min_child_weight,
      monotone_constraints = row$monotone_constraints
    )
  
  # do the cross validation
  wp_cv_model <- xgboost::xgb.cv(
    data = as.matrix(train_data),
    label = train_labels$label,
    params = params,
    # this doesn't matter with early stopping in xgb.cv, just set a big number
    # the actual optimal rounds will be found in this tuning process
    nrounds = 15000,
    # created above
    folds = folds,
    metrics = list("logloss"),
    early_stopping_rounds = 50,
    print_every_n = 50
  )
  
  # bundle up the results together for returning
  output <- params
  output$iter <- wp_cv_model$best_iteration
  output$logloss <- wp_cv_model$evaluation_log[output$iter]$test_logloss_mean
  
  row_result <- bind_rows(output)
  
  return(row_result)
}

results <- purrr::map_df(1:nrow(grid), function(x) {
  get_row(grid %>% dplyr::slice(x))
})

results %>%
  dplyr::select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
  tidyr::pivot_longer(
    eta:min_child_weight,
    values_to = "value",
    names_to = "parameter"
  ) %>%
  ggplot(aes(value, logloss, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "logloss") +
  theme_minimal()

best_model <- results %>%
  dplyr::arrange(logloss) %>%
  dplyr::slice(1)

params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = best_model$eta,
    gamma = best_model$gamma,
    subsample = best_model$subsample,
    colsample_bytree = best_model$colsample_bytree,
    max_depth = best_model$max_depth,
    min_child_weight = best_model$min_child_weight,
    monotone_constraints = best_model$monotone_constraints
  )

nrounds <- best_model$iter

wp_model <- xgboost::xgboost(
  params = params,
  data = as.matrix(train_data),
  label = train_labels$label,
  nrounds = nrounds,
  verbose = 2
)

importance <- xgboost::xgb.importance(
  feature_names = colnames(wp_model),
  model = wp_model
)
xgboost::xgb.ggplot.importance(importance_matrix = importance)
