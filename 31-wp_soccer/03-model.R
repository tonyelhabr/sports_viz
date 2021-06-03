
library(tidyverse)
dir_proj <- '31-wp_soccer'
path_model_data <- file.path(dir_proj, 'model_data.rds')
df <- path_model_data %>% read_rds()

n_match <- match_ids %>% nrow()
n_match <- floor(n_match * 0.8)
n_match
match_ids_trn <- match_ids %>% slice(c(1:n_match))
match_ids_tst <- match_ids %>% slice(c((n_match + 1):n_match))
df_trn <- df %>% semi_join(match_ids_trn)
df_tst <- df %>% semi_join(match_ids_tst)

f_select <- function(data) {
  data %>% select(is_h, g, g_opp, xg, xg_opp, gd, xgd, gd_ratio, prob, prob_opp, prob_d)
}
x_trn <- df_trn %>% f_select()
x_tst <- df_tst %>% f_select()

set.seed(42)
folds <- 
  splitTools::create_folds(
    y = df_trn %>% pull(match_id),
    k = 5,
    type = 'grouped',
    invert = TRUE
  )
folds
# folds %>% flatten_int() %>% unique()

set.seed(42)
grid <- 
  dials::grid_latin_hypercube(
    dials::finalize(dials::mtry(), x_trn),
    dials::min_n(),
    dials::tree_depth(),
    dials::learn_rate(range = c(-1.5, -0.5), trans = log10_trans()),
    dials::loss_reduction(),
    sample_size = dials::sample_prop(),
    size = grid_size
  ) %>%
  dplyr::mutate(
    # has to be between 0 and 1 for xgb
    # for some reason mtry gives the number of columns rather than proportion
    mtry = mtry / ncol(x_trn),
    # maybe add a 1 for prob?
    # is_h, g, g_opp, xg, xg_opp, gd, xgd, gd_ratio, prob, prob_opp, prob_d
    #    0, 0,    0,   0,      0,  1,   1,        1,    0,        0,      0
    monotone_constraints = "(0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0)"
    
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
      booster = 'gbtree',
      objective = 'binary:logistic',
      eval_metric = c('logloss'),
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
    data = as.matrix(x_trn),
    label = df_trn[['is_w']],
    params = params,
    # this doesn't matter with early stopping in xgb.cv, just set a big number
    # the actual optimal rounds will be found in this tuning process
    nrounds = 15000,
    # created above
    folds = folds,
    metrics = list('logloss'),
    early_stopping_rounds = 10,
    print_every_n = 50
  )
  
  # bundle up the results together for returning
  output <- params
  output$iter <- wp_cv_model$best_iteration
  output$logloss <- wp_cv_model$evaluation_log[output$iter]$test_logloss_mean
  
  bind_rows(output)
}

results <- purrr::map_df(1:nrow(grid), function(x) {
  get_row(grid %>% dplyr::slice(x))
})

write_rds(results, 'tune.rds')

viz_metrics <-
  results %>%
  dplyr::select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
  tidyr::pivot_longer(
    eta:min_child_weight,
    values_to = 'value',
    names_to = 'parameter'
  ) %>%
  ggplot(aes(value, logloss, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
  facet_wrap(~parameter, scales = 'free_x') +
  labs(x = NULL, y = 'logloss') +
  theme_minimal()
viz_metrics

best_model <-
  results %>%
  dplyr::arrange(logloss) %>%
  dplyr::slice(1)
best_model

params <-
  list(
    booster = 'gbtree',
    objective = 'binary:logistic',
    eval_metric = c('logloss'),
    eta = best_model$eta,
    gamma = best_model$gamma,
    subsample = best_model$subsample,
    colsample_bytree = best_model$colsample_bytree,
    max_depth = best_model$max_depth,
    min_child_weight = best_model$min_child_weight,
    monotone_constraints = best_model$monotone_constraints
  )

nrounds <- best_model$iter

wp_model <- 
  xgboost::xgboost(
    params = params,
    data = as.matrix(df_trn),
    label = df_trn[['is_w']],
    nrounds = nrounds,
    verbose = 2
  )

importance <- xgboost::xgb.importance(
  feature_names = colnames(wp_model),
  model = wp_model
)
# xgboost::xgb.ggplot.importance(importance_matrix = importance)
# xgboost::xgb.importance(importance_matrix = importance)
xgboost::xgb.plot.importance(importance_matrix = importance)
