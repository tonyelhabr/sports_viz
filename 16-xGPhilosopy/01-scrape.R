
source('functions.R')
user <- 'xGPhilosophy'
path_tweets <- .path_data(sprintf('%s_timeline.rds', user))
path_epl_tm_accounts <- .path_data('epl_tm_accounts.rds')
path_champ_tm_accounts <- .path_data('epl_tm_accounts.rds')

tm_mapping <- import_tm_mapping()
if(FALSE) {
  n <- 3200
  tweets <- rtweet::get_timeline(user = user, n = n)
  # earliest_tweet <- tweets %>% slice_min(created_at)
  # https://www.goalprofits.com/complete-list-of-official-football-club-twitter-handles/
  epl_tm_accounts <- rtweet::lists_members(786067957373894656)
  champ_tm_accounts <- rtweet::lists_members(786070708405272576)
  write_rds(tweets, path_tweets)
  write_rds(epl_tm_accounts, path_epl_tm_accounts)
  write_rds(champ_tm_accounts, path_champ_tm_accounts)
} else {
  tweets <- path_tweets %>% read_rds()
  epl_tm_accounts <- path_epl_tm_accounts %>% read_rds()
  champ_tm_accounts <- path_champ_tm_accounts %>% read_rds()
}

tm_accounts <- bind_rows(epl_tm_accounts, champ_tm_accounts)
tm_accounts

tm_accounts_mapping <-
  tm_mapping %>% 
  left_join(
    tm_accounts %>% 
      select(user_id, followers_count, created_at)
  ) %>% 
  select(-user_id) %>% 
  rename(tm = tm_xgp)
tm_accounts_mapping %>% arrange(-followers_count)

# We have to make an assumption about how many followers the account had when it started to make tweets frequently, which really wasn't until Jan. 2020
first_follower_count <- 10000
latest_tweet <- tweets %>% slice_max(created_at)
latest_follower_count <- latest_tweet$followers_count
follower_count_diff <- latest_follower_count - first_follower_count
latest_date <- latest_tweet$created_at %>% lubridate::date()
# created_date <- latest_tweet$account_created_at %>% lubridate::date()

# If I source this in an a separate script, the sourcing fails...
.postprocess_tm_str <- function(x) {
  x %>% str_remove_all('⭐️') %>% str_trim()
}

# tweets %>% filter(text %>% str_detect('follower')) %>% select(created_at, text)
scores <-
  tweets %>%
  select(
    status_id,
    created_at,
    # reply_count,
    # followers_count,
    retweet_count,
    # retweet_favorite_count,
    favorite_count,
    # favourites_count,
    # quote_count,
    # quoted_favorite_count,
    # quoted_retweet_count,
    text
  ) %>% 
  # This is a linear estimate of follower count at the tweet time.
  mutate(
    idx = row_number(created_at),
    estimated_follower_count = !!first_follower_count + round((idx / max(idx)) * !!follower_count_diff, 0)
  ) %>% 
  select(-idx) %>% 
  # Drop half time scores, and just anything with commas or new lines since those aren't score line tweets.
  filter(text %>% str_detect('^HT|\\,|\\n', negate = TRUE)) %>%
  # We know that a score line tweet has this.
  filter(text %>% str_detect('\\(')) %>%
  mutate(
    across(
      created_at,
      list(
        hour = lubridate::hour,
        wday = ~lubridate::wday(.x) %>% as.integer(),
        created_date = lubridate::date
      ),
      .names = '{fn}'
    ),
    # is_weekend = if_else(wday %in% c(1L, 7L), TRUE, FALSE),
    across(
      hour,
      list(
        x = ~sin(2 * pi * .x / 24),
        y = ~cos(2 * pi * .x / 24)
      )
    ),
    across(
      wday,
      list(
        x = ~sin(2 * pi * .x / 7),
        y = ~cos(2 * pi * .x / 7)
      )
    ),
    across(
      text,
      list(
        tm_h = ~ .str_replace_text(.x, 1) %>% .postprocess_tm_str(),
        xg_h = ~ .str_replace_text(.x, 2) %>% as.numeric(),
        g_h = ~ .str_replace_text(.x, 3) %>% as.integer(),
        g_a = ~ .str_replace_text(.x, 4) %>% as.integer(),
        xg_a = ~ .str_replace_text(.x, 5) %>% as.numeric(),
        tm_a = ~ .str_replace_text(.x, 6) %>% .postprocess_tm_str()
      ),
      .names = '{fn}'
    )
  ) %>%
  # select(-text) %>% 
  # Drop non-score line tweets that weren't caught by previous filter.
  drop_na(xg_h, g_h, g_a, xg_a) %>% 
  mutate(
    idx = row_number(created_at)
  ) %>% 
  .fix_tm_cols() %>% 
  .add_estimated_follower_count_cols(tm_accounts_mapping, latest_date) %>% 
  select(-created_date) %>% 
  arrange(created_at)
scores

# scores %>% drop_na(estimated_followers_count_h, estimated_followers_count_a)
# scores %>% summarize(across(matches('_count$'), mean))
# scores %>% arrange(-retweet_count)

.generate_col_vec <- function(prefix, suffix) {
  crossing(
    prefix = prefix,
    suffix = suffix
  ) %>% 
    unite('col', prefix, suffix) %>% 
    arrange(col) %>% 
    pull(col)
}
suffixes <- .get_valid_suffixes()
cols_suffix <-
  .generate_col_vec(
    prefix = c('g', 'xg', 'estimated_followers_count'),
    suffix = suffixes # .get_valid_suffixes(),
  )
cols_time <-
  .generate_col_vec(
    prefix = c('hour', 'wday'),
    suffix = c('x', 'y')
  )

cols_lst <-
  list(
    col_y = 'favorite_count',
    # cols_id = 'status_id',
    cols_id = 'idx',
    col_strata = 'created_at',
    cols_extra = c('created_at', 'status_id', 'tm_h', 'tm_a', 'text'),
    cols_x = c('estiamated_follower_count', cols_time, cols_suffix)
  )
cols_lst

# # Quick check on teams
# tms <-
#   bind_rows(
#     scores %>% 
#       drop_na(estimated_followers_count_h) %>% 
#       count(tm = tm_h),
#     scores %>% 
#       drop_na(estimated_followers_count_a) %>% 
#       count(tm = tm_a)
#   ) %>% 
#   group_by(tm) %>% 
#   summarize(across(n, sum)) %>% 
#   ungroup() %>% 
#   arrange(desc(n))
# tms

do_fit_model <- function(data, cols_lst, stem, overwrite = FALSE) {
  data = scores
  stem = 'favorites'
  overwrite = FALSE
  
  .path_data_x <- function(file, ext = NULL) {
    .path_data(file = sprintf('%s_%s', file, stem), ext = ext)
  }
  .path_figs_png_x <- function(file) {
    .path_figs_png(file = sprintf('%s_%s', file, stem))
  }
  .path_data_parquet_x <- purrr::partial(.path_data_x, ext = 'parquet', ... = )
  path_res_tune_cv <- .path_data_x('res_tune_cv', ext = 'rds')
  path_fit <- .path_data_x('fit')
  path_res_cv <- .path_data_parquet_x('res_cv')
  path_preds <- .path_data_parquet_x('preds')
  path_shap <- .path_data_parquet_x('shap')
  
  .df2mat_x <- function(data) {
    data %>% select(one_of(c(cols_lst$cols_x))) %>% .df2mat()
  }
  
  col_y_sym <- cols_lst$col_y %>% sym()
  df <-
    data %>%
    drop_na(!!col_y_sym)
  
  x_mat <- df %>% .df2mat_x()
  
  x_dmat <-
    xgboost::xgb.DMatrix(
      x_mat,
      label = df[[cols_lst$col_y]]
    )
  x_dmat
  
  set.seed(42)
  n_fold <- 10
  folds_ids <-
    caret::createFolds(
      data[[cols_lst$col_strata]],
      k = n_fold,
      list = FALSE,
      returnTrain = FALSE
    )
  folds_ids
  
  col_strata_sym <- cols_lst$col_strata %>% sym()
  folds <-
    data %>%
    bind_cols(tibble(fold = folds_ids)) %>%
    left_join(df %>% select(!!col_strata_sym, idx)) %>%
    select(fold, idx) %>%
    split(.$fold) %>%
    purrr::map(~select(.x, -fold) %>% pull(idx))
  folds
  n_obs <- folds %>% flatten_int() %>% length()
  max_idx <- folds %>% flatten_int() %>% max()
  assertthat::assert_that(n_obs == max_idx)
  
  nrounds <- 1000
  booster <- 'gbtree'
  objective <- 'reg:squarederror'
  eval_metrics <- list('rmse')
  early_stopping_rounds <- 10
  print_every_n <- 10

  set.seed(42)
  n_row <- 30
  grid_params <-
    dials::grid_latin_hypercube(
      dials::finalize(dials::mtry(), df),
      dials::min_n(),
      dials::tree_depth(),
      dials::learn_rate(),
      dials::loss_reduction(),
      sample_size = dials::sample_prop(),
      size = n_row
    ) %>%
    mutate(
      learn_rate = 0.1 * ((1:n_row) / n_row),
      mtry = mtry / ncol(df),
      idx = row_number()
    ) %>%
    relocate(idx)
  grid_params
  
  res_tune_cv <- 
    .do_tune_cv(
      overwrite = TRUE,
      stem = stem,
      grid_params = grid_params,
      x_dmat = x_dmat,
      booster = booster,
      objective = objective,
      eval_metrics = eval_metrics,
      early_stopping_rounds = early_stopping_rounds,
      print_every_n = print_every_n
    )
  
  eval_metric <- eval_metrics[1]
  eval_metric_tst <- sprintf('%s_tst', eval_metric)
  eval_metric_tst_sym <- eval_metric_tst %>% sym()
  res_cv_best <- res_tune_cv %>% slice_min(!!eval_metric_tst_sym)
  res_cv_best
  
  .pluck_param <- function(x) {
    res_cv_best %>% purrr::pluck(x)
  }
  
  params_best <-
    list(
      booster = booster,
      objective = objective,
      eval_metric = eval_metrics,
      eta = .pluck_param('eta'),
      gamma = .pluck_param('gamma'),
      subsample = .pluck_param('subsample'),
      colsample_bytree = .pluck_param('colsample_bytree'),
      max_depth = .pluck_param('max_depth'),
      min_child_weight = .pluck_param('min_child_weight')
    )
  params_best
  
  nrounds_best <- round((.pluck_param('iter') / ((n_fold - 1) / (n_fold))), 0) + early_stopping_rounds
  fit <-
    xgboost::xgboost(
      params = params_best,
      data = x_dmat,
      nrounds = nrounds_best,
      early_stopping_rounds = early_stopping_rounds,
      print_every_n = print_every_n,
      verbose = 1
    )
  xgboost::xgb.save(fit, path_fit)
  
  .augment_preds_x <-
    purrr::partial(
      .augment_preds,
      cols_id = cols_lst$cols_id,
      cols_extra = cols_lst$cols_extra,
      col_y = cols_lst$col_y,
      export = TRUE,
      ... =
    )
  
  preds <-
    fit %>%
    predict(x_mat) %>%
    .augment_preds_x(
      data = df,
      path = path_preds
    )
  preds
  
  preds_aug <-
    preds %>% 
    mutate(
      foe = favorite_count - .pred,
      foe_frac = foe / .pred
    ) %>% 
    arrange(desc(foe_frac))
  preds_aug
  preds_aug %>% 
    arrange(desc(foe)) %>% 
    select(created_at, text, favorite_count, .pred)

  preds %>% 
    ggplot() +
    aes(x = favorite_count, y = .pred) +
    geom_point() +
    geom_smooth()
  
  preds %>% 
    mutate(
      prnk = percent_rank(favorite_count)
    ) %>% 
    ggplot() +
    aes(x = prnk, y = .pred) +
    geom_point()
  
  df_mat <-
    x_mat %>%
    as.data.frame()
  
  feature_values_init <-
    df_mat %>%
    mutate_all(scale) %>%
    gather('feature', 'feature_value') %>%
    as_tibble()
  feature_values_init
  
  feature_values <-
    feature_values_init %>%
    pull(feature_value)
  feature_values
  
  shap_init <-
    fit %>%
    predict(newdata = x_mat, predcontrib = TRUE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(-matches('BIAS'))
  shap_init
  
  shap_wide <-
    shap_init %>%
    bind_cols(df %>% select(idx)) %>%
    left_join(
      preds %>%
        mutate(idx = row_number())
    )
  shap_wide
  arrow::write_parquet(shap_wide, path_shap)
  
  shap <-
    shap_init %>%
    bind_cols(df %>% select(idx)) %>%
    left_join(
      preds %>%
        mutate(idx = row_number())
    ) %>%
    pivot_longer(-c(idx, .pred, cols_lst$col_y), names_to = 'feature', values_to = 'shap_value')
  shap
  
  shap_agg_by_feature <-
    shap %>%
    group_by(feature) %>%
    summarize(
      across(shap_value, ~mean(abs(.x))),
    ) %>%
    ungroup() %>%
    mutate(
      across(shap_value, list(rnk = ~row_number(desc(.x))))
    ) %>%
    arrange(shap_value_rnk)
  shap_agg_by_feature
}
