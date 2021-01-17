
library(tidyverse)

.get_verbose <- function() {
  TRUE
}

.display_info <- function(x, ..., .envir = parent.frame(), .verbose = .get_verbose(), .f_glue = glue::glue_collapse) {
  if (!.verbose) {
    return(invisible(x))
  }
  x <- .f_glue(x, '\n')
  x <- glue::glue(x, .envir = .envir)
  cli::cat_line(x)
}

.display_warning <- function(x, ..., .envir = parent.frame()) {
  x <- glue::glue_collapse(x, '\n')
  x <- glue::glue(x, .envir = .envir)
  warning(x, call. = FALSE, immediate. = TRUE)
}

.display_error <- function(x, ..., .envir = parent.frame()) {
  x <- glue::glue_collapse(x, '\n')
  x <- glue::glue(x, .envir = .envir)
  cnd <- structure(class = c('usethis_error', 'error', 'condition'), list(message = x))
  stop(cnd)
}

.path_x <- function(dir, file, ext = NULL) {
  if(!is.null(ext)) {
    ext <- sprintf('.%s', ext)
  } else {
    ext <- ''
  }
  file.path(dir, sprintf('%s%s', file, ext))
}
.path_data <- partial(.path_x, dir = getwd(), ... = )
.path_figs <- partial(.path_x, dir = getwd(), ... = )
.path_figs_png <- partial(.path_x, dir = getwd(), ext = 'png', ... = )

import_tm_mapping <- function() {
  path <- .path_data('team_account_mapping.csv') # Manual mapping
  path %>% 
    read_csv(
      col_types = cols(
        tm_xgp = col_character(),
        user_id = col_character()
      )
    )
}

.str_replace_text <- function(x, i) {
  x %>% str_replace('(^.*)\\s\\(([0-9.]+)\\)\\s([0-9]+)[-]([0-9]+)\\s\\(([0-9.]+)\\)\\s(.*$)', sprintf('\\%d', i))
}

.get_valid_suffixes <- memoise::memoise({function() {
  c('h', 'a')
}})

.validate_suffix <- function(x = .get_valid_suffixes(), ...) {
  match.arg(x, ...)
}

.add_estimated_follower_count_col <- function(data, suffix = .get_valid_suffixes(), tm_accounts_mapping, latest_date) {
  .validate_suffix(suffix)
  col_created_at_sym <- sprintf('created_at_%s', suffix) %>% sym()
  col_diff <- sprintf('date_diff_%s', suffix)
  col_diff_sym <- col_diff %>% sym()
  col_diff_latest_sym <- sprintf('%s_latest', col_diff) %>% sym()
  col_followers_count_sym <- sprintf('followers_count_%s', suffix) %>% sym()
  col_res_sym <- sprintf('estimated_followers_count_%s', suffix) %>% sym()
  # browser()
  data %>% 
    left_join(
      tm_accounts_mapping %>% rename_all(~sprintf('%s_%s', .x, suffix))
    ) %>% 
    mutate(
      !!col_diff_sym := !!latest_date - lubridate::date(!!col_created_at_sym),
      !!col_diff_latest_sym := !!latest_date - created_date,
      across(matches(col_diff), as.numeric),
      # frac_h = ((date_diff_h - date_diff_h_latest) / date_diff_h),
      !!col_res_sym := ((!!col_diff_sym - !!col_diff_latest_sym) / !!col_diff_sym)^0.5 * !!col_followers_count_sym
    ) %>% 
    select(-matches(col_diff), -!!col_created_at_sym, -!!col_followers_count_sym) 
}

.add_estimated_follower_count_cols <- function(data, ...) {
  # browser()
  data %>% 
    .add_estimated_follower_count_col('h', ...) %>% 
    .add_estimated_follower_count_col('a', ...)
}

.tms_correct <-
  tibble(
    tm = c('Spurs', 'Man United'),
    tm_correct = c('Tottenham', 'Man Utd')
  )

.fix_tm_col <- function(data, suffix = .get_valid_suffixes()) {
  .validate_suffix(suffix)
  col_tm_sym <- sprintf('tm_%s', suffix) %>% sym()
  col_tm_correct_sym <- sprintf('tm_correct_%s', suffix) %>% sym()
  data %>% 
    left_join(.tms_correct %>% rename_all(~sprintf('%s_%s', .x, suffix))) %>% 
    mutate(
      across(!!col_tm_sym, ~coalesce(!!col_tm_correct_sym, .x))
    ) %>% 
    select(-!!col_tm_correct_sym)
}

.fix_tm_cols <- function(data) {
  data %>% 
    .fix_tm_col('h') %>% 
    .fix_tm_col('a')
}

.df2mat <- function(data) {
  model.matrix(
    ~.+0,
    data =
      model.frame(
        ~ .+0,
        data,
        na.action = na.pass
      )
  )
}

.generate_path <- function(path = NULL, dir, file, ext) {
  if(!is.null(path)) {
    return(path)
  }
  file.path(dir, sprintf('%s.%s', file, ext))
}

.postprocess_xgb_cv_res <- function(res, fit_cv, eval_metrics, path) {
  # NOTE: This doesn't generalize when there are more than one eval metrics.
  .eval_metric <- eval_metrics[1]
  col_trn <- sprintf('train_%s_mean', .eval_metric)
  col_trn_res <- sprintf('%s_trn', .eval_metric)
  col_trn_sym <- col_trn %>% sym()
  col_trn_res_sym <- col_trn_res %>% sym()
  col_tst <- sprintf('test_%s_mean', .eval_metric)
  col_tst_res <- sprintf('%s_tst', .eval_metric)
  col_tst_sym <- col_tst %>% sym()
  col_tst_res_sym <- col_tst_res %>% sym()
  # browser()
  log <- 
    fit_cv$evaluation_log %>% 
    as_tibble() %>% 
    rename(
      !!col_trn_res := !!col_trn,
      !!col_tst_res := !!col_tst
    )
  write_rds(log, path)
  
  res$iter <- fit_cv$best_iteration

  res[[col_trn_res]] = fit_cv$evaluation_log[res$iter][[col_trn]]
  res[[col_tst_res]] = fit_cv$evaluation_log[res$iter][[col_tst]]
  res[['eval_metric']] <- NULL
  bind_rows(res)
}

.do_tune_cv <-
  function(grid_params,
           x_dmat,
           booster,
           objective,
           eval_metrics,
           stem = NULL,
           # early_stopping_rounds = 10,
           # print_every = 10,
           ...,
           dir = getwd(),
           file = sprintf('res_tune_cv%s', ifelse(is.null(stem), '', sprintf('_%s', stem))),
           ext = 'rds',
           path = NULL,
           overwrite = FALSE) {
    path <- .generate_path(path = path, dir = dir, file = file, ext = ext)
    basename <- basename(path)
    file <- tools::file_path_sans_ext(basename)
    dir <- dirname(path)
    ext <- tools::file_ext(path)
    
    .get_metrics <-
      function(params,
               idx = 1 # ,
               # .path = .generate_path(dir = dir, file = sprintf('%s_%d', file, idx), ext = ext),
               # ...
               ) {
        # row = 1; data <- grid_params %>% slice(idx)
        # browser()
        .path = .generate_path(path = NULL, dir = dir, file = sprintf('%s_%d', file, idx), ext = ext)
        path_exists <- .path %>% file.exists()
        if(path_exists & !overwrite) {
          .display_info('Returning early for `idx = {idx}`.')
          return(readr::read_rds(.path))
        }
        .display_info('Row {cli::bg_cyan(idx)} (of {cli::bg_cyan(n_row)})')
       res <-
          list(
            booster = booster,
            objective = objective,
            eval_metric = eval_metrics,
            eta = params$learn_rate,
            gamma = params$loss_reduction,
            subsample = params$sample_size,
            colsample_bytree = params$mtry,
            max_depth = params$tree_depth,
            min_child_weight = params$min_n
          )
        
        fit_cv <-
          xgboost::xgb.cv(
            data = x_dmat,
            params = res,
            nrounds = nrounds,
            folds = folds,
            metrics = eval_metrics,
            ...
          )
        res <- .postprocess_xgb_cv_res(res, fit_cv, eval_metrics, path = .path)
        res
      }
    
    res <-
      grid_params %>%
      nest(params = -idx) %>%
      mutate(metrics = map2(params, idx, ~.get_metrics(params = ..1, idx = ..2))) %>%
      select(-params) %>%
      unnest(metrics)
    res
    write_rds(res, path)
    res
  }

.augment_preds <- function(v, data, cols_id = 'idx', cols_extra = NULL, col_y, export = TRUE, path) {
  # browser()
  col_y_sym <- col_y %>% sym()
  probs <-
    v %>%
    tibble(.pred = .) %>%
    bind_cols(
      data %>%
        select(
          all_of(cols_id),
          one_of(cols_extra),
          all_of(col_y)
        )
    )
  if(!export) {
    return(probs)
  }
  probs %>% arrow::write_parquet(path)
  probs
}
