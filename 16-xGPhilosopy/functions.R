
library(tidyverse)

# general functions ----
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

.generate_path <- function(path = NULL, dir, file, ext) {
  if(!is.null(path)) {
    return(path)
  }
  file.path(dir, sprintf('%s.%s', file, ext))
}

.get_x <-
  function(...,
           f = NULL,
           file = tempfile(),
           ext = 'rds',
           dir = getwd(),
           path = NULL,
           f_import = rio::import,
           f_export = rio::export,
           append = FALSE,
           export = TRUE,
           overwrite = FALSE) {
    path <- .generate_path(path = path, dir = dir, file = file, ext = ext)
    path_exists <- path %>% file.exists()
    if(path_exists & !overwrite) {
      return(f_import(path))
    }
    
    f_safe <- purrr::safely(f, otherwise = NULL)
    res <- f_safe(...)
    if (is.null(res)) {
      .display_warning('Something went wrong with function call `f`!')
      return(NULL)
    }
    
    if(export) {
      .display_info('Exporting `x` to `path = "{path}"`.')
      dir <- dirname(path)
      if(!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
      }
      f_export(res, path)
    }
    res
    
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

# project-specific functions ----
.get_valid_stems <- memoise::memoise({function() {
  c('favorite', 'retweet')
}})

.validate_stem <- function(x = .get_valid_stems(), ...) {
  match.arg(x, ...)
}

# https://www.goalprofits.com/complete-list-of-official-football-club-twitter-handles/
# TODO(?): Convert these to the dir,file,ext,path format of functions.
.import_tm_accounts <- function(path = .path_data('team_accounts.csv'), overwrite = FALSE) {
  path_exists <- path %>% file.exists()
  if(path_exists & !overwrite) {
    res <-
      path %>% 
      read_csv(
        col_types = cols(
          name = col_character(),
          user_id = col_character(),
          followers_count = col_integer()
        )
      )
  }
  epl_tm_accounts <- rtweet::lists_members(786067957373894656)
  champ_tm_accounts <- rtweet::lists_members(786070708405272576)
  bund_tm_accounts <- rtweet::lists_members(786265012612456448)
  seriea_tm_accounts <- rtweet::lists_members(787389659328327680)
  ligue1_tm_accounts <- rtweet::lists_members(787391173576884224)
  laliga_tm_accounts <- rtweet::lists_members(787391036179951616)
  ered_tm_accounts <- rtweet::lists_members(786072885798506496)
  res <- 
    list(
      'epl' = epl_tm_accounts, 
      'champ' = champ_tm_accounts,
      'bund' = bund_tm_accounts,
      'seriea' = seriea_tm_accounts,
      'ligue1' = ligue1_tm_accounts,
      'laliga' = laliga_tm_accounts,
      'ered' = ered_tm_accounts
    ) %>% 
    map_dfr(bind_rows, .id = 'lg') %>% 
    select(lg, name, screen_name, user_id, followers_count, created_at) %>% 
    arrange(desc(followers_count))
  write_parquet(res, path, na = '')
  res
}

# Manual mapping
.import_tm_mapping <- function(path = .path_data('team_account_mapping.csv')) {
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

.remove_emoticons <- function(x) {
  iconv(x, 'latin1', 'ASCII', sub='') %>% str_trim()
}

.distinct12_at <- function(data, col, suffix , sep = '_', ...) {
  # browser()
  col1 <- sprintf('%s%s%s', col, sep, suffix[1])
  col2 <- sprintf('%s%s%s', col, sep, suffix[2])
  col_sym <- sym(col)
  col1_sym <- sym(col1)
  col2_sym <- sym(col2)
  # browser()
  bind_rows(
    data %>% distinct(temp = !!col1_sym),
    data %>% distinct(temp = !!col2_sym)
  ) %>% 
    distinct(temp) %>% 
    arrange(temp) %>% 
    rename(!!col_sym := temp)
}

.add_estimated_follower_count_col <-
  function(data,
           suffix = .get_valid_suffixes(),
           tm_accounts_mapping = .import_tm_accounts_mapping(),
           latest_date,
           train = TRUE,
           retrieve = TRUE) {
    .validate_suffix(suffix)
    col_created_at_sym <- sprintf('created_at_%s', suffix) %>% sym()
    col_diff <- sprintf('date_diff_%s', suffix)
    col_diff_sym <- col_diff %>% sym()
    col_diff_latest_sym <- sprintf('%s_latest', col_diff) %>% sym()
    col_followers_count_sym <-
      sprintf('followers_count_%s', suffix) %>% sym()
    col_res_sym <-
      sprintf('estimated_followers_count_%s', suffix) %>% sym()
    # browser()
    if (!train & retrieve) {
      tms_distinct <- data %>% .distinct12_at(suffix = .get_valid_suffixes())
      users <-
        tm_accounts_mapping %>% 
        semi_join(tms_distinct) %>% 
        pull(user_id)
      if(length(users) == 0L) {
        .display_warning('Could not retrieve most up-to-date follower count for {length(tms_distinct)} teams. Using pre-saved info.')
      } else {
        tm_accounts <-
          users %>% 
          rtweet::lookup_users() %>% 
          select(user_id, followers_count)
        tm_accounts_mapping <-
          tm_accounts_mapping %>% 
          select(-followers_count) %>% 
          inner_join(tm_accounts, by = 'user_id')
      }
    } else {
      tm_accounts_mapping <- tm_accounts_mapping %>% select(-user_id)
    }
    
    res <-
      data %>%
      left_join(tm_accounts_mapping %>% rename_all( ~ sprintf('%s_%s', .x, suffix))) %>%
      mutate(
        !!col_diff_sym := !!latest_date - lubridate::date(!!col_created_at_sym),
        !!col_diff_latest_sym := !!latest_date - created_date,
        across(matches(col_diff), as.numeric),
        !!col_res_sym := ((!!col_diff_sym-!!col_diff_latest_sym) / !!col_diff_sym) ^0.5 * !!col_followers_count_sym
      ) %>%
      select(
        -matches(col_diff),
        -!!col_created_at_sym,
        -!!col_followers_count_sym
      )
    res
  }

.add_estimated_follower_count_cols <- function(data, ...) {
  # browser()
  data %>% 
    .add_estimated_follower_count_col('h', ...) %>% 
    .add_estimated_follower_count_col('a', ...)
}

.tms_correct <-
  tibble(
    tm = c('Spurs', 'Man Utd', 'Inter', 'Bayern', 'BVB', 'Leipzig'),
    tm_correct = c('Tottenham', 'Man United', 'Inter Milan', 'Bayern Munich', 'BVB Dortmund', 'RB Leipzig')
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

i.mport_tweets <-
  function(user = 'xGPhilosophy',
           n = 3200,
           ...,
           dir = getwd(),
           file = sprintf('%s_timeline.rds', user),
           ext = 'rds',
           path = NULL,
           f_import = read_rds,
           f_export = write_rds,
           append = TRUE,
           overwrite = FALSE) {
    # path <-
    #   .generate_path(
    #     path = path,
    #     dir = dir,
    #     file = file,
    #     ext = ext
    #   )
    # path_exists <- path %>% file.exists()
    if (path_exists & append) {
      tweets_existing <- f_import(path)
      latest_tweet <- tweets_existing %>% slice_max(created_at)
      tweets <- rtweet::get_timeline(user = user, n = n, max_id = latest_tweet$status_id, ...)
      tweets <- bind_rows(tweets, tweets_existing) %>% distinct(status_id, .keep_all = TRUE)
    } else if (path_exists & !overwrite & !append) {
      return(f_import(path))
    } else {
      tweets <- rtweet::get_timeline(user = user, n = n, ...)
    }
    f_export(tweets, path)
    tweets
  }

import_tweets <- function(train = TRUE) {
  if(train) {
    append <- FALSE
    export <- TRUE
    overwrite <- TRUE
  } else {
    append <- TRUE
    export <- TRUE
    overwrite <- FALSE
  }
  .get_x(
    f = .import_tweets,
    f_import = read_rds,
    f_export = write_rds,
    append = append,
    export = export,
    overwrite = overwrite
  )
    
}

.import_tm_accounts_mapping <- function(tm_accounts = .import_tm_accounts(), tm_mapping = .import_tm_mapping(), ...) {
  
  res <-
    tm_mapping %>% 
    left_join(
      tm_accounts %>% 
        select(user_id, followers_count, created_at),
      by = 'user_id'
    ) %>% 
    # select(-user_id) %>% 
    rename(tm = tm_xgp)
  # tm_accounts_mapping %>% filter(is.na(followers_count))
  # tm_accounts_mapping %>% arrange(-followers_count)
  res
}


.fourier_term <- function(x, period, f = sin, order = 1) {
  f(2 * order * pi * x / period)
}

.hour_fourier_term <- function(...) {
  .fourier_term(order = 24, ...)
}

.wday_fourier_term <- function(...) {
  .fourier_term(order = 7, ...)
}

# We have to make an assumption about how many followers the account had when it started to make tweets frequently, which really wasn't until Jan. 2020. Hence `first_follower_count`.
.transform_tweets <- function(tweets, tm_accounts_mapping = .import_tm_accounts_mapping(), ..., train = TRUE, first_followers_count = 5000) {
  res_init <-
    tweets %>%
    select(
      status_id,
      created_at,
      retweet_count,
      favorite_count,
      text
    )
  
  latest_tweet <- tweets %>% slice_max(created_at)
  latest_followers_count <- latest_tweet$followers_count
  
  if(train) {
    
    followers_count_diff <- latest_followers_count - first_followers_count
    latest_date <- latest_tweet$created_at %>% lubridate::date()
    
    res_init <-
      res_init %>% 
      # This is a linear estimate of follower count at the tweet time.
      mutate(
        idx = row_number(created_at),
        estimated_followers_count = !!first_followers_count + round((idx / max(idx)) * !!followers_count_diff, 0)
      ) %>% 
      select(-idx)
  } else {
    res_init <-
      res_init %>% 
      mutate(estimated_followers_count = !!latest_followers_count)
  }
  
  res <-
    res_init %>% 
    # Drop half time scores, and just anything with commas or new lines since those aren't score line tweets.
    filter(text %>% str_detect('^HT|\\,|\\n', negate = TRUE)) %>%
    # We know that a score line tweet has this.
    filter(text %>% str_detect('\\(')) %>%
    mutate(
      across(favorite_count, list(log = ~log(.x + 1))),
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
          x1 = ~.hour_fourier_term(.x, f = sin, order = 1),
          y1 = ~.hour_fourier_term(.x, f = cos, order = 1),
          x2 = ~.hour_fourier_term(.x, f = sin, order = 2),
          y2 = ~.hour_fourier_term(.x, f = sin, order = 2),
        )
      ),
      across(
        wday,
        list(
          x1 = ~.wday_fourier_term(.x, f = sin, order = 1),
          y1 = ~.wday_fourier_term(.x, f = cos, order = 1),
          x2 = ~.wday_fourier_term(.x, f = sin, order = 2),
          y2 = ~.wday_fourier_term(.x, f = sin, order = 2),
        )
      ),
      across(
        text,
        list(
          tm_h = ~ .str_replace_text(.x, 1) %>% .remove_emoticons(),
          xg_h = ~ .str_replace_text(.x, 2) %>% as.numeric(),
          g_h = ~ .str_replace_text(.x, 3) %>% as.integer(),
          g_a = ~ .str_replace_text(.x, 4) %>% as.integer(),
          xg_a = ~ .str_replace_text(.x, 5) %>% as.numeric(),
          tm_a = ~ .str_replace_text(.x, 6) %>% .remove_emoticons()
        ),
        .names = '{fn}'
      )
    ) %>%
    # select(-text) %>% 
    # Drop non-score line tweets that weren't caught by previous filter.
    drop_na(xg_h, g_h, g_a, xg_a) %>% 
    .fix_tm_cols() %>% 
    .add_estimated_follower_count_cols(tm_accounts_mapping, latest_date = latest_date, train = train) %>% 
    select(-created_date) %>% 
    arrange(created_at) %>% 
    mutate(
      idx = row_number(created_at)
    ) %>% 
    relocate(idx)
  if(train) {
    res <-
      res %>% 
      mutate(wt = idx / sum(idx)) %>% 
      relocate(idx, wt)
  }
  res
}

# general modelling functions ----
.df2mat <- function(data, na.action = na.pass) {
  model.matrix(
    ~.+0,
    data =
      model.frame(
        ~.+0,
        data,
        na.action = na.action
      )
  )
}

.postprocess_xgb_cv_res <- function(res, fit_cv, eval_metrics) # , path) {
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
  # # browser()
  # log <- 
  #   fit_cv$evaluation_log %>% 
  #   as_tibble() %>% 
  #   rename(
  #     !!col_trn_res := !!col_trn,
  #     !!col_tst_res := !!col_tst
  #   )
  # # write_rds(log, path)
  
  res$iter <- fit_cv$best_iteration
  
  res[[col_trn_res]] = fit_cv$evaluation_log[res$iter][[col_trn]]
  res[[col_tst_res]] = fit_cv$evaluation_log[res$iter][[col_tst]]
  res[['eval_metric']] <- NULL
  # browser()
  bind_rows(res)
}

.tune_xgb_cv <-
  function(grid_params,
           x_dmat,
           booster,
           objective,
           eval_metrics,
           ...) {
    # path <- .generate_path(path = path, dir = dir, file = file, ext = ext)
    # 
    # path_exists <- path %>% file.exists()
    # if(path_exists & !overwrite) {
    #   .display_info('Returning object at `path = "{path}"`.')
    #   return(f_import(path))
    # }
    # 
    # basename <- basename(path)
    # file <- tools::file_path_sans_ext(basename)
    # dir <- dirname(path)
    # ext <- tools::file_ext(path)
    
    .get_metrics <- function(params, idx = 1) {

        # .path = .generate_path(path = NULL, dir = dir, file = sprintf('%s_%d', file, idx), ext = ext)
        # path_exists <- .path %>% file.exists()
        # if(path_exists & !overwrite) {
        #   .display_info('Returning early for `idx = {idx}`.')
        #   return(readr::read_rds(.path))
        # }
        .display_info('Row {cli::bg_black(idx)}')
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
            metrics = eval_metrics,
            ...
          )
        res <- .postprocess_xgb_cv_res(res, fit_cv, eval_metrics) # , path = .path)
        res
      }
    
    res <-
      grid_params %>%
      nest(params = -idx) %>%
      mutate(metrics = map2(params, idx, ~.get_metrics(params = ..1, idx = ..2))) %>%
      select(-params) %>%
      unnest(metrics)
    res
    # f_export(res, path)
    # res
  }

.inverse_log <- function(x) {
  exp(x) - 1
}

.augment_preds <-
  function(v,
           data,
           # stem = .get_valid_stems()
           cols_id = 'idx',
           cols_extra = NULL,
           col_y,
           f_trans = NULL,
           # ...,
           # dir = getwd(),
           # file = sprintf('preds_%s', stem),
           # ext = 'parquet',
           # path = NULL,
           # f_import = arrow::read_parquet,
           # f_export = arrow::write_parquet,
           # export = TRUE,
           # overwrite = TRUE
           ) {
    # # browser()
    # path <- .generate_path(path = path, dir = dir, file = file, ext = ext)
    # path_exists <- path %>% file.exists()
    # if(path_exists & !overwrite) {
    #   return(f_import(path))
    # }
    col_y_sym <- col_y %>% sym()
    preds <-
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
    
    if (!is.null(f_trans) & is.function(f_trans)) {
      preds <-
        preds %>%
        mutate(across(.pred, f_trans))
    }
    
    if(export) {
      f_export(preds, path)
    }
    preds
  }

.shap_xgb <- function(fit, x_mat, data, preds, ...) {
    
  feature_values_init <-
    x_mat %>%
    as.data.frame() %>% 
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
    bind_cols(data %>% select(idx)) %>%
    left_join(
      preds %>%
        mutate(idx = row_number())
    )
  shap_wide
}

# project-specific modeling functions ----
.generate_col_vec <- function(prefix, suffix) {
  crossing(
    prefix = prefix,
    suffix = suffix
  ) %>% 
    unite('col', prefix, suffix) %>% 
    arrange(col) %>% 
    pull(col)
}

.get_cols_lst <- function(stem = .get_valid_stems()) {
  .validate_stem(stem)
  suffixes <- .get_valid_suffixes()
  cols_suffix <-
    .generate_col_vec(
      prefix = c('g', 'xg', 'estimated_followers_count'),
      suffix = suffixes # .get_valid_suffixes(),
    )
  cols_time <-
    .generate_col_vec(
      prefix = c('hour', 'wday'),
      suffix = c('x1', 'y1', 'x2', 'y2')
    )
  
  cols_lst <-
    list(
      col_y = sprintf('%s_count_log', stem),
      # cols_id = 'status_id',
      cols_id = 'idx',
      col_wt = 'wt',
      col_strata = 'created_at',
      cols_extra = c('created_at', 'status_id', 'tm_h', 'tm_a', 'text', 'favorite_count', 'retweet_count'),
      cols_x = c('estimated_followers_count', cols_time, cols_suffix)
    )
  cols_lst
}

predict_one <- function(tweet, stem = .get_valid_stems(), ...) {
  .validate_stem(stem)
  cols_lst <- .get_cols_lst(stem = stem)
  data <- tweets %>% .transform_tweets(train = FALSE, ...)
  
  .path_data_x <- function(file, ext = NULL) {
    .path_data(file = sprintf('%s_%s', file, stem), ext = ext)
  }
  path_fit <- .path_data_x('fit')
  path_preds <- .path_data_parquet_x('preds')
  path_shap_wide <- .path_data_parquet_x('shap_wide')

  fit <- xgboost::xgb.load(path_fit)
  
  col_y_sym <- cols_lst$col_y %>% sym()
  x_mat <- data %>% select(one_of(c(cols_lst$cols_x))) %>% .df2mat()
  
  .f_predict <- function() {
    
    preds <-
      fit %>%
      predict(x_mat) %>%
      .augment_preds(
        data = data,
        cols_id = cols_lst$cols_id,
        cols_extra = cols_lst$cols_extra,
        col_y = cols_lst$col_y,
        f_trans = .inverse_log
      )
    preds
  }
  
  preds <- 
    .get_x(
      f = .f_predict,
      path = path_preds,
      f_import = arrow::read_parquet,
      f_export = arrow::write_parquet,
      append = TRUE,
      export = TRUE,
      overwrite = overwrite
    )

  .f_shap_wide <- function() {
    .shap_xgb(
      data = data,
      x_mat = x_mat,
      fit = fit,
      preds = preds
    )
  }
  
  shap_wide <-
    .get_x(
      f = .f_shap_wide,
      path = path_shap_wide,
      f_import = arrow::read_parquet,
      f_export = arrow::write_parquet,
      append = TRUE,
      export = TRUE,
      overwrite = overwrite
    )
}

do_fit_model <-
  function(tweets,
           stem = .get_valid_stems(),
           overwrite = FALSE) {
    
    # data = scores
    # stem = 'favorite'
    # overwrite = FALSE
    
    .validate_stem(stem)
    cols_lst <- .get_cols_lst(stem = stem)
    data <- tweets %>% .transform_tweets(train = TRUE, ...)
    
    .path_data_x <- function(file, ext = NULL) {
      .path_data(file = sprintf('%s_%s', file, stem), ext = ext)
    }
    .path_figs_png_x <- function(file) {
      .path_figs_png(file = sprintf('%s_%s', file, stem))
    }
    .path_data_parquet_x <- purrr::partial(.path_data_x, ext = 'parquet', ... = )
    path_res_tune_cv <- .path_data_x('res_tune_cv', ext = 'rds')
    path_fit <- .path_data_x('fit')
    path_preds <- .path_data_parquet_x('preds')
    path_shap_wide <- .path_data_parquet_x('shap_wide')

    col_y_sym <- cols_lst$col_y %>% sym()
    data <- data %>% drop_na(!!col_y_sym)
    
    x_mat <- data %>% select(one_of(c(cols_lst$cols_x))) %>% .df2mat()

    nrounds <- 2500
    booster <- 'gbtree'
    objective <- 'reg:squarederror'
    eval_metrics <- list('rmse')
    early_stopping_rounds <- 10
    print_every_n <- 10
    
    .f_tune <- function() {
      
      x_dmat <-
        xgboost::xgb.DMatrix(
          x_mat,
          weights = data[[cols_lst$col_wt]],
          label = data[[cols_lst$col_y]]
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
        left_join(data %>% select(!!col_strata_sym, idx)) %>%
        select(fold, idx) %>%
        split(.$fold) %>%
        purrr::map(~select(.x, -fold) %>% pull(idx))
      folds
      n_obs <- folds %>% flatten_int() %>% length()
      max_idx <- folds %>% flatten_int() %>% max()
      assertthat::assert_that(n_obs == max_idx)
      
      n_row <- 50
      grid_params <-
        dials::grid_latin_hypercube(
          dials::finalize(dials::mtry(), data),
          dials::min_n(),
          dials::tree_depth(),
          dials::learn_rate(),
          dials::loss_reduction(),
          sample_size = dials::sample_prop(),
          size = n_row
        ) %>%
        mutate(
          learn_rate = 0.1 * ((1:n_row) / n_row),
          mtry = mtry / ncol(data),
          idx = row_number()
        ) %>%
        relocate(idx)
      grid_params
      
      res_tune_cv <- 
        .tune_xgb_cv(
          nrounds = nrounds,
          stem = stem,
          grid_params = grid_params,
          folds = folds,
          x_dmat = x_dmat,
          booster = booster,
          objective = objective,
          eval_metrics = eval_metrics,
          early_stopping_rounds = early_stopping_rounds,
          print_every_n = print_every_n
        )
      res_tune_cv
    }
    
    res_tune_cv <- 
      .get_x(
        f = .f_tune, 
        path = path_res_tune_cv, 
        f_import = read_rds,
        f_export = write_rds,
        append = FALSE,
        export = TRUE,
        overwrite = overwrite
      )
    
    .f_fit <- function() {
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
      # nrounds_best <- nrounds
      fit <-
        xgboost::xgboost(
          params = params_best,
          data = x_dmat,
          nrounds = nrounds_best,
          early_stopping_rounds = early_stopping_rounds,
          print_every_n = print_every_n,
          verbose = 1
        )
    }
    
    fit <- 
      .get_x(
        f = .f_fit, 
        path = path_fit, 
        f_import = xgboost::xgb.load,
        f_export = xgboost::xgb.save,
        append = FALSE,
        export = TRUE,
        overwrite = overwrite
      )
    
    .f_predict <- function() {
      
      preds <-
        fit %>%
        predict(x_mat) %>%
        .augment_preds(
          data = data,
          cols_id = cols_lst$cols_id,
          cols_extra = cols_lst$cols_extra,
          col_y = cols_lst$col_y,
          f_trans = .inverse_log
        )
      preds
    }
    
    preds <- 
      .get_x(
        f = .f_predict,
        path = path_preds,
        f_import = arrow::read_parquet,
        f_export = arrow::write_parquet,
        append = FALSE,
        export = TRUE,
        overwrite = overwrite
      )
    
    # preds_aug <-
    #   preds %>% 
    #   mutate(
    #     foe = favorite_count - .pred,
    #     foe_frac = foe / .pred
    #   ) %>% 
    #   arrange(desc(foe_frac))
    # preds_aug
    # 
    # preds_aug %>% 
    #   arrange(desc(foe)) %>% 
    #   select(created_at, text, favorite_count, .pred)
    # 
    # preds %>% 
    #   ggplot() +
    #   aes(x = favorite_count, y = .pred) +
    #   geom_point() +
    #   geom_smooth()
    # 
    # preds %>% 
    #   mutate(
    #     prnk = percent_rank(favorite_count)
    #   ) %>% 
    #   ggplot() +
    #   aes(x = prnk, y = .pred) +
    #   geom_point()

    .f_shap_wide <- function() {
      .shap_xgb(
        data = data,
        x_mat = x_mat,
        fit = fit,
        preds = preds
      )
    }
    
    shap_wide <-
      .get_x(
        f = .f_shap_wide,
        path = path_shap_wide,
        f_import = arrow::read_parquet,
        f_export = arrow::write_parquet,
        append = FALSE,
        export = TRUE,
        overwrite = overwrite
      )
    
    # .f_shap_long <- function() {
    #   shap <-
    #     shap_wide %>%
    #     pivot_longer(
    #       one_of(cols_lst$cols_x), 
    #       names_to = 'feature', 
    #       values_to = 'shap_value'
    #     )
    #   shap
    #   write_parquet(shap_wide, path_shap)
    # }
    # shap <- .get_x(f = .f_shap_long = path_shap, overwrite = overwrite)
    # shap_agg_by_feature <-
    #   shap %>%
    #   group_by(feature) %>%
    #   summarize(
    #     across(shap_value, ~mean(abs(.x))),
    #   ) %>%
    #   ungroup() %>%
    #   mutate(
    #     across(shap_value, list(rnk = ~row_number(desc(.x))))
    #   ) %>%
    #   arrange(shap_value_rnk)
    # shap_agg_by_feature
    fit
  }
