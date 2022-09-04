
## calibration ----
library(rlang)
library(yardstick)

is_0_or_1 <- function(v) {
  x <- sort(unique(v))
  all(as.integer(x) %in% c(0L, 1L))
}

compute_calibration_table <- function(data, outcome, prob, ..., width = 0.05, alpha = 0.05, event_level = c('first', 'second')) {
  
  stopifnot('`width` should be less than 1' = width < 1)
  event_level <- match.arg(event_level)
  outcome_sym <- ensym(outcome)
  prob_sym <- ensym(prob)
  
  outcome_class <- class(data[[outcome_sym]])
  outcome_is_usable <- outcome_class[1] %in% c('logical', 'factor', 'numeric')
  
  if(!outcome_is_usable) {
    abort(
      sprintf(
        'Outcome variable `%s` must be a numeric, logical, or binary factor, not a "%s"`.',
        as.character(outcome_sym),
        outcome_class[1]
      )
    )
  }
  
  is_factor <- outcome_class[1] == 'factor'
  
  if(is_factor) {
    lvls <- levels(data[[outcome_sym]])
    n_lvls <- length(lvls)
    if(n_lvls != 2) {
      abort(
        sprintf(
          'Since the outcome variable `%s` is a factor, it may only have 2 classes not %d.',
          as.character(outcome_sym),
          n_lvls
        )
      )
    }
    
    data <- data %>%
      mutate(across(!!outcome_sym, ~as.numeric(.x) - 1))
  }
  
  is_0_or_1 <- is_0_or_1(data[[outcome_sym]])
  if(!is_factor & !is_0_or_1) {
    abort(
      sprintf(
        'Outcome variable `%s` must have only 0 or 1 values.',
        as.character(outcome_sym)
      )
    )
  }
  
  if(event_level == 'first') {
    data <- data %>%
      mutate(
        across(
          !!outcome_sym,
          ~case_when(
            .x == 0 ~ 1,
            .x == 1 ~ 0
          )
        )
      )
  }
  
  n_buckets <- round(1 / width)
  data %>%
    mutate(
      across(!!prob_sym, ~round(.x * n_buckets) / n_buckets)
    ) %>%
    group_by(!!prob_sym, ...) %>%
    summarize(
      ## Jeffrey's prior
      ci_lower = qbeta(alpha / 2, sum(!!outcome_sym) + 0.5, n() - sum(!!outcome_sym) + 0.5),
      ci_upper = qbeta(1 - alpha / 2, sum(!!outcome_sym) + 0.5, n() - sum(!!outcome_sym) + 0.5),
      actual = sum(!!outcome_sym) / n(),
      n = n(),
      .groups = 'drop'
    )
}

mse_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  mse_impl <- function(truth, estimate) {
    mean((truth - estimate)^2)
  }
  
  metric_vec_template(
    metric_impl = mse_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = 'numeric',
    ...
  )
}

mse <- function(data, ...) {
  UseMethod('mse')
}

mse <- yardstick::new_numeric_metric(mse, direction = 'minimize')

mse.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  metric_summarizer(
    metric_nm = 'mse',
    metric_fn = mse_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ...
  )
}

brier_score <- function(data, ...) {
  UseMethod('brier_score')
}

brier_score <- yardstick::new_prob_metric(brier_score, direction = 'minimize')

brier_score_vec <- function(truth, estimate, na_rm = TRUE, event_level, ...) {
  
  brier_score_impl <- function(truth, estimate, event_level, ...) {
    truth <- 1 - (as.numeric(truth) - 1)
    
    if (event_level == 'second') {
      truth <- 1 - truth
    }
    mean((truth - estimate)^2)
  }
  
  metric_vec_template(
    metric_impl = brier_score_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = c('factor', 'numeric'),
    estimator = 'binary',
    event_level = event_level,
    ...
  )
}

brier_score.data.frame <- function(data, truth, estimate, na_rm = TRUE, event_level = 'first', ...) {
  metric_summarizer(
    metric_nm = 'brier_score',
    metric_fn = brier_score_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    event_level = event_level,
    ...
  )
}

brier_skill_score <- function(data, ...) {
  UseMethod('brier_skill_score')
}

brier_skill_score <- yardstick::new_prob_metric(brier_skill_score, direction = 'maximize')

brier_skill_score_vec <- function(truth, estimate, ref_estimate, na_rm = TRUE, event_level, ...) {
  
  brier_skill_score_impl <- function(truth, estimate, ref_estimate, event_level, ...) {
    truth_quo <- enquo(truth)
    
    estimate_bs <- brier_score_vec(
      truth = truth,
      estimate = estimate,
      na_rm = na_rm,
      event_level = event_level,
      ...
    )
    
    ref_bs <- brier_score_vec(
      truth = truth,
      estimate = ref_estimate,
      na_rm = na_rm,
      event_level = event_level,
      ...
    )
    
    1 - (estimate_bs / ref_bs)
  }
  
  metric_vec_template(
    metric_impl = brier_skill_score_impl,
    truth = truth,
    estimate = estimate,
    ref_estimate = ref_estimate,
    cls = c('factor', 'numeric'),
    estimator = 'binary',
    event_level = event_level,
    ...
  )
}

brier_skill_score.data.frame <- function(data, truth, estimate, ref_estimate, na_rm = TRUE, event_level = 'first', ...) {
  metric_summarizer(
    metric_nm = 'brier_skill_score',
    metric_fn = brier_skill_score_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    event_level = event_level,
    metric_fn_options = list(
      ref_estimate = enquo(ref_estimate)
    ),
    ...
  )
}