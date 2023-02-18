
is_0_or_1 <- function(v) {
  x <- sort(unique(v))
  all(as.integer(x) %in% c(0L, 1L))
}

make_calibration_table <- function(
    data, 
    truth, 
    estimate,
    ...,
    alpha = 0.05, 
    event_level = c('first', 'second')
) {

  event_level <- rlang::arg_match(event_level)
  truth_sym <- rlang::ensym(truth)
  estimate_sym <- rlang::ensym(estimate)
  
  truth_class <- class(data[[truth_sym]])
  truth_is_usable <- truth_class[1] %in% c('logical', 'factor', 'numeric')
  
  if (isFALSE(truth_is_usable)) {
    rlang::abort(
      sprintf(
        '`truth = %s` must be a numeric, logical, or binary factor, not a "%s"`.',
        as.character(truth_sym),
        truth_class[1]
      )
    )
  }
  
  is_factor <- truth_class[1] == 'factor'
  
  if (is_factor) {
    lvls <- levels(data[[truth_sym]])
    n_lvls <- length(lvls)
    if(n_lvls != 2) {
      rlang::abort(
        sprintf(
          'Since the truth variable `%s` is a factor, it may only have 2 classes not %d.',
          as.character(truth_sym),
          n_lvls
        )
      )
    }
    
    data <- dplyr::mutate(
      data,
      dplyr::across(.data[[truth_sym]], ~as.numeric(.x) - 1)
    )
  }
  
  is_0_or_1 <- is_0_or_1(data[[truth_sym]])
  if (!is_factor & !is_0_or_1) {
    rlang::abort(
      sprintf(
        "truth variable `%s` must have only 0 or 1 values.",
        as.character(truth_sym)
      )
    )
  }
  
  if (event_level == "first") {
    data <- dplyr::mutate(
      data,
      dplyr::across(
        .data[[truth_sym]],
        ~dplyr::case_when(
          .x == 0 ~ 1,
          .x == 1 ~ 0
        )
      )
    )
  }
  
  data |>
    dplyr::group_by(.data[[estimate_sym]], ...) |>
    dplyr::summarize(
      n = dplyr::n(),
      ## Jeffreys prior
      ci_lower = stats::qbeta(alpha / 2, sum(!!truth_sym) + 0.5, .data[['n']] - sum(!!truth_sym) + 0.5),
      ci_upper = stats::qbeta(1 - alpha / 2, sum(!!truth_sym) + 0.5, .data[['n']] - sum(!!truth_sym) + 0.5),
      !!truth_sym := sum(.data[[truth_sym]]) / .data[['n']]
    ) |> 
    dplyr::ungroup()
}

make_calibration_plot <- function(data, truth, estimate, ..., ci_lower = 'ci_lower', ci_upper = 'ci_upper', n = 'n') {
  
  facet_vars <- ggplot2::vars(...)

  p <- data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = !!rlang::ensym(estimate), 
        y = !!rlang::ensym(truth)
      )
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        size = !!rlang::ensym(n)
      )
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = !!rlang::ensym(ci_lower), 
        ymax = !!rlang::ensym(ci_upper)
      ), 
      width = 0.025, 
      alpha = 0.5
    ) +
    ggplot2::geom_abline(
      slope = 1, 
      intercept = 0
    ) +
    ggplot2::lims(
      x = c(-0.025, 1.025),
      y = c(-0.025, 1.025)
    ) +
    ggplot2::labs(
      x = 'Probability',
      y = 'Actual Proportion',
      caption = 'Error bars represent a 95% posterior credible interval for the mean predicted chance. Beta-Binomial conjugate, Jeffreys Prior.'
    )
  
  if (length(facet_vars) != 0) {
    p <- p + ggplot2::facet_wrap(facet_vars)
  }
  p
}

mse <- function(data, ...) {
  UseMethod('mse')
}

mse <- yardstick::new_numeric_metric(mse, direction = 'minimize')

mse_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  mse_impl <- function(truth, estimate) {
    mean((truth - estimate)^2)
  }
  
  yardstick::metric_vec_template(
    metric_impl = mse_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = 'numeric',
    ...
  )
}

mse.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  yardstick::metric_summarizer(
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
  
  if (length(estimate) == 1) {
    estimate <- rep(estimate, length(truth))
  }
  
  yardstick::metric_vec_template(
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
  yardstick::metric_summarizer(
    metric_nm = 'brier_score',
    metric_fn = brier_score_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
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
    truth_quo <- rlang::enquo(truth)
    
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
  
  if (length(estimate) == 1) {
    estimate <- rep(estimate, length(truth))
  }
  
  if (length(ref_estimate) == 1) {
    ref_estimate <- rep(ref_estimate, length(truth))
  }
  
  yardstick::metric_vec_template(
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
  yardstick::metric_summarizer(
    metric_nm = 'brier_skill_score',
    metric_fn = brier_skill_score_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm = na_rm,
    event_level = event_level,
    metric_fn_options = list(
      ref_estimate = rlang::enquo(ref_estimate)
    ),
    ...
  )
}

## probably ----
## Reference: https://github.com/tidymodels/probably/blob/main/R/cal-plot.R
## This is like .cal_groups
process_cut <- function(.data, truth, estimate, cuts, lev, conf_level) {
  truth <- rlang::enquo(truth)
  estimate <- rlang::enquo(estimate)
  
  cuts %>%
    purrr::transpose() %>%
    purrr::map_df(
      ~ {
        .data |>
          dplyr::filter(
            !!estimate >= !!.x$lower_cut & !!estimate <= !!.x$upper_cut
          ) |>
          probably:::process_midpoint(
            truth = !!truth,
            estimate = !!estimate,
            level = lev,
            conf_level = conf_level
          ) |>
          dplyr::mutate(
            lower_bound = .x$lower_cut,
            upper_bound = .x$upper_cut,
            predicted_midpoint = .x$lower_cut + ((.x$upper_cut - .x$lower_cut) / 2),
            .before = 1
          )
      }
    )
}

## This is like cal_plot_breaks_impl + .cal_binary_table_breaks_impl + .cal_binary_table_breaks_grp
make_cal_table <- function(
    .data,
    truth = NULL,
    estimate = NULL,
    cut_fn = ggplot2::cut_width,
    cut_args = list(
      width = 0.1
    ),
    conf_level = 0.90,
    event_level = c('first', 'second')
) {
  truth <- rlang::enquo(truth)
  estimate <- rlang::enquo(estimate)
  event_level <- rlang::arg_match(event_level)
  # assert_truth_two_levels(.data, !!truth)
  
  side <- rlang::exec(
    cut_fn, 
    !!!append(
      list(x = dplyr::pull(.data, !!estimate)),
      cut_args
    )
  )
  
  ## https://stackoverflow.com/questions/32356108/get-lower-and-upper-bounds-from-cut-as-numeric-values
  lvls <- levels(side)
  pattern <- '(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])'
  cuts <- list(
    lower_cut = as.numeric(gsub(pattern, '\\2', lvls)),
    upper_cut = as.numeric(gsub(pattern, '\\3', lvls))
  )
  
  lev <- ifelse(event_level == 'first', 1, 2)
  process_cut(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    cuts = cuts,
    lev = lev,
    conf_level = conf_level
  )
}
