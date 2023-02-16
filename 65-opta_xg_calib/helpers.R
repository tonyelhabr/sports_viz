
is_0_or_1 <- function(v) {
  x <- sort(unique(v))
  all(as.integer(x) %in% c(0L, 1L))
}

#' Make a calibration table
#'
#' @importFrom stats qbeta
#' @importFrom rlang ensym arg_match abort :=
#' @importFrom dplyr across group_by mutate summarize n
#'
#' @param data The data, including columns corresponding to `truth` and `estimate`.
#' @param truth The binary target variable. Should be a factor.
#' @param estimate The estimateability variable. Returned as a discrete variable.
#' @param ... Variables to group on
#' @param width The size of the groups.
#' @param alpha The percentile offset that is divided by 2 and subtracted/added to 5th and 95th percentiles for the beta distribution used for calculating Jeffrey's credible intervals. The default of 0.05 translates to 0.025 for `shape1` and 0.975 for `shape2` for `qbeta`.
#' @param event_level A single string. Either `"first"` or `"second"` to specify which level of `truth` to consider as the "event".
#' @return A `tibble` with columns for the `truth` (returned as numeric) and `estimate` (returned as a discrete variable), as well as `ci_lower` and `ci_upper` variables (corresponding to Jeffrey's credible intervals). `actual`, which is the actual frequency of `truth`, and `n`, the number of observations (by group, if `...` is not empty)
make_calibration_table <- function(
    data, 
    truth, 
    estimate,
    ...,
    width = 0.05, 
    alpha = 0.05, 
    event_level = c('first', 'second')
) {
  
  stopifnot('`width` should be less than 1' = width < 1)
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
  
  n_buckets <- round(1 / width)
  data |>
    dplyr::mutate(
      dplyr::across(.data[[estimate_sym]], ~round(.x * n_buckets) / n_buckets)
    ) |>
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


#' Make a calibration plot
#'
#' @param data the data, which should truthably be the result from `compute_calibration_table`
#' @param truth,actual,ci_lower,ci_upper,n columns in `data`
#' @inheritDotParams compute_calibration_table
#' @param ... additional expressions to pass to `filter`
#'
#' @return the plot
#' @export
make_calibration_plot <- function(data, truth, estimate, ..., ci_lower = 'ci_lower', ci_upper = 'ci_upper', n = 'n') {
  
  facet_vars <- ggplot2::vars(...)

  p <- data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = !!rlang::ensym(truth), 
        y = !!rlang::ensym(estimate)
      )
    )
  
  p <- p +
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
