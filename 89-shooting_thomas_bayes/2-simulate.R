## data manipulation
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)

## parallelization
library(parallel)
library(furrr)
library(future)

## other
library(withr)
library(MASS, include.only = 'fitdistr')

PROJ_DIR <- '89-shooting_thomas_bayes'

MIN_SHOT_THRESHOLD <- 50
N_CORES <- parallel::detectCores()
CORES_FOR_PARALLEL <- ceiling(N_CORES * 0.5)

estimate_gamma_prior <- function(x, ..., .start = list(shape = 1, rate = 1)) {
  suppressWarnings(
    MASS::fitdistr(
      x,
      stats::dgamma,
      start = .start,
      ...
    )
  )
}

do_estimate_gamma_prior <- function(
    shots,
    shot_metric,
    ratio_metric, 
    min_shot_threshold = MIN_SHOT_THRESHOLD,
    ratio_metric_low_cutoff = 0,
    ratio_metric_high_cutoff = 10
) {
  
  filt_shots <- shots[shots[[shot_metric]] >= min_shot_threshold,]
  filt_shots <- filt_shots[filt_shots[[ratio_metric]] > ratio_metric_low_cutoff,]
  filt_shots <- filt_shots[filt_shots[[ratio_metric]] < ratio_metric_high_cutoff,]
  estimate_gamma_prior(
    filt_shots[[ratio_metric]]
  )
}

simulate_gamma_posterior <- function(
    successes, 
    trials, 
    prior_shape, 
    prior_rate, 
    n_sims = 10000,
    seed = 42
) {
  posterior_shape <- prior_shape + successes
  posterior_rate <- prior_rate + trials
  withr::local_seed(seed)
  posterior_sample <- stats::rgamma(
    n = n_sims, 
    shape = posterior_shape, 
    rate = posterior_rate
  )
  list(
    mean = base::mean(posterior_sample),
    sd = stats::sd(posterior_sample),
    lower_ci_bound = unname(stats::quantile(posterior_sample, 0.1)),
    upper_ci_bound = unname(stats::quantile(posterior_sample, 0.9))
  )
}

gamma_gamma_eb_adjust <- function(
    shots, 
    ratio_metric = 'xg_ratio', 
    xg_metric = 'xg', 
    goal_metric = 'g',
    shot_metric = 'shots',
    forced_prior_shape = NULL,
    forced_prior_rate = NULL,
    ...
){
  
  if(is.null(forced_prior_shape) | is.null(forced_prior_rate)) {
    prior_distr <- do_estimate_gamma_prior(
      shots,
      ratio_metric = ratio_metric,
      shot_metric = shot_metric,
      ...
    )
    prior_shape <- prior_distr$estimate[1]
    prior_rate <- prior_distr$estimate[2]
  } else {
    prior_shape <- forced_prior_shape
    prior_rate <- forced_prior_rate
  }
  
  future::plan(
    future::multisession,
    workers = CORES_FOR_PARALLEL
  )
  shots$adj_ratio <- purrr::map2(
    shots[[goal_metric]],
    shots[[xg_metric]],
    function(.x, .y) {
      simulate_gamma_posterior(
        successes = .x,
        trials = .y,
        prior_shape = prior_shape,
        prior_rate = prior_rate,
        ...
      )
    }
  )
  future::plan(future::sequential)
  
  tidyr::unnest_wider(
    shots,
    adj_ratio, 
    names_sep = '_'
  )
}

simulate_xg_ratio_sensitivity <- function(
    shots, 
    prior_distr,
    shot_seq = seq(50, 50000, by = 25),
    xg_metric = 'xg',
    ratio_metric = 'xg_ratio', 
    shot_metric = 'shots',
    quantiles = c(0.01, seq(0.05, 0.65, 0.05)),
    ...
){
  
  quantile_values <- quantile(shots[[ratio_metric]], quantiles)
  median_xg_per_shot <- median(shots[[xg_metric]] / shots[[shot_metric]])
  median_ratio <- median(shots[[ratio_metric]]) 
  
  quantile_labels <- tibble::tibble(
    xg_ratio = quantile_values,
    percentile = names(quantile_values) |>
      stringr::str_remove('\\%') |>
      as.integer() |>
      purrr::map_dbl(
        \(.x) {
          100 - .x
        }
      ) |> 
      scales::ordinal()
  ) |> 
    dplyr::mutate(
      percentile = forcats::fct_reorder(percentile, -xg_ratio)
    )
  
  grid <- tidyr::crossing(
    shots = shot_seq,
    xg_ratio = quantile_values
  ) |> 
    dplyr::mutate(
      xg = shots * !!median_xg_per_shot,
      goals = xg * xg_ratio
    ) |> 
    dplyr::left_join(
      quantile_labels,
      by = dplyr::join_by(xg_ratio)
    )
  
  grid |> 
    gamma_gamma_eb_adjust(
      ratio_metric = 'xg_ratio',
      xg_metric = 'xg',
      goal_metric = 'goals',
      shot_metric = shot_metric,
      forced_prior_shape = prior_distr$estimate[1],
      forced_prior_rate = prior_distr$estimate[2],
      ...
    ) |> 
    dplyr::mutate(
      median_xg_per_shot = !!median_xg_per_shot,
      median_xg_ratio = !!median_ratio,
      adj_ratio_diff_from_one = median_ratio - adj_ratio_upper_ci_bound
    )
}

## main ----
shots <- qs::qread(file.path(PROJ_DIR, 'shots.qs'))

agg_shots <- shots |> 
  dplyr::rename(
    shot_g = g, 
    shot_xg = xg,
    shot_xgot = xgot
  ) |> 
  dplyr::group_by(
    player_href,
    player
  ) |> 
  dplyr::summarize(
    shots = n(),
    penalties = sum(is_penalty),
    np_shots = sum(!is_penalty),
    sot = sum(is_on_target),
    npsot = sum(is_on_target * !is_penalty),
    g = sum(shot_g, na.rm = TRUE),
    npg = sum(shot_g * !is_penalty, na.rm = TRUE),
    xg = sum(shot_xg, na.rm = TRUE),
    psxg = sum(shot_xgot, na.rm = TRUE),
    npxg = sum(shot_xg * !is_penalty, na.rm = TRUE),
    nppsxg = sum(shot_xgot * !is_penalty, na.rm = TRUE)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    # xg_ratio = g / xg,
    # psxg_ratio = psxg / xg
    xg_ratio = npg / npxg,
    psxg_ratio = nppsxg / npxg
  )

filt_agg_shots <- agg_shots |> 
  dplyr::filter(shots >= MIN_SHOT_THRESHOLD)

xg_prior_distr <- filt_agg_shots |> 
  do_estimate_gamma_prior(
    shot_metric = 'shots',
    ratio_metric = 'xg_ratio'
  )

psxg_prior_distr <- filt_agg_shots |> 
  do_estimate_gamma_prior(
    shot_metric = 'shots',
    ratio_metric = 'psxg_ratio'
  )

simulated_xg_ratio <- filt_agg_shots |> 
  simulate_xg_ratio_sensitivity(
    prior_dist = xg_prior_distr,
    xg_metric = 'xg', 
    ratio_metric = 'xg_ratio'
  ) |> 
  dplyr::mutate(metric = 'G/xG')

simulated_psxg_ratio <- filt_agg_shots |> 
  simulate_xg_ratio_sensitivity(
    prior_dist = psxg_prior_distr,
    xg_metric = 'psxg', 
    ratio_metric = 'psxg_ratio'
  ) |> 
  dplyr::mutate(metric = 'PSxG/xG')

simulated_xg_ratio_sensitivities <- dplyr::bind_rows(
  simulated_psxg_ratio,
  simulated_xg_ratio
) |> 
  dplyr::filter(adj_ratio_diff_from_one > 0) |> 
  dplyr::group_by(metric, xg_ratio) |> 
  dplyr::slice_min(n = 1, order_by = shots) |> 
  dplyr::ungroup()

qs::qsave(
  simulated_xg_ratio_sensitivities, 
  file.path(PROJ_DIR, 'simulated_xg_ratio_sensitivities.qs')
)
