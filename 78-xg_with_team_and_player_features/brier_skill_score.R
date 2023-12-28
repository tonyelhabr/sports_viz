library(yardstick)
library(rlang)

brier_skill_score <- function(data, ...) {
  UseMethod('brier_skill_score')
}

brier_skill_score <- yardstick::new_prob_metric(
  brier_skill_score, 
  direction = 'maximize'
)

bss <- function(
    truth, 
    estimate, 
    ref_estimate, 
    event_level,
    case_weights,
    ...
) {
  
  if (length(estimate) == 1) {
    estimate <- rep(estimate, length(truth))
  }
  
  if (length(ref_estimate) == 1) {
    ref_estimate <- rep(ref_estimate, length(truth))
  }
  
  estimate_brier_score <- brier_class_vec(
    truth = truth,
    estimate = estimate,
    event_level = event_level,
    case_weights = case_weights,
    ...
  )
  
  ref_brier_score <- brier_class_vec(
    truth = truth,
    estimate = ref_estimate,
    event_level = event_level,
    case_weights = case_weights,
    ...
  )
  
  1 - (estimate_brier_score / ref_brier_score)
}

brier_skill_score_estimator_impl <- function(
    truth, 
    estimate, 
    ref_estimate, 
    event_level,
    case_weights
) {
  bss(
    truth = truth,
    estimate = estimate,
    ref_estimate = ref_estimate,
    event_level = event_level,
    case_weights = case_weights
  )
}


brier_skill_score_vec <- function(
    truth, 
    estimate, 
    ref_estimate, 
    na_rm = TRUE, 
    event_level = yardstick:::yardstick_event_level(),
    case_weights = NULL, 
    ...
) {
  
  yardstick:::abort_if_class_pred(truth)
  
  estimator <- yardstick::finalize_estimator(
    truth, 
    metric_class = 'brier_skill_score'
  )
  
  yardstick::check_prob_metric(truth, estimate, case_weights, estimator)
  
  if (na_rm) {
    result <- yardstick::yardstick_remove_missing(truth, estimate, case_weights)
    
    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }
  
  brier_skill_score_estimator_impl(
    truth = truth,
    estimate = estimate,
    ref_estimate = ref_estimate,
    event_level = event_level,
    case_weights = case_weights
  )
}

brier_skill_score.data.frame <- function(
    data, 
    truth, 
    ...,
    ref_estimate = 0.5,
    na_rm = TRUE,
    event_level = yardstick:::yardstick_event_level(),
    case_weights = NULL
) {
  yardstick::prob_metric_summarizer(
    name = 'brier_skill_score',
    fn = brier_skill_score_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    ...,
    na_rm = na_rm,
    event_level = event_level,
    case_weights = !!rlang::enquo(case_weights),
    fn_options = list(
      ref_estimate = ref_estimate
    )
  )
}

fixed_brier_skill_score <- function(data, ...) {
  UseMethod('fixed_brier_skill_score')
}

# REF_ESTIMATE <- pen_play_shots |> 
#   dplyr::summarize(goal_rate = sum(scores == 'yes') / dplyr::n()) |> 
#   dplyr::pull(goal_rate)
REF_ESTIMATE <- 0.12

fixed_brier_skill_score.data.frame <- function(...) {
  brier_skill_score(
    ref_estimate = REF_ESTIMATE,
    ...
  )
}

fixed_brier_skill_score <- yardstick::new_prob_metric(
  fixed_brier_skill_score,
  direction = 'maximize'
)
