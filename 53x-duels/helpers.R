
## calibration ----
library(rlang)
library(yardstick)

is_0_or_1 <- function(v) {
  x <- sort(unique(v))
  all(as.integer(x) %in% c(0L, 1L))
}

compute_calibration_table <- function(data, outcome = 'outcome', prob = 'prob', ..., width = 0.05, alpha = 0.05, event_level = c('second', 'first')) {
  
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
  
  is_factor <- outcome_class[1] == "factor"
  
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
  
  if(event_level == "first") {
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
      ## Jeffreys prior
      ci_lower = qbeta(alpha / 2, sum(!!outcome_sym) + .5, n() - sum(!!outcome_sym) + .5),
      ci_upper = qbeta(1 - alpha / 2, sum(!!outcome_sym) + .5, n() - sum(!!outcome_sym) + .5),
      actual = sum(!!outcome_sym) / n(),
      n = n(),
      .groups = 'drop'
    )
}

make_calibration_plot <- function(data, prob = 'prob', actual = 'actual', ..., ci_lower = 'ci_lower', ci_upper = 'ci_upper', n = 'n') {
  prob_sym <- ensym(prob)
  actual_sym <- ensym(actual)
  
  data %>%
    ggplot(aes(x = !!prob_sym, y = !!actual_sym)) +
    geom_errorbar(aes(ymin = !!ensym(ci_lower), ymax = !!ensym(ci_upper)), width = .025, alpha = .5) +
    geom_abline(slope = 1, intercept = 0) +
    lims(
      x = c(-.025, 1.025),
      y = c(-.025, 1.025)
    ) +
    labs(
      x = 'Probability',
      y = 'Actual Proportion'
    )
}

mse <- function(data, ...) {
  UseMethod("mse")
}

mse <- yardstick::new_numeric_metric(mse, direction = "minimize")

mse_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  mse_impl <- function(truth, estimate) {
    mean((truth - estimate)^2)
  }
  
  metric_vec_template(
    metric_impl = mse_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

mse.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  metric_summarizer(
    metric_nm = "mse",
    metric_fn = mse_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ...
  )
}

brier_score <- function(data, ...) {
  UseMethod("brier_score")
}

brier_score <- yardstick::new_prob_metric(brier_score, direction = "minimize")

brier_score_vec <- function(truth, estimate, na_rm = TRUE, event_level, ...) {
  
  brier_score_impl <- function(truth, estimate, event_level, ...) {
    truth <- 1 - (as.numeric(truth) - 1)
    
    if (event_level == "second") {
      truth <- 1 - truth
    }
    mean((truth - estimate)^2)
  }
  
  metric_vec_template(
    metric_impl = brier_score_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = c("factor", "numeric"),
    estimator = "binary",
    event_level = event_level,
    ...
  )
}

brier_score.data.frame <- function(data, truth, estimate, na_rm = TRUE, event_level = "first", ...) {
  metric_summarizer(
    metric_nm = "brier_score",
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
  UseMethod("brier_skill_score")
}

brier_skill_score <- yardstick::new_prob_metric(brier_skill_score, direction = "maximize")

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
    cls = c("factor", "numeric"),
    estimator = "binary",
    event_level = event_level,
    ...
  )
}

brier_skill_score.data.frame <- function(data, truth, estimate, ref_estimate, na_rm = TRUE, event_level = "first", ...) {
  metric_summarizer(
    metric_nm = "brier_skill_score",
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

## plot ----
blackish_background <- '#1c1c1c'
gray_points <- '#4d4d4d'
gray_text <- '#f1f1f1'

font <- 'Titillium Web'
extrafont::loadfonts(quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = font),
  title = element_text(size = 16, color = 'white'),
  plot.title = element_text(face = 'bold', size = 16, color = 'white', hjust = 0.5),
  plot.title.position = 'plot',
  plot.subtitle = element_text(size = 16, color = '#f1f1f1'),
  axis.text = element_text(color = 'white', size = 14),
  axis.title = element_text(size = 16, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = gray_points),
  panel.grid.minor = element_line(color = gray_points),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  strip.text = element_text(size = 14, color = 'white', face = 'bold', hjust = 0.5),
  plot.background = element_rect(fill = blackish_background, color = blackish_background),
  plot.caption = element_text(color = 'white', hjust = 1, size = 10, face = 'italic'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 14, color = 'white', hjust = 0),
  plot.tag.position = c(0.01, 0.02),
  legend.text = element_text(font, size = 14, color = 'white', face = 'bold'),
  legend.title = element_text(font, size = 14, face = 'bold'),
  legend.position = 'top',
  panel.background = element_rect(fill = blackish_background, color = blackish_background)
)
update_geom_defaults('text', list(family = font, size = 12 / .pt, fontface = 'bold', color = 'white'))
update_geom_defaults('point', list(color = 'white'))

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

.pitch_international <- list(
  length = 105,
  width = 68,
  penalty_box_length = 16.5,
  penalty_box_width = 40.32,
  six_yard_box_length = 5.5,
  six_yard_box_width = 18.32,
  penalty_spot_distance = 11,
  goal_width = 7.32,
  origin_x = 0,
  origin_y = 0
)

gray_wv <- rgb(24, 24, 24, maxColorValue = 255)
gray_grid_wv <- rgb(64, 64, 64, maxColorValue = 255)
common_gg <- function(...) {
  x_lab <- 105 / 2
  y_lab <- -3
  lab_width <- 20
  list(
    ...,
    aes(x = x, y = y),
    ggsoccer::annotate_pitch(
      dimensions = .pitch_international,
      colour = gray_grid_wv,
      fill = gray_wv
    ),
    annotate(
      'segment',
      x = x_lab - (lab_width / 2),
      y = y_lab,
      xend = x_lab + (lab_width / 2),
      yend = y_lab,
      arrow = arrow(length = unit(0.02, 'npc'), type = 'closed'),
      colour = gray_text
    ),
    annotate(
      'text',
      family = font,
      size = 14 / .pt,
      x = x_lab,
      y = y_lab - 1,
      label = 'Direction of play',
      vjust = 1.5,
      colour = gray_text
    ),
    # coord_flip(ylim = c(0, 68), xlim = c(105, 0)),
    theme(
      axis.title = element_text(size = 12, hjust = 0),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ),
    labs(
      y = NULL,
      x = NULL
    )
  )
}

