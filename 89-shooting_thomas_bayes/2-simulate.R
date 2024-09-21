## data scrape
library(worldfootballR)

## data manipulation
library(dplyr)
library(lubridate)
library(tidyr)

PROJ_DIR <- '89-shooting_thomas_bayes'
shots <- qs::qread(file.path(PROJ_DIR, 'shots.qs'))

# distinct_players <- shots |> 
#   dplyr::distinct(
#     season,
#     country,
#     gender,
#     tier,
#     team,
#     player
#   )
# distinct_players |> 
#   dplyr::count(
#     season,
#     country,
#     gender,
#     tier,
#     player
#   ) |> 
#   dplyr::filter(n > 1)

agg_shots <- shots |> 
  dplyr::group_by(
    player_href,
    player
  ) |> 
  dplyr::summarize(
    shots = n(),
    np_shots = sum(!is_penalty),
    sot = sum(is_on_target),
    npsot = sum(is_on_target * !is_penalty),
    g = sum(g, na.rm = TRUE),
    xg = sum(xg, na.rm = TRUE),
    psxg = sum(xgot, na.rm = TRUE),
    npxg = sum(xg * !is_penalty, na.rm = TRUE),
    nppsxg = sum(xgot * !is_penalty, na.rm = TRUE)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    xg_ratio = g / xg,
    psxg_ratio = g / psxg
  )

filt_agg_shots <- agg_shots |> 
  dplyr::filter(shots > 50)

xg_prior_dist <- filt_agg_shots |> 
  dplyr::filter(
    shots > 50
  ) |> 
  dplyr::filter(xg_ratio > 0) |> 
  dplyr::pull(xg_ratio) |> 
  MASS::fitdistr(
    dgamma,
    start = list(shape = 1, rate = 1)
  )

psxg_prior_dist <- filt_agg_shots |> 
  dplyr::filter(
    shots > 50
  ) |> 
  dplyr::filter(psxg_ratio > 0) |> 
  dplyr::pull(psxg_ratio) |> 
  MASS::fitdistr(
    dgamma,
    start = list(shape = 1, rate = 1)
  )

simulate_posterior <- function(successes, trials, n_sims = 10000, 
                               .prior_shape=prior_shape, .prior_rate=prior_rate) {
  posterior_shape <- .prior_shape + successes
  posterior_rate <- .prior_rate + trials
  posterior_sample <- rgamma(n = n_sims, shape = posterior_shape, rate = posterior_rate)
  list(
    mean = mean(posterior_sample),
    sd = sd(posterior_sample),
    lower_ci_bound = quantile(posterior_sample, .1) %>% unname,
    upper_ci_bound = quantile(posterior_sample, .9) %>% unname
  )
}

eb_adjust <- function(
    df, 
    ratio_metric='xg_ratio', 
    xg_metric='xG', goal_metric='G',
    shot_threshold=50, force_prior_shape=NULL, force_prior_rate=NULL,
    metric_lo_cutoff = -999, metric_hi_cutoff = 999
){
  prior_df <- dplyr::filter(df, shots >= shot_threshold) 
  prior_df=prior_df[prior_df[[ratio_metric]] > metric_lo_cutoff,]
  prior_df=prior_df[prior_df[[ratio_metric]] < metric_hi_cutoff,]
  
  
  if(is.null(force_prior_shape)){
    ## Ignore the warning here.
    prior_distr <- MASS::fitdistr(
      prior_df[[ratio_metric]],
      dgamma,
      start = list(shape = 1, rate = 1)
    )
    
    prior_shape <- prior_distr$estimate[1]
    prior_rate <- prior_distr$estimate[2]
  } else{
    prior_shape = force_prior_shape
    prior_rate = force_prior_rate
  }
  
  
  df$eb_adj_ratio <- purrr::map2(
    df[[goal_metric]], df[[xg_metric]],
    simulate_posterior,
    .prior_shape=prior_shape,
    .prior_rate=prior_rate
  )
  
  res <- tidyr::unnest_wider(df, eb_adj_ratio, names_sep = '_')
  return(res)
}

simulate_sensitivity <- function(
    ntile_df, 
    prior_dist,
    shotmin = 50, 
    shotmax = 100000, 
    shotstep = 25,
    xg_metric = 'xg',
    ratio_metric = 'xg_ratio', 
    quantiles = c(.01, seq(.05, 0.5, .05))
){
  
  quantile_values <- quantile(ntile_df[[ratio_metric]], quantiles)
  quantile_labels <- tibble::tibble(
    xg_ratio = quantile_values,
    percentile = names(quantile_values) |> 
      stringr::str_remove('\\%') |> 
      as.integer() |> 
      purrr::map_chr(
        \(.x) {
          sprintf('%s %%tile', scales::ordinal(100 - .x))
        }
      )
  )
  
  simulated_xg_ratio <- tidyr::crossing(
    shots = seq(shotmin, shotmax, shotstep),
    xg_per_shot = median(ntile_df[[xg_metric]] / ntile_df$shots),
    xg_ratio = quantile_values
  ) |> 
    dplyr::left_join(
      quantile_labels,
      by = dplyr::join_by(xg_ratio)
    ) |> 
    dplyr::mutate(
      xg = shots * xg_per_shot,
      goals = xg * xg_ratio
    ) |> 
    eb_adjust(
      ratio_metric = 'xg_ratio',
      xg_metric = 'xg',
      goal_metric = 'goals',
      shot_threshold = 50,
      force_prior_shape = prior_dist$estimate[1],
      force_prior_rate = prior_dist$estimate[2]
    ) |> 
    dplyr::mutate(
      upper_bound = eb_adj_ratio_upper_ci_bound,
      diff_from_one = median(ntile_df[[ratio_metric]]) - upper_bound
    ) |> 
    dplyr::select(shots, diff_from_one, percentile) |> 
    dplyr::mutate(metric = ratio_metric)
}

simulated_xg_ratio <- filt_agg_shots |> 
  simulate_sensitivity(
    prior_dist = xg_prior_dist,
    xg_metric = 'xg', 
    ratio_metric = 'xg_ratio'
  )

simulated_psxg_ratio <- filt_agg_shots |> 
  simulate_sensitivity(
    prior_dist = psxg_prior_dist,
    xg_metric = 'psxg', 
    ratio_metric = 'psxg_ratio'
  )

sens_plot_dat <- dplyr::bind_rows(
  simulated_psxg_ratio,
  simulated_xg_ratio
) |> 
  dplyr::filter(diff_from_one > 0) |> 
  dplyr::group_by(metric, percentile) |> 
  dplyr::slice_min(n = 1, order_by = shots) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    metric = ifelse(metric == 'psxg_ratio', 'G/PsxG', 'G/xG')
  )

library(ggplot2)
library(scales)
sens_plot_dat |> 
  ggplot(
    aes(
      x = shots, 
      y = percentile, 
      group = metric,
      fill = metric
    )
  ) +
  geom_bar(stat='identity', position='dodge') +
  labs(
    y = 'Performance percentile',
    x = 'Shots needed to detect difference from average performance',
    title = stringr::str_wrap('How many shots does a player need to take before we can say that they are a %th percentile finisher?', 60),
    caption = 'Data: Big 5 + MLS, 2017/2018 - 2023/2024.\nPlayers need >100k shots for 50th percentile exceedance.'
  ) +
  guides(
    fill = guide_legend(title = '')
  ) +
  geom_text(
    aes(label=comma(shots)),
    position = position_dodge(.9),
    hjust=-0.02,
    fontface='bold', 
    size=12 / .pt
  )  +
  # theme_gplus()+
  coord_cartesian(
    xlim = c(0, 30000),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 14),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.ticks = element_blank(),
    title = element_text(size=16),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title.position = 'plot',
    plot.caption.position = 'plot',
    plot.background = element_rect(fill = 'white', color = 'white'),
    panel.background =  element_rect(fill = 'white', color = 'white')
  )

ggsave(
  file.path(PROJ_DIR, 'shots_needed_detection.png'),
  units = 'in', 
  width = 8,
  height = 8
)
