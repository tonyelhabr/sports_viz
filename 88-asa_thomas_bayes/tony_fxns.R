
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


eb_adjust = function(df, ratio_metric='xg_ratio', xg_metric='xG', goal_metric='G',
                     shot_threshold=50, force_prior_shape=NULL, force_prior_rate=NULL,
                     metric_lo_cutoff = -999, metric_hi_cutoff = 999){
  prior_df <- filter(df, shots >= shot_threshold) 
  prior_df=prior_df[prior_df[[ratio_metric]] > metric_lo_cutoff,]
  prior_df=prior_df[prior_df[[ratio_metric]] < metric_hi_cutoff,]
  
  
  if(is.null(force_prior_shape)){
    ## Ignore the warning here.
    prior_distr <- fitdistr(
      prior_df[[ratio_metric]],
      dgamma,
      start = list(shape = 1, rate = 1)
    )
    
    prior_shape <- prior_distr$estimate[1]
    prior_rate <- prior_distr$estimate[2]
  }
  else{
    prior_shape=force_prior_shape
    prior_rate = force_prior_rate
  }
  
  
  df$eb_adj_ratio <- map2(
    df[[goal_metric]], df[[xg_metric]],
    simulate_posterior,
    .prior_shape=prior_shape,
    .prior_rate=prior_rate
  )
  
  res <- unnest_wider(df, eb_adj_ratio, names_sep = '_')
  return(res)
  
}
