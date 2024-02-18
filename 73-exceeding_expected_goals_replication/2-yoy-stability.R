## data wrangling
library(worldfootballR)
library(dplyr)
library(tibble)
library(lubridate)

## distribution fitting and wrangling
library(MASS, include.only = 'fitdistr') ## to avoid `select` name conflict with dplyr
library(withr)
library(purrr)
library(tidyr)

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
  posterior_sample <- rgamma(n = n_sims, shape = posterior_shape, rate = posterior_rate)
  list(
    mean = mean(posterior_sample),
    sd = sd(posterior_sample)
  )
}

gamma_gamma_eb_adjust <- function(
    df, 
    ratio_metric = 'xg_ratio', 
    xg_metric = 'xg', 
    goal_metric = 'g',
    shot_metric = 'shots',
    shot_threshold = 50,
    forced_prior_shape = NULL,
    forced_prior_rate = NULL,
    metric_lo_cutoff = 0, 
    metric_hi_cutoff = 10
){
  
  if(is.null(forced_prior_shape) | is.null(forced_prior_rate)){
    prior_df <- df[df[[shot_metric]] >= shot_threshold,]
    prior_df <- prior_df[prior_df[[ratio_metric]] > metric_lo_cutoff,]
    prior_df <- prior_df[prior_df[[ratio_metric]] < metric_hi_cutoff,]
    
    ## Ignore the warning here.
    suppressWarnings(
      prior_distr <- fitdistr(
        prior_df[[ratio_metric]],
        dgamma,
        start = list(shape = 1, rate = 1)
      )
    )
    
    prior_shape <- prior_distr$estimate[1]
    prior_rate <- prior_distr$estimate[2]
    print(
      list(
        'shape' = prior_shape,
        'rate' = prior_rate
      )
    )
  } else {
    prior_shape <- forced_prior_shape
    prior_rate <- forced_prior_rate
  }
  
  df$adj_ratio <- map2(
    df[[goal_metric]], df[[xg_metric]],
    function(.x, .y) {
      simulate_gamma_posterior(
        successes = .x,
        trials = .y,
        prior_shape = prior_shape,
        prior_rate = prior_rate
      )
    }
  )
  
  res <- unnest_wider(df, adj_ratio, names_sep = '_')
  return(res)
}

## main ----
raw_shots <- purrr::map_dfr(
  c('1st', '2nd'),
  \(tier) {
    worldfootballR::load_fb_match_shooting(
      country = c('ENG', 'ESP', 'GER', 'ITA', 'FRA'),
      tier = tier,
      gender = 'M'
    )
  }
)

shots <- raw_shots |> 
  transmute(
    # season = sprintf('%s/%s', Season_End_Year - 1, substr(Season_End_Year , 3, 4)),
    season = Season_End_Year,
    country = Country,
    match_id = basename(dirname(MatchURL)),
    date = ymd(Date),
    period = as.integer(Match_Half),
    min = ifelse(
      grepl('[+]', Minute),
      as.integer(gsub('(^[0-9]+)[+]([0-9]+$)', '\\1', Minute)), 
      as.integer(Minute)
    ),
    min_added = ifelse(
      grepl('[+]', Minute), 
      as.integer(gsub('(^[0-9]+)[+]([0-9]+$)', '\\2', Minute)), 
      NA_integer_
    ),
    # is_home = Home_Away == 'Home',
    team = Squad,
    player_href = Player,
    player = Player,
    g = as.integer(Outcome == 'Goal'),
    xg = as.double(xG),
    is_penalty = coalesce((Distance == '13' & round(xg, 2) == 0.79), FALSE)
  ) |> 
  filter(season != 2024)

np_shots <- shots |> 
  filter(!is_penalty) |> 
  select(-is_penalty) |> 
  distinct()
np_shots

np_shots_by_player_season <- np_shots |> 
  group_by(season, player_href, player) |> 
  summarize(
    shots = n(),
    across(
      c(g, xg),
      \(.x) sum(.x, na.rm = TRUE)
    )
  ) |> 
  ungroup() |> 
  mutate(
    xg_ratio = g / xg
  )

adj_shooter_xg <- gamma_gamma_eb_adjust(
  np_shots_by_player_season,
  ratio_metric = 'xg_ratio',
  xg_metric = 'xg',
  goal_metric = 'g',
  # forced_prior_shape = 8.967011,
  # forced_prior_rate = 8.543405,
  shot_threshold = 50
)

yoy_adj_shooter_xg <- adj_shooter_xg |> 
  arrange(player, player_href, season) |> 
  group_by(player, player_href) |> 
  mutate(
    prev_season = lag(season),
    prev_xg_ratio = lag(xg_ratio),
    prev_adj_ratio_mean = lag(adj_ratio_mean)
  ) |> 
  ungroup() |> 
  filter(
    season == (prev_season + 1),
    !is.na(prev_adj_ratio_mean), 
    !is.na(adj_ratio_mean)
  )

library(ggplot2)
yoy_adj_shooter_xg |> 
  filter(shots >= 10) |> 
  ggplot() +
  aes(
    x = xg_ratio,
    y = adj_ratio_mean
  ) +
  geom_point(
    aes(
      size = sqrt(shots)
    )
  ) +
  facet_wrap(~season)

yoy_adj_shooter_xg |>
  select(
    prev_xg_ratio,
    xg_ratio
  ) |> 
  corrr::correlate()

yoy_adj_shooter_xg |>
  select(
    prev_adj_ratio_mean,
    adj_ratio_mean
  ) |> 
  corrr::correlate()
