library(DBI, include.only = 'dbConnect')
library(RPostgres, include.only = 'Postgres')
library(dbplyr, include.only = 'in_schema')
library(MASS, include.only = 'fitdistr')
library(purrr)
library(tidyr, include.only = 'unnest_wider')
library(ggplot2)
library(dplyr)
library(withr, include.only = 'local_seed')
library(forcats, include.only = 'fct_reorder')

proj_dir <- '73-exceeding_expected_goals_replication'
get_env_var <- function(var) {
  env_vars <- Sys.getenv()
  if (var %in% names(env_vars)) {
    env_vars[[var]]
  } else {
    warning(
      paste(
        'No environment variable named', var, 'found.'
      )
    )
  }
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
    force_prior_shape = NULL,
    force_prior_rate = NULL,
    metric_lo_cutoff = 0, 
    metric_hi_cutoff = 10
){
  prior_df <- df[df[[shot_metric]] >= shot_threshold,]
  prior_df <- prior_df[prior_df[[ratio_metric]] > metric_lo_cutoff,]
  prior_df <- prior_df[prior_df[[ratio_metric]] < metric_hi_cutoff,]
  
  
  if(is.null(force_prior_shape)){
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
  } else {
    prior_shape = force_prior_shape
    prior_rate = force_prior_rate
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

## Truncate John Doe to J. Doe
truncate_name <- function(full_name) {
  name_parts <- strsplit(full_name, ' ')[[1]]
  
  if (length(name_parts) < 2) {
    return(full_name)
  }
  
  first_initial <- substr(name_parts[1], 1, 1)
  paste(first_initial, '.', name_parts[length(name_parts)])
}

## main ----
con <- dbConnect(
  Postgres(),
  user = get_env_var('asa_user'),
  password = get_env_var('asa_password'),
  host = get_env_var('asa_host'),
  port = as.integer(get_env_var('asa_port')),
  dbname = get_env_var('asa_db_name'),
  sslmode = get_env_var('asa_sslmode')
)

raw_shooter_xg <- tbl(con, in_schema('mls', 'xgoals')) |>
  filter(!is.na(xg_shooter)) |>
  select(game_id, xg = xg_shooter, shooter_id, goalkeeper_id, g = goal) |>
  rename(player_id = shooter_id) |>
  left_join(
    tbl(con, in_schema('all_opta', 'players')) |>
      select(player_id, player = player_name),
    by = join_by(player_id)
  ) |>
  left_join(
    tbl(con, in_schema('mls', 'games')) |>
      select(game_id, season_name),
    by = join_by(game_id)
  ) |>
  as_tibble()|>
  filter(season_name %in% as.character(2021L:2023L))

agg_shooter_xg <- raw_shooter_xg |>
  group_by(player_id, player) |>
  mutate(g = as.integer(!is.na(g))) |> 
  summarise(
    xg = sum(xg),
    g = sum(g),
    shots = n()
  ) |> 
  ungroup() |> 
  mutate(
    xg_ratio = g / xg
  )

adjusted_shooter_xg <- gamma_gamma_eb_adjust(
  agg_shooter_xg,
  ratio_metric = 'xg_ratio',
  xg_metric = 'xg',
  goal_metric = 'g',
  shot_threshold = 50
)

top_adjusted_shooters <- adjusted_shooter_xg |> 
  filter(shots >= 100) |> 
  slice_max(adj_ratio_mean, n = 20, with_ties = FALSE) |> 
  arrange(desc(adj_ratio_mean)) |> 
  mutate(
    player = fct_reorder(player, adj_ratio_mean)
  )

plot_estimates <- function(data) {
  ggplot(data) +
    aes(y = player) +
    geom_vline(
      aes(xintercept = 1), 
      # linetype = 2,
      linewidth = 1.5,
      color = '#6E7275'
    ) +
    geom_errorbarh(
      aes(
        xmin = adj_ratio_mean - adj_ratio_sd,
        xmax = adj_ratio_mean + adj_ratio_sd
      ),
      color = 'white',
      height = 0.5
    ) +
    geom_point(
      aes(x = adj_ratio_mean, size = shots),
      color = 'white'
    )
}

theme_asa <- function(...) {
  list(
    ...,
    ## https://analysisevolved.slack.com/archives/CF8JS86Q7/p1690918539121609
    theme_minimal(base_family = 'Bebas Neue'),
    theme(
      text = element_text(color = 'white', size = 20),
      axis.text.y = element_text(color = 'white'),
      axis.text.x = element_text(color = 'white', size = 11),
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = 'top',
      plot.title = element_text(size = 24, hjust = 0),
      plot.title.position = 'plot',
      plot.caption.position = 'plot',
      plot.subtitle = element_text(color = '#6E7275', size = 14),
      plot.caption = element_text(color = '#6E7275', size = 11),
      panel.border = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = '#6E7275'),
      panel.background = element_rect(fill='#15202B', color='#15202B'),
      plot.background = element_rect(fill='#15202B', color='#15202B')
    ),
    labs(
      caption = 'Top 20 players according to adjusted G/xG ratio.',
      y = NULL,
      x = 'Adjusted G/xG Ratio'
    )
  )
}

top_adjusted_shooters_plot <- top_adjusted_shooters |>
  plot_estimates() +
  theme_asa() +
  labs(
    title = 'Top 20 shooting overperformers in the MLS',
    subtitle = 'Since beginning of 2021 season. Minimum 100 shots.'
  )

top_adjusted_shooters_plot_path <- file.path(proj_dir, 'mls-top-overperformers.png')
asa_logo_path <- file.path(proj_dir, 'ASAlogo.png')
ggsave(
  top_adjusted_shooters_plot,
  filename = top_adjusted_shooters_plot_path,
  units = 'in',
  width = 8,
  height = 8
)

add_logo(
  top_adjusted_shooters_plot_path,
  path_logo = asa_logo_path,
  delete = FALSE,
  path_suffix = '',
  logo_scale = 0.2,
  idx_x = 0.98,
  idx_y = 0.05,
  adjust_x = FALSE,
  adjust_y = FALSE
)

top_shrunken_adjusted_shooters <- adjusted_shooter_xg |> 
  filter(
    shots >= 25,
    shots < 100
  ) |> 
  slice_max(adj_ratio_mean, n = 20, with_ties = FALSE) |> 
  arrange(desc(adj_ratio_mean)) |> 
  mutate(
    player = fct_reorder(player, adj_ratio_mean)
  )

top_shrunken_adjusted_shooters

top_shrunken_adjusted_shooters_plot <- top_shrunken_adjusted_shooters |> 
  plot_estimates() +
  geom_curve(
    aes(
      x = xg_ratio,
      xend = adj_ratio_mean,
      yend = player
    ),
    color = '#01C4E7',
    curvature = 0.05,
    arrow = arrow(length = unit(0.02, 'npc'))
  ) +
  geom_point(
    aes(x = xg_ratio, size = shots),
    color = '#01C4E7'
  ) +
  geom_text(
    data = tibble(
      player = 'Javier López',
      xg_ratio = 2.9,
      label = 'Raw G/xG ratio'
    ),
    aes(x = xg_ratio, label = label),
    family = 'Bebas Neue',
    size = 14 / .pt,
    color = '#01C4E7',
    hjust = 1.1,
    vjust = 1
  ) +
  geom_curve(
    data = tibble(
      y = 'Javier López',
      yend = 'Júnior Moreno',
      x = 2.9,
      xend = 3.1,
    ),
    aes(
      y = y,
      yend = yend,
      x = x,
      xend = xend
    ),
    color = '#6E7275',
    curvature = 0,
    arrow = arrow(length = unit(0.02, 'npc'))
  ) +
  geom_text(
    data = tibble(
      player = 'Mikael Uhre',
      xg_ratio = 2.35,
      label = '"Shrinkage"'
    ),
    aes(x = xg_ratio, label = label),
    family = 'Bebas Neue',
    size = 14 / .pt,
    color = '#01C4E7',
    hjust = 0.5,
    vjust = 0
  ) +
  geom_curve(
    data = tibble(
      y = 'Gabriel Pereira',
      yend = 'Júnior Moreno',
      x = 2.35,
      xend = 2.35,
    ),
    aes(
      y = y,
      yend = yend,
      x = x,
      xend = xend
    ),
    color = '#6E7275',
    curvature = 0,
    arrow = arrow(length = unit(0.02, 'npc'))
  ) +
  theme_asa() +
  theme(
    plot.title = ggtext::element_markdown()
  ) +
  labs(
    title = 'Top 20 low volume shooting overperformers in the MLS',
    subtitle = 'Since beginning of 2021 season. Minimum 10 shots. Maximum 100 shots.'
  )
top_shrunken_adjusted_shooters_plot

top_shrunken_adjusted_shooters_plot_path <- file.path(proj_dir, 'mls-top-overperformers-shrunken.png')
ggsave(
  top_shrunken_adjusted_shooters_plot,
  filename = top_shrunken_adjusted_shooters_plot_path,
  units = 'in',
  width = 8,
  height = 8
)

add_logo(
  top_shrunken_adjusted_shooters_plot_path,
  path_logo = asa_logo_path,
  delete = FALSE,
  path_suffix = '',
  logo_scale = 0.2,
  idx_x = 0.98,
  idx_y = 0.05,
  adjust_x = FALSE,
  adjust_y = FALSE
)

## Júnior Moreno: midfielder
## Javier López: midfielder
## Lalas Abubakar: centerback
## Gabriel Pereir: midfielder
## Mikael Uhre: forward
## Jonathan Osorio: midfielder
## Frankie Amaya: midfielder
## Ismael Tajouri-Shradi: winger
## Christian Ramirez: forward
## Juan José Purata: centerback
## Ryan Hollingshead: rightback