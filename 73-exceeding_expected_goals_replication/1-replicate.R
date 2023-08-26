library(worldfootballR)
library(dplyr)
library(tibble)

## distribution fitting and wrangling
library(MASS, include.only = "fitdistr") ## to avoid `select` name conflict with dplyr
library(withr)
library(purrr)
library(tidyr)

raw_shots <- worldfootballR::load_understat_league_shots(league = 'EPL')
shots <- raw_shots |> 
  tibble::as_tibble() |> 
  dplyr::filter(
    season %in% c(2016L, 2017L), ## 2016/17 and 2017/18 seasons
    ## "excluding free-kicks" in the blog post
    situation != 'DirectFreeKick'
  ) |> 
  dplyr::arrange(id) |> 
  dplyr::transmute(
    id,
    player,
    xg = x_g,
    g = as.integer(result == 'Goal')
  )
shots

shots_by_player <- shots |> 
  dplyr::group_by(player) |> 
  dplyr::summarize(
    shots = dplyr::n(),
    dplyr::across(c(g, xg), sum)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(raw_g_xg_ratio = g / xg) |> 
  dplyr::arrange(dplyr::desc(shots))
shots_by_player

prior_shots_by_player <- dplyr::filter(
  shots_by_player, 
  shots >= 50,
  g > 0 ## prevent error with fitting prior distribution
)

prior_distr <- MASS::fitdistr(
  prior_shots_by_player$raw_g_xg_ratio,
  dgamma,
  start = list(shape = 1, rate = 1)
)
prior_shape <- prior_distr$estimate[1]
prior_rate <- prior_distr$estimate[2]
list(prior_shape = round(prior_shape, 2), prior_rate = round(prior_rate, 2))

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

shots_by_player$adj_g_xg_ratio <- purrr::map2(
  shots_by_player$g, shots_by_player$xg,
  function(g, xg) {
    simulate_gamma_posterior(
      successes = g,
      trials = xg,
      prior_shape = prior_shape,
      prior_rate = prior_rate
    )
  }
)

options(width = 120)
adj_shots_by_player <- shots_by_player |> 
  tidyr::unnest_wider(
    adj_g_xg_ratio, 
    names_sep = '_'
  ) |> 
  dplyr::arrange(dplyr::desc(adj_g_xg_ratio_mean))
adj_shots_by_player

shaw_players <- c(
  'Eden Hazard' = 'E. Hazard',
  'Mohamed Salah' = 'Mohamed Salah',
  'Son Heung-Min' = 'Heung-Min Son',
  'Joshua King' = 'J. King',
  'Romelu Lukaku' = 'R. Lukaku',
  'Harry Kane' = 'H. Kane',
  'Sadio Mané' = 'S. Mane',
  'Dele Alli' = 'D. Ali',
  'Riyad Mahrez' = 'R. Mahrez',
  'Christian Eriksen' = 'C. Eriksen',
  'Pedro' = 'Pedro',
  'Alexis Sánchez' = 'A. Sanchez',
  'Roberto Firmino' = 'Roberto Firmino',
  'Jamie Vardy' = 'J. Vardy',
  'Xherdan Shaqiri' = 'X. Shaqiri',
  'Wilfried Zaha' = 'W. Zaha',
  'Nathan Redmond' = 'N. Redmond',
  'Gylfi Sigurdsson' = 'G. Sigurdsson',
  'Kevin De Bruyne' = 'K. De Bruyne',
  'Andros Townsend' = 'A. Townsend',
  'Sergio Agüero' = 'S. Aguero',
  'Marcus Rashford' = 'M. Rashford',
  'Jermain Defoe' = 'J. Defoe',
  'Raheem Sterling' = 'R. Sterling',
  'Marko Arnautovic' = 'M. Arnautovic',
  'Paul Pogba' = 'P. Pogba',
  'Salomón Rondón' = 'S. Rondon',
  'Christian Benteke' = 'C. Benteke'
)


## plotting
library(ggplot2)
library(forcats)
library(ggh4x)

## Not shown, but `shaw_players` is a named vector of player names, mapping
##    understat names to the names in Shaw's plot
ordinal_adj_shots_by_player <- adj_shots_by_player |>
  filter(
    player %in% names(shaw_players)
  ) |> 
  mutate(
    player = fct_reorder(shaw_players[player], adj_g_xg_ratio_mean)
  )


p <- ordinal_adj_shots_by_player |>
  ggplot() +
  aes(y = player) +
  geom_errorbarh(
    aes(
      xmin = adj_g_xg_ratio_mean - adj_g_xg_ratio_sd,
      xmax = adj_g_xg_ratio_mean + adj_g_xg_ratio_sd
    ),
    color = 'blue',
    linewidth = 0.1,
    height = 0.3
  ) +
  geom_point(
    aes(x = adj_g_xg_ratio_mean),
    shape = 23,
    size = 0.75,
    stroke = 0.15,
    fill = 'red',
    color = 'black'
  ) +
  geom_vline(
    aes(xintercept = 1), 
    linewidth = 0.1, 
    linetype = 2
  ) +
  ## add duplicate axis for ticks: https://stackoverflow.com/questions/56247205/r-ggplot2-add-ticks-on-top-and-right-sides-of-all-facets
  scale_x_continuous(sec.axis = dup_axis()) +
  ## ggplot2 doesn't support duplicated and creatinga  second axis for discrete variables:
  ##   https://github.com/tidyverse/ggplot2/issues/3171.
  ##   using ggh4x is a workaround.
  guides(
    y.sec = guide_axis_manual(
      breaks = ordinal_adj_shots_by_player$player,
      labels = ordinal_adj_shots_by_player$player
    )
  ) +
  theme_linedraw(base_family = 'DejaVu Sans', base_size = 4) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 4.25, face = 'plain'),
    axis.ticks.length = unit(-1, 'pt'),
    axis.ticks = element_line(linewidth = 0.05),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = 2),
    axis.text.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank()
  ) +
  labs(
    title = 'Shots from 2016/17 & 2017/18 seasons',
    y = NULL,
    x = 'Outperformance (= G/xG)'
  )

proj_dir <- '73-exceeding_expected_goals_replication'
plot_path <- file.path(proj_dir, 'shaw-figure-1-replication.png')
ggsave(
  p,
  filename = plot_path,
  units = 'px',
  width = 549,
  height = 640
)

logo_plot_path <- add_logo(
  plot_path,
  path_logo = file.path(proj_dir, 'ASAlogo.png'),
  delete = FALSE,
  path_suffix = '',
  logo_scale = 0.3,
  idx_x = 0.03,
  idx_y = 0.09,
)


library(magick)
orig_image <- magick::image_read(file.path(proj_dir, 'shaw-figure-1.png'))
replicated_image <- magick::image_read(logo_plot_path)

## images should have the same dimensions since i used the orig dimensions for saving the replicated plot
combined_image <- magick::image_append(c(orig_image, replicated_image), stack = FALSE)

magick::image_write(combined_image, path = file.path(proj_dir, 'shaw-figure-1-compared.png'))
