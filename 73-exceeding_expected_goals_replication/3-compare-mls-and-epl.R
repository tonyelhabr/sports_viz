## data wrangling
library(worldfootballR)
library(dplyr)
library(tibble)

## distribution fitting and wrangling
library(MASS, include.only = 'fitdistr') ## to avoid `select` name conflict with dplyr
library(withr)
library(purrr)
library(tidyr)

## plotting
library(ggplot2)
library(forcats)
proj_dir <- '73-exceeding_expected_goals_replication'

raw_shots <- worldfootballR::load_fb_match_shooting(
  gender = 'M',
  country = c('ENG', 'USA'),
  tier = '1st',
  season_end_year = c(2021L, 2022L, 2023L)
)
filt_shots <- raw_shots |> 
  dplyr::filter(
    (Country == 'ENG' & (Season_End_Year %in% c(2022L, 2023L))) |
      (Country == 'USA' & (Season_End_Year %in% c(2021L, 2022L)))
  )
shots <- filt_shots |> 
  tibble::as_tibble() |> 
  dplyr::filter(
    # !(Distance == '13' & xG == '0.79') ## exclude penalties?
    Notes != 'Free kick'
  ) |> 
  # dplyr::arrange(id) |> 
  dplyr::transmute(
    # id,
    league = Competition_Name,
    player = Player,
    xg = as.numeric(xG),
    g = as.integer(Outcome == 'Goal')
  )

shots_by_player <- shots |> 
  dplyr::group_by(league, player) |> 
  dplyr::summarize(
    shots = dplyr::n(),
    dplyr::across(c(g, xg), sum)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(raw_ratio = g / xg) |> 
  dplyr::arrange(dplyr::desc(shots))
shots_by_player

prior_shots_by_player <- dplyr::filter(
  shots_by_player, 
  shots >= 50,
  g > 0 ## prevent error with fitting prior distribution
)

prior_distr <- MASS::fitdistr(
  prior_shots_by_player$raw_ratio,
  dgamma,
  start = list(shape = 1, rate = 1)
)
prior_shape <- prior_distr$estimate[1]
prior_rate <- prior_distr$estimate[2]

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

shots_by_player$adj_ratio <- purrr::map2(
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

adj_ratio_by_player <- shots_by_player |> 
  tidyr::unnest_wider(
    adj_ratio, 
    names_sep = '_'
  ) |> 
  dplyr::arrange(dplyr::desc(adj_ratio_mean))

ordinal_adj_ratio_by_player <- adj_ratio_by_player |>
  # group_by(league) |> 
  dplyr::filter(shots >= 100) |> 
  dplyr::slice_max(adj_ratio_mean, n = 40, with_ties = FALSE) |> 
  # ungroup() |> 
  dplyr::mutate(
    player = forcats::fct_reorder(player, adj_ratio_mean)
  )
# ordinal_adj_ratio_by_player |> dplyr::count(league)

blackish_background <- '#1f1f1f'
font <- 'Titillium Web'
sysfonts::font_add_google(font, font)

plot <- ordinal_adj_ratio_by_player |>
  ggplot2::ggplot() +
  ggplot2::aes(y = player, color = league) +
  ggplot2::geom_vline(
    ggplot2::aes(xintercept = 1), 
    linewidth = 1,
    linetype = 2,
    color = 'white'
  ) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(
      xmin = adj_ratio_mean - adj_ratio_sd,
      xmax = adj_ratio_mean + adj_ratio_sd
    ),
    height = 0.5
  ) +
  ggplot2::geom_point(
    ggplot2::aes(x = adj_ratio_mean, size = shots)
  ) +
  ggplot2::guides(
    size = ggplot2::guide_legend(override.aes = list(color = 'white')), 
    color = ggplot2::guide_legend(override.aes = list(size = 3))
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    text = ggplot2::element_text(family = font, color = 'white'),
    title = ggplot2::element_text(size = 14, color = 'white'),
    plot.title = ggplot2::element_text(face = 'bold', size = 16, color = 'white', hjust = 0),
    plot.title.position = 'plot',
    plot.subtitle = ggplot2::element_text(size = 14, color = 'white', hjust = 0),
    plot.margin = ggplot2::margin(10, 20, 10, 20),
    plot.caption = ggtext::element_markdown(color = 'white', hjust = 0, size = 10, face = 'plain', lineheight = 1.1),
    plot.caption.position = 'plot',
    plot.tag = ggtext::element_markdown(size = 10, color = 'white', hjust = 1),
    plot.tag.position = c(0.99, 0.01),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(linewidth = 0.1),
    plot.background = ggplot2::element_rect(fill = blackish_background, color = blackish_background),
    panel.background = ggplot2::element_rect(fill = blackish_background, color = blackish_background),
    axis.title = ggplot2::element_text(color = 'white', size = 14, face = 'bold', hjust = 0.99),
    axis.line = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(color = 'white', size = 12),
    axis.text.y = ggtext::element_markdown(),
    legend.text = ggplot2::element_text(color = 'white', size = 12),
    legend.position = 'top'
  ) +
  ggplot2::labs(
    title = 'Top 40 shooting overperformers across the EPL and MLS',
    subtitle = 'EPL 2021/22 and 2022/23 seasons, MLS 2021 and 2022 seasons',
    caption = 'Players sorted according to descending adjusted G/xG ratio. Minimum 100 shots.<br/>**Source**: Opta via FBref',
    y = NULL,
    x = 'Adjusted G/xG Ratio'
  )

ggplot2::ggsave(
  plot,
  filename = file.path(proj_dir, 'mls-epl-top-overperformers.png'),
  height = 10,
  width = 10
)
