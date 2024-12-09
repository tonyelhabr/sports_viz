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
library(ggh4x)

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
  'Sadio Man�' = 'S. Mane',
  'Dele Alli' = 'D. Ali',
  'Riyad Mahrez' = 'R. Mahrez',
  'Christian Eriksen' = 'C. Eriksen',
  'Pedro' = 'Pedro',
  'Alexis S�nchez' = 'A. Sanchez',
  'Roberto Firmino' = 'Roberto Firmino',
  'Jamie Vardy' = 'J. Vardy',
  'Xherdan Shaqiri' = 'X. Shaqiri',
  'Wilfried Zaha' = 'W. Zaha',
  'Nathan Redmond' = 'N. Redmond',
  'Gylfi Sigurdsson' = 'G. Sigurdsson',
  'Kevin De Bruyne' = 'K. De Bruyne',
  'Andros Townsend' = 'A. Townsend',
  'Sergio Ag�ero' = 'S. Aguero',
  'Marcus Rashford' = 'M. Rashford',
  'Jermain Defoe' = 'J. Defoe',
  'Raheem Sterling' = 'R. Sterling',
  'Marko Arnautovic' = 'M. Arnautovic',
  'Paul Pogba' = 'P. Pogba',
  'Salom�n Rond�n' = 'S. Rondon',
  'Christian Benteke' = 'C. Benteke'
)

ordinal_adj_shots_by_player <- adj_shots_by_player |>
  dplyr::filter(
    player %in% names(shaw_players)
  ) |> 
  dplyr::mutate(
    player = forcats::fct_reorder(shaw_players[player], adj_g_xg_ratio_mean)
  )

adj_ratio_plot <- ordinal_adj_shots_by_player |>
  ggplot2::ggplot() +
  ggplot2::aes(y = player) +
  ggplot2::geom_errorbarh(
    aes(
      xmin = adj_g_xg_ratio_mean - adj_g_xg_ratio_sd,
      xmax = adj_g_xg_ratio_mean + adj_g_xg_ratio_sd
    ),
    color = 'blue',
    linewidth = 0.1,
    height = 0.3
  ) +
  ggplot2::geom_point(
    ggplot2::aes(x = adj_g_xg_ratio_mean),
    shape = 23,
    size = 0.75,
    stroke = 0.15,
    fill = 'red',
    color = 'black'
  ) +
  ggplot2::geom_vline(
    ggplot2::aes(xintercept = 1), 
    linewidth = 0.1, 
    linetype = 2
  ) +
  ## add duplicate axis for ticks: https://stackoverflow.com/questions/56247205/r-ggplot2-add-ticks-on-top-and-right-sides-of-all-facets
  ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis()) +
  ## ggplot2 doesn't support duplicated and creatinga  second axis for discrete variables:
  ##   https://github.com/tidyverse/ggplot2/issues/3171.
  ##   using ggh4x is a workaround.
  ggplot2::guides(
    y.sec = ggh4x::guide_axis_manual(
      breaks = ordinal_adj_shots_by_player$player,
      labels = ordinal_adj_shots_by_player$player
    )
  ) +
  ggplot2::theme_linedraw(base_family = 'DejaVu Sans', base_size = 4) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, size = 4.25, face = 'plain'),
    axis.ticks.length = ggplot2::unit(-1, 'pt'),
    axis.ticks = ggplot2::element_line(linewidth = 0.05),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(linetype = 2),
    axis.text.x.top = ggplot2::element_blank(),
    axis.text.y.right = ggplot2::element_blank(),
    axis.title.x.top = ggplot2::element_blank(),
    axis.title.y.right = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    title = 'Shots from 2016/17 & 2017/18 seasons',
    y = NULL,
    x = 'Outperformance (= G/xG)'
  )

ggplot() +
  theme_void() +
  geom_text(
    data = data.frame(
      x = 0,
      y = 0,
      label = "(tony's version)"
    ),
    aes(
      x = x,
      y = y,
      label = label
    ),
    family = 'Titillium Web',
    hjust = 0.5,
    vjust = 0.5,
    size = 12 / ggplot2::.pt
  )

proj_dir <- '73-exceeding_expected_goals_replication'
plot_path <- file.path(proj_dir, 'shaw-figure-1-replication.png')
ggsave(
  filename = file.path(proj_dir, 'tony-logo.png'),
  width = 1000,
  height = 200,
  units = 'px'
)

ggsave(
  adj_ratio_plot,
  filename = plot_path,
  units = 'px',
  width = 549,
  height = 640
)

plot_with_tony_logo_path <- add_logo(
  plot_path,
  path_logo = file.path(proj_dir, 'tony-logo.png'),
  delete = FALSE,
  path_suffix = '-w-tony-logo',
  logo_scale = 0.9,
  idx_x = 0.47,
  idx_y = 0.05
)

plot_with_asa_logo_path <- add_logo(
  plot_path,
  path_logo = file.path(proj_dir, 'ASAlogo.png'),
  delete = TRUE,
  path_suffix = '-w-asa-logo',
  logo_scale = 0.3,
  idx_x = 0.03,
  idx_y = 0.09,
)

library(magick)
orig_image <- magick::image_read(file.path(proj_dir, 'shaw-figure-1.png'))
replicated_image_with_tony_logo <- magick::image_read(plot_with_tony_logo_path)
replicated_image_with_asa_logo <- magick::image_read(plot_with_asa_logo_path)

## images should have the same dimensions since i used the orig dimensions for saving the replicated plot
combined_image_with_asa_logo <- magick::image_append(
  c(orig_image, replicated_image_with_asa_logo), 
  stack = FALSE
)

magick::image_write(
  combined_image_with_asa_logo, 
  path = file.path(proj_dir, 'shaw-figure-1-compared-w-asa-logo.png')
)

## beyond-replication ----
raw_shots <- worldfootballR::load_understat_league_shots(league = 'EPL')
shots <- raw_shots |> 
  tibble::as_tibble() |> 
  dplyr::filter(
    season %in% c(2021L, 2022L),
    situation != 'DirectFreeKick'
  ) |> 
  dplyr::arrange(id) |> 
  dplyr::transmute(
    id,
    player,
    ## since 2022/23, xG is filled out, not x_g
    xg = dplyr::coalesce(x_g, xG),
    g = as.integer(result == 'Goal')
  )

shots_by_player <- shots |> 
  dplyr::group_by(player) |> 
  dplyr::summarize(
    shots = dplyr::n(),
    dplyr::across(c(g, xg), sum)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(raw_ratio = g / xg) |> 
  dplyr::arrange(dplyr::desc(shots))
shots_by_player

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
  dplyr::filter(
    player %in% names(shaw_players)
  ) |> 
  dplyr::mutate(
    player = forcats::fct_reorder(shaw_players[player], adj_ratio_mean)
  )

library(htmltools)
library(sysfonts)
library(showtext)

blackish_background <- '#1f1f1f'
font <- 'Titillium Web'
sysfonts::font_add_google(font, font)
sysfonts::font_add('fb', 'Font Awesome 6 Brands-Regular-400.otf')
showtext::showtext_auto()
plot_resolution <- 300
showtext::showtext_opts(dpi = plot_resolution)
## https://github.com/tashapiro/tanya-data-viz/blob/1dfad735bca1a7f335969f0eafc94cf971345075/nba-shot-chart/nba-shots.R#L64


tag_lab <- htmltools::tagList(
  htmltools::tags$span(htmltools::HTML(enc2utf8("&#xf099;")), style='font-family:fb'),
  htmltools::tags$span("@TonyElHabr"),
)

beyond_replication_adj_ratio_plot <- ordinal_adj_ratio_by_player |>
  ggplot2::ggplot() +
  ggplot2::aes(y = player) +
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
    color = 'white',
    height = 0.5
  ) +
  ggplot2::geom_point(
    ggplot2::aes(x = adj_ratio_mean, size = shots),
    color = 'white'
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
    title = 'Top 20 shooting overperformers in the EPL',
    subtitle = 'EPL 2021/22 and 2022/23 seasons. ',
    caption = 'Players sorted according to descending adjusted G/xG ratio. Minimum 100 shots.<br/>**Source**: understat.',
    y = NULL,
    x = 'Adjusted G/xG Ratio',
    tag = tag_lab
  )

beyond_replication_plot_path <- file.path(proj_dir, 'beyond-replication.png')
ggplot2::ggsave(
  beyond_replication_adj_ratio_plot,
  filename = beyond_replication_plot_path,
  width = 7,
  height = 7
)

