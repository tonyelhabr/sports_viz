library(qs)

library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtext)

library(ffsimulator)
library(dplyr)
library(tibble)

PROJ_DIR <- '85-ffsimulator_2024'

sim <- qs::qread(file.path(PROJ_DIR, 'sim.qs'))

sim

PLOT_RESOLUTION <- 300
WHITISH_FOREGROUND_COLOR <- 'white'
COMPLEMENTARY_FOREGROUND_COLOR <- '#cbcbcb'
BLACKISH_BACKGROUND_COLOR <- '#1c1c1c'
COMPLEMENTARY_BACKGROUND_COLOR <- '#4d4d4d'
FONT <- 'Titillium Web'
sysfonts::font_add_google(FONT, FONT)
sysfonts::font_add('fb', 'Font Awesome 6 Brands-Regular-400.otf')
# sysfonts::font_add('fa-brands', 'Font Awesome 6 Brands-Regular-400.otf')
# sysfonts::font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')
showtext::showtext_auto()
showtext::showtext_opts(dpi = PLOT_RESOLUTION)

plot_theme <- function(...) {
  list(
    ...,
    ggplot2::theme_minimal(),
    ggplot2::theme(
      text = ggplot2::element_text(family = FONT),
      title = ggplot2::element_text(size = 16, color = WHITISH_FOREGROUND_COLOR),
      plot.title = ggtext::element_markdown(face = 'bold', size = 18, color = WHITISH_FOREGROUND_COLOR),
      plot.title.position = 'plot',
      plot.subtitle = ggtext::element_markdown(size = 12, color = COMPLEMENTARY_FOREGROUND_COLOR),
      legend.text = ggplot2::element_text(size = 12, color = WHITISH_FOREGROUND_COLOR),
      axis.text = ggplot2::element_text(color = WHITISH_FOREGROUND_COLOR, size = 14),
      axis.title.x = ggtext::element_markdown(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
      axis.title.y = ggtext::element_markdown(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
      axis.line = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
      panel.grid.minor = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      plot.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR),
      plot.caption = ggtext::element_markdown(size = 11, color = WHITISH_FOREGROUND_COLOR, hjust = 0, face = 'plain'),
      plot.caption.position = 'plot',
      plot.tag = ggtext::element_markdown(size = 12, color = WHITISH_FOREGROUND_COLOR, hjust = 1),
      plot.tag.position = c(0.99, 0.01),
      panel.spacing.x = grid::unit(2, 'lines'),
      panel.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = WHITISH_FOREGROUND_COLOR),
      strip.text = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0)
    )
  )
}

ggplot2::update_geom_defaults(
  'text', 
  list(color = WHITISH_FOREGROUND_COLOR, size = 14 / .pt)
)

wins_plot <- ggplot2::autoplot(sim, type = 'wins') + 
  plot_theme() +
  ggplot2::theme(legend.position = 'none') + 
  ggplot2::labs(
    caption = paste0('Per ', sim$simulation_params$n_seasons, ' simulations'),
    subtitle = NULL,
    title = 'Projected Regular Season Win Totals'
  ) +
  ggplot2::scale_fill_viridis_d(
    option = 'H', 
    begin = 0.1, 
    end = 0.9
  )

ggplot2::ggsave(
  wins_plot,
  filename = file.path(PROJ_DIR, 'wins.png'), 
  width = 8, 
  height = 8
)

ranks_plot <- ggplot2::autoplot(sim, type = 'rank') +
  plot_theme() +
  ggplot2::theme(
    legend.position = 'top', 
    axis.text.x = ggplot2::element_blank(), 
    panel.grid.major.x = ggplot2::element_blank(), 
    # axis.text.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank()
  ) +
  ggplot2::labs(caption = NULL) + 
  ggplot2::labs(
    caption = paste0('Per ', sim$simulation_params$n_seasons, ' simulations'),
    subtitle = NULL,
    title = 'Projected Regular Season Rank'
  ) +
  ggplot2::scale_y_continuous(
    expand = c(0.01, 0.01)
  ) +
  ggplot2::scale_color_viridis_d(
    option = 'H',
    begin = 0.1,
    end = 0.9
  ) +
  # ggplot2::scale_color_manual(
  #   values = rep('white', 10)
  # ) +
  ggplot2::scale_fill_viridis_d(
    option = 'H', 
    begin = 0.1, 
    end = 0.9
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(''), 
    color = 'none'
  )

ggplot2::ggsave(
  ranks_plot,
  filename = file.path(PROJ_DIR, 'ranks.png'), 
  width = 8, 
  height = 8
)

season_summary <- sim$summary_season |> 
  tibble::as_tibble() |> 
  dplyr::group_by(season) |> 
  dplyr::mutate(
    # season_rank = rank(-h2h_wins, ties.method = 'min')
    season_rank = dplyr::row_number(dplyr::desc(h2h_wins) * 10000 + points_for)
  ) |> 
  dplyr::ungroup()

season_summary |> 
  dplyr::group_by(franchise_name) |> 
  dplyr::summarize(
    playoff_prob = sum(season_rank <= 4) / dplyr::n()
  ) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(dplyr::desc(playoff_prob))

season_summary |> 
  dplyr::group_by(franchise_name) |> 
  dplyr::summarize(
    playoff_prob = sum(season_rank <= 4) / dplyr::n(),
    title_prob = sum(season_rank == 1) / dplyr::n(),
    taco_prob = sum(season_rank == 10) / dplyr::n()
  ) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(dplyr::desc(playoff_prob))
