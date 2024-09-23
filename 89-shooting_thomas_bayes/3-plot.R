## data
library(dplyr)
library(tidyr)
library(qs)

## plotting
library(ggplot2)
library(sysfonts)
library(showtext)
library(ggtext)
library(htmltools)
library(scales)

PROJ_DIR <- '89-shooting_thomas_bayes'

TAG_LABEL <- htmltools::tagList(
  htmltools::tags$span(htmltools::HTML(enc2utf8('&#xf099;')), style = 'font-family:fb'),
  htmltools::tags$span('@TonyElHabr'),
)
PLOT_RESOLUTION <- 300
WHITISH_FOREGROUND_COLOR <- 'white'
COMPLEMENTARY_FOREGROUND_COLOR <- '#cbcbcb'
BLACKISH_BACKGROUND_COLOR <- '#1c1c1c'
COMPLEMENTARY_BACKGROUND_COLOR <- '#4d4d4d'
FONT <- 'Titillium Web'
sysfonts::font_add_google(FONT, FONT)
sysfonts::font_add('fb', 'Font Awesome 6 Brands-Regular-400.otf')

showtext::showtext_auto()
showtext::showtext_opts(dpi = PLOT_RESOLUTION)

ggplot2::theme_set(ggplot2::theme_minimal())
ggplot2::theme_update(
  text = ggplot2::element_text(family = FONT),
  title = ggplot2::element_text(size = 16, color = WHITISH_FOREGROUND_COLOR),
  plot.title = ggtext::element_markdown(face = 'bold', size = 18, color = WHITISH_FOREGROUND_COLOR),
  plot.title.position = 'plot',
  plot.subtitle = ggplot2::element_text(size = 12, color = COMPLEMENTARY_FOREGROUND_COLOR),
  legend.text = ggplot2::element_text(size = 12, color = WHITISH_FOREGROUND_COLOR),
  axis.text = ggplot2::element_text(color = WHITISH_FOREGROUND_COLOR, size = 14),
  axis.title.x = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.title.y = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.line = ggplot2::element_blank(),
  panel.grid.major = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor.x = ggplot2::element_blank(),
  panel.grid.minor.y = ggplot2::element_blank(),
  plot.margin = ggplot2::margin(10, 10, 10, 10),
  plot.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR),
  plot.caption = ggtext::element_markdown(size = 11, color = WHITISH_FOREGROUND_COLOR, hjust = 0, face = 'plain', margin = ggplot2::margin(t = 10)),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 12, color = WHITISH_FOREGROUND_COLOR, hjust = 1, margin = ggplot2::margin(t = 10, b = 5)),
  plot.tag.position = c(0.99, 0.01),
  panel.spacing.x = grid::unit(2, 'lines'),
  panel.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = WHITISH_FOREGROUND_COLOR),
)
ggplot2::update_geom_defaults('text', list(color = WHITISH_FOREGROUND_COLOR, size = 14 / .pt))

simulated_xg_ratio_sensitivities <- qs::qread(file.path(PROJ_DIR, 'simulated_xg_ratio_sensitivities.qs'))

filt_simulated_xg_ratio_sensitivities <- simulated_xg_ratio_sensitivities |> 
  dplyr::filter(
    percentile %in% scales::ordinal(c(seq(65, 95, by = 5), 99))
  )

filt_simulated_xg_ratio_sensitivities$percentile <- droplevels(filt_simulated_xg_ratio_sensitivities$percentile)

all_percentiles <- unique(filt_simulated_xg_ratio_sensitivities$percentile)
MAX_X <- 1100
sensitivity_plot <- filt_simulated_xg_ratio_sensitivities |> 
  dplyr::transmute(
    metric,
    percentile,
    orig_shots = shots,
    plot_shots = ifelse(orig_shots > MAX_X, MAX_X, orig_shots),
    label_position = ifelse(orig_shots > MAX_X, MAX_X * 0.9, orig_shots)
  ) |>  
  ggplot2::ggplot(
    ggplot2::aes(
      x = plot_shots, 
      y = percentile,
      group = metric
    )
  ) +
  ggplot2::coord_cartesian(
    xlim = c(0, MAX_X),
    clip = 'on',
    expand = FALSE
  ) +
  ggplot2::guides(
    fill = guide_legend(title = '')
  ) +
  ggplot2::scale_x_continuous(
    labels = scales::comma
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      'G/xG' = '#17B8E1', 
      'PSxG/xG' = '#FC5150'
    )
  ) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(),
    width = 0.6,
    ggplot2::aes(
      fill = metric
    )
  ) +
  ggplot2::geom_text(
    family = FONT,
    # fontface = 'bold',
    ggplot2::aes(
      x = label_position,
      label = scales::comma(orig_shots)
    ),
    position = ggplot2::position_dodge(0.6),
    hjust = -0.1,
    size = 14 / ggplot2::.pt
  ) +
  ggplot2::labs(
    title = 'How many shots does a player need to take before we can say<br/>that they are a %th percentile finisher?',
    y = 'Performance percentile',
    x = 'Shots needed to detect difference from average performance',
    caption = '**Data**: Big 5 + MLS, 2017/2018 - 2023/2024.',
    tag = TAG_LABEL,
  ) +
  ggplot2::theme(
    legend.position = 'top',
    panel.grid.major.y = ggplot2::element_blank()
  )

ggplot2::ggsave(
  sensitivity_plot,
  filename = file.path(PROJ_DIR, 'shots-needed-overperformance-sensitivity.png'),
  units = 'in', 
  width = 7,
  height = 7
)
