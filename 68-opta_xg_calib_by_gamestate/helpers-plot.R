TAG_LABEL <- htmltools::tagList(
  htmltools::tags$span(htmltools::HTML(enc2utf8("&#xf099;")), style = 'font-family:fb'),
  htmltools::tags$span("@TonyElHabr"),
)
CAPTION_LABEL <- '**Data**: Opta via fbref.<br/>Point size is proportional to number of observations.'
SUBTITLE_LABEL <- 'English Premier League, 2017/18 - 2022/23'
PLOT_RESOLUTION <- 300
WHITISH_FOREGROUND_COLOR <- 'white'
COMPLEMENTARY_FOREGROUND_COLOR <- '#f1f1f1'
BLACKISH_BACKGROUND_COLOR <- '#1c1c1c'
COMPLEMENTARY_BACKGROUND_COLOR <- '#4d4d4d'
FONT <- 'Titillium Web'
sysfonts::font_add_google(FONT, FONT)
## https://github.com/tashapiro/tanya-data-viz/blob/main/chatgpt-lensa/chatgpt-lensa.R for twitter logo
sysfonts::font_add('fb', 'Font Awesome 6 Brands-Regular-400.otf')
showtext::showtext_auto()
showtext::showtext_opts(dpi = PLOT_RESOLUTION)

ggplot2::theme_set(ggplot2::theme_minimal())
ggplot2::theme_update(
  text = ggplot2::element_text(family = FONT),
  title = ggplot2::element_text(size = 20, color = WHITISH_FOREGROUND_COLOR),
  plot.title = ggplot2::element_text(face = 'bold', size = 20, color = WHITISH_FOREGROUND_COLOR),
  plot.title.position = 'plot',
  plot.subtitle = ggplot2::element_text(size = 16, color = COMPLEMENTARY_FOREGROUND_COLOR),
  axis.text = ggplot2::element_text(color = WHITISH_FOREGROUND_COLOR, size = 14),
  axis.title = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.line = ggplot2::element_blank(),
  strip.text = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0),
  panel.grid.major = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor.x = ggplot2::element_blank(),
  panel.grid.minor.y = ggplot2::element_blank(),
  plot.margin = ggplot2::margin(10, 20, 10, 20),
  plot.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR),
  plot.caption = ggtext::element_markdown(color = WHITISH_FOREGROUND_COLOR, hjust = 0, size = 10, face = 'plain'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 12, color = WHITISH_FOREGROUND_COLOR, hjust = 1),
  plot.tag.position = c(0.99, 0.01),
  panel.spacing.x = grid::unit(2, 'lines'),
  panel.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR)
)
ggplot2::update_geom_defaults('text', list(color = WHITISH_FOREGROUND_COLOR, size = 12 / .pt))
ggplot2::update_geom_defaults('point', list(color = WHITISH_FOREGROUND_COLOR))

plot_and_save_calibration <- function(
    df,
    .by,
    size = 7,
    width = size, 
    height = size, 
    title = NULL,
    caption = NULL,
    filename = tempfile(),
    extra_layers
) {
  
  group_cols <- rlang::ensyms(.by)
  
  p <- df |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = predicted_midpoint, y = event_rate) +
    ggplot2::geom_abline(color = WHITISH_FOREGROUND_COLOR, linetype = 2) +
    ggplot2::geom_ribbon(
      fill = '#999999',
      alpha = 0.5,
      ggplot2::aes(ymin = lower, ymax = upper)
    ) +
    ggplot2::geom_line(color = WHITISH_FOREGROUND_COLOR) +
    ggplot2::geom_point(
      color = WHITISH_FOREGROUND_COLOR,
      ggplot2::aes(size = total),
      show.legend = FALSE
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0, 1),
      ylim = c(0, 1)
    ) +
    ggplot2::labs(
      title = paste0(c('xG calibration', title), collapse = ' by '),
      subtitle = SUBTITLE_LABEL,
      y = 'Actual goal rate',
      x = 'Expected goals (xG)',
      caption = paste0(c(CAPTION_LABEL, caption), collapse = '\n'),
      tag = TAG_LABEL
    ) + 
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(color = WHITISH_FOREGROUND_COLOR)
    ) + 
    ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(group_cols)))
  
  if (isFALSE(missing(extra_layers))) {
    p <- p + extra_layers
  }
  
  ggplot2::ggsave(
    p,
    filename = file.path(PROJ_DIR, sprintf('%s_calibration.png', filename)),
    width = width,
    height = height
  )
  invisible(p)
}

extra_xg_cal_plot_layers <- list(
  ggplot2::geom_segment(
    data = data.frame(
      x = 0.45,
      xend = 0.35,
      y = 0.6,
      yend = 0.7,
      pre_shot_game_state = ORDERED_GAME_STATE_LABELS[1]
    ),
    arrow = grid::arrow(length = grid::unit(6, 'pt'), type = 'closed'),
    ggplot2::aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    linewidth = 1,
    color = '#009ffd'
  ),
  ggplot2::geom_segment(
    data = data.frame(
      x = 0.55,
      xend = 0.65,
      y = 0.4,
      yend = 0.3,
      pre_shot_game_state = ORDERED_GAME_STATE_LABELS[1]
    ),
    arrow = grid::arrow(length = grid::unit(6, 'pt'), type = 'closed'),
    ggplot2::aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    linewidth = 1,
    color = '#ffa400'
  ),
  ggtext::geom_richtext(
    data = data.frame(
      x = 0.65,
      y = 0.8,
      label = 'Model *under-predicts*',
      pre_shot_game_state = ORDERED_GAME_STATE_LABELS[1]
    ),
    fill = NA, label.color = NA,
    label.padding = grid::unit(rep(0, 1), 'pt'),
    color = '#009ffd',
    family = FONT,
    hjust = 1,
    vjust = 0.5,
    size = 11 / ggplot2::.pt,
    ggplot2::aes(
      x = x,
      y = y,
      label = label
    )
  ),
  ggtext::geom_richtext(
    data = data.frame(
      x = 0.35,
      y = 0.2,
      label = 'Model *over-predicts*',
      pre_shot_game_state = ORDERED_GAME_STATE_LABELS[1]
    ),
    fill = NA, label.color = NA,
    label.padding = grid::unit(rep(0, 1), 'pt'),
    color = '#ffa400',
    family = FONT,
    hjust = 0,
    vjust = 0.5,
    size = 11 / ggplot2::.pt,
    ggplot2::aes(
      x = x,
      y = y,
      label = label
    )
  )
)
