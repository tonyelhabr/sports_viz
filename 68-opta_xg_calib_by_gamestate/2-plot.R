library(dplyr)
library(qs)
library(probably)

library(ggplot2)
library(sysfonts)
library(showtext)
library(ggtext)
# library(ggforce)
library(ragg)
library(htmltools)

proj_dir <- '68-opta_xg_calib_by_gamestate'
shots <- qs::qread(file.path(proj_dir, 'shots.qs'))
np_shots <- shots |> 
  filter(
    situation != 'Penalty',
    pov == 'primary'
  ) |> 
  mutate(
    .pred_yes = xg,
    .pred_no = 1 - xg,
    g_state5 = cut(
      g_state,
      breaks = c(-Inf, -2, -1, 0, 1, Inf), 
      labels = c('<=-2 goals', '-1 goal', 'neutral', '+1 goal', '>=+2 goals')
    ),
    g_state3 = cut(
      g_state,
      breaks = c(-Inf, -1, 0, Inf), 
      labels = c('trailing', 'neutral', 'leading')
    )
  )

calibrate_by_g_state <- function(df, group) {
  df |> 
    cal_plot_breaks(
      truth = is_goal,
      estimate = xg,
      group = {{ group }},
      num_breaks = 20,
      conf_level = 0.9,
      event_level = 'second'
    )
}

calib_g_state3 <- calibrate_by_g_state(
  np_shots,
  g_state3
)

calib_g_state5 <- calibrate_by_g_state(
  np_shots,
  g_state5
)

font <- 'Titillium Web'
sysfonts::font_add_google(font, font)
## https://github.com/tashapiro/tanya-data-viz/blob/main/chatgpt-lensa/chatgpt-lensa.R for twitter logo
sysfonts::font_add('fb', 'Font Awesome 6 Brands-Regular-400.otf')
showtext::showtext_auto()
plot_resolution <- 300
showtext::showtext_opts(dpi = plot_resolution)

blackish_background <- '#1c1c1c'
  
theme_set(theme_minimal())
theme_update(
  text = element_text(family = font),
  title = element_text(size = 20, color = 'white'),
  plot.title = element_text(face = 'bold', size = 20, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = element_text(size = 16, color = '#f1f1f1'),
  axis.text = element_text(color = 'white', size = 14),
  axis.title = element_text(size = 14, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  strip.text = element_text(size = 14, color = 'white', face = 'bold', hjust = 0),
  panel.grid.major = element_line(color = '#4d4d4d'),
  panel.grid.minor = element_line(color = '#4d4d4d'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 20, 10, 20),
  plot.background = element_rect(fill = blackish_background, color = '#1c1c1c'),
  plot.caption = element_text(color = 'white', hjust = 0, size = 12, face = 'plain'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 12, color = 'white', hjust = 1),
  plot.tag.position = c(0.99, 0.01),
  panel.spacing.x = unit(2, 'lines'),
  panel.background = element_rect(fill = blackish_background, color = '#1c1c1c')
)
update_geom_defaults('text', list(color = 'white', size = 12 / .pt))
## https://github.com/tashapiro/tanya-data-viz/blob/1dfad735bca1a7f335969f0eafc94cf971345075/nba-shot-chart/nba-shots.R#L64
tag_lab <-tagList(
  tags$span(HTML(enc2utf8("&#xf099;")), style='font-family:fb'),
  tags$span("@TonyElHabr"),
)

plot_and_save_calibration <- function(
    df,
    size = 7,
    width = size, 
    height = size, 
    title = NULL,
    # subtitle = 'Big 5 leagues',
    subtitle = 'English Premier League, 2020/21 - 2022/23',
    caption = NULL,
    filename = tempfile(),
    labels_layer
) {
  
  group_cols <- setdiff(
    colnames(df),
    c('predicted_midpoint', 'event_rate', 'events', 'total', 'lower', 'upper')
  )
  
  has_group_cols <- length(group_cols) > 0
  
  p <- df |> 
    ggplot() +
    aes(x = predicted_midpoint, y = event_rate) +
    geom_abline(color = 'white', linetype = 2) +
    geom_ribbon(
      fill = '#999999',
      alpha = 0.5,
      aes(ymin = lower, ymax = upper)
    ) +
    geom_line(color = 'white') +
    geom_point(
      color = 'white',
      aes(size = total),
      show.legend = FALSE
    ) +
    labs(
      title = paste0(c('Opta npxG calibration', title), collapse = ', by '),
      subtitle = subtitle,
      y = 'Actual non-penalty goal rate',
      x = 'npxG',
      caption = paste0(c('Point size is proportional to number of observations.', caption), collapse = '\n'),
      tag = tag_lab
    )
  
  if (isTRUE(has_group_cols)) {
    p <- p + 
      theme(
        panel.grid.major = element_blank(),
        panel.background = element_rect(color = 'white')
      ) + 
      facet_wrap(vars(!!!syms(group_cols)))
  }
  
  # if (!missing(labels_layer)) {
  #   p <- p + labels_layer
  # }
  
  ggsave(
    p,
    device = ragg::agg_png,
    res = plot_resolution,
    filename = file.path(proj_dir, sprintf('%s_calibration.png', filename)),
    width = width * plot_resolution,
    height = height * plot_resolution,
    units = 'px'
  )
  invisible(p)
}

calib_g_state3$data |> 
  plot_and_save_calibration(
    width = 10,
    height = 10 / 1.5,
    title = 'game state',
    filename = 'game_state3'
  )

calib_g_state5$data |> 
  plot_and_save_calibration(
    width = 10,
    height = 5,
    title = 'game state',
    filename = 'game_state5'
  )

# debugonce(probably:::cal_isoreg_impl)
## estimate must be `.pred_{level1}` and `.pred_{level2}`
isotonic_model <- np_shots |> 
  cal_estimate_isotonic(
    truth = is_goal
  )

debugonce(probably:::cal_apply)
new_np_shots <- cal_apply(
  np_shots,
  isotonic_model
)

bind_cols(
  np_shots |> select(starts_with('.pred')),
  new_np_shots |> select(starts_with('.pred'))
)
np_shots |> 
  mutate(
    .pred_adj = predict(isotonic_model, np_shots)
  )

adj_model <- glm(
  is_goal ~ g_state + xg,
  np_shots,
  family = 'binomial'
)
np_shots |> 
  mutate(
    .pred_xg = predict(adj_model, np_shots)
  )

