library(dplyr)
library(qs)
library(probably) ## 0.1.0.9008
# packageVersion('probably') 

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
    .pred1 = xg,
    .pred2 = 1 - xg,
    g_state = cut(
      g_state,
      breaks = c(-Inf, -1, 0, Inf), 
      labels = c('trailing', 'neutral', 'leading')
    )
  )

convert_seq_to_cuts <- function(seq) {
  list(
    lower_cut = seq[1:length(seq) - 1], 
    upper_cut = seq[2:length(seq)]
  )
}

convert_cuts_to_df <- function(cuts) {
  tibble::tibble(
    lower_cut = cuts$lower_cut,
    upper_cut = cuts$upper_cut,
    predicted_midpoint = lower_cut + (upper_cut - lower_cut) / 2
  )
}

n_breaks <- 20
side <- seq(0, 1, by = 1 / n_breaks)

calib_g_state <- cal_plot_breaks(
  np_shots,
  truth = is_goal,
  estimate = starts_with('.pred'),
  group = g_state,
  num_breaks = n_breaks,
  conf_level = 0.9,
  event_level = 'second'
)

calib_g_state$data |> 
  left_join(
    convert_seq_to_cuts(seq(0, 1, by = 1 / n_breaks)) |> 
      convert_cuts_to_df(),
    by = join_by(predicted_midpoint)
  )


cal_table_custom_breaks <- function(
    .data, 
    truth, 
    estimate = dplyr::starts_with(".pred"), 
    side, 
    conf_level = 0.9, 
    event_level = c("auto", "first", "second"), 
    ..., 
    group = NULL
) {
  
  ## mostly internals of probably:::.cal_table_breaks_impl (from probably:::.cal_table_breaks)
  truth <- rlang::enquo(truth)
  estimate <- rlang::enquo(estimate)
  group <- rlang::enquo(group)
  
  levels <- probably:::truth_estimate_map(
    .data = .data, 
    truth = !!truth, 
    estimate = !!estimate
  )
  

  ## internals of probably:::.cal_table_breaks_grp
  cuts <- convert_seq_to_cuts(side)
  ## return lower and upper cut for plotting
  cuts_df <- convert_cuts_to_df(cuts)
  
  res <- .data |> 
    dplyr::group_by(!!group, .add = TRUE) |> 
    dplyr::group_map(
      ~{
        ## replace call to probably:::.cal_table_breaks_grp with direct call to probably:::.cal_class_grps with pre-computed cuts
        grp <- probably:::.cal_class_grps(
          .data = .x,
          truth = !!truth,
          cuts = cuts,
          event_level = event_level,
          levels = levels,
          conf_level = conf_level
        )
        dplyr::bind_cols(.y, grp)
      }
    ) |> 
    dplyr::bind_rows() |> 
    dplyr::inner_join(
      cuts_df,
      by = dplyr::join_by(predicted_midpoint)
    )
  
  if (length(levels) > 2) {
    res <- dplyr::group_by(res, !!truth, .add = TRUE)
  }
  res
}


calib_g_state_custom <- cal_table_custom_breaks(
  np_shots,
  truth = is_goal,
  estimate = starts_with('.pred'),
  group = g_state,
  side = c(0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.15, 0.2, 0.25, 0.5, 1),
  conf_level = 0.9,
  event_level = 'second'
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

calib_g_state$data |> 
  plot_and_save_calibration(
    width = 10,
    height = 10 / 2,
    title = 'game state',
    filename = 'game_state'
  )

calib_g_state_custom |> 
  plot_and_save_calibration(
    width = 10,
    height = 10 / 2,
    title = 'game state',
    filename = 'game_state_custom'
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

