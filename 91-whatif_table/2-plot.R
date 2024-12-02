## plotting
library(ggplot2)
library(ggtext)
library(magick)
library(ggforce)

PROJ_DIR <- '91-whatif_table'
source(file.path(PROJ_DIR, 'plot-helpers.R'))
both_tables <- qs::qread(file.path(PROJ_DIR, 'both_tables.qs'))

PAL <- c(
  'More' = '#2feaa8', 
  'Neither' = COMPLEMENTARY_BACKGROUND_COLOR,
  'Less' = "#fb3640"
)

prepped_both_tables <- both_tables |> 
  dplyr::mutate(
    team_name = forcats::fct_reorder(factor(team_name), -counterfactual_rnk),
    pts_change = counterfactual_pts - actual_pts,
    rnk_change = counterfactual_rnk - actual_rnk,
    rnk_change_label = case_when(
      counterfactual_pts == actual_pts ~ names(PAL)[2], 
      counterfactual_pts > actual_pts ~ names(PAL)[1], 
      counterfactual_pts < actual_pts ~ names(PAL)[3]
    ),
    team_label = paste0(
      "<b><span style='font-size:12pt;color:white", 
      "'>", 
      team_name, 
      "</span></b> <span style='font-size:10pt;color:white", 
      "'>(",
      actual_rnk,
      ## https://albert-rapp.de/posts/ggplot2-tips/08_fonts_and_icons/08_fonts_and_icons.html
      " <span style='font-family:fa-solid",
      dplyr::case_when(
        rnk_change_label == 'Less' ~ paste0(';color:', PAL[['Less']]),
        rnk_change_label == 'More' ~  paste0(';color:', PAL[['More']]),
        rnk_change_label == 'Neither' ~  paste0(';color:', PAL[['Neither']]),
      ),
      "'>&#x",
      case_when(
        rnk_change_label == 'Less' ~ 'e097',
        rnk_change_label == 'More' ~ 'e098',
        TRUE ~ 'f061'
      ),
      ';</span> ',
      counterfactual_rnk,
      ')</span>'
    ) |> 
      forcats::fct_reorder(-counterfactual_rnk)
  )

MAX_PTS <- 40
X_BREAKS <- seq(0, MAX_PTS, by = 5)
# X_LABELS <- as.character(X_BREAKS)
# X_LABELS[seq(2, length(X_LABELS), by = 2)] <- ''

counterfactual_pts_plot <- prepped_both_tables |> 
  ggplot2::ggplot() +
  ggplot2::aes(
    x = actual_pts,
    xend = counterfactual_pts,
    y = team_label,
    yend = team_label,
    color = rnk_change_label
  ) +
  ggforce::geom_link(
    ggplot2::aes(
      linewidth = ggplot2::after_stat(index)
    ),
    n = 1000
  ) +
  ggplot2::geom_point(
    ggplot2::aes(
      x = counterfactual_pts, 
      y = team_label, 
      color = rnk_change_label
    ),
    shape = 21,
    fill = "white",
    size = 3.5
  ) +
  ggplot2::scale_color_manual(
    values = PAL,
    guide = NULL
  ) +
  ggplot2::scale_linewidth(
    range = c(0.01, 4),
    guide = NULL
  ) +
  ggplot2::scale_x_continuous(
    limits = c(0, MAX_PTS),
    breaks = seq(0, MAX_PTS, by = 5),
    labels = X_LABELS
  ) + 
  ggplot2::coord_cartesian(clip = 'off') +
  ggplot2::labs(
    title = glue::glue("Which teams would have <span style='color:{PAL[['More']]}'>more</span> or <span style='color:{PAL[['Less']]}'>less</span> points if the result<br/>of every 1-score match was flipped?"),
    subtitle = '2024/25 season, through 2024-11-29',
    x = 'Points',
    y = NULL,
    caption = '<i>Team labels show how standing in table would change.</i><br/><br/>',
    tag = TAG_LABEL
  ) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    axis.text.y = ggtext::element_markdown()
  )

counterfactual_pts_plot_path <- file.path(PROJ_DIR, '2025-epl-counterfactual-pts.png')
ggplot2::ggsave(
  counterfactual_pts_plot,
  filename = counterfactual_pts_plot_path,
  width = 8,
  height = 8
)

add_logo(
  counterfactual_pts_plot_path,
  logo_path = file.path(PROJ_DIR, 'epl-logo-white.png'),
  logo_scale = 0.15,
  idx_x = 0.99,
  idx_y = 0.01
)

