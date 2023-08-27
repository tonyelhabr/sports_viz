## data ----
library(dplyr)
library(readr)

proj_dir <- '74-2023_womens_world_cup_xg'
sb_shots <- read_rds(file.path(proj_dir, 'sb_shots.rds'))
opta_shots <- read_rds(file.path(proj_dir, 'opta_shots.rds'))

sb_np_shots <- sb_shots |> 
  filter(
    shot_type_name != 'Penalty'
  ) |>
  transmute(
    # period,
    # minute,
    # second,
    # team_id,
    # team_name,
    xg = shot_statsbomb_xg,
    g = shot_outcome_name == 'Goal'
  )

opta_np_shots <- opta_shots |> 
  mutate(across(x_g, as.numeric)) |> 
  # filter(
  #   shot_type_name != 'Penalty'
  # ) |> 
  filter(
    !(distance == '13' & x_g == 0.79)
  ) |> 
  transmute(
    # squad,
    # match_half,
    # minute,
    xg = x_g,
    g = outcome == 'Goal'
  )

np_shots <- bind_rows(
  sb_np_shots |> mutate(source = 'StatsBomb'),
  opta_np_shots |> mutate(source = 'Opta')
) |> 
  mutate(
    .pred_yes = xg,
    is_goal = factor(ifelse(g == 1L, 'yes', 'no'))
  )

# library(probably)
# sb_npxg |> 
#   transmute(
#     g = factor(ifelse(g == 1L, 'yes', 'no')),
#     .pred_yes = xg
#   ) |> 
#   cal_plot_breaks(
#     truth = g,
#     estimate = dplyr::matches('^[.]pred'),
#     event_level = 'second'
#   )
# 
# opta_npxg |> 
#   transmute(
#     g = factor(ifelse(g == 1L, 'yes', 'no')),
#     .pred_yes = xg
#   ) |> 
#   cal_plot_breaks(
#     truth = g,
#     estimate = dplyr::matches('^[.]pred'),
#     event_level = 'second'
#   )
library(probably) ## 0.1.0.9008

overall_calibration <- cal_plot_breaks(
  np_shots,
  truth = is_goal,
  estimate = .pred_yes,
  num_breaks = 20,
  conf_level = 0.9,
  event_level = 'second'
)

## bss ----
library(yardstick)
library(rlang)
brier_score <- function(data, ...) {
  UseMethod('brier_score')
}

brier_score <- yardstick::new_prob_metric(brier_score, direction = 'minimize')

brier_score_vec <- function(truth, estimate, na_rm = TRUE, event_level, ...) {
  
  brier_score_impl <- function(truth, estimate, event_level, ...) {
    truth <- 1 - (as.numeric(truth) - 1)
    
    if (event_level == 'second') {
      truth <- 1 - truth
    }
    
    mean((truth - estimate)^2)
  }
  
  ## Recycle the estimate value if it's scalar-ish.
  if (length(estimate) == 1) {
    estimate <- rep(estimate, length(truth))
  }
  
  yardstick::metric_vec_template(
    metric_impl = brier_score_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = c('factor', 'numeric'),
    estimator = 'binary',
    event_level = event_level,
    ...
  )
}

brier_score.data.frame <- function(data, truth, estimate, na_rm = TRUE, event_level = 'first', ...) {
  yardstick::metric_summarizer(
    metric_nm = 'brier_score',
    metric_fn = brier_score_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm = na_rm,
    event_level = event_level,
    ...
  )
}

np_goal_rate <- np_shots |> 
  count(is_goal) |> 
  mutate(prop = n / sum(n)) |> 
  filter(is_goal == 'yes') |> 
  pull(prop)
np_goal_rate
#> 0.08540031

np_goal_rate_brier_score <- np_shots |> 
  brier_score(
    truth = is_goal,
    estimate = !!np_goal_rate,
    event_level = 'second'
  ) |> 
  pull(.estimate)
np_goal_rate_brier_score
#> [1] 0.0781071

npxg_brier_score <- np_shots |> 
  brier_score(
    truth = is_goal,
    estimate = xg,
    event_level = 'second'
  ) |> 
  pull(.estimate)
npxg_brier_score
#> [1] 0.06419964

## plotting ----
library(purrr)
library(ggplot2)
library(sysfonts)
library(showtext)
library(ggtext)
library(ragg)
library(htmltools)
library(glue)

TAG_LABEL <- htmltools::tagList(
  htmltools::tags$span(htmltools::HTML(enc2utf8("&#xf099;")), style='font-family:fb'),
  htmltools::tags$span("@TonyElHabr"),
)

PLOT_RESOLUTION <- 300

font <- 'Titillium Web'
sysfonts::font_add_google(font, font)
## https://github.com/tashapiro/tanya-data-viz/blob/main/chatgpt-lensa/chatgpt-lensa.R for twitter logo
sysfonts::font_add('fb', 'Font Awesome 6 Brands-Regular-400.otf')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

blackish_background <- '#1c1c1c'

theme_set(theme_minimal())
theme_update(
  text = element_text(family = font),
  title = element_text(size = 20, color = 'white'),
  plot.title = element_text(face = 'bold', size = 20, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown(size = 12, color = '#f1f1f1'),
  axis.text = element_text(color = 'white', size = 12),
  axis.title = element_text(size = 12, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  strip.text = element_text(size = 124, color = 'white', face = 'bold', hjust = 0),
  panel.grid.major = element_line(color = '#4d4d4d'),
  panel.grid.minor = element_line(color = '#4d4d4d'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 20, 10, 20),
  plot.background = element_rect(fill = blackish_background, color = '#1c1c1c'),
  plot.caption = element_text(color = 'white', hjust = 0, size = 11, face = 'italic'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 11, color = 'white', hjust = 1),
  plot.tag.position = c(0.99, 0.01),
  panel.spacing.x = unit(2, 'lines'),
  panel.background = element_rect(fill = blackish_background, color = '#1c1c1c')
)
update_geom_defaults('text', list(color = 'white', size = 12 / .pt))

SOURCE_COLORS <- c(
  'StatsBomb' = '#e40b27',
  'Opta' = '#00ADEF'
)

bss <- np_shots |> 
  group_by(source) |> 
  brier_score(
    truth = is_goal,
    estimate = xg,
    event_level = 'second'
  ) |> 
  ungroup() |> 
  transmute(
    source,
    labeled_source = glue::glue('<b><span style=color:{SOURCE_COLORS[source]}>{source}</span></b>'),
    bss =  1 - (.estimate / !!np_goal_rate_brier_score),
    label = sprintf(
      '<span style=color:#ffffff>%s %s: %.4f</span>', 
      labeled_source, 
      ifelse(source == 'StatsBomb', 'BSS', 'Brier Skill Score (BSS)'),
      bss
    )
  ) |> 
  arrange(source) |> 
  summarize(
    label = paste0(label, collapse = '<br/>')
  )

p <- np_shots |> 
  cal_plot_breaks(
    group = source,
    truth = is_goal,
    estimate = .pred_yes,
    num_breaks = 10,
    conf_level = 0.9,
    event_level = 'second'
  ) |> 
  pluck('data') |> 
  ggplot() +
  aes(
    x = predicted_midpoint, 
    y = event_rate, 
    color = source
  ) +
  geom_abline(color = 'white', linetype = 2) +
  ggtext::geom_richtext(
    inherit.aes = FALSE,
    data = bss,
    # color = 'white',
    # fontface = 'italic',
    size = 12 / .pt,
    hjust = 0,
    vjust = 1,
    lineheight = 1,
    fill = NA, label.color = NA,
    # label.padding = grid::unit(rep(0, 4), "pt"),
    family = font,
    aes(
      x = 0.02,
      y = 0.98,
      label = label
    )
  ) +
  geom_line() +
  geom_point(
    aes(size = total)
  ) +
  scale_color_manual(
    values = SOURCE_COLORS
  ) +
  guides(
    color = 'none',
    size = guide_legend(title = '# of shots', override.aes = list(color = 'white'))
  ) +
  theme(
    legend.position = 'top',
    legend.title = element_text(color = 'white', size = 12),
    legend.text = element_text(color = 'white', size = 12)
  ) +
  labs(
    title = "2023 FIFA Women's World Cup xG calibration", 
    subtitle = glue::glue('<span style=color:{SOURCE_COLORS["Opta"]}>Opta</span>\'s and <span style=color:{SOURCE_COLORS["StatsBomb"]}>StatsBomb</span>\'s xG models performed equally well overall.<br/>Opta\'s model tended to predict slightly lower than StatsBomb\'s.'),
    y = 'Actual non-penalty goal rate',
    x = 'non-penalty expected goals (npxG)',
    caption = 'Point size is proportional to number of observations.\nA higher Brier Skill Score is better.\n1 = perfect model. 0 = equal to baseline (average goal conversion rate).',
    tag = TAG_LABEL
  )
p
plot_path <- file.path(proj_dir, 'wwc_2023_xg_source_calibration.png')
ggsave(
  p,
  width = 7,
  height = 7,
  filename = plot_path
)
logo_path <- file.path(proj_dir, '2023-fifa-womens-world-cup-logo.jpg')
add_logo(
  path_viz = plot_path,
  path_logo = logo_path,
  delete = TRUE,,
  path_suffix = '',
  logo_scale  = 0.15,
  idx_x = 0.01,
  idx_y = 0.89
)
