library(readr)
library(dplyr)
library(tidyr)

library(gt)
library(gtExtras)

library(ggforce)
library(ggrepel)

PROJ_DIR <- '76-2023_ff_luck'
DATA_DIR <- file.path(PROJ_DIR, 'data')
TOP_N_FOR_TABLES <- 10
GTSAVE_ZOOM <- 1.5
source(file.path(PROJ_DIR, 'theme.R'))

## data ----
scores <- readr::read_csv(file.path(DATA_DIR, 'team-scores-all.csv')) |> 
  dplyr::mutate(
    w = user_score > opponent_score,
    l = user_score < opponent_score
  )
seasons <- unique(scores$season)

## data processing ----
current_season <- max(seasons)
scores_current <- scores |> 
  dplyr::filter(season == current_season)

agg_scores_current <- scores_current |> 
  dplyr::group_by(user_name) |> 
  dplyr::summarize(
    dplyr::across(
      c(
        user_score,
        opponent_score
      ),
      sum
    )
  ) |> 
  dplyr::ungroup()


latest_week <- scores_current |> 
  dplyr::filter(user_score > 0) |> 
  dplyr::slice_max(week, n = 1, with_ties = FALSE) |> 
  dplyr::pull(week)

actual_records <- scores |> 
  dplyr::mutate(
    w = user_score > opponent_score,
    l = user_score < opponent_score
  ) |> 
  dplyr::arrange(season, week) |> 
  dplyr::group_by(season, user_name) |> 
  dplyr::mutate(
    dplyr::across(
      c(
        w, l, 
        user_score, opponent_score,
        user_projected_score, opponent_projected_score
      ),
      cumsum
    )
  ) |> 
  dplyr::ungroup() |> 
  dplyr::group_by(season, week) |> 
  dplyr::mutate(
    n = sum(!is.na(result)),
    rnk = dplyr::row_number(dplyr::desc(w * 10000 + user_score))
  ) |> 
  dplyr::ungroup()

actual_pos_props_current <- actual_records |> 
  dplyr::filter(season == current_season) |>
  dplyr::filter(!is.na(result)) |> 
  dplyr::group_by(user_name) |> 
  dplyr::summarize(
    n = dplyr::n(),
    n_top4 = sum(rnk <= 4),
    n_bot2 = sum(rnk >= 9)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    prop_top4 = n_top4 / n,
    prop_bot2 = n_bot2 / n
  )

all_h2h_records <- tidyr::crossing(
  'season' = seasons,
  'week' = 1:latest_week,
  'user_name' = sort(unique(scores$user_name)),
  'opponent_user_name' = sort(unique(scores$opponent_user_name))
) |> 
  dplyr::filter(user_name != opponent_user_name) |> 
  dplyr::inner_join(
    scores |>
      dplyr::select(season, week, user_name, user_score),
    by = dplyr::join_by(season, week, user_name)
  ) |>
  dplyr::inner_join(
    scores |> 
      dplyr::select(
        season,
        week,
        opponent_user_name = user_name,
        opponent_score = user_score
      ),
    by = dplyr::join_by(season, week, opponent_user_name)
  ) |> 
  dplyr::mutate(
    w = user_score > opponent_score,
    l = user_score < opponent_score
  ) |> 
  dplyr::group_by(season, user_name) |> 
  dplyr::summarize(
    n = dplyr::n(),
    dplyr::across(
      c(w, l),
      sum
    )
  ) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(season, dplyr::desc(w))

compared_records <- all_h2h_records |> 
  dplyr::transmute(
    season,
    user_name,
    all_play_season = season,
    all_play_w = w,
    all_play_l = l,
    all_play_w_prop = w / (w + l)
  ) |> 
  dplyr::inner_join(
    actual_records |> 
      dplyr::filter(week == latest_week) |> 
      dplyr::transmute(
        season,
        user_name,
        actual_n = n,
        actual_w = w,
        actual_l = l,
        actual_w_prop = w / (w + l)
      ),
    by = dplyr::join_by(season, user_name),
    relationship = 'many-to-many'
  ) |> 
  dplyr::mutate(
    xw = actual_n * all_play_w_prop,
    w_prop_d = actual_w_prop - all_play_w_prop,
    w_d = actual_w - xw
  ) |> 
  dplyr::arrange(w_prop_d)
compared_records |> filter(season == 2024)

tabulate_unluckiest <- function(df, n = TOP_N_FOR_TABLES) {
  df |> 
    dplyr::arrange(w_prop_d) |> 
    head(n) |> 
    dplyr::select(
      season,
      user_name,
      actual_w,
      actual_l,
      xw,
      w_d
    ) |> 
    gt::gt() |> 
    .gt_theme_538() |> 
    gt::cols_label(
      'season' = 'Season',
      'user_name' = 'Player',
      'actual_w' = 'W',
      'actual_l' = 'L',
      'xw' = 'xW',
      'w_d' = 'W - xW'
    ) |> 
    gt::fmt_number(
      c(xw, w_d),
      decimals = 1
    ) |> 
    gt::tab_footnote(
      footnote = 'Based on all play win %.',
      locations = gt::cells_column_labels(columns = 'xw')
    )
}

tb_unluckiest_records_all <- compared_records |> 
  tabulate_unluckiest() |> 
  gtExtras::gt_highlight_rows(
    rows = season == .env$current_season,
    fill = 'gold'
  ) |>
  gt::tab_header(
    title = gt::md(sprintf('**%s seasons**', 'Unluckiest')),
    subtitle = gt::md(
      sprintf(
        '%s %s records through week %s, since %s', 
        TOP_N_FOR_TABLES, 
        tolower('Unluckiest'), 
        latest_week, 
        min(seasons)
      )
    )
  )
tb_unluckiest_records_all

CURRENT_DATE <- Sys.Date()
gt::gtsave(
  tb_unluckiest_records_all,
  filename = file.path(PROJ_DIR, sprintf('%s-unluckiest-records.png', current_date)),
  zoom = GTSAVE_ZOOM
)

tb_unluckiest_records_current <- compared_records |> 
  filter(season == current_season) |> 
  tabulate_unluckiest() |> 
  gt::cols_hide('season') |> 
  gt::tab_header(
    title = gt::md('Who has had bad schedule luck?'),
    subtitle = gt::md(
      sprintf(
        'Teams ordered by most unlucky<br/>(through week %s, %s)', 
        latest_week, 
        current_season
      )
    )
  )
tb_unluckiest_records_current

gt::gtsave(
  tb_unluckiest_records_current,
  filename = file.path(PROJ_DIR, sprintf('%s-unluckiest-records-%s.png', current_date, current_season)),
  zoom = GTSAVE_ZOOM
)

tb_actual_pos_props_current <- actual_pos_props_current |>
  # filter(season == current_season) |> 
  dplyr::arrange(dplyr::desc(prop_top4), prop_bot2, user_name) |> 
  dplyr::transmute(
    user_name,
    prop_weeks_in_top4 = sprintf('%s (%d)', scales::percent(prop_top4), n_top4),
    prop_weeks_in_bot2 = sprintf('%s (%d)', scales::percent(prop_bot2), n_bot2)
  ) |> 
  gt::gt() |> 
  gtExtras::gt_theme_538() |> 
  gt::cols_label(
    'user_name' = 'Player',
    'prop_weeks_in_top4' = 'Top 4',
    'prop_weeks_in_bot2' = 'Bottom 2'
  ) |> 
  # gt::cols_hide('season') |> 
  gt::tab_header(
    title = gt::md('Who has consistently been at the<br/>top and bottom of the standings?'),
    subtitle = gt::md(
      'Teams ordered by descending count of weeks in top 4,<br/>then ascending count in bottom 2.'
    )
  )
tb_actual_pos_props_current
gt::gtsave(
  tb_actual_pos_props_current,
  filename = file.path(PROJ_DIR, sprintf('%s-top2-bottom4-props-%s.png', current_date, current_season)),
  zoom = GTSAVE_ZOOM
)

tabulate_and_save_all_h2h_records <- function(df, which = c('top', 'bottom')) {
  match.arg(which)
  if (which == 'top') {
    order_op <- `-`
    highlight_fill <- 'gold'
    adjective <- 'Best'
  } else if (which == 'bottom') {
    order_op <- `+`
    highlight_fill <- 'indianred'
    adjective <- 'Worst'
  }
  
  tb <- df |> 
    dplyr::transmute(
      `Season` = season,
      `Player` = user_name,
      `W` = w,
      `L` = l,
      `Win %` = w / (w + l)
    ) |> 
    dplyr::arrange(order_op(`Win %`)) |> 
    head(n = TOP_N_FOR_TABLES) |> 
    gt::gt() |> 
    gtExtras::gt_theme_538() |> 
    gt::fmt_percent(
      `Win %`,
      decimals = 0
    ) |> 
    gtExtras::gt_highlight_rows(
      rows = `Season` == .env$current_season,
      fill = highlight_fill
    ) |> 
    gt::tab_header(
      title = gt::md(sprintf('**%s "All Play" record**', adjective)),
      subtitle = gt::md(
        sprintf(
          '%s %s all play records through week %s, since %s', 
          TOP_N_FOR_TABLES, 
          tolower(adjective), 
          latest_week, 
          min(seasons)
        )
      )
    )
  gt::gtsave(
    tb,
    filename = file.path(PROJ_DIR, sprintf('%s-%s-all-play-records.png', CURRENT_DATE, which)),
    zoom = GTSAVE_ZOOM
  )
  invisible(tb)
}

tabulate_and_save_all_h2h_records(
  all_h2h_records,
  which = 'top'
)

tabulate_and_save_all_h2h_records(
  all_h2h_records,
  which = 'bottom'
)

## data plotting ----
max_score <- max(pmax(agg_scores_current$user_score, agg_scores_current$opponent_score))
min_score <- min(pmin(agg_scores_current$user_score, agg_scores_current$opponent_score))
label_buffer <- 5
plot_buffer <- 50
get_ceiling_value <- function(x, buffer, sign = 1) {
  buffer * ceiling(x / buffer) + buffer
}

label_max <- label_buffer * ceiling(max_score / label_buffer) + label_buffer
plot_max <- plot_buffer * ceiling(max_score / plot_buffer) + plot_buffer
label_min <- label_buffer * floor(min_score / label_buffer) - label_buffer
plot_min <- plot_buffer * floor(min_score / plot_buffer) - plot_buffer

agg_scores_plot <- agg_scores_current |> 
  ggplot2::ggplot() +
  ggplot2::aes(
    x = user_score,
    y = opponent_score
  ) +
  ggplot2::geom_vline(
    ggplot2::aes(
      xintercept = mean(user_score)
    ),
    color = COMPLEMENTARY_BACKGROUND_COLOR,
    linetype = 2,
    linewidth = 1
  ) +
  ggplot2::geom_hline(
    ggplot2::aes(
      yintercept = mean(opponent_score)
    ),
    color = COMPLEMENTARY_BACKGROUND_COLOR,
    linetype = 2,
    linewidth = 1
  ) +
  ggplot2::geom_point(
    color = WHITISH_FOREGROUND_COLOR
  ) +
  ggrepel::geom_text_repel(
    seed = 42,
    size = 16 / .pt,
    color = WHITISH_FOREGROUND_COLOR,
    family = FONT,
    ggplot2::aes(label = user_name)
  ) +
  # ggplot2::geom_text(
  #   data = tidyr::tibble(
  #     label = c('Good and\nUnlucky', 'Good and\nLucky', 'Bad and\nLucky', 'Bad and\nUnlucky'),
  #     x = c(label_max, label_max, label_min, label_min),
  #     y = c(label_max, label_min, label_min, label_max)
  #   ),
  #   size = 18 / .pt,
  #   color = 'cyan',
  #   family = FONT,
  #   hjust = 0.5,
  #   vjust = 0.5,
  #   ggplot2::aes(
  #     x = x,
  #     y = y,
  #     label = label
  #   )
  # ) +
  ggtext::geom_richtext(
    data = tidyr::tibble(
      label = c('Good and\nUnlucky', 'Good and\nLucky', 'Bad and\nLucky', 'Bad and\nUnlucky'),
      x = c(label_max, label_max, label_min, label_min),
      y = c(label_max, label_min, label_min, label_max)
    ),
    size = 18 / .pt,
    color = 'cyan',
    family = FONT,
    hjust = 0.5,
    vjust = 0.5,
    fill = BLACKISH_BACKGROUND_COLOR,
    label.color = NA,
    ggplot2::aes(
      x = x,
      y = y,
      label = label
    )
  ) +
  ggplot2::scale_x_continuous(
    labels = scales::comma
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::comma
  ) +
  ggplot2::coord_cartesian(
    xlim = c(plot_min, plot_max),
    ylim = c(plot_min, plot_max)
  ) +
  ggplot2::labs(
    title = 'Is your team "good" (more points scored than average)?\nHave you been lucky (opponent has scored less than average)?',
    subtitle = sprintf('%s season, through week %s', current_season, latest_week),
    x = 'Points For',
    y = 'Points Against'
  )
agg_scores_plot

ggplot2::ggsave(
  agg_scores_plot,
  filename = file.path(PROJ_DIR, sprintf('%s-agg-scores-plot.png', CURRENT_DATE)),
  width = 8,
  height = 8,
  units = 'in'
)
