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

## data ----
scores <- readr::read_csv(file.path(DATA_DIR, 'team-scores-all.csv'))
seasons <- unique(scores$season)

clean_scores <- scores |> 
  dplyr::mutate(
    dplyr::across(
      c(
        user_name,
        opponent_user_name
      ),
      \(.x) 
      dplyr::case_when(
        .x == 'Andrew ElHabr' ~ 'Andrew E.',
        .x == 'Andrew Lara' ~ 'Andrew L.',
        .x == 'Manuel Espinosa' ~ 'Manny',
        TRUE ~ gsub('\\s.*$', '', .x)
      )
    )
  )

## data processing ----
current_season <- max(seasons)
agg_scores <- clean_scores |> 
  dplyr::filter(season == current_season) |> 
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

latest_week <- scores |> 
  dplyr::filter(season == current_season) |> 
  dplyr::filter(user_score > 0) |> 
  dplyr::slice_max(week, n = 1, with_ties = FALSE) |> 
  dplyr::pull(week)

actual_records <- clean_scores |> 
  dplyr::mutate(
    w = user_score > opponent_score,
    l = user_score < opponent_score
  ) |> 
  dplyr::arrange(season, week) |> 
  dplyr::group_by(season, user_name) |> 
  dplyr::mutate(
    dplyr::across(
      c(w, l),
      cumsum
    )
  ) |> 
  dplyr::ungroup()

all_h2h_records <- tidyr::crossing(
  'season' = seasons,
  'week' = 1:latest_week,
  'user_name' = clean_scores$user_name,
  'opponent_user_name' = clean_scores$opponent_user_name
) |> 
  dplyr::filter(user_name != opponent_user_name) |> 
  dplyr::inner_join(
    clean_scores |>
      dplyr::select(season, week, user_name, user_score),
    by = dplyr::join_by(season, week, user_name)
  ) |>
  dplyr::inner_join(
    clean_scores |> 
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
        actual_w = w,
        actual_l = l,
        actual_w_prop = w / (w + l)
      ),
    by = dplyr::join_by(season, user_name),
    relationship = 'many-to-many'
  ) |> 
  dplyr::mutate(
    w_prop_d = actual_w_prop - all_play_w_prop
  ) |> 
  dplyr::arrange(w_prop_d)

unluckiest_records <- compared_records |> 
  dplyr::arrange(w_prop_d) |> 
  dplyr::transmute(
    `Season` = season,
    `Player` = user_name,
    `W` = actual_w,
    `L` = actual_l,
    `Win %` = actual_w_prop,
    `All Play Win %` = all_play_w_prop,
    `Win % - All Play Win %` = w_prop_d
  ) |> 
  gt::gt() |> 
  gtExtras::gt_theme_538() |> 
  gt::fmt_percent(
    matches('[%]$'),
    decimals = 0
  ) |> 
  gtExtras::gt_highlight_rows(
    rows = `Season` == .env$current_season,
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
unluckiest_records

gt::gtsave(
  unluckiest_records,
  filename = file.path(PROJ_DIR, sprintf('2023-11-21-unluckiest-records-%s.png', current_season)),
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
    filename = file.path(PROJ_DIR, sprintf('2023-11-21-%s-all-play-records.png', which)),
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
agg_scores_plot <- agg_scores |> 
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
  ggplot2::geom_text(
    data = tidyr::tibble(
      label = c('Good and\nUnlucky', 'Good and\nLucky', 'Bad and\nLucky', 'Bad and\nUnlucky'),
      x = c(625, 625, 475, 475),
      y = c(650, 475, 475, 650)
    ),
    size = 18 / .pt,
    color = 'cyan',
    family = FONT,
    hjust = 0.5,
    vjust = 0.5,
    ggplot2::aes(
      x = x,
      y = y,
      label = label
    )
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
  filename = file.path(PROJ_DIR, 'agg-scores-plot.png'),
  width = 8,
  height = 8,
  units = 'in'
)
