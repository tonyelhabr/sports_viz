library(qs)
library(dplyr)
library(tidyr)

library(gt)
library(gtExtras)

library(ggplot2)
library(ggforce)
library(sysfonts)
library(ggrepel)
library(ggtext)
library(grid)

WHITISH_FOREGROUND_COLOR <- 'white'
COMPLEMENTARY_FOREGROUND_COLOR <- '#cbcbcb' # '#f1f1f1'
BLACKISH_BACKGROUND_COLOR <- '#1c1c1c'
COMPLEMENTARY_BACKGROUND_COLOR <- '#4d4d4d'
FONT <- 'Lato'
sysfonts::font_add_google(FONT, FONT)

ggplot2::theme_set(ggplot2::theme_minimal())
ggplot2::theme_update(
  text = ggplot2::element_text(family = FONT),
  title = ggplot2::element_text(size = 26, color = WHITISH_FOREGROUND_COLOR),
  plot.title = ggplot2::element_text(face = 'bold', size = 20, color = WHITISH_FOREGROUND_COLOR),
  plot.title.position = 'plot',
  plot.subtitle = ggplot2::element_text(size = 16, color = COMPLEMENTARY_FOREGROUND_COLOR),
  axis.text = ggplot2::element_text(color = WHITISH_FOREGROUND_COLOR, size = 14),
  # axis.title = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.title.x = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.title.y = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.line = ggplot2::element_blank(),
  strip.text = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0),
  panel.grid.major = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor.x = ggplot2::element_blank(),
  panel.grid.minor.y = ggplot2::element_blank(),
  plot.margin = ggplot2::margin(10, 20, 10, 20),
  plot.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR),
  plot.caption = ggplot2::element_text(color = WHITISH_FOREGROUND_COLOR, hjust = 0, size = 10, face = 'plain'),
  plot.caption.position = 'plot',
  plot.tag = ggplot2::element_text(size = 10, color = WHITISH_FOREGROUND_COLOR, hjust = 1),
  plot.tag.position = c(0.99, 0.99),
  panel.spacing.x = grid::unit(2, 'lines'),
  panel.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR)
)

PROJ_DIR <- '76-2023_ff_luck'
TOP_N_FOR_TABLES <- 10
GTSAVE_ZOOM <- 1.5

## data ----
scores <- qs::qread(file.path(PROJ_DIR, 'seasons.qs'))
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

all_h2h_latest_table <- all_h2h_records |> 
  dplyr::filter(season == current_season) |>
  dplyr::transmute(
    `Player` = user_name,
    `W` = w,
    `L` = l,
    `Win %` = w / (w + l)
  ) |> 
  gt::gt() |> 
  gtExtras::gt_theme_538() |> 
  gt::fmt_percent(
    `Win %`,
    decimals = 0
  ) |> 
  gt::tab_header(
    title = gt::md(sprintf('**%s "All Play" record**', current_season)),
    subtitle = gt::md('Your record this season if your team<br/>played all other teams every week.')
  )
all_h2h_latest_table

gt::gtsave(
  all_h2h_latest_table,
  filename = file.path(PROJ_DIR, sprintf('all-play-record-%s.png', current_season)),
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
    filename = file.path(PROJ_DIR, sprintf('%s-all-play-records.png', which)),
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
