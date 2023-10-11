
clean_scores <- scores |> 
  mutate(
    across(
      c(
        user_name,
        opponent_user_name
      ),
      \(.x) 
      case_when(
        .x == 'Andrew ElHabr' ~ 'Andrew E.',
        .x == 'Andrew Lara' ~ 'Andrew L.',
        .x == 'Manuel Espinosa' ~ 'Manny',
        TRUE ~ gsub('\\s.*$', '', .x)
      )
    )
  )

## data processing ----
current_season <- max(SEASONS)
agg_scores <- clean_scores |> 
  filter(season == current_season) |> 
  group_by(user_name) |> 
  summarize(
    across(
      c(
        user_score,
        opponent_score
      ),
      sum
    )
  ) |> 
  ungroup()

latest_week <- scores |> 
  filter(season == current_season) |> 
  filter(user_score > 0) |> 
  slice_max(week, n = 1, with_ties = FALSE) |> 
  pull(week)

library(tidyr)
all_h2h_records <- crossing(
  'season' = SEASONS,
  'week' = 1:latest_week,
  'user_name' = clean_scores$user_name,
  'opponent_user_name' = clean_scores$opponent_user_name
) |> 
  filter(user_name != opponent_user_name) |> 
  inner_join(
    clean_scores |>
      select(season, week, user_name, user_score),
    by = join_by(season, week, user_name)
  ) |>
  inner_join(
    clean_scores |> 
      select(
        season,
        week,
        opponent_user_name = user_name,
        opponent_score = user_score
      ),
    by = join_by(season, week, opponent_user_name)
  ) |> 
  mutate(
    w = user_score > opponent_score,
    l = user_score < opponent_score
  ) |> 
  group_by(season, user_name) |> 
  summarize(
    across(
      c(w, l),
      sum
    )
  ) |> 
  ungroup() |> 
  arrange(season, desc(w))
all_h2h_records |> arrange(desc(w))
all_h2h_records |> arrange(w)

library(gt)
library(gtExtras)

all_h2h_latest_table <- all_h2h_records |> 
  filter(season == current_season) |>
  transmute(
    `Player` = user_name,
    `W` = w,
    `L` = l,
    `Win %` = w / (w + l)
  ) |> 
  gt() |> 
  gt_theme_538() |> 
  fmt_percent(
    `Win %`,
    decimals = 0
  ) |> 
  tab_header(
    title = md(sprintf('**%s "All Play" record**', current_season)),
    subtitle = md('Your record this season if your team<br/>played all other teams every week.')
  )
all_h2h_latest_table


gtsave(
  all_h2h_latest_table,
  filename = file.path(PROJ_DIR, sprintf('all-play-record-%s.png', current_season)),
  zoom = 1.5
)

widen_image <- function(path, ratio = NULL, ratio_resolution = 0.25, height_resolution = 100) {
  img <- magick::image_read(path)
  info <- magick::image_info(img)
  w <- info$width
  h <- info$height
  r <- h / w
  new_ratio <- if (is.null(ratio)) {
    ratio_resolution * ceiling(ratio / ratio_resolution)
  } else {
    if (ratio > r) {
      stop(
        sprintf('`ratio` must be smaller than %s', round(r, 2))
      )
    }
    ratio
  }
  
  new_h <- height_resolution * ceiling(h / height_resolution)
  new_w <- new_h / new_ratio
  dw <- round((new_w - w) / 2)
  if (dw < 0) {
    stop(
      'New width smaller than original width'
    )
  }
  dh <- round((new_h - h) / 2)
  new_img <- magick::image_border(img, 'white', sprintf('%sx%s', dw, dh))
  magick::image_write(new_img, path)
}

TOP_N <- 10
top_all_h2h_table <- all_h2h_records |> 
  transmute(
    `Season` = season,
    `Player` = user_name,
    `W` = w,
    `L` = l,
    `Win %` = w / (w + l)
  ) |> 
  slice_max(`Win %`, n = TOP_N, with_ties = FALSE) |> 
  arrange(desc(`Win %`)) |> 
  gt() |> 
  gt_theme_538() |> 
  fmt_percent(
    `Win %`,
    decimals = 0
  ) |> 
  gt_highlight_rows(
    rows = `Season` == 2023,
    fill = 'gold'
  ) |> 
  tab_header(
    title = md('**Best "All Play" record**'),
    subtitle = md(sprintf('%s best all play records through week %s, since %s', TOP_N, latest_week, min(SEASONS)))
  )

top_all_h2h_table_path <- file.path(PROJ_DIR, sprintf('top-all-play-records.png'))
gtsave(
  top_all_h2h_table,
  filename = top_all_h2h_table_path,
  zoom = 1.5
)
# widen_image(
#   top_all_h2h_table_path,
#   ratio = 2
# )

bottom_all_h2h_table <- all_h2h_records |> 
  transmute(
    `Season` = season,
    `Player` = user_name,
    `W` = w,
    `L` = l,
    `Win %` = w / (w + l)
  ) |> 
  slice_min(`Win %`, n = TOP_N, with_ties = FALSE) |> 
  arrange(`Win %`) |> 
  gt() |> 
  gt_theme_538() |> 
  fmt_percent(
    `Win %`,
    decimals = 0
  ) |> 
  gt_highlight_rows(
    rows = `Season` == 2023,
    fill = 'indianred'
  ) |>
  tab_header(
    title = md('**Worst "All Play" records**'),
    subtitle = md(sprintf('%s worst all play records through week %s, since %s', TOP_N, latest_week, min(SEASONS)))
  )

bottom_all_h2h_table_path <- file.path(PROJ_DIR, sprintf('bottom-all-play-records.png'))
gtsave(
  bottom_all_h2h_table,
  filename = bottom_all_h2h_table_path,
  zoom = 1.5
)
# widen_image(
#   bottom_all_h2h_table_path,
#   ratio = 2
# )

## data plotting ----
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

agg_scores_plot <- agg_scores |> 
  ggplot() +
  aes(
    x = user_score,
    y = opponent_score
  ) +
  geom_vline(
    aes(
      xintercept = mean(user_score)
    ),
    color = COMPLEMENTARY_BACKGROUND_COLOR,
    linetype = 2,
    linewidth = 1
  ) +
  geom_hline(
    aes(
      yintercept = mean(opponent_score)
    ),
    color = COMPLEMENTARY_BACKGROUND_COLOR,
    linetype = 2,
    linewidth = 1
  ) +
  geom_point(
    color = WHITISH_FOREGROUND_COLOR
  ) +
  ggrepel::geom_text_repel(
    seed = 42,
    size = 16 / .pt,
    color = WHITISH_FOREGROUND_COLOR,
    family = FONT,
    aes(label = user_name)
  ) +
  geom_text(
    data = tibble(
      label = c('Good and\nUnlucky', 'Good and\nLucky', 'Bad and\nLucky', 'Bad and\nUnlucky'),
      x = c(625, 625, 475, 475),
      y = c(650, 475, 475, 650)
    ),
    size = 18 / .pt,
    color = 'cyan',
    family = FONT,
    hjust = 0.5,
    vjust = 0.5,
    aes(
      x = x,
      y = y,
      label = label
    )
  ) +
  labs(
    title = 'Is your team "good" (more points scored than average)?\nHave you been lucky (opponent has scored less than average)?',
    subtitle = sprintf('%s season, through week %s', current_season, latest_week),
    x = 'Points For',
    y = 'Points Against'
  )
agg_scores_plot

ggsave(
  agg_scores_plot,
  filename = file.path(PROJ_DIR, 'agg-scores-plot.png'),
  width = 8,
  height = 8,
  units = 'in'
)

