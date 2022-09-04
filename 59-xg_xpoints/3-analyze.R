library(tidyverse)
library(qs)

dir_proj <- '59-xg_xpoints'
understat_sim_placings <- file.path(dir_proj, 'understat_sim_placings.qs') |> qs::qread()
raw_understat_xpts_by_season <- file.path(dir_proj, 'raw_understat_xpts_by_season.qs') |> qs::qread()
table <- file.path(dir_proj, 'table.qs') |> qs::qread()

## what were the most unlikely placings? ----
slice_top_sim_placings <- function(which = c('top', 'bottom'), .n = 10) {
  col <- switch(
    which,
    'top' = 'cumu_prop',
    'bottom' = 'inv_cumu_prop'
  )
  understat_sim_placings |> 
    filter(rank == actual_rank) |> 
    slice_min(.data[[col]], n = .n, with_ties = FALSE) |> 
    mutate(
      which = !!which,
      .before = 1
    )
}

unexpected_understat_sim_placings <- bind_rows(
  slice_top_sim_placings('top'),
  slice_top_sim_placings('bottom')
)
unexpected_understat_sim_placings

## compare season-long pts preds ----
predict_pts_with_understat_feature <- function(col) {
  fit <- parsnip::linear_reg() |> 
    parsnip::fit_xy(
      x = raw_understat_xpts_by_season |> select(.data[[col]]),
      y = raw_understat_xpts_by_season |> pull(pts)
    )
  
  preds <- fit |> 
    broom::augment(raw_understat_xpts_by_season) |> 
    select(season, team, pts, .pred)
  
  preds |> 
    group_by(season) |> 
    yardstick::rmse(pts, .pred) |> 
    ungroup() |> 
    select(season, .estimate)
}

understat_rmses <- c('xgd', 'xpts') |> 
  set_names() |> 
  map_dfr(predict_pts_with_understat_feature, .id = 'feature')
understat_rmses

xpts_understat_by_season_rolledup <- understat_sim_placings |> 
  ## mostly likely pts for each team... could also do a weighted average
  group_by(season, team) |> 
  slice_max(n, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  group_by(season) |> 
  mutate(
    xrank = row_number(desc(pts_mean))
  ) |> 
  ungroup() |>
  select(
    season,
    team,
    pts = actual_pts,
    rank = actual_rank,
    xpts = pts_mean,
    xrank
  ) |>
  arrange(season, xrank)

understat_rolledup_fit <- parsnip::linear_reg() |> 
  parsnip::fit_xy(
    x = xpts_understat_by_season_rolledup |> select(xpts),
    y = xpts_understat_by_season_rolledup |> pull(pts)
  )

understat_rolledup_preds <- understat_rolledup_fit |> 
  broom::augment(raw_understat_xpts_by_season) |> 
  select(season, team, pts, .pred)

understat_rolledup_rmses <- understat_rolledup_preds |> 
  group_by(season) |>
  yardstick::rmse(pts, .pred) |> 
  ungroup() |> 
  select(season, .estimate)

## yikes, the rolledup xpts is worse, which may have expected since a simple regression on season-long values
##   is less prone to game-to-game variance
bind_rows(
  understat_rmses,
  understat_rolledup_rmses |> mutate(feature = 'rolledup_xpts')
) |> 
  pivot_wider(
    names_from = feature,
    values_from = .estimate
  )

