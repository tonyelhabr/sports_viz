library(tidyverse)
library(qs)

dir_proj <- '59-xg_xpoints'
sim_placings <- file.path(dir_proj, 'sim_placings.qs') |> qs::qread()
# sim_placings |> group_by(team, season) |> summarize(across(n, sum)) |> ungroup()
raw_understat_xpts_by_season <- file.path(dir_proj, 'raw_understat_xpts_by_season.qs') |> qs::qread()
table <- file.path(dir_proj, 'table.qs') |> qs::qread()

predict_with_feature <- function(col) {
  fit <- parsnip::linear_reg() |> 
    parsnip::fit_xy(
      x = raw_understat_xpts_by_season |> select(.data[[col]]),
      y = raw_understat_xpts_by_season |> pull(pts)
    )
  
  preds <- fit |> 
    broom::augment(raw_understat_xpts_by_season) |> 
    select(season, team, rank, .pred) |> 
    group_by(season) |> 
    mutate(.pred_rank = row_number(desc(.pred)))
  
  preds |> 
    yardstick::mae(rank, .pred_rank) |> 
    ungroup() |> 
    select(season, .estimate)
}

maes <- c('xgd', 'xpts') |> 
  set_names() |> 
  map_dfr(predict_with_feature, .id = 'feature')
maes

slice_top_sim_placings <- function(which = c('top', 'bottom'), .n = 10) {
  col <- switch(
    which,
    'top' = 'cumu_prop',
    'bottom' = 'inv_cumu_prop'
  )
  sim_placings |> 
    filter(rank == actual_rank) |> 
    slice_min(.data[[col]], n = .n, with_ties = FALSE) |> 
    mutate(
      which = !!which,
      .before = 1
    )
}

unexpected_sim_placings <- bind_rows(
  slice_top_sim_placings('top'),
  slice_top_sim_placings('bottom')
)
unexpected_sim_placings

xpts_understat_by_season_rolledup <- sim_placings |> 
  group_by(season, team) |> 
  slice_max(n, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  group_by(season) |> 
  mutate(
    xrank = row_number(desc(pts_mean))
  ) |> 
  ungroup() |> 
  select(season, team, pts = actual_pts, rank = actual_rank, xpts = pts_mean, xrank) |> 
  arrange(season, xrank)

fit <- parsnip::linear_reg() |> 
  parsnip::fit_xy(
    x = xpts_understat_by_season_rolledup |> select(xpts),
    y = xpts_understat_by_season_rolledup |> pull(pts)
  )

preds <- fit |> 
  broom::augment(raw_understat_xpts_by_season) |> 
  select(season, team, rank, .pred) |> 
  group_by(season) |> 
  mutate(.pred_rank = row_number(desc(.pred)))

preds |> 
  yardstick::mae(rank, .pred_rank) |> 
  ungroup() |> 
  select(season, .estimate)

