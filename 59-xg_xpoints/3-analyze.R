library(tidyverse)
library(qs)

dir_proj <- '59-xg_xpoints'
sim_placings <- file.path(dir_proj, 'sim_placings.qs') |> qs::qread()
# sim_placings |> group_by(team, season) |> summarize(across(n, sum)) |> ungroup()
raw_understat_xpts_by_season <- file.path(dir_proj, 'raw_understat_xpts_by_season.qs') |> qs::qread()

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

slice_top_sim_placings <- function(which = c('top', 'bottom'), n = 5) {
  col <- switch(
    which,
    'top' = 'cumu_prop',
    'bottom' = 'inv_cumu_prop'
  )
  sim_placings |> 
    filter(rank == sim_rank) |> 
    slice_min(.data[[col]], n = 5, with_ties = FALSE) |> 
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

sim_placings |> 
  filter(team == 'Fulham', season == '2020/2021')

## some teams don't have a placing?
unjoined <- table |> 
  distinct(season, team, rank) |> 
  anti_join(
    sim_placings |> 
      filter(rank == sim_rank) |> 
      distinct(season, team)
  )
unjoined

sim_placings |> 
  filter(season == '2020/2021', team == 'West Bromwich Albion')
