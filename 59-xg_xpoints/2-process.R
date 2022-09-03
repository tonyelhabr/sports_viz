
library(tidyverse)
library(qs)

dir_proj <- '59-xg_xpoints'
understat_permuted_xg <- file.path(dir_proj, 'understat_permuted_xg.qs') |> qs::qread()
fotomob_permuted_xg <- file.path(dir_proj, 'fotmob_permuted_xg.qs') |> qs::qread()
table <- file.path(dir_proj, 'table.qs') |> qs::qread()

summarize_pivoted_permuted_xg <- function(df) {
  outer_prod <- outer(df$value_away, df$value_home)
  p_draw <- sum(diag(outer_prod), na.rm = TRUE)
  p_home <- sum(gdata::upperTriangle(outer_prod), na.rm = TRUE)
  p_away <- sum(gdata::lowerTriangle(outer_prod), na.rm = TRUE)
  tibble(
    draw = p_draw,
    home = p_home,
    away = p_away
  )
}

summarize_permuted_xg <- function(df) {
  pivoted <- df |>
    transmute(
      season,
      match_id,
      date,
      g,
      is_home = ifelse(is_home, 'home', 'away'),
      value,
      cumprob
    ) |>
    pivot_wider(
      names_from = c(is_home),
      values_from = c(value, cumprob),
      values_fill = list(value = 0L)
    ) |> 
    mutate(
      draw = value_away * value_home
    )
  
  pivoted |> 
    select(season, match_id, date, value_away, value_home) |>
    left_join(
      df |> distinct(match_id, team, opponent, is_home),
      by = 'match_id'
    ) |> 
    nest(data = c(value_home, value_away)) |> 
    mutate(
      prob = map(data, summarize_pivoted_permuted_xg)
    ) |> 
    select(-data) |> 
    unnest(prob, names_sep = '_') |> 
    mutate(
      prob_win = ifelse(is_home, prob_home, prob_away),
      prob_lose = ifelse(is_home, prob_away, prob_home),
      xpts = 3 * prob_win + 1 * prob_draw
    ) |> 
    select(-c(prob_home, prob_away))
}

understat_xpts_by_game <- understat_permuted_xg |> 
  summarize_permuted_xg()

fotmob_xpts_by_game <- fotmob_permuted_xg |> 
  summarize_permuted_xg()

aggregate_xpts_by_season <- function(df) {
  df |> 
    group_by(season, team) |> 
    summarize(
      across(xpts, sum)
    ) |> 
    ungroup() |> 
    group_by(season) |> 
    mutate(
      xrank = row_number(desc(xpts)),
      .after = team
    ) |> 
    ungroup() |> 
    arrange(season, desc(xpts))
}

understat_xpts_by_season <- understat_xpts_by_game |> 
  aggregate_xpts_by_season()

fotmob_xpts_by_season <- fotmob_xpts_by_game |> 
  aggregate_xpts_by_season()

joined_xpts_by_season <- table |> 
  inner_join(
    understat_xpts_by_season |> 
      rename_with(~sprintf('%s_understat', .x), c(xpts, xrank)),
    by = c('season', 'team')
  ) |> 
  inner_join(
    fotmob_xpts_by_season |> 
      rename_with(~sprintf('%s_fotmob', .x), c(xpts, xrank)),
    by = c('season', 'team')
  )
joined_xpts_by_season

