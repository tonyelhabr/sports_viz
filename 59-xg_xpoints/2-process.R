
library(tidyverse)
library(qs)

dir_proj <- '59-xg_xpoints'
understat_permuted_xg <- file.path(dir_proj, 'understat_permuted_xg.qs') |> qs::qread()
fotmob_permuted_xg <- file.path(dir_proj, 'fotmob_permuted_xg.qs') |> qs::qread()
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

summarize_permuted_xg_by_match <- function(df) {
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

understat_xpts_by_match <- understat_permuted_xg |> 
  summarize_permuted_xg_by_match()

fotmob_xpts_by_match <- fotmob_permuted_xg |> 
  summarize_permuted_xg_by_match()

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

understat_xpts_by_season <- understat_xpts_by_match |> 
  aggregate_xpts_by_season()

fotmob_xpts_by_season <- fotmob_xpts_by_match |> 
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

calculate_prob_of_season_placing <- function(xpts_by_match, team, season) {
  xpts_by_match <- understat_xpts_by_match
  team <- 'Leicester City'
  season <- 2020
  str_season <- sprintf('%s/%s', season, season + 1)
  
  matches <- xpts_by_match |> 
    filter(season == !!str_season) |> 
    filter(team == !!team)
  
  probs <- matches |> 
    select(match_id, starts_with('prob')) |> 
    rename_with(~str_remove(.x, '^prob_'), -match_id) |> 
    pivot_longer(
      -match_id,
      names_to = 'result',
      values_to = 'prob'
    )

  ## This is too big of a sample space
  # crossing(!!!imap(set_names(matches$match_id), ~c('lose', 'draw', 'win')))

  sims <- imap_dfr(
    set_names(1:1000),
    ~{
      probs |> 
        group_by(match_id) |> 
        slice_sample(n = 1, weight_by = prob) |> 
        ungroup() |>
        mutate(
          sim_idx = !!.y,
          .before = 1
        )
    }
  )
  agg_sims <- sims |> 
    mutate(
      pts = case_when(
        result == 'win' ~ 3L,
        result == 'lose' ~ 1L,
        TRUE ~ 0L
      )
    ) |> 
    group_by(sim_idx) |> 
    summarize(
      across(pts, sum)
    ) |> 
    ungroup()
  
  agg_sims |> 
    count(pts > 66)
  
  n_sims <- 10000
  n_matches_per_season <- 38
  set.seed(42)
  results <- sample(c('lose', 'draw', 'win'), size = n_matches_per_season * n_sims, replace = TRUE)
  m <- matrix(results, nrow = n_sims, ncol = n_matches_per_season)
  df <- as_tibble(m)
  names(df) <- sprintf('%d', 1:n_matches_per_season)
  df$i <- 1:nrow(df)
  
  sim_matches <- tibble(
    sim_idx = rep(1:n_sims, each = n_matches_per_season),
    match_idx = rep(1:n_sims, times = n_matches_per_season),
    result = results
  ) |> 
    left_join(
      matches |> 
        transmute(match_idx = row_number(), match_id),
      by = 'match_idx'
    ) |> 
    left_join(
      probs,
      by = c('match_id', 'result')
    )
  
  agg_sims <- sim_matches |> 
    mutate(
      pts = case_when(
        result == 'win' ~ 3L,
        result == 'lose' ~ 1L,
        TRUE ~ 0L
      )
    ) |> 
    group_by(sim_idx) |> 
    summarize(
      across(pts, sum)
    ) |> 
    ungroup()
  agg_sims |> 
    count(pts >= 66L)
    
}

