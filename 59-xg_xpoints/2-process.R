
library(tidyverse)
library(qs)
library(parallel)
library(future)
library(furrr)

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

calculate_prob_of_season_placing <- function(xpts_by_match, season, team, seed = 42, n_sims = 1000) {
  # xpts_by_match <- understat_xpts_by_match
  # team <- 'Leicester City'
  # season <- '2020/2021'
  message(
    sprintf('%s, %s', season, team)
  )
  
  matches <- xpts_by_match |> 
    filter(season == !!season) |> 
    filter(team == !!team)
  
  probs <- xpts_by_match |> 
    select(match_id, starts_with('prob')) |> 
    rename_with(~str_remove(.x, '^prob_'), -match_id) |> 
    pivot_longer(
      -match_id,
      names_to = 'result',
      values_to = 'prob'
    )
  
  ## This is too big of a sample space
  # crossing(!!!imap(set_names(matches$match_id), ~c('lose', 'draw', 'win')))
  
  withr::local_seed(seed)
  sims <- imap_dfr(
    set_names(1:n_sims),
    ~{
      probs |> 
        group_by(match_id) |> 
        slice_sample(n = 1, weight_by = prob) |> 
        ungroup() |>
        mutate(
          sim_idx = as.integer(!!.y),
          .before = 1
        )
    }
  )
  
  agg <- sims |> 
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
  
  prop <- agg |> 
    count(sim_pts = pts) |> 
    mutate(i = row_number(sim_pts), prop = n / sum(n))
  
  prop_table <- prop |> 
    left_join(
      table |> filter(team != !!team, season == !!season),
      by = character()
    )
  
  equal_pts <- prop_table |> 
    filter(sim_pts == pts)
  
  n_equal <- nrow(equal_pts)
  equal_pts_ranks <- if (n_equal > 0) {
    equal_pts |> 
      group_by(i, sim_pts) |> 
      slice_sample(n = 1) |>  
      ungroup() |> 
      select(i, sim_pts, sim_rank = rank)
  } else {
    tibble()
  }
  
  more_pts <- prop_table |> 
    anti_join(
      equal_pts |> select(i),
      by = 'i'
    ) |> 
    filter(sim_pts > pts)
  
  n_more <- nrow(more_pts)
  more_pts_ranks <- if (n_more > 0) {
    more_pts |> 
      group_by(i) |> 
      slice_min(rank, n = 1, with_ties = FALSE) |> 
      ungroup() |> 
      transmute(i, sim_pts, sim_rank = rank - 1)
  } else {
    tibble()
  }
  
  less_pts_ranks <- if ((n_equal + n_more) == nrow(prop)) {
    tibble()
  } else {
    actual_rank <- table |> 
      filter(team == !!team, season == !!season) |> 
      pull(rank)
    
    max_rank <- 20
    if (actual_rank == 20) {
      max_rank <- 19
    }
    
    prop_table |> 
      filter(sim_pts < pts) |> 
      filter(rank == !!max_rank) |> 
      distinct(i, sim_pts, sim_rank = rank)
  }
  
  bind_rows(
    more_pts_ranks,
    equal_pts_ranks,
    less_pts_ranks
  ) |> 
    inner_join(
      prop,
      by = c('i', 'sim_pts')
    ) |> 
    arrange(i) |> 
    group_by(sim_rank) |> 
    summarize(
      across(sim_pts, list(min = min, max = max)),
      across(c(n, prop), sum)
    )
  
}

n_cores <- parallel::detectCores()
cores_for_parallel <- ceiling(n_cores * 1/2)
future::plan(
  future::multisession,
  workers = cores_for_parallel
)

nested_sim_placings <- joined_xpts_by_season |> 
  distinct(team, season) |> 
  mutate(
    sims = furrr::future_map2(
      season, team, 
      ~calculate_prob_of_season_placing(
        understat_xpts_by_match, 
        season = ..1, 
        team = ..2,
        n_sims = 10
      ),
      .options = furrr::furrr_options(seed = 42)
    )
  )
future::plan(future::sequential)

sim_placings <- nested_sim_placings |> 
  unnest(sims) |> 
  arrange(season, team, desc(sim_rank)) |> 
  group_by(season, team) |> 
  mutate(
    inv_cumu_prop = cumsum(prop)
  ) |> 
  arrange(season, team, sim_rank) |> 
  group_by(season, team) |> 
  mutate(
    cumu_prop = cumsum(prop),
    .before = inv_cumu_prop
  ) |> 
  ungroup() |> 
  left_join(
    table,
    by = c('season', 'team')
  )
qs::qsave(sim_placings, file.path(dir_proj, 'sim_placings.qs'))

