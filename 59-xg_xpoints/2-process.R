library(tidyverse)
library(qs)
library(parallel)
library(future)
library(furrr)

dir_proj <- '59-xg_xpoints'
understat_permuted_xg <- file.path(dir_proj, 'understat_permuted_xg.qs') |> qs::qread()
fotmob_permuted_xg <- file.path(dir_proj, 'fotmob_permuted_xg.qs') |> qs::qread()
raw_understat_xpts_by_match <- file.path(dir_proj, 'raw_understat_xpts_by_match.qs') |> qs::qread()
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

## all seasons for modeling baselines with xgd and xpts
## verified pts from this source matches up with those from table. we should use the data from table wherever possible
raw_understat_xpts_by_season <- raw_understat_xpts_by_match |> 
  group_by(season, team) |> 
  summarize(
    across(c(xpts, xgd), sum)
  ) |> 
  ungroup() |> 
  group_by(season) |> 
  mutate(xrank = row_number(desc(xpts))) |> 
  ungroup() |> 
  inner_join(
    table |> select(season, team, pts, rank),
    by = c('season', 'team')
  ) |> 
  arrange(season, rank)
qs::qsave(raw_understat_xpts_by_season, file.path(dir_proj, 'raw_understat_xpts_by_season.qs'))

xpts_by_season <- table |> 
  inner_join(
    understat_xpts_by_season |> 
      rename_with(~sprintf('%s_understat', .x), c(xpts, xrank)),
    by = c('season', 'team')
  ) |> 
  inner_join(
    fotmob_xpts_by_season |> 
      rename_with(~sprintf('%s_fotmob', .x), c(xpts, xrank)),
    by = c('season', 'team')
  ) |> 
  inner_join(
    raw_understat_xpts_by_season |> 
      select(season, team, xpts, xgd, xrank) |> 
      rename_with(~sprintf('%s_understat_raw', .x), c(xpts, xrank, xgd)),
    by = c('season', 'team')
  )
## lm(xpts_understat_raw ~ xpts_understat + 0, xpts_by_season) ## 0.9985
qs::qsave(xpts_by_season, file.path(dir_proj, 'xpts_by_season.qs'))

## sim ----
## This is too big of a sample space
# crossing(!!!imap(set_names(matches$match_id), ~c('lose', 'draw', 'win')))
init_probs <- understat_xpts_by_match |> 
  select(team, season, match_id, starts_with('prob')) |> 
  rename_with(~str_remove(.x, '^prob_'), starts_with('prob')) |> 
  pivot_longer(
    -c(team, season, match_id),
    names_to = 'result',
    values_to = 'prob'
  )

guaranteed_losses <- init_probs |> 
  group_by(team, season, match_id) |> 
  filter(all(prob == 0)) |> 
  ungroup() |> 
  filter(result == 'lose') |> 
  mutate(prob = 1)

probs <- init_probs |> 
  anti_join(
    guaranteed_losses |> select(team, season, match_id, result),
    by = c('team', 'season', 'match_id', 'result')
  ) |> 
  bind_rows(
    guaranteed_losses
  )

n_cores <- parallel::detectCores()
cores_for_parallel <- ceiling(n_cores * 1/2)
future::plan(
  future::multisession,
  workers = cores_for_parallel
)

set.seed(42)
n_sims <- 10000
sim_pts <- furrr::future_imap_dfr(
  set_names(1:n_sims),
  ~{
    probs |> 
      group_by(team, season, match_id) |> 
      slice_sample(n = 1, weight_by = prob) |> 
      ungroup() |>
      mutate(
        sim_idx = as.integer(!!.y),
        .before = 1
      ) |> 
      mutate(
        pts = case_when(
          result == 'win' ~ 3L,
          result == 'lose' ~ 1L,
          TRUE ~ 0L
        )
      ) |> 
      group_by(season, team, sim_idx) |> 
      summarize(
        across(pts, sum)
      ) |> 
      ungroup() |> 
      group_by(season, sim_idx) |> 
      mutate(
        rank = row_number(desc(pts))
      ) |> 
      ungroup() |> 
      arrange(season, sim_idx, rank)
  }
)

future::plan(future::sequential)

sim_placings <- sim_pts |> 
  group_by(season, team, rank) |> 
  summarize(
    n = n(),
    across(pts, list(min = min, mean = mean, max = max))
  ) |> 
  ungroup() |> 
  group_by(season, team) |> 
  mutate(
    prop = n / sum(n)
  ) |> 
  ungroup() |> 
  arrange(season, team, desc(rank)) |> 
  group_by(season, team) |> 
  mutate(
    inv_cumu_prop = cumsum(prop)
  ) |> 
  arrange(season, team, rank) |> 
  group_by(season, team) |> 
  mutate(
    cumu_prop = cumsum(prop),
    .before = inv_cumu_prop
  ) |> 
  ungroup() |> 
  left_join(
    table |> select(season, team, actual_pts = pts, actual_rank = rank),
    by = c('season', 'team')
  )
qs::qsave(sim_placings, file.path(dir_proj, 'sim_placings.qs'))

