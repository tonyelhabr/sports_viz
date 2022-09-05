library(tidyverse)
library(gdata)
library(qs)
library(parallel)
library(future)
library(furrr)
library(nnet)
library(ggplot2)
library(extrafont)
library(scales)

dir_proj <- '59-xg_xpoints'
understat_permuted_xg <- file.path(dir_proj, 'understat_permuted_xg.qs') |> qs::qread()
fotmob_permuted_xg <- file.path(dir_proj, 'fotmob_permuted_xg.qs') |> qs::qread()
raw_understat_xpts_by_match <- file.path(dir_proj, 'raw_understat_xpts_by_match.qs') |> qs::qread()
table <- file.path(dir_proj, 'table.qs') |> qs::qread()

summarize_pivoted_permuted_xg <- function(prob_away, prob_home) {
  outer_prod <- outer(prob_away, prob_home)
  p_draw <- sum(diag(outer_prod), na.rm = TRUE)
  p_home <- sum(gdata::upperTriangle(outer_prod), na.rm = TRUE)
  p_away <- sum(gdata::lowerTriangle(outer_prod), na.rm = TRUE)
  list(
    draw = p_draw,
    home = p_home,
    away = p_away
  )
}

summarize_permuted_xg_by_match <- function(df) {
  pivoted <- df |>
    transmute(
      match_id,
      season,
      date,
      g,
      is_home = ifelse(is_home, 'home', 'away'),
      prob
    ) |>
    pivot_wider(
      names_from = is_home,
      names_prefix = 'prob_',
      values_from = prob,
      values_fill = 0L
    )
  
  pivoted |> 
    select(match_id, season, date, prob_away, prob_home) |>
    group_by(match_id, season, date) |> 
    summarize(
      across(starts_with('prob_'), ~list(.x))
    ) |> 
    ungroup() |> 
    inner_join(
      df |> distinct(match_id, team, opponent, is_home),
      by = 'match_id'
    ) |> 
    mutate(
      prob = map2(prob_away, prob_home, summarize_pivoted_permuted_xg)
    ) |> 
    select(-starts_with('prob_')) |> 
    unnest_wider(prob, names_sep = '_') |> 
    mutate(
      prob_win = ifelse(is_home, prob_home, prob_away),
      prob_lose = ifelse(is_home, prob_away, prob_home),
      xpts = 3 * prob_win + 1 * prob_draw
    ) |> 
    select(-c(prob_home, prob_away))
}

understat_xpts_by_match <- understat_permuted_xg |> summarize_permuted_xg_by_match()
fotmob_xpts_by_match <- fotmob_permuted_xg |> summarize_permuted_xg_by_match()

## match calib ----
xpts_by_match <- raw_understat_xpts_by_match |> 
  select(season, date, team, result, pts) |> 
  inner_join(
    understat_xpts_by_match |> 
      select(season, date, team, starts_with('prob_'), xpts) |> 
      rename_with(~sprintf('%s_understat', .x), c(starts_with('prob_'), xpts)),
    by = c('season', 'date', 'team')
  ) |> 
  inner_join(
    fotmob_xpts_by_match |> 
      select(season, date, team, starts_with('prob_'), xpts) |> 
      rename_with(~sprintf('%s_fotmob', .x), c(starts_with('prob_'), xpts)),
    by = c('season', 'date', 'team')
  ) |> 
  mutate(
    across(result, ~factor(.x, levels = c('l', 'd', 'w')))
  )

cor_draw <- cor(xpts_by_match$prob_draw_fotmob, xpts_by_match$prob_draw_understat)
cor_win <- cor(xpts_by_match$prob_win_fotmob, xpts_by_match$prob_win_understat)
cor_lose <- cor(xpts_by_match$prob_lose_fotmob, xpts_by_match$prob_lose_understat)
round(c(cor_draw, cor_win, cor_lose), 3)

result_props <- xpts_by_match |> 
  count(result) |> 
  mutate(prop = n / sum(n))

compute_mse <- function(truth, estimate) {
  mean((truth - estimate)^2)
}

diagnose_prob_by_match <- function(src, result) {
  
  df <- xpts_by_match |> 
    mutate(
      result = ifelse(result == !!result, 1L, 0L) |> factor()
    )
  
  result_name <- switch(
    result,
    'w' = 'win',
    'l' = 'lose',
    'd' = 'draw'
  )
  col <- sprintf('prob_%s_%s', result_name, src)

  fit <- glm(
    df$result ~ df[[col]],
    family = 'binomial'
  )
  
  probs <- tibble(
    result_num = as.numeric(df$result) - 1,
    .prob = unname(predict(fit, type = 'response'))
  )
  n_buckets <- 20
  alpha <- 0.05
  calib <- probs |>
    mutate(
      across(.prob, ~round(.x * n_buckets) / n_buckets)
    ) |>
    group_by(.prob) |>
    summarize(
      ## Jeffrey's prior
      ci_lower = qbeta(alpha / 2, sum(result_num) + 0.5, n() - sum(result_num) + 0.5),
      ci_upper = qbeta(1 - alpha / 2, sum(result_num) + 0.5, n() - sum(result_num) + 0.5),
      actual = sum(result_num) / n(),
      n = n()
    ) |> 
    ungroup()

  mse <- compute_mse(probs$result_num, probs$.prob)

  ref_prob <- result_props |> 
    filter(result == !!result) |> 
    pull(prop)
  
  ref_mse <- compute_mse(probs$result_num, ref_prob)
  bss <- 1 - (mse / ref_mse)
  
  list(
    calib = calib,
    mse = mse,
    bss = bss
  )
}

diagnostics <- crossing(
  result = c('w', 'd'),
  src = c('understat', 'fotmob')
) |> 
  mutate(
    diagnostics = map2(src, result, diagnose_prob_by_match)
  ) |> 
  unnest_wider(diagnostics)
diagnostics

calib <- diagnostics |> 
  select(result, src, calib) |> 
  unnest_longer(calib) |> 
  unnest_wider(calib) |> 
  mutate(
    across(result, ~ifelse(.x == 'w', 'Win', 'Draw'))
  )

blackish_background <- '#1c1c1c'
gray_points <- '#4d4d4d'
gray_text <- '#999999'

font <- 'Karla'
extrafont::loadfonts(quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = font),
  title = element_text(size = 20, color = 'white'),
  plot.title = element_text(face = 'bold', size = 16, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = element_text(size = 14, color = '#f1f1f1'),
  axis.text = element_text(color = 'white', size = 14),
  axis.title = element_text(size = 14, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  legend.text = element_text(size = 14, color = 'white'),
  legend.position = 'top',
  panel.grid.major = element_line(color = gray_points),
  panel.grid.minor = element_line(color = gray_points),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  strip.text = element_text(size = 14, color = 'white', face = 'bold', hjust = 0),
  plot.background = element_rect(fill = blackish_background, color = blackish_background),
  plot.caption = element_text(color = 'white', hjust = 1, size = 10, face = 'italic'),
  plot.caption.position = 'plot',
  # plot.tag = ggtext::element_markdown(size = 14, color = 'white', hjust = 0),
  # plot.tag.position = c(0.01, 0.02),
  panel.background = element_rect(fill = blackish_background, color = blackish_background)
)
update_geom_defaults('point', list(color = 'white'))

p_calib <- calib |> 
  ggplot() +
  aes(x = .prob, y = actual, color = src) +
  geom_point(
    aes(size = n),
    position = position_dodge(width = 0.05)
    # position = 'dodge'
  ) +
  geom_errorbar(
    # data = calib |> filter(n >= 10),
    aes(
      ymin = ci_lower, 
      ymax = ci_upper
    ), 
    # color = gray_points,
    position = position_dodge(width = 0.05),
    # position = 'dodge',
    width = 0.025
  ) +
  geom_abline(slope = 1, intercept = 0, color = 'white') +
  scale_x_continuous(labels = scales::percent, limits = c(-0.025, 1.025)) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.025, 1.025)) +
  facet_wrap(~result) +
  guides(
    color = guide_legend('Source', override.aes = list(size = 3)),
    size = guide_legend('Sample size')
  ) +
  labs(
    title = 'Calibration of implied match outcome probabilities',
    subtitle = '2021/22 - 2021/22 English Premier League',
    x = 'Probability',
    y = 'Actual Proportion',
    caption = 'Error bars represent a 95% posterior credible interval for the mean predicted chance using a beta-binomial conjugate (i.e. Jeffreys\' Prior).\nLoss calibration is redundant with win calibration.'
  )
p_calib
path_calib <- file.path(dir_proj, 'calib.png')

ggsave(
  p_calib,
  filename = path_calib,
  width = 10,
  height = 7.5
)

compute_rmse <- function(truth, estimate) {
  sqrt(mean((truth - estimate)^2))
}

compute_xpts_by_match_rmse <- function(src) {

  col <- sprintf('xpts_%s', src)
  fit <- lm(xpts_by_match$pts ~ xpts_by_match[[col]])
  
  pred <- predict(fit)
  
  compute_rmse(xpts_by_match$pts, pred)
}

rmses_xpts_by_match <- c('understat', 'fotmob') |> 
  set_names() |> 
  map_dbl(compute_xpts_by_match_rmse)
rmses_xpts_by_match

compute_implied_xpts_by_match_rmse <- function(src) {
  col_win <- sprintf('prob_win_%s', src)
  col_draw <- sprintf('prob_draw_%s', src)
  
  df <- xpts_by_match |> 
    mutate(
      win = ifelse(result == 'w', 1L, 0L) |> factor(),
      draw = ifelse(result == 'd', 1L, 0L) |> factor()
    )
  
  fit_win <- glm(
    df$win ~ df[[col_win]],
    family = 'binomial'
  )
  
  fit_draw <- glm(
    df$draw ~ df[[col_draw]],
    family = 'binomial'
  )
  
  prob_win <- predict(fit_win, type = 'response')
  prob_draw <- predict(fit_draw, type = 'response')
  xpts <- 3 * prob_win + 1 * prob_draw
  
  compute_rmse(df$pts, xpts)
}

rmses_implied_xpts_by_match <- c('understat', 'fotmob') |> 
  set_names() |> 
  map_dbl(compute_implied_xpts_by_match_rmse)
round(rmses_implied_xpts_by_match, 2)

compute_implied_xpts_by_match_rmse <- function(src) {

  col_win <- sprintf('prob_win_%s', src)
  col_draw <- sprintf('prob_draw_%s', src)
  fit <- nnet::multinom(
    xpts_by_match$result ~ xpts_by_match[[col_win]] + xpts_by_match[[col_draw]],
    trace = FALSE
  )
  probs <- predict(fit, type = 'probs') |> as_tibble()
  xpts <- 3 * probs$w + 1 * probs$d
  
  compute_rmse(xpts_by_match$pts, xpts)
}

rmses_implied_xpts_by_match <- c('understat', 'fotmob') |> 
  set_names() |> 
  map_dbl(compute_implied_xpts_by_match_rmse)
round(rmses_implied_xpts_by_match, 2)


## by season ----
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

understat_xpts_by_season <- understat_xpts_by_match |> aggregate_xpts_by_season()
fotmob_xpts_by_season <- fotmob_xpts_by_match |> aggregate_xpts_by_season()

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
# crossing(!!!imap(set_names(unique(understat_xpts_by_match$match_id)), ~c('lose', 'draw', 'win')))
init_understat_probs_by_match <- understat_xpts_by_match |> 
  select(team, season, match_id, starts_with('prob')) |> 
  rename_with(~str_remove(.x, '^prob_'), starts_with('prob')) |> 
  pivot_longer(
    -c(team, season, match_id),
    names_to = 'result',
    values_to = 'prob'
  )

understat_guaranteed_losses <- init_understat_probs_by_match |> 
  group_by(team, match_id) |> 
  filter(all(prob == 0)) |> 
  ungroup() |> 
  filter(result == 'lose') |> 
  mutate(prob = 1)

understat_probs_by_match <- init_understat_probs_by_match |> 
  anti_join(
    understat_guaranteed_losses |> select(team, match_id, result),
    by = c('team', 'match_id', 'result')
  ) |> 
  bind_rows(
    understat_guaranteed_losses
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
    understat_probs_by_match |> 
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
qs::qsave(understat_sim_pts_by_season, file.path(dir_proj, 'understat_sim_pts_by_season.qs'))

understat_sim_placings <- understat_sim_pts_by_season |> 
  group_by(season, team, xrank = rank) |> 
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
  arrange(season, team, xrank) |> 
  group_by(season, team) |> 
  mutate(
    cumu_prop = cumsum(prop)
  ) |> 
  ungroup()
qs::qsave(understat_sim_placings, file.path(dir_proj, 'understat_sim_placings.qs'))

