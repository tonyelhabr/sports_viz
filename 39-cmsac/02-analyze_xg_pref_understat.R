
library(tidyverse)
library(tidymodels)
library(tonythemes)
tonythemes::theme_set_tony()

file.path('31-wp_soccer', 'teams_players_stats.rds') %>%
  read_rds() %>% 
  janitor::clean_names() %>% 
  filter(position == 'S') %>% 
  arrange(-time) %>% 
  count(year)
  count(position, sort = TRUE)

stats_mini <-
  file.path('31-wp_soccer', 'teams_players_stats.rds') %>%
  read_rds() %>% 
  janitor::clean_names() %>% 
  filter(league_name != 'RFPL') %>% 
  mutate(
    across(position, ~str_sub(.x, 1, 1)),
    # across(position, ~ifelse(.x == 'S', 'F', .x)),
    across(year, ~sprintf('%s-%s', .x, str_sub(.x + 1, 3, 4)))
  ) %>% 
  filter(position %>% str_detect('F|M')) %>%
  select(
    player_id,
    player_name,
    position,
    season = year,
    league_name,
    team_name,
    gp = games,
    mp = time,
    g = goals,
    xg = x_g,
    a = assists,
    xa = x_a,
    shots,
    key_passes,
    # npxg = npx_g,
    xg_chain = x_g_chain
  )
stats_mini

# Quick check
stats_mini %>% count(league_name)
stats_mini %>% count(position)

# Individual players have multiple positions... need to fix so that there's a 1-to-1 mapping
stats_mini %>% count(player_id, player_name, position) %>% count(player_id, player_name, sort = TRUE) %>% filter(n == 2L) -> Z

# Save as variable since we'll use it again
stats_agg_init <-
  stats_mini %>% 
  group_by(player_name, player_id, season, position) %>% 
  summarize(
    # n = n(),
    across(c(gp:last_col()), sum, na.rm = TRUE)
  ) %>% 
  ungroup()
stats_agg_init %>% filter(player_name == 'Lionel Messi')

# Assign a player a position based on where they've played the most minutes.
pos <-
  stats_agg_init %>% 
  group_by(player_name, player_id, position) %>% 
  summarize(
    # n = n(),
    across(c(gp:last_col()), sum, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  group_by(player_id, player_name) %>% 
  slice_max(mp, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(player_id, player_name, position)
pos
pos %>% filter(player_name == 'Lionel Messi')
stats_agg_init %>% select(matches('a$'))
stats_agg <-
  stats_agg_init %>%
  select(-position) %>% 
  left_join(pos) %>% 
  filter(g > 0, mp > 500) %>% 
  mutate(
    xg_ps = xg / shots,
    xgd = xg - g,
    xgd_frac = xgd / xg,
    g_gt_xg = ifelse(xgd < 0, 1L, 0L)
  ) %>% 
  arrange(player_name, player_id, season) %>% 
  group_by(player_id, player_name, position) %>% 
  mutate(
    across(g_gt_xg, list(lag1 = ~dplyr::lag(.x)))
  ) %>% 
  ungroup()
stats_agg

# naive counting ----
stats_agg_lag <-
  stats_agg %>% 
  group_by(player_name) %>% 
  mutate(
    across(
      g_gt_xg,
      list(
        lag1 = ~lag(.x, 1),
        lag2 = ~lag(.x, 2),
        lag3 = ~lag(.x, 3),
        lag4 = ~lag(.x, 4),
        lag5 = ~lag(.x, 5)
      ),
      .names = '{fn}'
    )
  ) %>% 
  ungroup() %>% 
  mutate(
    xgd_frac = xgd / xg
  )
stats_agg_lag %>% select(player_name, season, g, xg, g_gt_xg, matches('^lag')) %>% filter(player_name == 'Lionel Messi')

stats_agg_agg <-
  stats_agg_lag %>% 
  group_by(player_id, player_name, position) %>% 
  summarize(
    across(c(gp:g_gt_xg), sum)
  ) %>% 
  ungroup()
stats_agg_agg

stats_agg_lag_filt <-
  stats_agg_lag %>% 
  filter(season == '2020-21') %>% 
  select(-season)
stats_agg_lag_filt

stats_agg_lag_filt %>% 
  count(lag1, lag2, lag3, lag4, lag5)

# the best of the best
stats_agg_lag_filt_top <-
  stats_agg_lag_filt %>% 
  filter(g_gt_xg == 1L, lag1 == 1L, lag2 == 1L, lag3 == 1L, lag4 == 1L, lag5 == 1L) %>% 
  select(-matches('^lag'))
stats_agg_lag_filt_top

# show stats aggregated over all years where we have player data
stats_agg_agg %>% 
  semi_join(stats_agg_lag_filt_top %>% select(player_id, player_name)) %>% 
  left_join(
    # show league where player played the most seasons (by minute would be better, like we did for position)
    stats_mini %>% 
      count(player_id, player_name, league_name, name = 'n_league') %>% 
      group_by(player_id, player_name) %>% 
      slice_max(n_league, with_ties = FALSE) %>% 
      ungroup()
  ) %>% 
  arrange(xgd) %>% 
  select(league_name, player_name, g, matches('xg'), -xg_chain)

stats_agg_lag %>% filter(player_name %>% str_detect('Son Heung')) %>% select(season, g, xg, g_gt_xg)
stats_agg_lag %>% filter(player_name == 'Lionel Messi') %>% select(season, g, xg, g_gt_xg)

# model ----
stats_agg_filt <- stats_agg %>% drop_na(g_gt_xg_lag1)

metset <- metric_set(accuracy, roc_auc)
ctrl <- control_grid(verbose = TRUE)
set.seed(42)
ids <- stats_agg_filt %>% count(player_id, sort = TRUE)
ids_trn <- ids %>% slice_sample(weight_by = n, prop = 0.8)
ids_tst <- ids %>% anti_join(ids_trn)

.split <- function(ids) {
  stats_agg_filt %>% mutate(across(g_gt_xg, factor)) %>% semi_join(ids %>% select(player_id))
}
df_trn <- .split(ids_trn)
df_tst <- .split(ids_tst)

folds <- df_trn %>% group_vfold_cv(group = player_id, v = 5)
rec_log <-
  df_trn %>%
  recipe(
    formula(
      g_gt_xg ~ player_name + player_id + season + position + gp + mp + xg + a + xa + shots + key_passes + xg_chain + xg_ps + g_gt_xg_lag1
    ),
    formula(g_gt_xg ~ .),
    data = .
  ) %>%
  # step_rm(g, xgd, xgd_frac, npxg) %>% 
  update_role(player_name, player_id, season, new_role = 'id') %>% 
  # step_indicate_na(g_gt_xg_lag1)
  step_dummy(all_nominal_predictors())
rec_log
jui_trn_log <- rec_log %>% prep() %>% juice()
jui_trn_log
jui_trn_log %>% skimr::skim()

n_col_log <- jui_trn_log %>% ncol() %>% {. - 3}

rec_log %>% 
  workflow(
    logistic_reg()
  ) %>% 
  fit(df_trn) %>% 
  tidy() %>% 
  arrange(desc(abs(estimate))) %>% 
  mutate(
    across(p.value, round, 2),
    across(term, ~fct_reorder(.x, estimate))
  ) %>% 
  # filter(term != 'xg_ps') %>% 
  ggplot() +
  aes(x = estimate, y = term) +
  geom_errorbarh(aes(xmax = estimate + 1.96 * std.error, xmin = estimate - 1.96 * std.error)) +
  geom_vline(aes(xintercept = 0))

rec_log %>% 
  workflow(
    logistic_reg()
  ) %>% 
  fit(df_trn) %>% 
  augment(df_tst, type.predict = 'response') %>% 
  relocate(.pred_class) %>% 
  count(g_gt_xg, .pred_class) %>% 
  mutate(
    correct = .pred_class == g_gt_xg,
    total = sum(n)
  ) %>% 
  group_by(correct) %>% 
  summarize(frac = sum(n) / total) %>% 
  ungroup()

# linear ----
stats_agg_filt2 <- 
  stats_agg %>% 
  group_by(player_id, player_name) %>% 
  mutate(
    across(xg, list(lag1 = ~dplyr::lag(.x)))
  ) %>% 
  ungroup() %>% 
  # select(-xgd_frac, -matches('g_gt')) %>% 
  drop_na(xg_lag1)
stats_agg_filt2

set.seed(42)
ids <- stats_agg_filt2 %>% count(player_id, sort = TRUE)
ids_trn <- ids %>% slice_sample(weight_by = n, prop = 0.8)
ids_tst <- ids %>% anti_join(ids_trn)

.split <- function(ids) {
  stats_agg_filt2 %>% semi_join(ids %>% select(player_id))
}
df_trn <- .split(ids_trn)
df_tst <- .split(ids_tst)
  
rec_lin <-
  df_trn %>%
  recipe(
    formula(
       xg ~ player_name + player_id + season + g_gt_xg + position + gp + mp + a + xa + shots + key_passes
    ),
    data = .
  ) %>%
  update_role(player_name, player_id, season, g_gt_xg, new_role = 'id') %>% 
  step_dummy(all_nominal_predictors())
rec_lin
jui_trn_lin <- rec_lin %>% prep() %>% juice()
jui_trn_lin
jui_trn_lin %>% skimr::skim()

n_col_lin <- jui_trn_lin %>% ncol() %>% {. - 3}

rec_lin %>% 
  workflow(
    linear_reg()
  ) %>% 
  fit(df_trn) %>% 
  tidy() %>% 
  arrange(desc(abs(estimate))) %>% 
  mutate(
    # across(p.value, round, 2),
    across(term, ~fct_reorder(.x, estimate))
  ) %>% 
  ggplot() +
  aes(x = estimate, y = term) +
  geom_errorbarh(aes(xmax = estimate + 1.96 * std.error, xmin = estimate - 1.96 * std.error)) +
  geom_vline(aes(xintercept = 0))

preds_tst_lin <-
  rec_lin %>% 
  workflow(
    linear_reg()
  ) %>% 
  fit(df_trn) %>% 
  augment(df_tst) %>% 
  select(player_name:season, g, xg, xgd, g_gt_xg, .pred) %>% 
  mutate(
    .resid = xg - .pred, 
    xgd_pred = .pred - g, 
    g_gt_xg_pred = ifelse(xgd_pred < 0L, 1L, 0L)
  )

preds_tst_lin %>% 
  count(g_gt_xg_pred, g_gt_xg) %>% 
  mutate(
    correct = g_gt_xg_pred == g_gt_xg,
    total = sum(n)
  ) %>% 
  group_by(correct) %>% 
  summarize(frac = sum(n) / total) %>% 
  ungroup()
preds_lin %>% arrange(desc(abs(.resid)))

# tune? ----
grid_params <-
  crossing(
    mixture = c(0, 0.25, 0.5, 0.75, 1),
    penalty = c(0, 10 ^ seq(-3, 0, 1))
  )
grid_params

wf_log <- 
  rec_log %>% 
  workflow(
    logistic_reg(engine = 'glmnet', penalty = tune(), mixture = tune())
  )
wf_log

tune_log <-
  wf_log %>% 
  tune_grid(
    resamples = folds,
    control = ctrl,
    metrics = metset
  )
tune_log
autoplot(tune_log)

mets_log <- tune_log %>% collect_metrics()
mets_log

params_best_log <- tune_log %>% select_best('roc_auc')
params_best_log
wf_best_log <- wf_log %>% finalize_workflow(params_best_log)
# There are issues down the line with augment
wf_log_fix <-
  rec %>%
  workflow(
    logistic_reg(
      engine = 'glmnet',
      penalty = params_best_log$penalty, 
      mixture = params_best_log$mixture
    )
  )

fit_trn_log <- wf_log_fix %>% fit(df_trn)
imp_trn_log <-
  fit_trn_log %>% 
  extract_fit_engine() %>% 
  vip::vi(
    method = 'model',
    lambda = params_best_log$penalty, 
    alpha = params_best_log$mixture
    
  ) %>% 
  set_names(c('feature', 'imp', 'sign')) %>% 
  mutate(across(feature, ~fct_reorder(.x, imp))) %>% 
  ggplot() +
  aes(x = imp, y = feature, fill = sign) +
  geom_col() +
  labs(
    title = 'glmnet feature importance',
    y = NULL,
    x = 'Coefficient'
  ) +
  theme(
    panel.grid.major.y = element_blank()
  )
imp_trn_log

fit_trn_log %>% 
  tidy(
    penalty = params_best_log$penalty, 
    mixture = params_best_log$mixture
  ) %>% 
  arrange(desc(abs(estimate)))

probs_tst_log <- 
  fit_trn_log %>% 
  augment(
    df_tst, 
    penalty = params_best$penalty, 
    mixture = params_best$mixture,
    type = 'prob'
  )
probs_tst_log

preds_tst_log <- fit_trn_log %>% augment(df_tst)

preds_tst_log %>% 
  accuracy(.pred_class, g_gt_xg)

preds_tst_log %>% 
  conf_mat(.pred_class, g_gt_xg) %>% 
  autoplot('heatmap') +
  labs(
    title = 'Confusion matrix for glmnet'
  )

probs_tst_log %>% 
  roc_curve(g_gt_xg, .pred_0) %>% 
  autoplot() +
  tonythemes::theme_tony() +
  labs(
    title = 'ROC AUC for glmnet'
  )

