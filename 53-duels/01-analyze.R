library(tidyverse)
library(qs)
library(ebbr)
library(yardstick)
library(broom)
dir_proj <- '53-duels'

source(file.path(dir_proj, 'helpers.R'))
duels <- file.path(dir_proj, 'duels.qs') %>% 
  qs::qread()

count_duels <- function(df) {
  df %>% 
    count(season_id, player_name, is_successful) %>% 
    group_by(season_id, player_name) %>% 
    mutate(
      across(is_successful, ~ifelse(.x, 'won', 'lost')),
      prop = n / sum(n)
    ) %>% 
    ungroup() %>% 
    pivot_wider(
      names_from = is_successful,
      values_from = c(n, prop)
    ) %>% 
    mutate(n = coalesce(n_lost, 0L) + coalesce(n_won, 0L))
}

total_split <- duels %>% count_duels()
splits <- duels %>% 
  count(is_aerial, season_id, player_name, is_successful) %>% 
  group_by(is_aerial, season_id, player_name) %>% 
  mutate(
    across(is_successful, ~ifelse(.x, 'won', 'lost')),
    prop = n / sum(n)
  ) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = is_successful,
    values_from = c(n, prop)
  ) %>% 
  drop_na(n_lost, n_won) %>% 
  group_by(is_aerial) %>% 
  # mutate(n = coalesce(n_lost, 0L) + coalesce(n_won, 0L)) %>% 
  mutate(n = n_lost + n_won) %>% 
  ungroup()

q25s <- splits %>%
  group_by(is_aerial) %>% 
  summarize(
    q = round(quantile(n, 0.25))
  )

ground_splits <- splits %>% filter(!is_aerial)
aerial_splits <- splits %>% filter(is_aerial)

postprocess_ebbr_split <- function(df, suffix, ...) {
  df %>% 
    select(
      season_id, player_name, n_lost, n_won, prop_lost, prop_won, prop_won_adj = .fitted
    ) %>% 
    rename_with(~sprintf('%s_%s', .x, suffix), -c(season_id, player_name))
}

splits_adj <- inner_join(
  aerial_splits %>% 
    ebbr::add_ebb_estimate(n_won, n, prior_subset = n >= 20) %>% 
    postprocess_ebbr_split('aerial'),
  ground_splits %>% 
    ebbr::add_ebb_estimate(n_won, n, prior_subset = n >= 40) %>% 
    postprocess_ebbr_split('ground')
) %>% 
  mutate(
    prop_won_diff = prop_won_ground - prop_won_aerial,
    prop_won_adj_diff = prop_won_adj_ground - prop_won_adj_aerial
  ) %>% 
  arrange(desc(prop_won_adj_diff))
splits_adj

## model ----
pitch_x <- 105
pitch_y <- 68
goal_x <- pitch_x
goal_y <- pitch_y / 2
add_angle_col <- function(df) {
  df %>% 
    mutate(
      angle = atan((goal_y - y) / (goal_x - x))
    )
}
mirror_y_col <- function(df) {
  df %>% mutate(across(y, ~ifelse(.x > goal_y, goal_y - (.x - goal_y), .x)))
}
flip_bool_cols <- function(df) {
  df %>% mutate(across(c(is_offensive, is_successful), ~ifelse(.x, FALSE, TRUE)))
}
invert_xy_cols <- function(df) {
  df %>% 
    mutate(
      across(x, ~scales::rescale(.x, to = c(pitch_x, 0), from = c(0, pitch_x))),
      across(y, ~scales::rescale(.x, to = c(pitch_y, 0), from = c(0, pitch_y)))
    )
}
factor_is_successful_col <- function(df) {
  df %>% 
    mutate(
      across(is_successful, ~ifelse(.x, 'yes', 'no') %>% factor())
    )
}
fit_model <- function(duels, ...) {
  model_df <- bind_rows(
    duels %>% add_angle_col(),
    duels %>% mirror_y_col() %>% add_angle_col(),
    duels %>% flip_bool_cols() %>% invert_xy_cols() %>% add_angle_col(),
    duels %>% flip_bool_cols() %>% mirror_y_col() %>% invert_xy_cols() %>% add_angle_col()
  ) %>% 
    filter(is_offensive) %>% 
    select(-is_offensive) %>% 
    filter(...) %>% 
    factor_is_successful_col()
  
  glm(
    is_successful ~ x + y + angle + x*angle + y*angle,
    data = model_df,
    family = 'binomial'
  )
}
add_xw_col <- function(df, fit, ...) {
  df %>%
    mirror_y_col() %>% 
    add_angle_col() %>% 
    factor_is_successful_col() %>% 
    broom::augment(fit, newdata = ., type.predict = 'response') %>% 
    mutate(
      xw = ifelse(is_offensive, .fitted, 1 - .fitted)
    ) %>% 
    select(-c(.fitted))
}
do_model <- function(...) {
  fit <- duels %>% fit_model(...)
  probs <- duels %>% add_xw_col(fit, ...)
  
  grid <- crossing(
    x = seq.int(0 + 0.5, goal_x - 0.5),
    y = seq.int(0 + 0.5, goal_y - 0.5)
  ) %>% 
    add_angle_col()
  
  center_prob <- broom::augment(
    fit,
    newdata = tibble(x = goal_x / 2, y = goal_y) %>% add_angle_col(),
    type.predict = 'response'
  ) %>% 
    pull(.fitted)
  
  probs_grid <- grid %>% 
    broom::augment_columns(fit, newdata = ., type.predict = 'response')
  
  probs_grid_mirrored <- bind_rows(
    probs_grid,
    probs_grid %>% mutate(across(y, ~pitch_y - .x))
  )
  
  breaks <- probs_grid_mirrored$.fitted %>%
    cut_interval(n = 4) %>% 
    levels() %>% 
    str_replace_all('(\\[|\\()(.*)\\,(.*)', '\\2') %>% 
    as.double()
  
  prob_min <- probs_grid_mirrored %>% 
    slice_min(.fitted) %>% 
    distinct()
  
  prob_max <- probs_grid_mirrored %>% 
    slice_max(.fitted) %>% 
    distinct()
  
  p_grid <- probs_grid_mirrored %>% 
    ggplot() +
    aes(x = x, y = y) +
    common_gg() +
    geom_tile(
      alpha = 0.5,
      # color = gray_wv,
      aes(fill = .fitted)
    ) +
    geom_point(
      data = prob_min,
      color = 'white',
      shape = 16,
      size = 4
    ) +
    geom_point(
      data = prob_max,
      color = 'white',
      shape = 18,
      size = 4
    ) +
    scale_fill_viridis_c(
      name = '',
      option = 'B',
      begin = 0.2,
      end = 0.9,
      breaks = breaks,
      labels = c('', 'Lower', '', 'Higher')
    ) +
    theme(
      legend.key.width = unit(0.1, 'npc'),
      legend.key.height = unit(0.01, 'npc')
    )
  p_grid
  
  bs <- probs %>% 
    brier_score(
      truth = factor(is_successful),
      estimate = xw,
      event_level = 'second'
    )
  
  bss <- probs %>% 
    mutate(ref = 0.5) %>% 
    brier_skill_score(
      truth = factor(is_successful),
      estimate = xw,
      ref_estimate = ref,
      event_level = 'second'
    )
  
  p_roc <- probs %>% 
    roc_curve(
      estimate = xw,
      truth = is_successful,
      event_level = 'second'
    ) %>% 
    autoplot()
  
  prob_center <- predict(
    fit,
    tibble(x = goal_x / 2, y = goal_y) %>% add_angle_col(),
    type = 'response'
  ) %>% 
    unname()
  
  acc <- probs %>% 
    mutate(
      cls = ifelse(xw <= prob_center, 'no', 'yes') %>% factor()
    ) %>%
    accuracy(
      estimate = cls,
      truth = is_successful,
      event_level = 'second'
    )
  
  p_calib <- probs %>% 
    compute_calibration_table(
      outcome = 'is_successful',
      prob = xw,
      event_level = 'second',
      n_buckets = 50
    ) %>% 
    make_calibration_plot(
      prob = 'xw'
    )
  
  list(
    fit = fit,
    probs = probs,
    pitch_plot = p_grid,
    brier_score = bs$.estimate,
    brier_skill_score = bss$.estimate,
    roc_curve = p_roc,
    threshold = prob_center,
    accuracy = acc$.estimate,
    calibration_plot = p_calib
  )
}

aerial_res <- do_model(is_aerial)
aerial_fit <- aerial_res$fit
aerial_duels <- aerial_res$probs
ground_res <- do_model(!is_aerial)
ground_fit <- ground_res$fit
ground_duels <- ground_res$probs
aerial_res$roc_curve
ground_res$roc_curve
aerial_res$pitch_plot
ground_res$pitch_plot

## final ----
xwoe <- inner_join(
  splits_adj %>% 
    # filter(player_name == 'Harry Maguire') %>% 
    transmute(
      season_id,
      player_name,
      n_won_aerial,
      n_lost_aerial,
      n_won_ground,
      n_lost_ground,
      n_won = n_won_aerial + n_won_ground, 
      n_lost = n_lost_aerial + n_lost_ground,
      n = n_won + n_lost,
      prop_won_adj_aerial,
      prop_won_adj_ground
    ),
  duels %>% 
    # filter(player_name == 'Harry Maguire') %>% 
    group_by(season_id, player_name, is_aerial) %>% 
    summarize(
      # n_duels = n(),
      across(xw, mean)
    ) %>% 
    # ungroup() %>% 
    # group_by(season_id, player_name) %>% 
    # mutate(
    #   across(n_duels, sum)
    # ) %>% 
    # ungroup() %>% 
    mutate(across(is_aerial, ~ifelse(.x, 'aerial', 'ground'))) %>% 
    pivot_wider(
      names_from = is_aerial,
      names_prefix = 'xw_',
      values_from = xw
    )
) %>% 
  mutate(
    xwoe_aerial = prop_won_adj_aerial - xw_aerial,
    xwoe_ground = prop_won_adj_ground - xw_ground
  )
xwoe %>% 
  arrange(desc(n))
xwoe %>% 
  ggplot() +
  aes(
    x = xw_ground,
    y = xw_aerial
  ) +
  geom_point(aes(size = n))
xwoe %>% 
  ggplot() +
  aes(x = xwoe_ground, y = xwoe_aerial) +
  geom_point()
