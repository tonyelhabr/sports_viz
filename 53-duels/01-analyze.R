library(tidyverse)
library(qs)
library(ebbr)
library(yardstick)
library(broom)
dir_proj <- '53-duels'

source(file.path(dir_proj, 'helpers.R'))
init_duels <- file.path(dir_proj, 'duels.qs') |> 
  qs::qread()

keepers <- init_duels |> 
  # filter(player_name %in% c('Ederson', 'Alisson', 'Édouard Mendy', 'Virgil van Dijk', 'Harry Maguire', 'James Tarkowski')) |> 
  select(player_id, player_name, x, y) |> 
  group_by(player_id, player_name) |> 
  summarize(across(x, mean)) |> 
  ungroup() |> 
  arrange(desc(x)) |> 
  filter(x < 18) |> 
  distinct(player_id)
keepers

duels <- init_duels |> 
  anti_join(
    keepers
  )

count_duels <- function(df) {
  df |> 
    count(season_id, player_name, is_successful) |> 
    group_by(season_id, player_name) |> 
    mutate(
      across(is_successful, ~ifelse(.x, 'won', 'lost')),
      prop = n / sum(n)
    ) |> 
    ungroup() |> 
    pivot_wider(
      names_from = is_successful,
      values_from = c(n, prop)
    ) |> 
    mutate(n = coalesce(n_lost, 0L) + coalesce(n_won, 0L))
}

total_split <- duels |> count_duels()
splits <- duels |> 
  count(is_aerial, season_id, player_name, is_successful) |> 
  group_by(is_aerial, season_id, player_name) |> 
  mutate(
    across(is_successful, ~ifelse(.x, 'won', 'lost')),
    prop = n / sum(n)
  ) |> 
  ungroup() |> 
  pivot_wider(
    names_from = is_successful,
    values_from = c(n, prop)
  ) |> 
  drop_na(n_lost, n_won) |> 
  group_by(is_aerial) |> 
  # mutate(n = coalesce(n_lost, 0L) + coalesce(n_won, 0L)) |> 
  mutate(n = n_lost + n_won) |> 
  ungroup()

q25s <- splits |>
  group_by(is_aerial) |> 
  summarize(
    q = round(quantile(n, 0.25))
  )

ground_splits <- splits |> filter(!is_aerial)
aerial_splits <- splits |> filter(is_aerial)

postprocess_ebbr_split <- function(df, suffix, ...) {
  df |> 
    select(
      season_id, player_name, n_lost, n_won, prop_lost, prop_won, prop_won_adj = .fitted
    ) |> 
    rename_with(~sprintf('%s_%s', .x, suffix), -c(season_id, player_name))
}

splits_adj <- inner_join(
  aerial_splits |> 
    ebbr::add_ebb_estimate(n_won, n, prior_subset = n >= 20) |> 
    postprocess_ebbr_split('aerial'),
  ground_splits |> 
    ebbr::add_ebb_estimate(n_won, n, prior_subset = n >= 40) |> 
    postprocess_ebbr_split('ground')
) |> 
  mutate(
    prop_won_diff = prop_won_ground - prop_won_aerial,
    prop_won_adj_diff = prop_won_adj_ground - prop_won_adj_aerial
  ) |> 
  arrange(desc(prop_won_adj_diff))
splits_adj

## model ----
pitch_x <- 105
pitch_y <- 68
goal_x <- pitch_x
goal_y <- pitch_y / 2
add_angle_col <- function(df) {
  df |> 
    mutate(
      angle = atan((goal_y - y) / (goal_x - x))
    )
}   
shift_xy_cols <- function(df) {
  df |> 
    mutate(
      x = x - pitch_x / 2,
      y = y - pitch_y / 2
    )
}
revert_xy_cols <- function(df) {
  df |> 
    mutate(
      x = x + pitch_x / 2,
      y = y + pitch_y / 2
    )
}
mirror_y_col <- function(df) {
  df |> mutate(across(y, ~ifelse(.x > goal_y, goal_y - (.x - goal_y), .x)))
}
flip_bool_cols <- function(df) {
  df |> mutate(across(c(is_offensive, is_successful), ~ifelse(.x, FALSE, TRUE)))
}
invert_xy_cols <- function(df) {
  df |> 
    mutate(
      across(x, ~scales::rescale(.x, to = c(pitch_x, 0), from = c(0, pitch_x))),
      across(y, ~scales::rescale(.x, to = c(pitch_y, 0), from = c(0, pitch_y)))
    )
}
factor_is_successful_col <- function(df) {
  df |> 
    mutate(
      across(is_successful, ~ifelse(.x, 'yes', 'no') |> factor())
    )
}

fit_model <- function(df, ...) {
  df <- duels |> filter(!is_aerial)
  model_df <- bind_rows(
    df |> add_angle_col(),
    df |> mirror_y_col() |> add_angle_col(),
    df |> flip_bool_cols() |> invert_xy_cols() |> add_angle_col(),
    df |> flip_bool_cols() |> mirror_y_col() |> invert_xy_cols() |> add_angle_col()
  ) |>
    # filter(...) |>
    # filter(!is_aerial) |> 
    factor_is_successful_col() |> 
    shift_xy_cols()

  # fit <- nls(
  #   is_successful ~ 0 + x + y + is_offensive, #  + x*is_offensive + y*is_offensive + x:y,
  #   data = model_df,
  #   start = list(x = 0, y = 0, is_offensive = 0),
  #   lower = list(x = -Inf, y = -Inf, is_offensive = -Inf),
  #   upper = list(x = +Inf, y = +Inf, is_offensive = +Inf),
  #   # family = 'binomial',
  #   algorithm = 'port'
  # )
  # fit
  
  fit <- glm(
    is_successful ~ x + y + x:y + 0, # angle + x*angle + y*angle + 0,
    data = model_df,
    # data = duels |> add_angle_col(),
    family = 'binomial'
  )
}

add_xw_col <- function(df, fit, ...) {
  new_df <- df |>
    filter(...) |> 
    # filter(!is_aerial) |> 
    mirror_y_col() |> 
    add_angle_col() |> 
    factor_is_successful_col() |> 
    shift_xy_cols()
  
  broom::augment(fit, newdata = new_df, type.predict = 'response') |> 
    revert_xy_cols() |> 
    mutate(
      xw = ifelse(is_offensive, .fitted, 1 - .fitted)
    ) |> 
    select(-c(.fitted))
}

do_model <- function(...) {
  # fit <- duels |> fit_model(...)
  #A probs <- duels |> add_xw_col(fit, ...)
  n_cls <-  duels |> filter(!is_aerial) |> count(is_successful, is_offensive)
  smallest_cls <- n_cls |> slice_min(n, n = 1)
  balanced_duels <- bind_rows(
    duels |> 
      filter(!is_aerial, is_successful == smallest_cls$is_successful, is_offensive == smallest_cls$is_offensive),
    duels |> 
      filter(!is_aerial, is_successful != smallest_cls$is_successful, is_offensive == smallest_cls$is_offensive) |> 
      slice_sample(n = smallest_cls$n),
    duels |> 
      filter(!is_aerial, is_successful == smallest_cls$is_successful, is_offensive != smallest_cls$is_offensive) |> 
      slice_sample(n = smallest_cls$n),
    duels |> 
      filter(!is_aerial, is_successful != smallest_cls$is_successful, is_offensive != smallest_cls$is_offensive) |> 
      slice_sample(n = smallest_cls$n)
  )
  balanced_duels |> filter(!is_aerial) |> count(is_successful, is_offensive)
  fit <- balanced_duels |> fit_model(!is_aerial)
  probs <- duels |> add_xw_col(fit, !is_aerial) ## 0.48
  
  .is_offensive <- TRUE
  grid <- crossing(
    x = seq.int(0 + 0.5, goal_x - 0.5),
    y = seq.int(0 + 0.5, goal_y - 0.5),
    is_offensive = .is_offensive
  ) |> 
    add_angle_col()

  center_prob <- broom::augment(
    fit,
    newdata = tibble(
      x = goal_x / 2, 
      y = goal_y, 
      is_offensive = .is_offensive
    ) |> 
      shift_xy_cols() |> 
      add_angle_col(),
    type.predict = 'response'
  ) |> 
    pull(.fitted)
  
  probs_grid <- broom::augment_columns(
      fit, 
      newdata = grid |> shift_xy_cols(), 
      type.predict = 'response'
    ) |> 
    revert_xy_cols()
  
  probs_grid_mirrored <- bind_rows(
    probs_grid,
    probs_grid |> mutate(across(y, ~pitch_y - .x))
  )
  
  breaks <- probs_grid_mirrored$.fitted |>
    cut_interval(n = 4) |> 
    levels() |> 
    str_replace_all('(\\[|\\()(.*)\\,(.*)', '\\2') |> 
    as.double()
  
  prob_min <- probs_grid_mirrored |> 
    slice_min(.fitted) |> 
    distinct()
  
  prob_max <- probs_grid_mirrored |> 
    slice_max(.fitted) |> 
    distinct()
  
  p_grid <- probs_grid_mirrored |> 
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
    geom_tile(
      inherit.aes = FALSE,
      data = probs_grid_mirrored |> filter(round(.fitted, 2) == .50),
      color = 'white',
      fill = 'white',
      aes(x = x, y = y)
    ) +
    scale_fill_viridis_c(
      # name = '',
      # option = 'B',
      # begin = 0.2,
      # end = 0.9,
      # breaks = breaks,
      # labels = c('', 'Lower', '', 'Higher')
    ) +
    theme(
      legend.key.width = unit(0.1, 'npc'),
      legend.key.height = unit(0.01, 'npc')
    )
  p_grid
  bs <- probs |> 
    brier_score(
      truth = factor(is_successful),
      estimate = xw,
      event_level = 'second'
    )
  
  bss <- probs |> 
    mutate(ref = 0.5) |> 
    brier_skill_score(
      truth = factor(is_successful),
      estimate = xw,
      ref_estimate = ref,
      event_level = 'second'
    )
  
  p_roc <- probs |> 
    roc_curve(
      estimate = xw,
      truth = is_successful,
      event_level = 'second'
    ) |> 
    autoplot()

  acc <- probs |> 
    mutate(
      cls = ifelse(xw <= center_prob, 'no', 'yes') |> factor()
    ) |>
    accuracy(
      estimate = cls,
      truth = is_successful,
      event_level = 'second'
    )
  
  p_calib <- probs |> 
    compute_calibration_table(
      outcome = 'is_successful',
      prob = xw,
      event_level = 'second',
      n_buckets = 50
    ) |> 
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
    threshold = center_prob,
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
duels_xw <- bind_rows(aerial_duels, ground_duels)

## final ----
xwoe <- inner_join(
  splits_adj |> 
    # filter(player_name == 'Harry Maguire') |> 
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
      prop_won_aerial,
      prop_won_adj_aerial,
      prop_won_adj_ground
    ),
  duels_xw |> 
    # filter(player_name == 'Harry Maguire') |> 
    group_by(season_id, player_name, is_aerial) |> 
    summarize(
      # n_duels = n(),
      across(xw, mean)
    ) |> 
    ungroup() |> 
    # group_by(season_id, player_name) |> 
    # mutate(
    #   across(n_duels, sum)
    # ) |> 
    # ungroup() |> 
    mutate(across(is_aerial, ~ifelse(.x, 'aerial', 'ground'))) |> 
    pivot_wider(
      names_from = is_aerial,
      names_prefix = 'xw_',
      values_from = xw
    )
) |> 
  mutate(
    xwoe_aerial = prop_won_adj_aerial - xw_aerial,
    xwoe_ground = prop_won_adj_ground - xw_ground
  )

xwoe |> arrange(desc(n))
xwoe |> 
  ggplot() +
  aes(x = xw_aerial, y = n) +
  geom_point()
xwoe |> 
  ggplot() +
  aes(x = xw_ground, y = n) +
  geom_point()

xwoe |> 
  ggplot() +
  aes(
    x = xw_ground,
    y = xw_aerial
  ) +
  geom_point(aes(size = n))

xwoe |> filter(xw_aerial < 0.4, xw_ground < 0.4)
xwoe |> 
  count(xwoe_ground > 0)

xwoe |> 
  filter(n > 50) |>
  slice_min(xwoe_ground)
  ggplot() +
  aes(
    x = xwoe_ground,
    y = xwoe_aerial
  ) +
  geom_point(aes(size = n))

xwoe |> 
  # filter(n > 50) |>
  mutate(
    across(
      matches('^xwoe'),
      list(norm = ~(.x - mean(.x)) / sd(.x))
    )
  ) |> 
  ggplot() +
  aes(
    x = xwoe_ground_norm,
    y = xwoe_aerial_norm
  ) +
  geom_point(aes(size = n))

xwoe |> 
  mutate(
    across(
      matches('^xwoe'),
      list(norm = ~(.x - mean(.x)) / sd(.x))
    ),
    z = sqrt(xwoe_aerial_norm^2 + xwoe_ground_norm^2)
  ) |> 
  filter(n > 50) |>
  # filter(xwoe_aerial > 0, xwoe_ground > 0) |> 
  arrange(desc(z))
