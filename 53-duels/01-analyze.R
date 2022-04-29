library(tidyverse)
library(qs)
library(ebbr)
dir_proj <- '53-duels'

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

fit_xw_model <- function(df) {
  model_df <- bind_rows(
    df %>% add_angle_col(),
    df %>% mirror_y_col() %>% add_angle_col(),
    df %>% flip_bool_cols() %>% invert_xy_cols() %>% add_angle_col(),
    df %>% flip_bool_cols() %>% mirror_y_col() %>% invert_xy_cols() %>% add_angle_col()
  ) %>% 
    filter(is_offensive) %>% 
    select(-is_offensive) %>% 
    factor_is_successful_col()
  
  glm(
    is_successful ~ x + y + angle + is_aerial + x*angle + y*angle,
    data = model_df,
    family = 'binomial'
  )
}

debugonce(broom:::augment.glm)
add_xw_col <- function(df, fit) {
  df %>%
    mirror_y_col() %>% 
    add_angle_col() %>% 
    factor_is_successful_col() %>% 
    broom::augment(fit, newdata = ., type.predict = 'response') %>% 
    mutate(
      xw = ifelse(is_offensive, .pred_yes, .pred_no)
    ) %>% 
    select(-c(.pred_class, .pred_no, .pred_yes))
}

fit_xw <- duels %>% fit_xw_model()
duels_xw <- duels %>% add_xw_col(fit_xw)

## final ----
splits_adj %>% 
  filter(player_name == 'Harry Maguire') %>% 
  mutate(n_won = n_won_aerial + n_won_ground, n_lost = n_lost_aerial + n_lost_ground) %>% 
  glimpse()
