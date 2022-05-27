library(tidyverse)
library(qs)
dir_proj <- '53-duels'

duels <- file.path(dir_proj, 'duels.qs') |> 
  qs::qread()

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

model_df <- bind_rows(
  duels |> add_angle_col(),
  duels |> mirror_y_col() |> add_angle_col(),
  duels |> flip_bool_cols() |> invert_xy_cols() |> add_angle_col(),
  duels |> flip_bool_cols() |> mirror_y_col() |> invert_xy_cols() |> add_angle_col()
) |> 
  filter(is_offensive) |> 
  select(-is_offensive) |> 
  filter(is_aerial) |> 
  factor_is_successful_col()

fit <- glm(
  is_successful ~ x + y + angle + x*angle + y*angle, #  + poly(x, 2) + poly(y, 2), # angle + x*angle + y*angle,
  data = model_df,
  family = 'binomial'
)

grid <- tidyr::crossing(
  x = seq.int(0.5, goal_x - 0.5),
  y = seq.int(0.5, goal_y - 0.5)
) |> 
  add_angle_col()
grid

probs <- grid |> 
  broom::augment_columns(fit, newdata = ., type.predict = 'response')

dual_probs <- bind_rows(
  probs,
  probs |> mutate(across(y, ~pitch_y - .x))
)

probs |> 
  group_by(x) |> 
  summarize(
    across(
      .fitted,
      list(min = min, max = max, median = median)
    )
  ) |> 
  ungroup() |> 
  ggplot() +
  aes(x = x) +
  geom_line(aes(y = .fitted_median), color = 'black') +
  geom_line(aes(y = .fitted_min), color = 'red') +
  geom_line(aes(y = .fitted_max), color = 'blue')

dual_probs |> 
  summarize(
    cuts = ggplot2::cut_interval(.fitted, n = 4)
  )
  summarize(
    q25 = quantile(.fitted, 0.25),
    q75 = quantile(.fitted, 0.75)
  )

breaks <- dual_probs$.fitted |>
  ggplot2::cut_interval(n = 4) |> 
  levels() |> 
  str_replace_all("(\\[|\\()(.*)\\,(.*)", '\\2') |> 
  as.double()
breaks

dual_probs |> 
  ggplot() +
  aes(x = x, y = y) +
  common_gg() +
  # scale_fill_viridis_c() +
  # scale_color_viridis_c() +
  geom_tile(
    alpha = 0.5,
    # color = gray_wv,
    aes(fill = .fitted)
  ) +
  scale_fill_viridis_c(
    name = '',
    breaks = breaks,
    labels = c('', 'Lower', '', 'Higher')
  ) +
  # guides(fill = guide_colorbar(title = '', )) +
  theme(legend.key.width = unit(0.1, 'npc'), legend.key.height = unit(0.01, 'npc'))

actual_probs <- duels |> 
  add_angle_col() |> 
  filter(is_offensive, is_aerial) |> 
  broom::augment(fit, newdata = ., type.predict = 'response')

# 0.220 with 4, 0.218 with mirror or just 1
actual_probs |> 
  brier_score(
    truth = factor(is_successful),
    estimate = .fitted,
    event_level = 'second'
  )

# 0.122 with 4, 0.126 with mirror or just 1
actual_probs |> 
  mutate(ref = 0.5) |> 
  brier_skill_score(
    truth = factor(is_successful),
    estimate = .fitted,
    ref_estimate = ref,
    event_level = 'second'
  )

actual_probs |> 
  mutate(
    across(is_successful, as.integer)
  ) |> 
  compute_calibration_table(outcome = is_successful, prob = .fitted) |> 
  make_calibration_plot(prob = .fitted, actual = actual)

duels |> 
  add_angle_col() |> 
  filter(is_offensive, is_aerial) |> 
  broom::augment(fit, newdata = ., type.predict = 'response') |> 
  group_by(is_successful) |> 
  summarize(
    n = n(),
    across(.fitted, mean)
  )

duels |> 
  filter(!is_offensive) |> 
  count(is_successful) |> 
  mutate(prop = n / sum(n))


library(recipes)
rec <- recipe(
  is_successful ~ x + y + angle + is_aerial,
  data = model_df
) |> 
  step_interact(~x:angle) |> 
  step_interact(~y:angle) # |> 
# step_interact(~x:y)
rec

library(workflows)
library(parsnip)
wf <- workflow(
  rec,
  # boost_tree(
  #   mode = 'classification'
  # )
  logistic_reg()
)
fit_glm <- wf |> fit(model_df)
fit_glm

grid <- tidyr::crossing(
  x = seq.int(0, goal_x),
  y = seq.int(0, goal_y),
  # is_offensive = c(TRUE, FALSE),
  is_aerial = c(TRUE, FALSE)
) |> 
  add_angle_col()
grid

probs <- fit_glm |> 
  broom::augment(
    new_data = grid,
    type.predict = 'response'
  )
fit_glm |> 
  broom::augment(
    new_data = duels,
    type.predict = 'response'
  )

duels_aug <- duels %>%
  # select(idx, x, y, is_aerial, is_offensive, is_successful) %>% 
  mirror_y_col() %>% 
  add_angle_col() %>% 
  broom::augment(fit, ., type = 'prob') %>% 
  mutate(
    xw = ifelse(is_offensive, .pred_yes, .pred_no)
  ) %>% 
  select(-c(.pred_class, .pred_no, .pred_yes))

duels_aug %>% 
  group_by(season_id, player_name, is_aerial) %>% 
  summarize(
    n = n(),
    across(xw, list(sum = sum, mean = mean))
  ) %>% 
  ungroup()

probs %>% 
  # filter(!is_aerial) %>% 
  filter(x == 0) %>% 
  filter(y == 0 | y == 33)

probs %>% 
  filter(x == round(goal_x / 2)) %>% 
  select(x, y, is_aerial, .pred_yes) %>% 
  arrange(.pred_yes)
probs %>% 
  filter(round(.pred_yes, 2) == 0.50)

probs %>% 
  # filter(!is_offensive, is_aerial) %>% 
  ggplot() +
  aes(x = x, y = y) +
  geom_tile(
    aes(fill = .pred_yes)
  ) +
  facet_wrap(~is_aerial)
