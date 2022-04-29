library(tidyverse)
library(arrow)
library(qs)
# library(xgboost)
dir_proj <- '53-duels'

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

duels <- file.path(dir_proj, 'duels.qs') %>% 
  qs::qread() %>% 
  add_angle_col()

mirror_y <- function(x) {
  ifelse(x > goal_y, goal_y - (x - goal_y), x)
}
flip_bool <- function(x) {
  ifelse(x, FALSE, TRUE)
}
mirror_y_col <- function(df) {
  df %>% mutate(across(y, mirror_y))
}
flip_bool_cols <- function(df) {
  df %>% mutate(across(c(is_offensive, is_successful), flip_bool))
}
invert_xy_cols <- function(df) {
  df %>% 
    mutate(
      across(x, ~scales::rescale(.x, to = c(pitch_x, 0), from = c(0, pitch_x))),
      across(y, ~scales::rescale(.x, to = c(pitch_y, 0), from = c(0, pitch_y)))
    )
}

# duels_slim <- duels %>% select(game_id, original_event_id, is_offensive, is_successful, x, y)
model_df <- bind_rows(
  duels,
  duels %>% flip_y_col(),
  duels %>% flip_bool_cols() %>% invert_xy_cols(),
  duels %>% flip_y_col() %>% flip_bool_cols() %>% invert_xy_cols()
) %>% 
  filter(is_offensive) %>% 
  select(-is_offensive) %>% 
  mutate(
    across(is_successful, ~ifelse(.x, 'yes', 'no') %>% factor())
  )
# duels %>% count(is_successful) %>% mutate(prop = n / sum(n))
df %>% count(is_successful) %>% mutate(prop = n / sum(n))
# df %>% count(is_offensive)
df %>% count(is_successful)
# df %>% count(is_offensive, is_successful)
duels %>% count(is_successful)
duels %>% flip_bool_cols() %>% count(is_successful)

# duels %>% 
#   transmute(
#     x,
#     y,
#     dx = goal_x - x,
#     dy = goal_y - y,
#     angle = atan((goal_y - y) / (goal_x - x)),
#     angle_deg = 180 * angle / pi
#   ) %>% 
#   head(1000) %>% 
#   # slice_min(angle)
#   ggplot() +
#   aes(x = x, y = y) +
#   geom_point() +
#   geom_spoke(aes(angle = angle), radius = 10)

# socceraction_names <- c(
#   'player_games',
#   'players',
#   'results',
#   'games',
#   'teams'
# )
# 
# socceraction_names %>% 
#   walk(
#     ~{
#       res <- file.path(dir_proj, sprintf('%s.parquet', .x)) %>% 
#         map_dfr(arrow::read_parquet) %>% 
#         distinct()
#       assign(value = res, x = .x, envir = .GlobalEnv)
#     }
#   )
library(recipes)
rec <- recipe(
  is_successful ~ x + y + angle + is_aerial,
  data = model_df
) %>% 
  step_interact(~x:angle) %>% 
  step_interact(~y:angle) # %>% 
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
fit_glm <- wf %>% fit(model_df)
fit_glm

grid <- tidyr::crossing(
  x = seq.int(0, goal_x),
  y = seq.int(0, goal_y),
  # is_offensive = c(TRUE, FALSE),
  is_aerial = c(TRUE, FALSE)
) %>% 
  add_angle_col()
grid

probs <- fit_glm %>% 
  broom::augment(
    new_data = grid,
    type.predict = 'response'
  )
fit_glm %>% 
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
