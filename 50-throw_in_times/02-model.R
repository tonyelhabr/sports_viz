
library(tidyverse)
library(arrow)
library(xgboost)

df <- file.path('..', 'sports_viz', '50-throw_in_times', 'throw_ins.parquet') %>%
  arrow::read_parquet()

df <- throw_ins %>%
  transmute(
    idx,
    game_id,
    across(period_id, as.integer),
    wt,
    time,
    minute2,
    g_ind = as.integer(g_state),
    time_diff
  ) %>% 
  filter(!is.na(wt))
df

x_mat <- df %>% 
  select(minute2, period_id, g_ind) %>% 
  as.matrix()

fit <- xgboost::xgboost(
  data = xgboost::xgb.DMatrix(
    x_mat,
    weight = df$wt,
    label = df$time_diff
  ),
  # objective = 'count:poisson',
  nrounds = 1000,
  early_stopping_rounds = 50,
  print_every_n = 100,
  params = list(
    monotone_constraints  = '(-1, 0, 0)'
  )
)

newdata_grid <- crossing(
  minute2 = 1L:45L,
  period_id = 1L:2L,
  g_ind = 1L:3L
)

g_states <- c(
  '1' = 'behind',
  '2' = 'tied',
  '3' = 'ahead'
)

preds <- predict(
  fit,
  newdata = newdata_grid %>% as.matrix()
) %>% 
  tibble(.pred = .) %>% 
  bind_cols(newdata_grid) %>% 
  mutate(
    across(
      g_ind,
      # ~g_states[[as.character(.x)]]
      ~case_when(
        .x == 1 ~ 'behind',
        .x == 2 ~ 'tied',
        .x == 3 ~ 'ahead'
      )
    )
  )
preds

preds %>% 
  ggplot() +
  aes(
    x = minute2,
    y = .pred
  ) +
  geom_line(
    aes(
      color = g_ind
    )
  ) +
  facet_wrap(~period_id, nrow = 2)
