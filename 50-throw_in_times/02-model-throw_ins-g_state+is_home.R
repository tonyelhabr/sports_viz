
library(tidyverse)
library(arrow)
library(xgboost)

dir_proj <- '50-throw_in_times'
throw_ins <- file.path(dir_proj, 'throw_ins.parquet') %>%
  arrow::read_parquet() %>% 
  filter(is_after_sub == 0L)

x_mat <- throw_ins %>% 
  select(minute, period_id, g_state, is_home) %>% 
  as.matrix()

fit <- xgboost::xgboost(
  data = xgboost::xgb.DMatrix(
    x_mat,
    weight = throw_ins$wt,
    label = throw_ins$time_diff
  ),
  nrounds = 1000,
  early_stopping_rounds = 50,
  print_every_n = 100
)

newdata_grid <- crossing(
  minute = 1L:50L,
  period_id = 2L,
  g_state = 1L:3L,
  is_home = 1L:2L
)

g_states <- c(
  '1' = 'Behind',
  '2' = 'Tied',
  '3' = 'Ahead'
)

preds <- predict(
  fit,
  newdata = newdata_grid %>% as.matrix()
) %>% 
  tibble(.pred = .) %>% 
  bind_cols(newdata_grid) %>% 
  mutate(
    across(
      g_state,
      ~factor(
        unname(g_states[as.character(.x)]),
        levels  = unname(g_states)
      )
    ),
    across(
      is_home,
      ~ifelse(.x == 1L, 'Home', 'Away')
    ),
    across(
      minute,
      list(
        lab = ~ifelse(period_id == 2L, 45L + .x, .x)
      )
    )
  )

arrow::write_parquet(preds, file.path(dir_proj, 'preds-throw_ins-g_state+is_home.parquet'))
