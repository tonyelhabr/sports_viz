
library(tidyverse)
library(arrow)
library(xgboost)

gks <- file.path(dir_proj, 'gks.parquet') %>%
  arrow::read_parquet() %>% 
  filter(is_after_sub == 0L)

x_mat <- gks %>% 
  select(minute, period_id, is_home, g_state) %>% 
  as.matrix()

fit <- xgboost::xgboost(
  data = xgboost::xgb.DMatrix(
    x_mat,
    weight = gks$wt,
    label = gks$time_diff
  ),
  nrounds = 1000,
  early_stopping_rounds = 50,
  print_every_n = 100
)

newdata_grid <- crossing(
  minute = 1L:50L,
  period_id = 1L:2L,
  is_home = 1L:2L,
  g_state = 1L:3L
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

arrow::write_parquet(preds, file.path(dir_proj, 'preds-gks-g_state+is_home.parquet'))
