library(tidyverse)
library(arrow)
library(qs)
library(xgboost)
dir_proj <- '53-duels'

duels <- file.path(dir_proj, 'duels.qs') %>% 
  qs::qread() # %>% 
  # mutate(
  #   across(
  #     x,
  #     ~ifelse(
  #       is_home,
  #       105 - .x,
  #       .x
  #     )
  #   ),
  #   across(
  #     y,
  #     ~ifelse(
  #       is_home,
  #       68 - .x,
  #       .x
  #     )
  #   )
  # )


socceraction_names <- c(
  'player_games',
  'players',
  'results',
  'games',
  'teams'
)

socceraction_names %>% 
  walk(
    ~{
      res <- file.path(dir_proj, sprintf('%s.parquet', .x)) %>% 
        map_dfr(arrow::read_parquet) %>% 
        distinct()
      assign(value = res, x = .x, envir = .GlobalEnv)
    }
  )

df <- duels %>% 
  filter(!is.na(result_name)) %>% 
  filter(is_home) %>% 
  mutate(
    across(y, ~ifelse(.x > 34, 34 - (.x - 34), .x)),
    across(result_name, ~ifelse(.x == 'Successful', 'won', 'lose') %>% factor())
  )
df %>% count(result_name)

library(tidymodels)
rec <- recipe(
  result_name ~ x + y + is_offensive + is_aerial,
  data = df
) %>% 
  step_interact(~x:y) # %>% 
  # step_interact(~x:is_offensive) %>% 
  # step_interact(~x:is_aerial) %>% 
  # step_interact(~y:is_offensive) %>% 
  # step_interact(~y:is_aerial)

wf <- workflow(
  rec,
  # boost_tree(
  #   mode = 'classification'
  # )
  logistic_reg()
)
fit <- wf %>% fit(df)
fit

grid <- crossing(
  x = seq.int(0, 105),
  y = seq.int(0, 34),
  is_offensive = c(TRUE, FALSE),
  is_aerial = c(TRUE, FALSE)
)


probs <- fit %>% 
  broom::augment(
    new_data = grid,
    pred.type = 'response'
  )
probs %>% 
  group_by(is_offensive, is_aerial) %>% 
  slice_max(.pred_won)
probs %>% 
  # filter(!is_offensive, is_aerial) %>% 
  ggplot() +
  aes(x = x, y = y) +
  geom_tile(
    aes(fill = .pred_won)
  ) +
  facet_grid(is_offensive~is_aerial)
