library(lightgbm)
library(bonsai)
spec_full <- parsnip::boost_tree(
  trees = 500,
  learn_rate = 0.01,
  tree_depth = tune(),
  min_n = tune(), 
  loss_reduction = tune(),
  sample_size = tune(), 
  mtry = tune(),
  stop_iter = tune()
) |>
  parsnip::set_engine('lightgbm') |> 
  parsnip::set_mode('classification')

library(dials)
library(workflowsets)
library(finetune)
grid <- grid_latin_hypercube(
  # trees(),
  # learn_rate(),
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train),
  stop_iter(range = c(10L, 50L)),
  size = 50
)

wf_sets <- workflow_set(
  preproc = list(
    full = rec_full
  ),
  models = list(model = spec_full),
  cross = TRUE
)

control <- control_race(
  save_pred = TRUE,
  parallel_over = 'everything',
  save_workflow = TRUE,
  verbose = TRUE,
  verbose_elim = TRUE
)

val_folds <- updated_val_and_test |> 
  dplyr::filter(set == 'validation') |> 
  rsample::vfold_cv(v = 5, strata = scores)

options(tidymodels.dark = TRUE)
t1 <- Sys.time()
tuned_results <- workflow_map(
  wf_sets,
  fn = 'tune_race_anova',
  grid = grid,
  control = control,
  metrics = met_set,
  resamples = val_folds,
  seed = 42
)
t2 <- Sys.time()
t2 - t1
best_set <- tuned_results |>
  extract_workflow_set_result('full_model') |> 
  select_best(metric = 'f_meas')
knitr::kable(best_set)

##   | mtry| min_n| tree_depth| loss_reduction| sample_size| stop_iter|.config               |
##   |----:|-----:|----------:|--------------:|-----------:|---------:|:---------------------|
##   |    9|    32|         10|      0.0048179|   0.1744129|        17|Preprocessor1_Model26 |