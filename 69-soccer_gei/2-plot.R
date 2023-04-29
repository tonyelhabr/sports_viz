library(dplyr)
library(qs)
library(scales)

PROJ_DIR <- '69-soccer_gei'
momentum_path <- file.path(PROJ_DIR, 'momentum.qs')

momentum <- qs::qread(momentum_path)

## Reference: https://lukebenz.com/post/gei/
momentum |> filter(minute > 90) |> count(minute)
filt_momentum <- momentum |> 
  filter(type == 'main') |> 
  arrange(match_id, minute) |> 
  mutate(
    across(minute, round),
    across(value, ~scales::rescale(.x, to = c(0, 1), from = c(-100, 100)))
  ) |> 
  group_by(match_id) |> 
  mutate(
    max_minute = max(minute),
    lag_value = dplyr::lag(value, n = 1)
  ) |>
  ungroup() |> 
  filter(minute > 1)
filt_momentum

gei <- filt_momentum |> 
  group_by(match_id, max_minute) |> 
  summarize(
    n = n(),
    gei = sum(abs(value - lag_value))
  ) |> 
  ungroup() |> 
  mutate(
    gei = (90 / max_minute) * gei
  ) |> 
  arrange(desc(gei))

gei |> 
  inner_join(
    unnested_matches,
    by = join_by(match_id)
  )
