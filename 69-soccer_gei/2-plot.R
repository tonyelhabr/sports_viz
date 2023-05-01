library(dplyr)
library(qs)
library(scales)
library(lubridate)
library(stringr)

PROJ_DIR <- '69-soccer_gei'
matches_path <- file.path(PROJ_DIR, 'matches.qs')
momentum_path <- file.path(PROJ_DIR, 'momentum.qs')
match_team_stats_path <- file.path(PROJ_DIR, 'match_team_stats.qs')

matches <- qs::qread(matches_path)
momentum <- qs::qread(momentum_path)
match_team_stats <- qs::qread(match_team_stats_path)

## Reference: https://lukebenz.com/post/gei/
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
    lag_value = lag(value, n = 1)
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

gei_with_results <- gei |> 
  inner_join(
    matches,
    by = join_by(match_id)
  ) |> 
  transmute(
    league_id,
    round,
    match_id,
    match_date = date(match_time),
    home_team,
    away_team,
    gei
  )

long_match_team_stats <- match_team_stats |> 
  mutate(
    home = home_value |> str_remove('\\(.*$') |> as.numeric(), 
    away = away_value |> str_remove('\\(.*$') |> as.numeric()
  ) |> 
  drop_na(home, away) |> 
  transmute(
    match_id, 
    key, ## need key since some stats_title are replicated across keys
    stats_title, 
    total = home + away,
    delta = home - away
  ) |> 
  pivot_longer(
    -c(match_id, key, stats_title),
    names_to = 'type',
    values_to = 'value'
  )

wide_match_team_stats <- long_match_team_stats |> 
  pivot_wider(
    names_from = c(key, stats_title, type),
    values_from = value
  )

inner_join(
  gei_with_results |> select(league_id, round, match_id, gei),
  wide_match_team_stats,
  by = join_by(match_id)
) |> 
  select(gei, )

inner_join(
  gei |> select(match_id, gei),
  wide_match_team_stats,
  by = join_by(match_id)
) |> 
  select(-match_id) |> 
  corrr::correlate() |>
  filter(term == 'gei') |> 
  select(-term) |> 
  pivot_longer(
    everything(),
    names_to = 'term',
    values_to = 'cor'
  ) |> 
  # select(-term) |> 
  arrange(desc(abs(cor)))
