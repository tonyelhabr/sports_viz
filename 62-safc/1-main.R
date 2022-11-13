library(worldfootballR)
library(dplyr)
library(tidyr)
library(purrr)
library(qs)
library(lubridate)

dir_proj <- '62-safc'

usl_matches <- load_fotmob_matches_by_date(league_id = 8972) |> 
  mutate(
    across(date, lubridate::date)
  )

usl_2022_matches <- usl_matches |> 
  filter(!match_status_cancelled) |> 
  # filter(home_name == 'San Antonio FC' | away_name == 'San Antonio FC') |> 
  filter(date >= ymd('2022-03-12'))

usl_2022_match_winners <- usl_2022_matches |>
  distinct(match_id, date, home_id, away_id, home_name, away_name, home_score, away_score, home_pen_score, away_pen_score) |> 
  mutate(
    winning_side = case_when(
      !is.na(home_pen_score) & home_pen_score > away_pen_score ~ 'home',
      !is.na(away_pen_score) & home_pen_score < away_pen_score ~ 'away',
      home_score == away_score ~ 'draw',
      home_score > away_score ~ 'home',
      home_score < away_score ~ 'away'
    ),
    winning_team_id = case_when(
      winning_side == 'home' ~ home_id,
      winning_side == 'away' ~ away_id,
      winning_side == 'draw' ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  )
usl_2022_match_winners

## most recently used team names for each id
team_mapping <- bind_rows(
  usl_2022_match_winners |> 
    group_by(team_id = home_id, team_name = home_name) |> 
    slice_max(match_id, n = 1) |> 
    ungroup(),
  usl_2022_match_winners |>
    group_by(team_id = away_id, team_name = away_name) |> 
    slice_max(match_id, n = 1) |> 
    ungroup()
) |> 
  select(team_id, team_name, match_id) |> 
  group_by(team_id) |> 
  slice_max(match_id, n = 1) |> 
  ungroup() |>
  select(team_id, team_name) |> 
  arrange(team_name)

get_fotmob_match_team_stats <- function(match_id, overwrite = FALSE) {
  path <- file.path(dir_proj, 'data', sprintf('%s.qs', match_id))
  if (file.exists(path) & isFALSE(overwrite)) {
    return(qs::qread(path))
  }
  Sys.sleep(runif(1))
  res <- worldfootballR::fotmob_get_match_team_stats(match_id)
  qs::qsave(res, path)
  res
}

usl_2022_match_team_stats <- usl_2022_matches$match_id |> 
  map_dfr(get_fotmob_match_team_stats)

poss <- usl_2022_match_team_stats |> 
  filter(stats_title == 'Ball possession')

long_poss <- poss |> 
  select(match_id, home_id = home_team_id, away_id = away_team_id, home_value, away_value) |> 
  mutate(
    across(ends_with('value'), as.integer)
  ) |> 
  pivot_longer(
    -match_id,
    names_to = c('side', 'stat'),
    names_sep = '_'
  ) |> 
  pivot_wider(
    names_from = stat,
    values_from = value
  ) |> 
  rename(team_id = id) |> 
  inner_join(
    team_mapping,
    by = 'team_id'
  )

team_mapping |> filter(team_name == 'San Antonio FC')
long_match_winners <- usl_2022_match_winners |> 
  select(match_id, home_id, away_id, winning_team_id) |> 
  pivot_longer(
    -c(match_id, winning_team_id),
    names_to = 'side',
    values_to = 'team_id',
    names_pattern = '(^.*)_id'
  ) |> 
  mutate(
    match_result = case_when(
      is.na(winning_team_id) ~ 'd',
      team_id == winning_team_id ~ 'w',
      team_id != winning_team_id ~ 'l',
    )
  )

poss_result_n <- long_poss |> 
  inner_join(
    long_match_winners |> select(match_id, team_id, match_result),
    by = c('match_id', 'team_id')
  ) |> 
  count(team_id, match_result, less_poss = value < 50)

wide_poss_result_n <- poss_result_n |> 
  mutate(
    across(less_poss, ~ifelse(.x, 'lt50_poss', 'gte50_poss'))
  ) |> 
  pivot_wider(
    names_from = c(less_poss, match_result),
    values_from = n,
    values_fill = 0L
  ) |> 
  inner_join(
    team_mapping,
    by = 'team_id'
  ) |> 
  relocate(team_name, .after = team_id)

wide_poss_result_n |> 
  filter(team_name == 'San Antonio FC')
options(tibble.print_min = 30)
wide_poss_result_n |> arrange(desc(lt50_poss_w)) |> readr::write_csv(file.path(dir_proj, 'table.csv'))
wide_poss_result_n |> arrange(desc(gte50_poss_w))

poss_result_n |> 
  group_by(team_id, less_poss) |> 
  summarize(across(n, sum)) |> 
  ungroup() |> 
  group_by(team_id) |> 
  mutate(
    total = sum(n)
  ) |> 
  ungroup() |> 
  filter(less_poss) |> 
  mutate(
    prop = n / total
  ) |> 
  arrange(desc(prop))
