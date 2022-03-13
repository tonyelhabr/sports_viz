
## 1. need a grid of minutes per game that can be uncounted to account for missing minutes
## 2. need to rescale minutes in the first half after the 45th to 45-48 and in the second half after the 90th to 90-96
library(tidyverse)
convert_seconds_to_expanded_minute <- function(seconds) {
  (seconds + 60) %/% 60
}

all_actions <- '../canzhi/dev/data/2021_xg.parquet' %>% 
  arrow::read_parquet() %>% 
  filter(league_id == 2) %>%
  mutate(
    expanded_minute = convert_seconds_to_expanded_minute(time_seconds)
  )

first_half_minute <- 45
second_half_minute <- 90
max_first_half_minute <- 47
max_second_half_minute <- 95
first_half_second <- first_half_minute * 60
second_half_second <- second_half_minute * 60
max_first_half_second <- max_first_half_minute * 60
max_second_half_second <- max_second_half_minute * 60

max_times <- all_actions %>% 
  select(
    game_id,
    team_id,
    period_id,
    time_seconds,
    expanded_minute
  ) %>% 
  group_by(game_id, team_id, period_id) %>%
  mutate(
    across(
      time_seconds,
      list(max = max)
    )
  ) %>%
  ungroup()
max_times

is_extra_time <- function(period_id, minute_col) {
  period_id <- ensym(period_id)
  minute_sym <- ensym(minute_col)
  case_when(
    period_id == 1L & minute_sym >= 45 ~ TRUE,
    period_id == 2L & minute_sym >= 90 ~ TRUE,
    TRUE ~ FALSE
  )
}

time_seconds_ratio <- function(period_id, time_seconds, max_second) {
  half_second <- ifelse(
    period_id == 1L,
    first_half_second,
    second_half_second
  ) 
  if(time_seconds < half_second) {
    return(
      1
    )
  }
  max_half_second <- ifelse(
    period_id == 1L,
    max_first_half_second,
    max_second_half_second
  )
  (max_second - half_second) / (max_half_second - half_second)
}


rescale_injury_time_second_v <- function(period_id, time_seconds, max_second) {
  half_second <- ifelse(
    period_id == 1L,
    first_half_second,
    second_half_second
  ) 
  if(time_seconds < half_second) {
    return(
      time_seconds
    )
  }
  max_half_second <- ifelse(
    period_id == 1L,
    max_first_half_second,
    max_second_half_second
  )
  
  scales::rescale(
    time_seconds,
    from = c(half_second, max_second),
    to = c(half_second, max_half_second)
  )
}

rescale_injury_time_second <- Vectorize(rescale_injury_time_second_v) 


max_times %>% 
  head(10000) %>%
  filter(period_id == 2L, expanded_minute > 90) %>% 
  # filter(is_extra_time(period_id, expanded_minute)) %>% 
  mutate(
    r = time_seconds_ratio(period_id, time_seconds, time_seconds_max)
  )

minutes_rescaled <- max_times %>% 
  mutate(
    expanded_minute_rescaled = rescale_injury_time_second(period_id, time_seconds, time_seconds_max) %>% 
      convert_seconds_to_expanded_minute()
  ) %>% 
  select(-time_seconds_max) %>% 
  group_by(
    game_id,
    team_id,
    period_id,
    expanded_minute
  ) %>% 
  slice_max(
    expanded_minute_rescaled,
    n = 1,
    with_ties = FALSE
  ) %>% 
  ungroup()

minutes_rescaled %>% 
  group_by(game_id, team_id, period_id) %>% 
  slice_max(expanded_minute_rescaled, n = 1, with_ties = FALSE) %>% 
  ungroup()

# minutes_rescaled %>% 
#   group_by(game_id, team_id, period_id) %>% 
#   filter(
#     period_id == 1,
#     time_seconds >= first_half_second
#   ) %>% 
#   distinct(
#     expanded_minute,
#     expanded_minute_rescaled
#   )

actual_ids <- minutes_rescaled %>% 
  distinct(
    game_id,
    team_id,
    period_id,
    expanded_minute,
    expanded_minute_rescaled
  ) %>% 
  arrange(
    game_id,
    team_id,
    period_id,
    expanded_minute,
    expanded_minute_rescaled
  )
actual_ids

expected_ids <- bind_rows(
  actual_ids %>% 
    distinct(
      game_id,
      team_id
    ) %>% 
    crossing(
      period_id = 1L,
      expanded_minute_rescaled = 1L:48L
    ) %>% 
    mutate(
      is_extra_time = expanded_minute_rescaled >= 45L
    ),
  actual_ids %>% 
    distinct(
      game_id,
      team_id
    ) %>% 
    crossing(
      period_id = 2L,
      expanded_minute_rescaled = 45L:96L
    ) %>% 
    mutate(
      is_extra_time = expanded_minute_rescaled >= 90L
    )
) %>% 
  arrange(
    game_id, team_id, period_id, expanded_minute_rescaled
  )

expected_ids %>% 
  left_join(actual_ids) %>% 
  filter(is_extra_time) %>% 
  filter(lead(is.na(expanded_minute)) | is.na(expanded_minute) | lag(is.na(expanded_minute)))

expected_ids %>% 
  filter(is_extra_time) %>% 
  count(expanded_minute_rescaled)

cumu_xg <- all_actions %>% 
  group_by(game_id, team_id, period_id, expanded_minute) %>% 
  summarize(
    across(
      xg,
      sum,
      na.rm = TRUE
    )
  ) %>% 
  ungroup() %>% 
  arrange(
    game_id,
    team_id,
    expanded_minute
  )

cumu_xg_with_ids <- expected_ids %>% 
  left_join(actual_ids) %>% 
  left_join(cumu_xg)
cumu_xg_with_ids %>% 
  fill(
    xg
  )

fouls <- minutes_rescaled %>% 
  filter(
    type_name == 'Foul'
  ) %>% 
  select(
    season, 
    game_id,
    team_id,
    player_id,
    expanded_minute_rescaled,
    result_id
  )

cards <- all_actions %>% 
  filter(
    type_name == 'Card'
  )