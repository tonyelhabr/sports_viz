library(tidyverse)
library(qs)

dir_proj <- '49-yellow_cards'
dir_data <- file.path(dir_proj, 'data')

summaries <- qs::qsave(file.path(dir_data, 'summeries.qs'))

# Saturday August 17, 2013 is the first time single yellow cards started to be recorded
events <- summaries |>
  transmute(
    rn = row_number(),
    country,
    season,
    date = match_date |>lubridate::mdy(),
    link = game_url,
    event_half,
    event_time,
    event_type,
    event_players,
    score = score_progression
  ) |>
  filter(date >= lubridate::ymd('2013-08-17')) |>
  separate(
    score, into = c('home_g', 'away_g'), sep = '[:]'
  )
events |> 
  filter(is.na(date))

match_ids <- events |>
  arrange(date, link) |>
  distinct(link) %>%
  mutate(
    match_idx = row_number(),
    .before = 1
  )

events_numbered <- inner_join(
  match_ids,
  events,
  by = 'link'
) |>
  # arrange(match_idx, event_half, event_type) |>
  group_by(match_idx) |>
  mutate(
    event_idx = row_number(rn),
    .after = 1
  ) |>
  ungroup()
events_numbered

first_yellow_cards <- events_numbered |>
  filter(event_type == 'Yellow Card') |>
  select(
    match_idx,
    first_yellow_idx = event_idx,
    first_yellow_player = event_players,
    first_yellow_min = event_time
  )

second_yellow_cards <- events_numbered |>
  filter(event_type == 'Second Yellow Card') |>
  select(
    match_idx,
    second_yellow_idx = event_idx,
    second_yellow_player = event_players,
    second_yellow_min = event_time
  )

red_cards <- events_numbered |>
  filter(event_type == 'Red Card') |>
  select(
    match_idx,
    red_idx = event_idx,
    red_player = event_players,
    red_min = event_time
  )

subs <- events_numbered |>
  filter(event_type == 'Substitute') |>
  separate(
    event_players,
    into = c('player_in', 'player_out'),
    sep = ' for '
  ) |>
  transmute(
    match_idx,
    subbed_out_idx = event_idx,
    subbed_out_player = player_out,
    subbed_out_min = event_time,
    subbed_in_idx = event_idx,
    subbed_in_player = player_in,
    subbed_in_min = event_time
  )

## why i needed to correct for some times
# first_yellows_and_subs |>
#   filter(subbed_out_min < first_yellow_min) |>
#   arrange(desc(match_idx)) |>
#   left_join(
#     match_ids,
#     by = 'match_idx'
#   )

first_yellows_and_subs <- inner_join(
  first_yellow_cards,
  subs,
  by = c('match_idx', 'first_yellow_player' = 'subbed_out_player')
) |>
  mutate(
    across(
      subbed_out_min,
      ~ifelse(
        .x > first_yellow_min,
        first_yellow_min,
        .x
      )
    )
  )

two_yellow_cards <- inner_join(
  first_yellow_cards,
  second_yellow_cards,
  by = c('match_idx', 'first_yellow_player' = 'second_yellow_player')
)

first_yellow_and_red_cards <- inner_join(
  first_yellow_cards,
  red_cards,
  by = c('match_idx', 'first_yellow_player' = 'red_player')
) 
first_yellow_and_red_cards

second_yellow_and_red_cards <- inner_join(
  second_yellow_cards,
  red_cards,
  by = c('match_idx', 'second_yellow_player' = 'red_player')
) 

solo_red_cards <- red_cards |>
  anti_join(
    first_yellow_and_red_cards,
    by = c('match_idx', 'red_player' = 'first_yellow_player')
  ) |>
  anti_join(
    second_yellow_and_red_cards,
    by = c('match_idx', 'red_player' = 'second_yellow_player')
  )

lineups_with_event_times <- lineups |>
  filter(min > 0) |>
  left_join(
    match_ids,
    by = 'link'
  ) |>
  left_join(
    first_yellow_cards,
    by = c('match_idx', 'player' = 'first_yellow_player')
  ) |>
  left_join(
    second_yellow_cards,
    by = c('match_idx', 'player' = 'second_yellow_player')
  ) |>
  left_join(
    red_cards,
    by = c('match_idx', 'player' = 'red_player')
  ) |>
  left_join(
    subs |>
      select(
        match_idx,
        subbed_in_idx,
        subbed_in_player,
        subbed_in_min
      ),
    by = c('match_idx', 'player' = 'subbed_in_player')
  ) |>
  left_join(
    subs |>
      select(
        match_idx,
        subbed_out_idx,
        subbed_out_player,
        subbed_out_min
      ),
    by = c('match_idx', 'player' = 'subbed_out_player')
  ) |>
  arrange(match_idx, player)

lineups_with_event_times |> 
  select(-link) |> 
  filter(season == '2021-2022') |> 
  filter(event_type %in% c('Yellow Card', 'Second Yellow Card', 'Red Card')) |> 
  count()

df <- lineups_with_event_times |>
  ## just look at starters for now
  filter(is.na(subbed_in_min)) |>
  ## TODO: Add exogenous variables (home_g, away_g)
  transmute(
    match_idx,
    matchday,
    player,
    time = coalesce(first_yellow_min, min),
    status = ifelse(!is.na(first_yellow_min), 1, 0),
    # censored = ifelse(!is.na(subbed_out_min), 1, 0)
    group = case_when(
      !is.na(first_yellow_min) ~ 1,
      !is.na(subbed_out_min) ~ 0,
      TRUE ~ 0
    )
  )

df |>
  filter(
    status == 1,
    censored == 1
  )
library(survival)
f1 <- survfit(Surv(time, status) ~ 1, data = df)
plot(f1, mark.time = TRUE)
summary(
  f1, 
  times = c(1,30,60,89,90,91,92,93,94,95)
)

lineups_with_event_times |>
  filter(!is.na(first_yellow_min)) |>
  ggplot() +
  aes(x = first_yellow_min) +
  geom_histogram(binwidth = 1)

lineups_with_event_times |>
  filter(!is.na(first_yellow_min) & !is.na(second_yellow_min)) |>
  mutate(
    diff = second_yellow_min - first_yellow_min
  ) |>
  ggplot() +
  aes(
    x = diff
  ) +
  geom_histogram(binwidth = 5)

lineups_with_event_times |>
  filter(!is.na(first_yellow_min)) |>
  mutate(
    diff = coalesce(subbed_out_min - first_yellow_min, 0)
  ) |>
  ggplot() +
  aes(
    x = diff
  ) +
  geom_histogram(binwidth = 1)

lineups_with_event_times |>
  count(
    !is.na(first_yellow_min),
    !is.na(second_yellow_min)
  ) |>
  mutate(prop = n / sum(n))
