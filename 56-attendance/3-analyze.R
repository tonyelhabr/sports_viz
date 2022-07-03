
library(tidyverse)
library(qs)
library(lubridate)

dir_proj <- '56-attendance'
path_team_logos <- file.path(dir_proj, 'team_logos.qs')
path_clean_data <- file.path(dir_proj, 'clean_data.qs')
path_venue_capacities <- file.path(dir_proj, 'venue_capacities.qs')

## outputs
path_model_data <- file.path(dir_proj, 'model_data.qs')
clean_df <- path_clean_data |> qs::qread()

init_venue_capacities <- path_venue_capacities |> qs::qread()

seasons <- 2016:2022
venue_capacities <- bind_rows(
  init_venue_capacities |> 
    filter(is.na(season)) |> 
    select(-season) |> 
    full_join(
      tibble(season = seasons),
      by = character()
    ),
  crossing(
    venue_fbref = init_venue_capacities |> 
      filter(!is.na(season)) |> 
      distinct(venue_fbref) |> 
      pull(venue_fbref),
    season = seasons
  ) |> 
    left_join(
      init_venue_capacities |> 
        filter(!is.na(season)) |> 
        select(season, venue_fbref, team_538, capacity, capacity_source), 
      by = c('venue_fbref', 'season')
    ) |> 
    arrange(venue_fbref, season) |> 
    group_by(venue_fbref) |> 
    fill(team_538, capacity, capacity_source, .direction = 'downup') |> 
    ungroup()
) |> 
  select(season, venue = venue_fbref, team = team_538, capacity, capacity_source)

model_df <- clean_df |> 
  ## Possibly could not join on team
  inner_join(
    venue_capacities |>
      select(venue, season, team, capacity, capacity_source),
    by = c('venue', 'season', 'team')
  ) |>
  filter(!is.na(capacity)) |> 
  group_by(season, team) |> 
  mutate(
    is_first_home_match = gw == first(gw)
  ) |> 
  ungroup() |> 
  mutate(
    is_weekend = lubridate::wday(date, label = TRUE) %in% c('Sat', 'Sun'),
    across(matches('importance$'), ~.x / 100),
    avg_importance = (importance + importance_opp) / 2,
    attendance_prop = attendance / capacity,
    trunc_attendance_prop = ifelse(attendance_prop > 1, 1, attendance_prop),
    log_attendance = log(attendance),
    log_capacity = log(capacity),
    is_second_half = gw_prop >= 0.5
  )

changing_caps <- model_df |> 
  mutate(
    z = round(trunc_attendance_prop, 1)
  ) |> 
  count(season, league, team, z) |> 
  group_by(season, league, team) |> 
  filter(n() > 1) |> 
  mutate(rn = row_number(desc(n))) |> 
  slice_min(rn, n = 2) |> 
  ungroup() |>
  select(-n) |> 
  pivot_wider(
    names_from = rn,
    values_from = z,
    names_prefix = 'n'
  ) |> 
  mutate(
    diff = round(n1 - n2, 1)
  ) |> 
  filter(abs(diff) > 0.1)
changing_caps |> count(team, sort = TRUE)
ex_teams <- changing_caps |> count(team, sort = TRUE) |> filter(n > 2L) |> pull(team)

model_df |> 
  group_by(season, team) |> 
  mutate(
    across(
      trunc_attendance_prop,
      list(
        q25 = ~quantile(.x, 0.25),
        q75 = ~quantile(.x, 0.75)
      ),
      .names = '{fn}'
    )
  ) |> 
  ungroup() |> 
  mutate(
    z = (trunc_attendance_prop - q25) / (q75 - q25),
    .before = 1
  ) |> 
  select(league, season, team, trunc_attendance_prop, z, q25, q75) |> 
  arrange(desc(z))

ex <- model_df |> 
  filter(league == 'EFL', season != 2021)

ex |> 
  group_by(team, capacity) |> 
  summarize(
    across(
      c(attendance, trunc_attendance_prop),
      mean
    )
  ) |> 
  ungroup() |> 
  arrange(desc(trunc_attendance_prop))

ex |> 
  arrange(desc(attendance)) |> 
  select(team, date, attendance, capacity)

ex |> 
  ggplot() +
  aes(x = trunc_attendance_prop) +
  geom_histogram()

ex |> 
  filter(team == 'Atlanta United FC') |> 
  select(team_opp, importance, attendance, capacity)
model_df |> 
  filter(team %in% ex_teams) |> 
  ggplot() +
  aes(
    x = season + gw_prop, y = trunc_attendance_prop, color = factor(season)
  ) +
  geom_point() +
  geom_step() +
  facet_wrap(
    ~team,
    scales = 'fixed'
  )
model_df |> 
  filter(league == 'EPL') |> 
  filter(team == 'Manchester City') |> 
  filter(season == 2021) |> 
  arrange(attendance)

ex |> 
  filter(team == 'San Antonio FC') |> 
  arrange(league, season, team, date) |> 
  ggplot() +
  aes(
    x = importance, y = trunc_attendance_prop,
  ) +
  geom_point() +
  # geom_text(aes(label = gw, y = trunc_attendance_prop + 0.01)) +
  geom_path(aes(colour = gw)) +
  ggforce::geom_link()
  # geom_step() +
  facet_wrap(
    ~team,
    scales = 'free'
  )


df <- model_df |> 
  filter(season != 2020, !is.na(avg_importance)) |> 
  select(
    league,
    season,
    date,
    venue,
    gw_prop,
    is_first_home_match,
    # gd_per_game_prnk_lag1,
    pts_per_game_prnk_lag1,
    is_weekend,
    is_second_half,
    avg_importance,
    log_attendance,
    log_capacity,
    trunc_attendance_prop
  )

# df |> count(is_first_home_match)
df |> 
  pivot_longer(
    -c(league:venue, trunc_attendance_prop)
  ) |> 
  ggplot() +
  aes(
    
  )

library(ggforce)
df |> 
  filter(league == 'MLS') |>
  select(
    gw_prop,
    is_first_home_match,
    # gd_per_game_prnk_lag1,
    pts_per_game_prnk_lag1,
    is_weekend,
    is_second_half,
    avg_importance,
    log_attendance,
    log_capacity,
    trunc_attendance_prop
  ) |> 
  # skimr::skim()
  ggplot(
    aes(x = .panel_x, y = .panel_y, fill = trunc_attendance_prop, colour = trunc_attendance_prop)
  ) +
  geom_point(
    shape = 16,
    size = 0.5,
    position = 'auto',
    na.rm = TRUE
  ) +
  geom_autodensity(
    alpha = 0.3,
    colour = NA,
    position = 'identity',
    na.rm = TRUE
  ) +
  facet_matrix(
    vars(
      gw_prop,
      is_first_home_match,
      pts_per_game_prnk_lag1,
      is_weekend,
      is_second_half,
      avg_importance,
      log_attendance,
      log_capacity
    ),
    layer.diag = 2
  )

library(tidymodels)
library(multilevelmod)
rec <- recipe(
  log_attendance ~ league + gw_prop + is_first_home_match + is_weekend + is_second_half + pts_per_game_prnk_lag1 + log_capacity + avg_importance + pts_per_game_prnk_lag1 + (1 | venue),
  data = df
)
rec

spec <- linear_reg(engine = 'lmer')
wf <- workflow() |> 
  add_recipe(rec) |> 
  add_model(spec)

fit <- wf |> fit(df)
spec <- linear_reg(engine = 'lmer')
fit <- spec |> 
  fit(
    log_attendance ~ league + gw_prop + is_first_home_match + is_weekend + is_second_half + pts_per_game_prnk_lag1 + log_capacity + avg_importance + pts_per_game_prnk_lag1 + (1 | venue),
    data = df
  )
fit

fit <- spec |> 
  fit(
    log_attendance ~ league + gw_prop + is_first_home_match + is_weekend + is_second_half + pts_per_game_prnk_lag1 + log_capacity + pts_per_game_prnk_lag1 + (avg_importance | venue),
    data = df
  )
fit