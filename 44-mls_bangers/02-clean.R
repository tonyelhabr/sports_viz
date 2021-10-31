
library(tidyverse)
library(qs)
library(janitor)
library(fs)
library(lubridate)

dir_proj <- '44-mls_bangers'
dir_data <- file.path(dir_proj, 'data')

pluck_matches <- function(x) {
  bind_cols(tibble(date = lubridate::ymd(x$date)), x$leagues)
}

league_mapping <- tibble(
  league_id = c(47, 54, 87, 53, 55, 130),
  league_name = c('EPL', 'Bundesliga', 'LaLiga', 'Ligue 1', 'Serie A', 'MLS')
)

matches_init <-
  fs::dir_ls(
    dir_data,
    regexp = 'matches.*qs$'
  )[1] %>% 
  map_dfr(~qs::qread(.x) %>% pluck_matches()) %>% 
  janitor::clean_names()

matches <- 
  matches_init %>% 
  inner_join(
    league_mapping,
    by = c('primary_id' = 'league_id')
  ) %>% 
  hoist(
    matches,
    'match_id' = 'id'
  ) %>% 
  # `name` in matches is sort of like my `league_name`, but it also includes stuff like MLS tourneys
  # these are also indicated by `id`
  # `parent_league_id` is another possible thing to use for league id
  select(date, league_id = primary_id, league_name, match_id) %>% 
  unnest_longer(match_id)
matches

pluck_match_data <- function(x) {
  content <- x$content
  bind_cols(
    tibble(match_id = content$matchFacts$matchId),
    content$shotmap$shots
  )
}

match_data_init <-
  fs::dir_ls(
    dir_data,
    regexp = 'match_id.*qs$'
  ) %>% 
  map_dfr(~qs::qread(.x) %>% pluck_match_data()) %>% 
  janitor::clean_names() %>% 
  select(
    match_id,
    id,
    team_id,
    player_id,
    # player_name,
    period,
    min,
    min_added,
    event_type,
    shot_type,
    
    x,
    y,
    x_b = blocked_x,
    y_b = blocked_y,
    y_g = goal_crossed_y,
    z_g = goal_crossed_z,
    
    xg = expected_goals,
    xgot = expected_goals_on_target,
    # on_goal_shot
    is_blocked,
    is_on_target,
    is_own_goal
  )

matches_aug <- matches %>% 
  inner_join(match_data) %>% 
  # filter(!is_own_goal) %>% 
  mutate(
    xg_tier = case_when(
      is.na(xg) ~ NA_character_,
      xg >= 0.33 ~ 'great',
      xg >= 0.15 ~ 'good',
      xg >= 0.05 ~ 'average',
      TRUE ~ 'poor'
    ) %>% 
      ordered(c('great', 'good', 'average', 'poor', NA_character_))
  )

matches_aug %>% 
  count(league_name, is_na = is.na(id))
options(tibble.print_min = 24)
matches_aug %>% 
  filter(!is_own_goal) %>% 
  group_by(league_name, xg_tier) %>% 
  summarize(
    g = sum(event_type == 'Goal' & !is_own_goal),
    shots = n(),
    frac_conv = g / shots,
    across(c(xg, xgot), mean, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  group_by(league_name) %>% 
  mutate(
    frac_shots = shots / sum(shots),
    frac_g = g / sum(g)
  ) %>% 
  ungroup() %>% 
  mutate(
    frac_conv_lo = qbeta(0.025, g + 0.5, shots - g + 0.5),
    frac_conv_hi = qbeta(0.975, g + 0.5, shots - g + 0.5)
  )

