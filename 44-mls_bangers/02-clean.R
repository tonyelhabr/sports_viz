
library(tidyverse)
library(qs)
library(janitor)

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
  ) %>% 
  map_dfr(~qs::qread(.x) %>% pluck_matches()) %>% 
  janitor::clean_names()

matches <- 
  matches_init %>% 
  inner_join(
    league_mapping,
    by = c('primary_id' = 'league_id')
    # by = c('parent_league_id' = 'league_id')
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

match_data <-
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
    #  on_goal_shot
    is_blocked,
    is_on_target,
    is_own_goal
  )
matches_w_meta <- matches %>% 
  inner_join(match_data) %>% 
  filter(!is_own_goal) # %>% 
  # mutate(
  #   g = if_else(event_type == 'Goal', 1L, 0L),
  #   is_g = if_else(g == 1L & !is_own_goal, TRUE, FALSE)
  # )

matches_w_meta %>% 
  mutate(
    g = ifelse(event_type == 'Goal', TRUE, FALSE),
    across(xg, log)
  ) %>% 
  ggplot() +
  aes(x = xg) +
  # facet_wrap(~league_name, scales = 'fixed') +
  # geom_histogram(
  #   binwidth = 0.05,
  #   aes(fill = g),
  #   alpha = 0.5
  # )
  geom_density(
    # binwidth = 0.1,
    aes(
      linetype = league_name,
      color = g
    )
  )
matches_w_meta %>% 
  group_by(league_name) %>% 
  summarize(
    g = sum(event_type == 'Goal'),
    n = n(),
    prop = g / n,
    across(c(xg, xgot), mean, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    lo = qbeta(0.025, g + 0.5, n - g + 0.5),
    hi = qbeta(0.975, g + 0.5, n - g + 0.5)
  )

# 3610021: 10/30 Leicester City Arsenal
match_data %>% 
  filter(match_id == 3610021) %>% 
  
