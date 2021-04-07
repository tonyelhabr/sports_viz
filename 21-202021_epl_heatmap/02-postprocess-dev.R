
library(tidyverse)
dir_proj <- '21-202021_epl_heatmap'
dir_data <- file.path(dir_proj, 'data')
paths <- fs::dir_ls(dir_data, regexp = 'js$')
paths
text_trim <- paths[1] %>% read_lines() %>% paste0(collapse = '', sep = '')
# text_trim %>% map_int(nchar)
lst_match_id <-
  text_trim %>% 
  str_replace_all('^.*matchId[:]\\s?', '') %>% 
  str_replace_all('\\,\\s+matchCentreData.*$', '') %>% 
  jsonlite::parse_json()

lst_event <-
  text_trim %>% 
  str_replace_all('^.*matchCentreData[:]\\s+', '') %>% 
  str_replace_all('\\,\\s+matchCentreEventTypeJson.*$', '') %>% 
  jsonlite::parse_json()

lst_event_type <-
  text_trim %>% 
  str_replace_all('^.*matchCentreEventTypeJson[:]\\s+', '') %>% 
  str_replace_all('\\,\\s+formationIdNameMap.*$', '') %>% 
  jsonlite::parse_json()

events_init <- lst_event %>% enframe()
event_types <-
  lst_event_type %>%
  enframe('event_type_name', 'event_type') %>% 
  unnest(event_type)
event_types

events <-
  events_init %>% 
  filter(name == 'events') %>% 
  select(value) %>% 
  unnest(value) %>% 
  unnest_wider(value)

players <-
  events_init %>%
  filter(name == 'playerIdNameDictionary') %>%
  select(value) %>%
  unnest_longer(value, indices_to = 'player_id', values_to = 'player_name') %>% 
  mutate(across(player_id, as.integer))
players

team <-
  events_init %>%
  filter(name == 'home') %>%
  select(value) %>% 
  unnest_wider(value) %>%
  janitor::clean_names()

form_init <-
  team %>% 
  select(formations) %>% 
  unnest(formations) %>% 
  unnest_wider(formations) %>% 
  mutate(idx = row_number()) %>%
  janitor::clean_names()

form_slots <-
  form_init %>% 
  select(-formation_positions) %>% 
  unnest(c(jersey_numbers, formation_slots, player_ids)) %>% 
  unnest(c(jersey_numbers, formation_slots, player_ids)) %>% 
  rename_with(~str_remove(.x, 's$'), c(jersey_numbers, formation_slots, player_ids)) %>% 
  filter(formation_slot != 0L)

form_pos <-
  form_init %>% 
  select(idx, formation_positions) %>% 
  unnest(formation_positions) %>% 
  unnest_wider(formation_positions)
pos <-
  tibble(
    row = seq.int(1, 5),
    pos = c('G', 'D', 'M', rep('F', 2))
  )

form <- 
  bind_cols(
    form_slots, 
    form_pos %>% select(-idx)
  ) %>% 
  relocate(idx) %>% 
  left_join(players) %>% 
  select(idx, formation_id, formation_name, formation_slot, jersey_number, player_id, player_name, x = horizontal, y = vertical) %>% 
  group_by(idx) %>% 
  mutate(row = dense_rank(y)) %>% 
  ungroup() %>% 
  left_join(pos, by = 'row')
form

form %>% 
  ggplot() +
  aes(x = horizontal, y = vertical) +
  geom_label(aes(label = playerIds)) +
  facet_wrap(~.idx)


.unnest_side <- function(side) {
  events_init %>%
    filter(name == !!side) %>%
    select(value) %>% 
    unnest_wider(value) %>%
    janitor::clean_names() %>% 
    select(team_id, team_name = name, side = field)
}

teams <- c('home', 'away') %>% map_dfr(.unnest_side)
teams

# x <-
#   events_init %>%
#   filter(name == 'away') %>%
#   select(value) %>% 
#   unnest_wider(value) %>%
#   janitor::clean_names()
# x
# x %>% 
#   select(incident_events) %>% 
#   unnest(incident_events) %>% 
#   unnest_wider(incident_events)
# x %>% 
#   select(shot_zones) %>% 
#   unnest(shot_zones) %>% 
#   unnest_longer(shot_zones)
# x %>% 
#   select(stats) %>% 
#   unnest(stats) %>% 
#   unnest_longer(stats)

events_clean_init <-
  events %>%
  janitor::clean_names() %>% 
  hoist(period, 'period' = 'value', 'period_name' = 'displayName') %>% 
  hoist(type, 'type' = 'value', 'type_name' = 'displayName') %>% 
  hoist(outcome_type, 'outcome_type' = 'value', 'outcome_type_name' = 'displayName') %>% 
  # There's some weird thing here with `cardType` compared to the other elements that are hoisted.
  # Since `cardType` is NULL by default while the others are always named lists, automatically renaming
  # a hoisted element to the same name of the column that is being hoisted drops the column; one way
  # to avoid this is to temporarily name the hoisted column to something different (in this case `cardTypex`).
  hoist(card_type, 'card_typex' = 'value', 'card_type_name' = 'displayName') %>% 
  rename(cardtype = card_typex) %>% 
  left_join(teams) %>% 
  left_join(players)
events_clean_init

qualifiers <-
  events_clean_init %>% 
  select(id, qualifiers) %>% 
  unnest_longer(qualifiers) %>% 
  unnest_wider(qualifiers) %>% 
  drop_na(value) %>% 
  hoist(type, 'qualifier_id' = 'value', 'qualifier_name' = 'displayName')
qualifiers

satisfied_events_types <-
  events_clean_init %>% 
  select(id, satisfied_events_types) %>% 
  unnest_longer(satisfied_events_types) %>% 
  rename(event_type = satisfied_events_types) %>% 
  drop_na(event_type) %>% 
  left_join(event_types)
satisfied_events_types

events_clean <- 
  events_clean_init %>% 
  select(-c(qualifiers, satisfied_events_types))
  # select(where(~class(.x) != 'list'))
events_clean
events_clean %>% filter(player_name == 'Adama Traoré') %>% relocate(player_id, player_name)

# events_foul <- 
#   satisfied_events_types %>% 
#   filter(event_type_name == 'foulCommitted') %>% 
#   left_join(events_clean)
# events_foul

events_foul <-
  events_clean %>% 
  filter(type_name == 'Foul') %>% 
  # This corresponds with `event_type_name == "foulCommited"`, so no need to join `satisfied_events_types`.
  filter(outcome_type_name == 'Unsuccessful')
events_foul

events_foul %>% count(player_name, sort = TRUE)

events_foul %>% 
  # filter(player_name == 'Adama Traoré') %>% 
  filter(period_name == 'SecondHalf') %>% 
  mutate(
    across(
      c(x, y),
      ~case_when(
        side == 'away' ~ 1 * (100 - .x),
        TRUE ~ .x
      )
    )
  ) %>% 
  ggplot() +
  ggsoccer::annotate_pitch() +
  ggsoccer::theme_pitch() +
  aes(x = x, y = y, color = side) +
  geom_point() +
  guides(color = FALSE) +
  ggrepel::geom_label_repel(aes(label = player_name))


events_clean %>% 
  select(matches('_[xy]$')) %>% 
  skimr::skim()

# event_types %>% filter(event_type_name %>% str_detect('oul'))
# full_join(qualifiers, satisfied_events_types)

# TODO: Change `dims` to `rng`?
.get_valid_coords <- function() {
  c('x', 'y')
}

.validate_coord <- function(x = .get_valid_coords(), ...) {
  match.arg(x, ...)
}

.get_rng_m <- function(coord = .get_valid_coords()) {
  .validate_coord(coord)
  switch(coord, x = c(0, 105), y = c(0, 68))
}

.rescale_vec <- function(x, rng1, rng2) {
  rng2[1] + ((x - rng1[1]) * (rng2[2] - rng2[1])) / (rng1[2] - rng1[1])
}


.rescale_xy_cols <-
  function(.data,
           rng_x_from = c(0, 100),
           rng_y_from = rng_x_from,
           rng_x_to = .get_rng_m('x'),
           rng_y_to = .get_rng_m('y'),
           rgx_x = '^x$',
           rgx_y = '^y$',
           scaler_x = 1,
           scaler_y = 1) {
    # multiplier_x <- ifelse(flip_x, -1, 1)
    # multiplier_y <- ifelse(flip_y, -1, 1)
    res <-
      .data %>%
      mutate(
        across(
          matches(rgx_x),
          ~ .rescale_vec(scaler_x * .x, rng_x_from, rng_x_to)
        ),
        across(
          matches(rgx_y),
          ~ .rescale_vec(scaler_y * .x, rng_y_from, rng_y_to)
        )
      )
    res
  }
