

library(tidyverse)

dir_proj <- '21-202021_epl_heatmap'
path_events <- file.path(dir_proj, 'events.rds')
subs <-
  events %>% 
  # filter(type_name %in% c('SubstitutionOn', 'SubstitutionOff')) %>% 
  filter(type_name == 'SubstitutionOff') %>% 
  select(season, match_id, team_id, team_name, player_id, player_name, type_name, expanded_minute)
subs

events %>% relocate(card_type_name) %>% filter(!is.na(card_type_name))

events_last <-
  events %>% 
  # There's at least one outlier with `match_id == 1485296`
  filter(expanded_minute < 120) %>% 
  group_by(match_id) %>% 
  slice_max(expanded_minute) %>% 
  ungroup() %>% 
  distinct(match_id, expanded_minute)
events_last

subs_grp <-
  subs %>% 
  arrange(expanded_minute) %>% 
  group_by(match_id, team_id, type_name) %>% 
  # This should match the `idx` from `forms`.
  mutate(idx = row_number(expanded_minute) + 1L) %>% 
  ungroup() %>% 
  select(match_id, team_id, idx, expanded_minute)

minutes_init <-
  subs_grp %>% 
  filter(idx == min(idx)) %>% 
  mutate(idx = 1L, expanded_minute = 0L)
minutes_init

minutes_max <-
  subs_grp %>% 
  group_by(match_id, team_id) %>% 
  slice_max(idx, with_ties = FALSE) %>% 
  ungroup() %>% 
  left_join(events_last %>% rename(expanded_minute_last = expanded_minute)) %>% 
  mutate(
    across(idx, ~.x + 1L), 
    across(expanded_minute, ~ifelse(expanded_minute > expanded_minute_last, .x, expanded_minute_last))
  )
minutes_max

minutes <-
  list(
    minutes_init,
    subs_grp,
    minutes_max %>% select(-expanded_minute_last)
  ) %>% 
  reduce(bind_rows)
minutes

mp_init <-
  forms %>% 
  left_join(minutes) %>% 
  left_join(
    minutes %>% 
      mutate(across(idx, ~.x - 1L)) %>% 
      rename(expanded_minute_lead1 = expanded_minute)
  ) %>% 
  mutate(mp = expanded_minute_lead1 - expanded_minute)
mp_init %>% filter(is.na(mp)) %>% count(match_id)

# There are mistmatches cuz I have to account for red cards and 2 yellow cards. There might be more cases as well.
forms %>% 
  filter(match_id == 1485276, team_id == 13) %>% 
  filter(idx == max(idx))
mp_init %>% 
  filter(match_id == 1485276, team_id == 13) %>% 
  filter(idx == max(idx))

subs_grp %>% 
  
  minutes %>% 
  filter(match_id == 1485276, team_id == 13) %>% 
  mutate(across(idx, ~.x - 1L)) %>% 
  rename(expanded_minute_lead1 = expanded_minute)

mp <-
  mp_init %>% 
  group_by(match_id, player_id, player_name) %>% 
  summarize(across(mp, sum)) %>% 
  ungroup()
mp %>% filter(is.na(mp))
mp %>% arrange(desc(mp))
mp_agg <-
  mp %>% 
  group_by(player_id, player_name) %>% 
  summarize(g = n(), across(mp, sum)) %>% 
  ungroup()
mp_agg