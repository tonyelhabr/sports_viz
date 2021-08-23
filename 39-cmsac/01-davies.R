
library(tidyverse)
dir_proj <- '39-cmsac'
df_init <- 
  file.path(dir_proj, 'DAVIES.xlsx') %>% 
  readxl::read_excel(skip = 1) %>% 
  janitor::clean_names()
df_init

df <-
  df_init %>% 
  # Drop weird labels.
  filter(player %>% str_detect('_blank', negate = TRUE)) %>% 
  mutate(
    across(age_during_season, ~ifelse(.x == 0, current_age, .x) %>% as.integer()),
    # across(season, ~ifelse(league == 'MLS', sprintf('%s-%s', .x, paste0(str_sub(.x, 1, 2), str_sub(.x, 3, 4) %>% as.integer() + 1L)), .x)),
    across(season, ~str_remove(.x, '[-].*$') %>% as.integer()),
    across(min, as.integer)
  ) %>% 
  distinct(
    player,
    team,
    position,
    season,
    league,
    play_style,
    age = age_during_season,
    mp = min,
    elo = team_elo,
    xga = x_goals_added,
    davies
  ) %>% 
  arrange(player, season) %>% 
  group_by(player, position) %>% 
  mutate(
    across(c(league, season, team), list(prev = ~lag(.x))),
    is_same_season = ifelse(season == season_prev, TRUE, FALSE),
    is_same_team = ifelse(team == team_prev, TRUE, FALSE),
    is_transfer = ifelse(league != league_prev, TRUE, FALSE)
  ) %>% 
  ungroup()
df

df %>% count(is_transfer)
df %>% filter(is_transfer) %>% count(league, league_prev, sort = TRUE)
df %>% count(is_transfer, is_same_season)
df %>% count(is_transfer, is_same_team)
# df %>% filter(season == '2020-2021')

library(tidymodels)
set.seed(42)
players <- df %>% count(player) %>% mutate(player_id = row_number())
players
df_w_id <- df %>% left_join(players)
df_w_id
n_player <- players %>% nrow()
# idx_trn <- 1:(0.8 * n_player)
idx_trn <- sample(0.8 * n_player, replace = FALSE)
idx_trn
players_trn <- players[idx_trn, ]
players_tst <- players[-idx_trn, ]

# df %>% filter(player == 'Raul Garcia')
# split <- df %>% initial_split(strata = play_style)
# df_trn <- split %>% training()
# df_tst <- split %>% testing()
# folds <- df_trn %>% group_vfold_cv(group = player, v = 10, strata = xga)
df_trn <- df_w_id %>% semi_join(players_trn)
df_tst <- df_w_id %>% semi_join(players_tst)
folds <- df_w_id %>% group_vfold_cv(group = player_id, v = 10, strata = xga)
folds

rec <-
  df_trn %>% 
  recipe(formula(xga ~ league + play_style + elo + age), .) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~is_transfer:matches('league_'))

jui_trn <- rec %>% prep() %>% juice()
jui_trn

wf <-
  rec %>% 
  workflow(spec = linear_reg())
fit <- wf %>% fit(df_trn)
fit
coefs <- fit %>% broom::tidy()
coefs

ids <-
  df %>% 
  distinct(player, league)
ids

ids_gt1 <-
  ids %>% 
  count(player, sort = TRUE) %>% 
  filter(n > 1L) %>% 
  select(-n) %>% 
  inner_join(ids)
ids_gt1

rgx_rename <- '^(season|idx|team_name|league|z)'
ids_gt1_meta <-
  ids_gt1 %>% 
  left_join(
    data %>% 
      select(player_id, player_name, matches('position'), age_grp, country, matches(rgx_rename)) %>% 
      unite('league', country, league_name, sep = '_') %>% 
      mutate(across(league, ~str_replace_all(.x, '\\s|[.]', '_') %>% str_replace_all('[_]+', '_'))) %>% 
      left_join(league_mapping %>% select(-path)) %>% 
      select(-league) %>% 
      rename(league = league_lab)
  )