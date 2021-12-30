
library(tidyverse)
dir_proj <- '46-euro_fouls'

f_import <- function(name) {
  path <- file.path(dir_data, sprintf('%s.parquet', name))
  res <- path %>% arrow::read_parquet()
  assign(value = res, x = name, envir = .GlobalEnv)
}

c(
  'actions',
  'actiontypes',
  'bodyparts',
  'players',
  'player_games',
  'teams',
  'results',
  'games'
) %>%
  walk(f_import)

.pos_grps <- c('F', 'M', 'D', 'G', 'z')
.pos_grp_labs <- c('Forward/Attacker', 'Midfielder', 'Defender', 'Goalkeeper', 'Other')
.factor_pos_grp_col <- function(data) {
  data %>% 
    mutate(
      across(pos_grp, ~ordered(.x, .pos_grps))
    )
}

.factor_pos_grp_lab_col <- function(data) {
  data %>% 
    mutate(pos_grp_lab = ordered(pos_grp_lab, !!.pos_grp_labs))
}

pos_grp_labs <-
  tibble(
    pos_grp = .pos_grps,
    pos_grp_lab = .pos_grp_labs
  ) %>% 
  .factor_pos_grp_col() %>% 
  .factor_pos_grp_lab_col()

pos_info <-
  tibble(
    pos = c('AMC', 'AML', 'AMR', 'DC', 'DL', 'DMC', 'DML', 'DMR', 'DR', 'FW', 'FWL', 'FWR', 'GK', 'MC', 'ML', 'MR', 'Sub'),
    pos_grp = c('F', 'F', 'F', 'D', 'D', 'M', 'M', 'M', 'D', 'F', 'F', 'F', 'G', 'M', 'M', 'M', 'z'),
    pos_11 = c('FWC', 'FWL', 'FWR', 'DC', 'DL', 'MC', 'ML', 'MR', 'DR', 'FWC', 'FWL', 'FWR', 'GK', 'MC', 'ML', 'MR', 'z')
  ) %>% 
  .factor_pos_grp_col()

players <- players %>% 
  distinct(player_id, player_name) %>% 
  group_by(player_id) %>% 
  slice_head(n = 1) %>% 
  ungroup()

games <- games %>% 
  mutate(
    across(duration, ~case_when(game_id == 1485349L ~ 98L, TRUE ~ .x))
  )

player_games <- player_games %>% 
  mutate(
    across(minutes_played, ~case_when(game_id == 1485349L ~ 98L, TRUE ~ .x))
  )

games_by_team <- bind_rows(
  games %>% select(game_id, team_id = home_team_id),
  games %>% select(game_id, team_id = away_team_id)
) %>% 
  left_join(games)%>% 
  group_by(season_id, competition_id, team_id) %>% 
  summarize(
    n_matches = n(),
    across(duration, sum)
  ) %>% 
  ungroup() %>% 
  arrange(desc(duration)) %>% 
  mutate(
    inv_prop_matches = 38 / n_matches
  )

player_games_aug <- player_games %>% 
  left_join(teams) %>% 
  left_join(players) %>% 
  left_join(games)

player_pos <- player_games_aug %>%
  # filter(player_id == 97710) %>% 
  group_by(
    season_id,
    competition_id,
    player_id,
    player_name,
    pos = starting_position_name
  ) %>%
  summarize(position_mp = sum(minutes_played, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(position_mp > 0L) %>% 
  group_by(season_id, competition_id, player_id, player_name) %>% 
  mutate(
    total_mp = sum(position_mp), 
    position_prop = coalesce(position_mp / total_mp, 0)
  ) %>% 
  ungroup() %>% 
  left_join(pos_info)

player_teams <- player_games_aug %>% 
  group_by(
    season_id,
    competition_id,
    team_id,
    team_name,
    player_id,
    player_name
  ) %>%
  summarize(team_mp = sum(minutes_played, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(team_mp > 0L) %>% 
  group_by(season_id, competition_id, player_id, player_name) %>% 
  mutate(
    total_mp = sum(team_mp), 
    team_prop = coalesce(team_mp / total_mp, 0)
  ) %>% 
  ungroup()

player_pos_filt <- player_pos %>% 
  # filter(pos != 'Sub') %>% 
  group_by(season_id, competition_id, player_id, player_name) %>% 
  slice_max(position_prop, with_ties = FALSE) %>% 
  ungroup() # %>% 
# select(-position_prop)

player_teams_filt <- player_teams %>% 
  group_by(season_id, competition_id, player_id, player_name) %>% 
  slice_max(team_prop, with_ties = FALSE) %>% 
  ungroup() # %>% 
# select(-team_prop)

players_filt <- full_join(
  player_pos_filt,
  player_teams_filt %>% select(-total_mp)
)

.add_poss_and_turn_cols <- function(df) {
  df <- df %>% 
    # filter(game_id == 1549726) %>% 
    arrange(season_id, competition_id, game_id, period_id, action_id) %>% 
    mutate(
      across(action_id, list(lag1 = lag)),
      poss_change = case_when(
        # for some reason i'm having to create this column explicitly, instead of just doing lag(action_id)
        is.na(action_id_lag1) ~ TRUE, # for first record in data set
        season_id != lag(season_id) ~ TRUE,
        competition_id != lag(competition_id) ~ TRUE,
        team_id != lag(team_id) ~TRUE, 
        game_id != lag(game_id) ~ TRUE,
        period_id != lag(period_id) ~ TRUE,
        team_id != lag(team_id) ~ TRUE, 
        TRUE ~ FALSE
      ),
      turnover = case_when(
        lead(type_name) %in% c('throw_in', 'freekick_crossed', 'freekick_short', 'corner_crossed', 'corner_short', 'non_action', 'goalkick', 'shot_penalty', 'shot_freekick') ~ FALSE,
        type_name == 'interception' ~ TRUE,
        # type_name == 'clearance' & !lag(poss_change) ~ TRUE,
        # type_name == 'clearance' ~ TRUE,
        type_name == 'bad_touch' & lead(poss_change) ~ TRUE,
        type_name == 'tackle' & result_name == 'success' & lead(poss_change) ~ TRUE,
        type_name %in% c('pass', 'cross', 'throw_in', 'freekick_crossed', 'freekick_short', 'corner_crossed', 'corner_short', 'goalkick') & result_name == 'fail' & lead(poss_change) & lead(type_name) != 'interception' ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>% 
    select(-action_id_lag1) %>% 
    relocate(poss_change, matches('lag1$'))
  
  poss_ids <- df %>% 
    filter(poss_change) %>% 
    group_by(season_id, competition_id, game_id) %>% 
    mutate(idx_inter = row_number()) %>% 
    ungroup() %>%
    select(
      season_id,
      competition_id,
      game_id,
      period_id,
      poss_start_action_id = action_id,
      idx_inter,
      poss_start_time = time
    )
  
  turnovers <- df %>% 
    filter(turnover) %>% 
    mutate(
      turnover_player_id = case_when(
        type_name == 'interception' ~ lag(player_id),
        type_name == 'tackle' & result_name == 'success' ~ lag(player_id),
        TRUE ~ player_id
      ),
      recover_player_id = case_when(
        type_name == 'interception' ~ player_id,
        type_name == 'tackle' & result_name == 'success' ~ player_id,
        TRUE ~ lead(player_id)
      )
    ) %>% 
    select(
      season_id,
      competition_id,
      game_id,
      period_id,
      action_id,
      time,
      turnover_player_id,
      recover_player_id
    )
  
  df <- df %>% 
    left_join(
      poss_ids %>% 
        mutate(
          action_id = poss_start_action_id,
          time = poss_start_time
        )
    ) %>% 
    left_join(
      turnovers %>% mutate(last_turn_time = time)
    ) %>%
    fill(
      idx_inter,
      poss_start_action_id,
      poss_start_time,
      last_turn_time
    ) %>%
    mutate(
      across(last_turn_time, ~ifelse(idx_inter == 1, NA_real_, .x))
    ) %>% 
    group_by(season_id, competition_id, game_id, idx_inter) %>% 
    mutate(
      idx_intra = row_number()
    ) %>% 
    ungroup()
  
  poss_ends <- df %>% 
    group_by(season_id, competition_id, game_id, idx_inter) %>%
    slice_max(idx_intra, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(
      season_id,
      competition_id,
      game_id,
      period_id,
      poss_end_action_id = action_id,
      poss_end_time = time,
      idx_inter,
      poss_n_actions = idx_intra
    )
  
  poss_gaps <- poss_ids %>% 
    left_join(
      poss_ends
    ) %>% 
    left_join(
      poss_ends %>% 
        mutate(
          across(idx_inter, ~.x + 1L)
        ) %>% 
        rename(
          last_poss_end_action_id = poss_end_action_id,
          last_poss_end_time = poss_end_time, 
          last_poss_n_actions = poss_n_actions
        )
    ) %>% 
    mutate(
      time_between_poss = poss_start_time - last_poss_end_time
    )
  
  df %>% 
    left_join(poss_gaps) %>%
    fill(
      poss_n_actions, 
      last_poss_n_actions,
      poss_start_action_id,
      poss_end_action_id,
      poss_start_time,
      poss_end_time,
      last_poss_end_time,
      last_poss_end_action_id,
      time_between_poss,
      turnover_player_id,
      recover_player_id
    ) %>% 
    mutate(
      across(c(last_turn_time, poss_start_time), ~ifelse(.x > time, NA_real_, .x)),
      time_since_poss_start = time - poss_start_time,
      time_since_last_turn = time - last_turn_time
    ) %>%
    arrange(season_id, competition_id, game_id, period_id, action_id) %>% 
    # select(-c(poss_change, turnover)) %>%
    relocate(
      matches('^idx'), 
      poss_n_actions, 
      last_poss_n_actions,
      poss_start_action_id,
      poss_end_action_id,
      poss_start_time,
      poss_end_time,
      last_poss_end_time,
      last_poss_end_action_id,
      time_between_poss,
      time_since_poss_start,
      time_since_last_turn,
      turnover_player_id,
      recover_player_id
    )
}

all_actions <- actions %>% 
  # filter(game_id == 1549726) %>% 
  rename(time = time_seconds) %>% 
  left_join(actiontypes) %>% 
  left_join(bodyparts) %>% 
  left_join(players) %>% 
  left_join(teams) %>%
  left_join(results) %>% 
  left_join(
    games %>%
      select(
        game_date,
        competition_id,
        season_id,
        game_id,
        away_team_id,
        home_team_id,
        away_score,
        home_score,
        duration
      )
  ) %>% 
  # left_join(player_pos_filt %>% select(-c(team_name, player_name))) %>% 
  .add_poss_and_turn_cols() %>% 
  left_join(players %>% select(recover_player_id = player_id, recover_player_name = player_name)) %>% 
  left_join(players %>% select(turnover_player_id = player_id, turnover_player_name = player_name)) %>% 
  left_join(teams %>% rename_all(~sprintf('home_%s', .x))) %>% 
  left_join(teams %>% rename_all(~sprintf('away_%s', .x))) %>% 
  relocate(game_date, game_id, player_name, team_name, type_name, result_name)

f_write <- function(x, name = deparse(substitute(x))) {
  path <- file.path(dir_proj, sprintf('%s.parquet', name))
  arrow::write_parquet(x, path)
}

f_write(all_actions)
f_write(players_filt)
f_write(games_by_team)
