
library(tidyverse)
library(arrow)
library(ebbr)

dir_proj <- '46-euro_fouls'
f_import <- function(name) {
  path <- file.path(dir_proj, sprintf('%s.parquet', name))
  res <- path %>% arrow::read_parquet()
  assign(value = res, x = name, envir = .GlobalEnv)
}

c(
  'all_actions',
  'players_filt',
  'games_by_team'
) %>% 
  walk(f_import)


all_actions <- all_actions %>% 
  mutate(
    half = case_when(
      team_id == home_team_id & start_x >= 1 * 105 / 2 ~ 'attacking',
      team_id == away_team_id & start_x <= 1 * 105 / 2 ~ 'attacking',
      TRUE ~ 'defensive'
    ),
    third = case_when(
      team_id == home_team_id & start_x >= 2 * 105 / 3 ~ 'attacking',
      team_id == home_team_id & start_x >= 1 * 105 / 3 ~ 'middle',
      team_id == away_team_id & start_x <= 1 * 105 / 3 ~ 'attacking',
      team_id == away_team_id & start_x <= 2 * 105 / 3 ~ 'middle',
      TRUE ~ 'defensive'
    )
  )

fouls <- all_actions %>% filter(type_name == 'foul')
turnovers <- all_actions %>% filter(turnover)

euro_fouls <- all_actions %>%
  filter(
    type_name == 'foul',
    time_between_poss <= 5,
    time_since_poss_start <= 3
  )
euro_fouls

rage_quits <- all_actions %>% 
  filter(
    type_name == 'foul',
    # time_between_poss <= 5,
    # time_since_poss_start <= 3,
    turnover_player_id == player_id,
    time_since_last_turn <= 7
  ) %>% 
  arrange(desc(game_date))
rage_quits

do_count_euro_fouls <- function(cols) {
  col_syms <- syms(cols)
  n_euro_fouls_by_x <- euro_fouls %>%
    count(
      competition_id,
      season_id,
      !!!col_syms,
      name = 'n_euro_fouls',
      sort = TRUE
    )
  
  n_fouls_by_x <- fouls %>%
    count(
      competition_id,
      season_id,
      !!!col_syms,
      name = 'total_fouls',
      sort = TRUE
    )
  
  n_euro_fouls_by_x %>%
    left_join(
      n_fouls_by_x
    ) %>% 
    mutate(
      prop_fouls = n_euro_fouls / total_fouls
    )
}

player_pos_filt %>% count(season_id, player_id, player_name, sort = TRUE)

euro_fouls_by_player <- do_count_euro_fouls(
  c('player_id')
) %>% 
  left_join(players_filt) %>%
  left_join(games_by_team)

# normalize <- function(x) {
#   (x - mean(x)) / sd(x)
# }

df <- euro_fouls_by_player %>% 
  transmute(
    season_id,
    player_id, 
    player_name,
    pos_11,
    pos_grp,
    n_euro_fouls,
    total_fouls,
    prop_fouls,
    total_mp_scaled = total_mp * inv_prop_matches,
    prop_fouls_scaled = prop_fouls * inv_prop_matches,
    n_euro_fouls_scaled = n_euro_fouls * inv_prop_matches,
    total_fouls_scaled = total_fouls * inv_prop_matches
  )
df

df_adj <- df %>% 
  filter(pos_11 != 'GK') %>% 
  add_ebb_estimate(
    n_euro_fouls,
    total_fouls,
    method = 'mle',
    prior_subset = total_fouls >= 10
  ) %>% 
  group_by(season_id) %>% 
  mutate(
    prnk = percent_rank(.fitted)
  ) %>% 
  ungroup() %>% 
  relocate(prnk)
df_adj %>% filter(prnk == 1)

df_adj2 <- df_adj %>% 
  group_by(player_id, player_name) %>% 
  summarize(
    n = n(),
    across(.fitted, sum)
  ) %>% 
  ungroup() %>% 
  filter(n >= 2) %>% 
  mutate(avg = .fitted / n) %>% 
  arrange(desc(avg))

df_adj %>% 
  filter(total_fouls_scaled >= 10) %>% 
  mutate(
    prnk1 = percent_rank(.fitted),
    prnk2 = percent_rank(-total_fouls_scaled),
    prnk = prnk1 * prnk2
  ) %>% 
  arrange(desc(prnk))

df_adj %>% 
  filter(total_fouls_scaled >= 10)
  mutate(
    z  = .fitted * (1 / total_fouls_scaled)
  ) %>% 
  arrange(desc(z))
coady <- df_adj %>% filter(player_name == 'Conor Coady')
silva <- df_adj %>% filter(player_name == 'Thiago Silva')

xi <- mean(df_adj$.fitted)
xi

fit_poly <- lm(total_fouls_scaled ~ .fitted + I(.fitted^2), df_adj)
fit_poly
df_adj %>% 
  filter(pos_grp %in% c('D')) %>% 
  ggplot() +
  aes(x = .fitted, y = total_fouls_scaled) +
  geom_point(aes(alpha = total_mp_scaled, size = total_mp_scaled)) +
  scale_size_area(max_size = 3) +
  # geom_smooth(
  #   method = 'lm',
  #   formula = formula(y ~ x + I(x^2))
  # )
  geom_function(
    fun = function(x) 110-351.1*x + 275.3*(x^2)
  ) +
  geom_vline(
    aes(xintercept = mean(.fitted))
  ) +
  geom_hline(
    aes(yintercept = mean(total_fouls_scaled))
  ) +
  geom_point(
    data = coady,
    size = 3,
    color = 'red'
  ) +
  geom_point(
    data = silva,
    size = 3,
    color = 'magenta'
  )

  