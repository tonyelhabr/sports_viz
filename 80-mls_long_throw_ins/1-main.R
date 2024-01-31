library(dplyr)

setwd('../socceraction-streamlined')
source(file.path('R', 'helpers.R'))
games <- import_parquet('games')
teams <- import_parquet('teams')

x <- import_parquet(add_suffix('x', suffix = '')) |> 
  select(-matches('_a[1-2]$'))

xy <- import_xy(suffix = '', games = games)
x
xy |> filter(type_shot_penalty_a0 == 1) |> select(start_x_a0, start_y_a0)
xy |> filter(type_shot_a0 == 1) |> pull(start_x_a0) |> range()

long_throw_ins <- xy |> 
  filter(type_throw_in_a0 == 1) |> 
  count(
    team_id, 
    final_third_throw_in = start_x_a0 >= (2 * 105 / 3),
    thrown_into_box = (end_x_a0 >= (105 - 16.5)) & (end_y_a0 >= (34 - 20.15) & end_y_a0 <= (34 + 20.15))
  )

n_games <- xy |> 
  distinct(team_id, game_id) |> 
  count(team_id, name = 'n_games')

prop_thrown_into_box <- long_throw_ins |> 
  filter(
    final_third_throw_in 
  ) |> 
  group_by(team_id) |> 
  mutate(
    total = sum(n),
    prop_thrown_into_box = n / sum(n)
  ) |> 
  ungroup() |> 
  filter(thrown_into_box) |> 
  select(
    team_id,
    prop_thrown_into_box,
    n_final_third_throw_ins = total,
    n_final_third_throw_ins_into_box = n
  )

df <- prop_thrown_into_box |> 
  inner_join(n_games, by = join_by(team_id)) |> 
  inner_join(teams, by = join_by(team_id)) |> 
  arrange(desc(prop_thrown_into_box)) |> 
  mutate(
    n_final_third_throw_ins_per_game = n_final_third_throw_ins / n_games
  )
df

library(gt)
library(gtExtras)


?gtExtras::gt_double_table
prop_thrown_into_box |> 
  select(
    team_name,
    prop_thrown_into_box
  ) |> 
  slice_max(prop_thrown_into_box, n = 10) |> 
  gt::gt() |>
  gt::cols_label(
    'team_name' = 'Team',
    'prop_thrown_into_box' = gt::md('% of Final Third</br>Throw-ins into the Box')
  ) |> 
  gt::fmt_percent(
    columns = prop_thrown_into_box,
    decimals = 0
  ) |> 
  gt::tab_options(
    table.font.names = c('Roboto')
  )


