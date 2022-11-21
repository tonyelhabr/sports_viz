library(tidyverse)
library(qs)
library(ebbr)
library(yardstick)
library(broom)
dir_proj <- '53x-duels'

source(file.path(dir_proj, 'helpers.R'))
init_duels <- file.path(dir_proj, 'duels.qs') |> 
  qs::qread()

keepers <- init_duels |> 
  select(player_id, player_name, x, y) |> 
  group_by(player_id, player_name) |> 
  summarize(across(x, mean)) |> 
  ungroup() |> 
  arrange(desc(x)) |> 
  filter(x < 18) |> 
  distinct(player_id)
keepers

duels <- init_duels |> 
  anti_join(
    keepers
  )

teams <- duels |> 
  count(season_id, player_id, player_name, team_name) |> 
  group_by(season_id, player_id) |> 
  slice_max(n, n = 1, with_ties = FALSE) |> 
  ungroup()

count_duels <- function(df) {
  df |> 
    count(season_id, player_name, is_successful) |> 
    group_by(season_id, player_name) |> 
    mutate(
      across(is_successful, ~ifelse(.x, 'won', 'lost')),
      prop = n / sum(n)
    ) |> 
    ungroup() |> 
    pivot_wider(
      names_from = is_successful,
      values_from = c(n, prop)
    ) |> 
    mutate(n = coalesce(n_lost, 0L) + coalesce(n_won, 0L))
}

total_split <- duels |> count_duels()
splits <- duels |> 
  count(is_aerial, season_id, player_id, player_name, is_successful) |> 
  group_by(is_aerial, season_id, player_id, player_name) |> 
  mutate(
    across(is_successful, ~ifelse(.x, 'won', 'lost')),
    prop = n / sum(n)
  ) |> 
  ungroup() |> 
  pivot_wider(
    names_from = is_successful,
    values_from = c(n, prop)
  ) |> 
  drop_na(n_lost, n_won) |> 
  group_by(is_aerial) |> 
  # mutate(n = coalesce(n_lost, 0L) + coalesce(n_won, 0L)) |> 
  mutate(n = n_lost + n_won) |> 
  ungroup()

q25s <- splits |>
  group_by(is_aerial) |> 
  summarize(
    q = round(quantile(n, 0.25))
  )
q25s
ground_splits <- splits |> filter(!is_aerial)
aerial_splits <- splits |> filter(is_aerial)

postprocess_ebbr_split <- function(df, suffix, ...) {
  df |> 
    select(
      season_id, player_id, player_name, n_lost, n_won, prop_lost, prop_won, prop_won_adj = .fitted
    ) |> 
    rename_with(~sprintf('%s_%s', .x, suffix), -c(season_id, player_id, player_name))
}

splits_adj <- inner_join(
  aerial_splits |> 
    ebbr::add_ebb_estimate(n_won, n, prior_subset = n >= 20) |> 
    postprocess_ebbr_split('aerial'),
  ground_splits |> 
    ebbr::add_ebb_estimate(n_won, n, prior_subset = n >= 40) |> 
    postprocess_ebbr_split('ground')
) |> 
  mutate(
    prop_won_diff = prop_won_ground - prop_won_aerial,
    prop_won_adj_diff = prop_won_adj_ground - prop_won_adj_aerial
  ) |> 
  arrange(desc(prop_won_adj_diff))
splits_adj

## model ----
pitch_x <- 105
pitch_y <- 68
goal_x <- pitch_x
goal_y <- pitch_y / 2
shift_xy_cols <- function(df) {
  df |> 
    mutate(
      x = x - pitch_x / 2,
      y = y - pitch_y / 2
    )
}
revert_xy_cols <- function(df) {
  df |> 
    mutate(
      x = x + pitch_x / 2,
      y = y + pitch_y / 2
    )
}
mirror_y_col <- function(df) {
  df |> mutate(across(y, ~ifelse(.x > goal_y, goal_y - (.x - goal_y), .x)))
}
flip_bool_cols <- function(df) {
  df |> mutate(across(c(is_offensive, is_successful), ~ifelse(.x, FALSE, TRUE)))
}
invert_xy_cols <- function(df) {
  df |> 
    mutate(
      across(x, ~scales::rescale(.x, to = c(pitch_x, 0), from = c(0, pitch_x))),
      across(y, ~scales::rescale(.x, to = c(pitch_y, 0), from = c(0, pitch_y)))
    )
}
factor_is_successful_col <- function(df) {
  df |> 
    mutate(
      across(is_successful, ~ifelse(.x, 'yes', 'no') |> factor())
    )
}

fit_model <- function(df) {
  model_df <- bind_rows(
    df,
    df |> mirror_y_col(),
    df |> flip_bool_cols() |> invert_xy_cols(),
    df |> flip_bool_cols() |> mirror_y_col() |> invert_xy_cols()
  ) |>
    factor_is_successful_col() |> 
    shift_xy_cols()
  
  fit <- glm(
    is_successful ~ x + y + x:y + I(x^2) + I(y^2) + I(x^2):I(y^2) + 0,
    data = model_df,
    family = 'binomial'
  )
}

add_xw_col <- function(df, fit) {
  new_df <- df |>
    mirror_y_col() |> 
    factor_is_successful_col() |> 
    shift_xy_cols()
  
  broom::augment(fit, newdata = new_df, type.predict = 'response') |> 
    revert_xy_cols() |> 
    rename(xw = .fitted)
}

do_model <- function(df, option) {
  n_cls <- df |> count(is_successful)
  
  target_prop <- n_cls |> 
    mutate(prop = n / sum(n)) |> 
    filter(is_successful) |>
    pull(prop)
  
  smallest_cls <- n_cls |> slice_min(n, n = 1)
  balanced_df <- bind_rows(
    df |>
      filter(is_successful == smallest_cls$is_successful),
    df |>
      filter(is_successful != smallest_cls$is_successful) |>
      slice_sample(n = smallest_cls$n)
  )
  fit <- balanced_df |> fit_model()
  probs <- duels |> add_xw_col(fit)
  
  prop_at_threshold <- function(.threshold) {
    prop <- probs |> 
      mutate(pred_cls = ifelse(xw > .threshold, 'yes', 'no')) |> 
      count(pred_cls) |> 
      mutate(prop = n / sum(n)) |> 
      filter(pred_cls == 'yes') |>
      pull(prop)
    tibble(
      threshold = .threshold,
      prop = prop
    )
  }
  thresholds <- seq(0.4, 0.6, by = 0.005) |> map_dfr(prop_at_threshold)
  threshold <- thresholds |> 
    slice_min(abs(prop - target_prop), n = 1) |> 
    pull(threshold)
  threshold_diff <- threshold - 0.5

  grid <- crossing(
    x = seq.int(0 + 0.5, goal_x - 0.5),
    y = seq.int(0 + 0.5, goal_y - 0.5)
  )
  
  center_prob <- broom::augment(
    fit,
    newdata = tibble(
      x = goal_x / 2, 
      y = goal_y
    ) |> 
      shift_xy_cols(),
    type.predict = 'response'
  ) |> 
    pull(.fitted) |> 
    magrittr::add(threshold_diff)
  
  probs_grid <- broom::augment_columns(
    fit, 
    newdata = grid |> shift_xy_cols(), 
    type.predict = 'response'
  ) |> 
    revert_xy_cols() |> 
    mutate(across(.fitted, ~.x + threshold_diff))
  
  probs_grid_mirrored <- bind_rows(
    probs_grid,
    probs_grid |> mutate(across(y, ~pitch_y - .x))
  )
  
  p_grid <- probs_grid_mirrored |> 
    ggplot() +
    aes(x = x, y = y) +
    common_gg() +
    geom_tile(
      alpha = 0.5,
      aes(fill = .fitted)
    ) +
    scale_fill_viridis_c(
      name = '',
      option = option,
      begin = 0.1,
      end = 0.9,
      labels = scales::percent_format(accuracy = 1)
      # breaks = breaks # ,
      # labels = c('', 'Lower', '', 'Higher')
    ) +
    theme(
      legend.key.width = unit(0.1, 'npc'),
      legend.key.height = unit(0.01, 'npc')
    )
  
  list(
    fit = fit,
    probs = probs,
    pitch_plot = p_grid,
    threshold = center_prob
  )
}

aerial_res <- duels |> filter(is_aerial) |> do_model(option = 'B')
aerial_res$pitch_plot +
  labs(
    title = 'Probability of winning an aerial duel'
  )
ggsave(
  filename = file.path(dir_proj, 'areal_duel_xw.png'),
  width = 8,
  height = 6
)
ground_res <- duels |> filter(!is_aerial) |> do_model(option = 'G')
ground_res$pitch_plot +
  labs(
    title = 'Probability of winning a ground duel'
  )
ggsave(
  filename = file.path(dir_proj, 'areal_duel_xw.png'),
  width = 8,
  height = 6
)

duels_xw <- bind_rows(aerial_res$probs, ground_res$probs)

## final ----
team_mapping <- xengagement::team_accounts_mapping |> 
  as_tibble() |> 
  select(team_name = team_whoscored, url_logo = url_logo_espn)

xwoe <- inner_join(
  splits_adj |> 
    transmute(
      season_id,
      player_id,
      player_name,
      n_won_aerial,
      n_lost_aerial,
      n_won_ground,
      n_lost_ground,
      n_won = n_won_aerial + n_won_ground, 
      n_lost = n_lost_aerial + n_lost_ground,
      n_ground = n_won_ground + n_lost_ground,
      n_aerial = n_won_aerial + n_lost_aerial,
      n = n_won + n_lost,
      prop_won_aerial,
      prop_won_adj_aerial,
      prop_won_adj_ground
    ),
  duels_xw |> 
    group_by(season_id, player_id, player_name, is_aerial) |> 
    summarize(
      across(xw, mean)
    ) |> 
    ungroup() |>
    mutate(across(is_aerial, ~ifelse(.x, 'aerial', 'ground'))) |> 
    pivot_wider(
      names_from = is_aerial,
      names_prefix = 'xw_',
      values_from = xw
    )
) |> 
  mutate(
    xwoe_aerial = prop_won_adj_aerial - xw_aerial,
    xwoe_ground = prop_won_adj_ground - xw_ground,
    q = case_when(
      xwoe_aerial > 0 & xwoe_ground > 0 ~ 'A',
      xwoe_aerial <= 0 & xwoe_ground > 0 ~ 'B',
      xwoe_aerial <= 0 & xwoe_ground <= 0 ~ 'C',
      xwoe_aerial > 0 & xwoe_ground <= 0 ~ 'D'
    ),
    across(
      matches('^xwoe'),
      list(norm = ~(.x - mean(.x)) / sd(.x))
    ),
    xwoe_z = sqrt(xwoe_aerial_norm^2 + xwoe_ground_norm^2)
  ) |> 
  left_join(teams |> select(season_id, player_id, team_name)) |> 
  inner_join(team_mapping) |> 
  relocate(team_name, .after = 'player_name')

top_players_in_each_q <- xwoe |> 
  group_by(player_name, q) |> 
  summarize(
    n_seasons = n(),
    n = sum(n)
  ) |> 
  ungroup() |> 
  group_by(q) |> 
  slice_max(n_seasons, n = 5) |> 
  slice_max(n, n = 5, with_ties = FALSE) |> 
  ungroup()
top_players_in_each_q

xwoe |> arrange(desc(n))
xwoe |> 
  ggplot() +
  aes(x = xw_aerial, y = n) +
  geom_point()

xwoe |> 
  ggplot() +
  aes(x = xw_ground, y = n) +
  geom_point()

xwoe |> 
  ggplot() +
  aes(
    x = xw_ground,
    y = xw_aerial
  ) +
  geom_point(aes(size = n))

xwoe |> 
  # filter(n > 50) |> 
  count(xwoe_ground > 0)
xwoe |> 
  # filter(n > 50) |> 
  count(xwoe_aerial > 0)

xwoe |> 
  filter(n > 50) |>
  ggplot() +
  aes(
    x = xwoe_ground,
    y = xwoe_aerial
  ) +
  geom_point(aes(size = n))

xwoe |> 
  ggplot() +
  aes(
    x = xwoe_ground_norm,
    y = xwoe_aerial_norm
  ) +
  geom_point(aes(size = n))


xwoe |> 
  filter(n > 50) |>
  filter(season_id == 2022) |> 
  filter(q == 'A')

xwoe |> 
  filter(n > 50) |>
  filter(season_id == 2022) |> 
  filter(q == 'C') |> 
  arrange(desc(xwoe_z))
## 2022 Mason Mount (bad, bad)
## 2021&2 Timo Werner (bad, bad)
## 2022 Tomas Soucek (bad, bad)

## table ----
.gt_theme_538 <- function(data, ...) {
  data %>%
    gt::opt_table_font(
      font = list(
        gt::google_font('Karla'), #<-
        gt::default_fonts()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = 'bottom', color = 'black', weight = gt::px(2) #<-
      ),
      locations = gt::cells_column_labels(
        columns = gt::everything()
      )
    )  %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = 'bottom', color = 'black', weight = gt::px(1)
      ),
      locations = gt::cells_row_groups()
    ) %>%
    gt::tab_options(
      column_labels.background.color = 'white',
      heading.border.bottom.style = 'none',
      table.border.top.width = gt::px(3),
      table.border.top.style = 'none', #transparent
      table.border.bottom.style = 'none',
      column_labels.font.weight = 'normal',
      column_labels.border.top.style = 'none',
      column_labels.border.bottom.width = gt::px(1), #<-
      column_labels.border.bottom.color = 'black',
      row_group.border.top.style = 'none',
      row_group.border.top.color = 'black',
      row_group.border.bottom.width = gt::px(1),
      row_group.border.bottom.color = 'white',
      stub.border.color = 'white',
      stub.border.width = gt::px(0),
      data_row.padding = gt::px(1), # px(3), #<-
      source_notes.font.size = 12,
      source_notes.border.lr.style = 'none',
      table.font.size = 16,
      heading.align = 'left',
      footnotes.font.size = 12,#<-
      footnotes.padding = gt::px(0),#<-
      row_group.font.weight = 'bold',#<-
      ...
    ) |> 
    gt::opt_css(
      "tbody tr:last-child {
    border-bottom: 2px solid #ffffff00;
      }
    
    ",
    add = TRUE
    )
}

.add_gt_bar <- function(gt, col, color) {
  gt %>% 
    gtExtras::gt_plt_bar(
      color = color,
      column = all_of(col),
      scaled = FALSE, 
      width = 20
    )
}

xwoe |> 
  filter(n_ground >= 25) |> 
  slice_min(xw_ground) |> 
  glimpse()

season_xg_cor_color <- '#b2fd89' # '#ff8200'
game_xt_cor_color <- '#f089ed'
# https://twitter.com/etmckinley/status/1461706153117696008?s=20&t=rnsUfmdUyqxnFTl95d23EQ
df <- xwoe |> 
  filter(season_id == 2022) |> 
  # filter(q == 'A') |>
  # slice_max(xwoe_z, n = 10, with_ties = FALSE) |> 
  # filter(n >= 50) |> 
  filter(n_ground >= 25) |> 
  slice_max(xwoe_ground, n = 10, with_ties = FALSE) |> 
  select(
    player_name,
    # team_name,
    url_logo,
    n_ground,
    xw_ground,
    xwoe_ground,
    `xWin - Win %` = prop_won_adj_ground,
    n_aerial,
    prop_won_adj_aerial,
    xw_aerial,
    xwoe_aerial
  )
df |> 
  gt::gt() %>% 
  # gtExtras::gt_theme_538() |> 
  .gt_theme_538() |> 
  gt::cols_label(
    .list = list(
      player_name = 'Player',
      url_logo = ' ',
      n_ground = '#',
      # xw_ground = 'xWin %',
      xwoe_ground = ' ',
      n_aerial = '#',
      prop_won_adj_aerial = 'Win %',
      xw_aerial = 'xWin %',
      xwoe_aerial = gt::html('xWin%<br/>- Win %')
    )
  ) %>% 
  gt::text_transform(
    locations = gt::cells_body(
      'url_logo'
    ),
    fn = function(x) {
      gt::web_image(
        url = x,
        height = 25
      )
    }
  ) %>% 
  gtExtras::gt_plt_bullet(
    column = `xWin - Win %`,
    target = xw_ground,
    colors = c('#b2fd89', 'black'),
    width = 20
  ) |> 
  gt::fmt_number(
    columns = c(
      'xWin - Win %', 'xwoe_ground', 
      'prop_won_adj_aerial', 'xw_aerial', 'xwoe_aerial'
    ),
    decimals = 0,
    scale_by = 100
  ) |> 
  gt::tab_spanner(
    label = 'Ground',
    columns = c('n_ground', 'xwoe_ground', 'xWin - Win %')
  ) -> x
x
x |> 
  gt::cols_move(
    columns = n_ground,
    after = `xWin - Win %`
  )
  gt::tab_spanner(
    label = 'Aerial',
    columns = c('n_aerial', 'prop_won_adj_aerial', 'xw_aerial', 'xwoe_aerial')
  ) %>% 
  gt::tab_header(
    title = gt::md('Who won most ground duels over expected in the 2021/22 EPL season?'),
    subtitle = gt::md('')
  ) %>%
  gt::tab_footnote(
    footnote = gt::md('This is **not** the empirical win %; rather, this win % is adjusted from the raw value with emperical Bayes shrinkage (for the purpose of reducing variance due to small sample size).'),
    locations = gt::cells_column_labels(columns = c('prop_won_adj_aerial'))
  ) |> 
  gt::tab_footnote(
    footnote = gt::md('The expected win % is based on models (for ground and aerial duels separately) that account for field position.'),
    locations = gt::cells_column_labels(columns = c('xw_aerial'))
  ) |> 
  gt::tab_source_note(
    source_note = gt::md('**Data**: Updated through 2022-05-01.')
  )
