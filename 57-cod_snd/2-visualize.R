
library(tidyverse)
library(qs)
library(scales)
library(extrafont)
library(ggtext)

blackish_background <- '#1c1c1c'
gray_points <- '#4d4d4d'
gray_text <- '#999999'

font <- 'Titillium Web' ## Karla
extrafont::loadfonts(quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = font),
  title = element_text(size = 20, color = 'white'),
  plot.title = element_text(face = 'bold', size = 20, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = element_text(size = 16, color = '#f1f1f1'),
  axis.text = element_text(color = 'white', size = 14),
  axis.title = element_text(size = 14, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = gray_points),
  panel.grid.minor = element_line(color = gray_points),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  strip.text = element_text(color = 'white', size = 16, face = 'bold'),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = blackish_background, color = blackish_background),
  plot.caption = element_text(size = 12, color = 'white', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 12, color = 'white', hjust = 0),
  plot.tag.position = c(0.01, 0.01),
  panel.background = element_rect(fill = blackish_background, color = blackish_background)
)
# update_geom_defaults('text', list(colour = 'white', font = font))

game_mapping <- c(
  '2022' = 'Vanguard',
  '2021' = 'Cold War',
  '2020' = 'MW'
)

rounds <- qs::qread(file.path(dir_proj, 'cod_rounds.qs')) |> 
  mutate(
    game = game_mapping[as.character(year)],
    .before = 1
  )

## offensive win % in "neutral" rounds (1 and 2) ----
calculate_offensive_round_win_prop <- function(df) {
  df |> 
    count(year, game, map, is_offense, win_round) |> 
    group_by(map, is_offense) |> 
    mutate(total = sum(n), prop = n / total) |> 
    ungroup() |>
    filter(win_round, is_offense) |>
    select(
      year,
      game,
      map,
      rounds_won = n,
      total_rounds = total,
      win_round_prop = prop
    ) |>
    arrange(desc(win_round_prop))
}

devtools::source_url('https://raw.githubusercontent.com/dgrtwo/splittestr/master/R/vectorized-prop-test.R')
summarize_offensive_round_win_prop <- function(.round) {
  
  specified_round <- rounds |> 
    filter(round == .round) |> 
    calculate_offensive_round_win_prop()
  
  other_rounds <- rounds |> 
    filter(round != .round) |> 
    calculate_offensive_round_win_prop()
  
  specified_round |> 
    inner_join(
      other_rounds |> rename_with(~sprintf('other_%s', .x), -c(year, game, map)),
      by = c('year', 'game', 'map')
    ) |> 
    mutate(round = .round, .after = 'map') |> 
    arrange(desc(win_round_prop)) |> 
    mutate(
      prop_test = vectorized_prop_test(rounds_won, total_rounds, other_rounds_won, other_total_rounds),
      p_value = prop_test$p.value
    ) |> 
    arrange(p_value)
}

offensive_win_r1_prop <- summarize_offensive_round_win_prop(1)
offensive_win_r2_prop <- summarize_offensive_round_win_prop(2)

## comparing round playouts vs expected if each side has a pre-series win expectation of 50% ----
prior_clinching_round_win_prop <- rounds |> 
  filter(round == (n_rounds - 1L)) |> 
  count(n_rounds, win_series, won_prior_round = win_round) |> 
  mutate(
    across(win_series, ~ifelse(.x, 'win', 'loss'))
  ) |> 
  pivot_wider(
    names_from = win_series,
    values_from = n,
    values_fill = 0L
  ) |> 
  filter(won_prior_round) |> 
  select(-won_prior_round) |> 
  mutate(
    win_series_prop = win / (win + loss),
    e_win_series_prop = (6L - 1L) / (n_rounds - 1L)
  )

n_rounds_max <- 11
n_sims <- 10000
set.seed(42)
w <- sample(c(0, 1), size = n_rounds_max * n_sims, replace = TRUE)
m <- matrix(w, nrow = n_sims, ncol = n_rounds_max)
# data.frame(x)
df <- as_tibble(m)
names(df) <- sprintf('%d', 1:n_rounds_max)
df$i <- 1:nrow(df)

sim_rounds <- tibble(
  i = rep(1:n_sims, each = n_rounds_max),
  r = rep(1:n_sims, times = n_rounds_max),
  w = w
) |> 
  group_by(i) |> 
  mutate(
    cumu_w = cumsum(w),
    cumu_l = cumsum(w == 0)
  ) |> 
  ungroup() |> 
  mutate(
    cumu_wl_max = ifelse(cumu_w > cumu_l, cumu_w, cumu_l)
  ) |> 
  filter(cumu_wl_max <= 6)

sim_rounds <- df |> 
  pivot_longer(
    -i,
    names_to = 'r',
    values_to = 'w'
  ) |> 
  mutate(
    across(r, as.integer)
  ) |> 
  group_by(i) |> 
  mutate(
    cumu_w = cumsum(w),
    cumu_l = cumsum(w == 0)
  ) |> 
  ungroup() |> 
  mutate(
    cumu_wl_max = ifelse(cumu_w > cumu_l, cumu_w, cumu_l)
  ) |> 
  filter(cumu_wl_max <= 6)

sim_rounds_to_drop <- anti_join(
  sim_rounds |> 
    filter(cumu_wl_max == 6L),
  sim_rounds |> 
    filter(cumu_wl_max == 6L) |> 
    group_by(i) |> 
    slice_min(r, n = 1) |> 
    ungroup()
)

sim_rounds <- sim_rounds |> 
  anti_join(
    sim_rounds_to_drop
  )

# sim_rounds |> 
#   group_by(i) |> 
#   summarize(
#     cumu_w = max(cumu_w),
#     cumu_l = max(cumu_l),
#     ws = paste0(w, collapse = '')
#   ) |> 
#   unite(
#     record, cumu_w, cumu_l, sep = '-'
#   ) |> 
#   count(record, ws, sort = TRUE) |> 
#   mutate(prop = n / sum(n))

## always from offensive perspective
e_records <- sim_rounds |> 
  inner_join(
    sim_rounds |> 
      filter(cumu_wl_max == 6) |> 
      mutate(
        zeros_win = cumu_l == cumu_wl_max
      ) |> 
      select(i, zeros_win)
  ) |> 
  mutate(
    across(w, ~ifelse(zeros_win, abs(1 - .x), .x))
  ) |> 
  group_by(i) |> 
  summarize(
    cumu_w = max(cumu_w),
    cumu_l = max(cumu_l),
    ws = paste0(w, collapse = '')
  ) |> 
  ungroup() |> 
  mutate(
    wins = ifelse(cumu_w == 6, cumu_w, cumu_l),
    losses = ifelse(cumu_w == 6, cumu_l, cumu_w)
  ) |> 
  unite(
    record, wins, losses, sep = '-'
  ) |> 
  count(record, ws, sort = TRUE) |> 
  mutate(prop = n / sum(n))

rounds |> 
  filter(win_series) |> 
  mutate(across(win_round, as.integer)) |> 
  group_by(year, event, series) |> 
  summarize(
    wins = max(cumu_w),
    losses = max(cumu_l),
    ws = paste0(win_round, collapse = '')
  ) |> 
  ungroup() |> 
  unite(
    record, wins, losses, sep = '-'
  ) |> 
  count(record, ws, sort = TRUE) |> 
  mutate(prop = n / sum(n))

## when are b2b rounds most commmon? ----
## probably should only lookup to round 6

## use round state instead of actual round?

## match win %, given round state ---
summarize_rounds <- function(rounds, ...) {
  rounds |> 
    group_by(pre_cumu_w, pre_cumu_l, ...) |> 
    summarize(
      n = n(),
      across(c(win_round, win_match), sum)
    ) |> 
    ungroup() |> 
    mutate(
      win_round_prop = win_round / n,
      win_match_prop = win_match / n
    )
}

common_heatmap_layers <- function(...) {
  list(
    ...,
    guides(
      fill = 'none'
    ),
    scale_x_continuous(
      labels = 0:5,
      breaks = seq(.5, 5.5, by = 1),
      expand = c(0, 0)
    ),
    scale_y_continuous(
      labels = 0:5,
      breaks = seq(.5, 5.5, by = 1),
      expand = c(0, 0),
      sec.axis = sec_axis(
        trans = I, 
        name = ' ', 
        breaks = seq(0.5, 5.5, by = 1), 
        labels = rep('', 6)
      )
    ),
    theme(
      panel.grid.major = element_blank(),
      axis.title = element_text(hjust = 0.5),
      plot.title = ggtext::element_markdown(hjust = 0.5),
      axis.text = element_text(size = 16, face = 'bold')
    ),
    labs(
      caption = 'CDL SnD Major and Weekly matches, 2020 - present',
      tag = '**Viz**: @TonyElHabr | **Data**: @IOUTurtle',
      x = "Offensive Team's # of Pre-Round Wins",
      y = "Defensive Team's # of Pre-Round Wins"
    )
  )
}

n_rounds_side <- rounds |> summarize_rounds(is_offense)
p_match_win_prop <- n_rounds_side |> 
  mutate(
    facet_label = ifelse(is_offense, 'Offense Match Win %', 'Defense Match Win %')
  ) |> 
  ggplot() +
  common_heatmap_layers() +
  geom_rect(
    color = blackish_background,
    alpha = 0.8,
    aes(
      fill = win_match_prop,
      xmin = pre_cumu_w, 
      ymin = pre_cumu_l,
      xmax = pre_cumu_w + 1,
      ymax = pre_cumu_l + 1
    )
  ) +
  geom_text(
    family = font,
    color = 'white',
    size = 12 / .pt,
    fontface = 'bold',
    aes(
      x = pre_cumu_w + 0.5, 
      y = pre_cumu_l + 0.5,
      label = sprintf(
        '%s\n(%s/%s)', 
        scales::percent(win_match_prop, accuracy = 1), 
        scales::comma(win_match), scales::comma(n)
      )
    )
  ) +
  labs(
    title = 'Which side goes on to win the match?'
  ) +
  theme(
    strip.text = element_text(color = 'white', size = 16, face = 'bold')
  ) +
  facet_wrap(~facet_label, scales = 'fixed')
p_match_win_prop

ggsave(
  p_match_win_prop,
  filename = file.path(dir_proj, 'match_win_prop.png'),
  width = 14,
  height = 7
)

## round win %, given round state ---
n_rounds_side <- rounds |> summarize_rounds(is_offense)
offense_round_win_prop <- n_rounds_side |> 
  filter(is_offense) |> 
  mutate(
    diff_win_round_prop = win_round_prop - 0.5
  )

max_diff_win_round_prop <- max(abs(offense_round_win_prop$diff_win_round_prop))
p_offensive_round_win_prop <- offense_round_win_prop |> 
  ggplot() +
  common_heatmap_layers() +
  geom_rect(
    color = blackish_background,
    aes(
      fill = win_round_prop,
      xmin = pre_cumu_w, 
      ymin = pre_cumu_l,
      xmax = pre_cumu_w + 1,
      ymax = pre_cumu_l + 1
    )
  ) +
  geom_text(
    family = font,
    color = blackish_background,
    size = 12 / .pt,
    fontface = 'bold',
    aes(
      x = pre_cumu_w + 0.5, 
      y = pre_cumu_l + 0.5,
      label = sprintf('%s\n(%s/%s)', scales::percent(win_round_prop, accuracy = 1), scales::comma(win_round), scales::comma(n))
    )
  ) +
  scale_fill_gradient2(
    low = '#af8dc3',
    high = '#7fbf7b',
    mid = 'white',
    midpoint = 0.5,
    limits = c(0.5 - max_diff_win_round_prop, 0.5 + max_diff_win_round_prop)
  ) +
  labs(
    title = 'Offensive Round Win %'
  )
p_offensive_round_win_prop

ggsave(
  p_offensive_round_win_prop,
  filename = file.path(dir_proj, 'offensive_round_win_prop.png'),
  width = 8,
  height = 8
)

## actual match win % vs. expected match win %, given round state ---
e_match_win_prop <- crossing(
  pre_cumu_w = 0:5,
  pre_cumu_l = 0:5
) |> 
  mutate(
    n_remain_max = 11 - (pre_cumu_w + pre_cumu_l),
    n_remain_w = 6 - pre_cumu_w,
    e_win_match_prop = map2_dbl(n_remain_w, n_remain_max, ~sum(dbinom(..1:..2, ..2, 0.5)))
  )

e_offensive_match_win_prop <- offense_round_win_prop |> 
  inner_join(
    e_match_win_prop,
    by = c('pre_cumu_w', 'pre_cumu_l')
  ) |> 
  mutate(
    diff_win_match_prop = win_match_prop - e_win_match_prop
  )

max_diff_win_match_prop <- max(abs(e_offensive_match_win_prop$diff_win_match_prop))
p_e_offensive_match_win_prop <- e_offensive_match_win_prop |> 
  ggplot() +
  common_heatmap_layers() +
  geom_rect(
    color = blackish_background,
    alpha = 0.8,
    aes(
      fill = diff_win_match_prop,
      xmin = pre_cumu_w, 
      ymin = pre_cumu_l,
      xmax = pre_cumu_w + 1,
      ymax = pre_cumu_l + 1
    )
  ) +
  geom_text(
    family = font,
    # color = 'white',
    color = blackish_background,
    size = 12 / .pt,
    fontface = 'bold',
    aes(
      x = pre_cumu_w + 0.5, 
      y = pre_cumu_l + 0.5,
      label = sprintf('%+.1f%%\n(%s)', round(100 * diff_win_match_prop, 1), scales::comma(n))
    )
  ) +
  scale_fill_gradient2(
    low = '#d8b365',
    high = '#5ab4ac',
    # low = '#e9a3c9',
    # high = '#a1d76a',
    mid = 'white',
    midpoint = 0,
    limits = c(0 - max_diff_win_match_prop, 0 + max_diff_win_match_prop)
  ) +
  labs(
    title = 'Actual Offensive Match Win % - Expected Offensive Match Win %'
  )
p_e_offensive_match_win_prop

ggsave(
  p_e_offensive_match_win_prop,
  filename = file.path(dir_proj, 'e_offensive_match_win_prop.png'),
  width = 8,
  height = 8
)

## todo: streaks ----
n_rounds_won_prior_round <- rounds |> summarize_rounds(won_prior_round)
n_rounds_won_prior_round |> 
  filter(!is.na(won_prior_round)) |> 
  # filter(pre_cumu_w == pre_cumu_l) |> 
  transmute(
    pre_cumu_w,
    pre_cumu_l,
    across(won_prior_round, ~ifelse(.x, 'won_prior_round', 'lost_prior_round')),
    win_round
  ) |> 
  pivot_wider(
    names_from = won_prior_round,
    values_from = win_round,
    values_fill = 0L
  ) |> 
  mutate(
    win_round = won_prior_round / (lost_prior_round + won_prior_round)
  ) |> 
  arrange(desc(win_round))

rounds |> 
  filter(!is.na(won_prior_round)) |> 
  filter(win_match) |> 
  # filter(n_rounds >= 10) |> 
  filter(round == 11) |> 
  count(n_rounds, win_match, win_round, won_prior_round, is_offense) |> 
  group_by(n_rounds, won_prior_round) |> 
  mutate(
    prop = n / sum(n)
  ) |> 
  ungroup()

## actual outcomes vs expected ----
w_probs <- dbinom(6, 6:11, 0.5)
l_probs <- dbinom(0:5, 6:11, 0.5)
all_probs <- sum(w_probs, l_probs)
match_round_probs <- tibble(
  n_rounds = rep(6L:11L, times = 2),
  win_match = rep(c(TRUE, FALSE), each = 6),
  e_prop = c(w_probs, l_probs) / all_probs
)

rounds |> 
  count(win_match, n_rounds, sort = TRUE) |> 
  mutate(
    prop = n / sum(n)
  ) |> 
  inner_join(
    match_round_probs,
    by = c('n_rounds', 'win_match')
  ) |> 
  mutate(
    diff_prop = prop - e_prop
  ) |> 
  filter(win_match) |>
  arrange(desc(abs(diff_prop)))

rounds |> 
  count(win_match, n_rounds, is_offense_last_round, sort = TRUE) |> 
  mutate(
    prop = n / sum(n)
  ) |> 
  inner_join(
    match_round_probs |> mutate(across(e_prop, ~0.5 * .x)),
    by = c('n_rounds', 'win_match')
  ) |> 
  mutate(
    diff_prop = prop - e_prop
  ) |> 
  filter(win_match) |>
  arrange(desc(abs(diff_prop)))

## most frequent round playout ----
round_seqs <- rounds |> 
  transmute(year, sheet, match, team, round, across(win_round, as.integer)) |> 
  pivot_wider(
    names_from = round,
    values_from = win_round,
    names_prefix = 'r'
  ) |> 
  group_by(across(starts_with('r'))) |> 
  count(sort = TRUE) |> 
  ungroup()
round_seqs
total_matches <- sum(round_seqs$n)
37 / total_matches
dbinom(6, 6, 0.5)
round_seqs |> 
  filter(!is.na(r11)) |> 
  select(r9:n) |> 
  group_by(across(starts_with('r'))) |> 
  summarize(
    across(n, sum)
  ) |> 
  ungroup() |> 
  filter(r9 == 1L) |> 
  mutate(prop = n / sum(n))

rounds |> 
  filter(pre_cumu_w == 5, pre_cumu_l == 3) |> 
  count(n_rounds, win_match) |> 
  mutate(prop = n / sum(n))
dbinom(3, 3, 0.5)

rounds |> 
  filter(pre_cumu_w == 5, pre_cumu_l == 3) |> 
  count(n_rounds, win_match, is_offense) |> 
  group_by(is_offense) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup()
dbinom(3, 3, 0.5)
prop.test(
  
)

rounds |> 
  filter(pre_cumu_w == 5, pre_cumu_l == 4) |> 
  count(n_rounds, win_match) |> 
  mutate(prop = n / sum(n))
dbinom(2, 2, 0.5)
