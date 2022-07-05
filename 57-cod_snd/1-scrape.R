
library(tidyverse)
library(googlesheets4)
library(magrittr)
library(qs)
library(scales)

dir_proj <- '57-cod_snd'
dir_data <- file.path(dir_proj, 'data')
dir.create(dir_data, showWarnings = FALSE)

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
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = blackish_background, color = blackish_background),
  plot.caption = element_text(size = 12, color = 'white', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 12, color = 'white', hjust = 0),
  plot.tag.position = c(0.01, 0.01),
  panel.background = element_rect(fill = blackish_background, color = blackish_background)
)
# update_geom_defaults('text', list(colour = 'white', font = font))

sheets <- list(
  '2022' = crossing(
    s = c(1, 2, 3, 4),
    matches = c(1, 2, 3, 4)
  ) |> 
    mutate(
      label = sprintf('S%d %s', s, ifelse(matches == 4, 'Major', sprintf('Wk%d', matches)))
    ) |> 
    pull(label) %>%
    .[1:14],
  '2021' = c(
    'Champs', 
    crossing(
      s = c(1, 2, 3, 4, 5),
      matches = c(1, 2, 3, 4)
    ) |> 
      mutate(
        label = sprintf('S%d %s', s, ifelse(matches == 4, 'Major', sprintf('HS%d', matches)))
      ) |> 
      pull(label)
  ),
  '2020' = sprintf(
    '%s_SnD',
    c('CHAMPS', 'LAUNCH', 'LON', 'ATL', 'LA', 'DAL', 'CHI', 'FLA', 'SEA', 'MIN', 'PAR', 'NY', 'LON2', 'TOR')
  )
) |> 
  enframe('year', 'sheet') |> 
  unnest(sheet) |> 
  mutate(
    across(year, as.integer),
    event = sprintf('%s - %s', year, sheet) |> fct_inorder()
  )

read_snd_sheet <- function(year, sheet, overwrite = FALSE) {
  
  path <- file.path(dir_data, sprintf('snd-%s-%s.qs', year, sheet))
  if(file.exists(path) & !overwrite) {
    return(qs::qread(path))
  }
  
  year <- as.character(year)
  ss <- switch(
    year,
    '2020' = '1urYnnEjYdjC320Ua7ccZ3GC6-3Lmpudp_fybe2D3Xbs',
    '2021' = '1tkxRiu5i4XUDkLqJxEZMrwD9-jiXVLTD56j9pdHGjzc',
    '2022' = '10Ou2UbMtox3pVfrW9mziLkE9138SV281t6yGTg5IMXY'
  )
  
  label_lvls <- c('w', 'plant', 'side', 'fb')
  if(year == '2020') {
    range <- 'A6:AI145'
    times <- 35
    label_lvls <- label_lvls[1:3]
  } else {
    range <- 'CA5:DT144'
    times <- 46
  }
  
  col_names <- c(
    'map',
    'team',
    crossing(
      r = sprintf('r%02d', 1:11),
      label = factor(label_lvls, levels = label_lvls)
    ) |> 
      arrange(r, label) |> 
      mutate(
        label = sprintf('%s_%s', r, label)
      ) |> 
      pull(label)
  )
  
  col_types <- paste0(rep('c', times), collapse = '')
  
  matches <- read_sheet(
    ss = ss,
    sheet = sheet,
    range = range,
    col_names = col_names,
    col_types = col_types
  )
  
  matches$match <- rep(1:28, each = 5)
  
  first_na_grp <- matches |> 
    group_by(match) |> 
    filter(row_number() == 2) |> 
    ungroup() |> 
    filter(is.na(team)) |> 
    pull(match) |> 
    min()
  
  clean_matches <- matches |> 
    filter(match < !!first_na_grp) |> 
    relocate(match) |> 
    group_by(match) |> 
    mutate(rn = row_number() - 1L, .before = 1) |> 
    filter(rn %in% c(1L, 2L)) |> 
    ungroup()
  
  long_matches <- clean_matches |> 
    pivot_longer(
      -c(rn:team)
    ) |> 
    mutate(
      round = str_remove(name, '_.*$') |> str_remove('^r') |> as.integer(),
      label = str_remove(name, '^.*_')
    ) |> 
    select(-name) |> 
    pivot_wider(
      names_from = label,
      values_from = value
    ) |> 
    arrange(match, round, rn) |> 
    group_by(match, map, round) |> 
    filter(
      any(!is.na(side)) | any(!is.na(w)) | any(!is.na(plant))
    ) |> 
    ungroup()
  
  res <- long_matches |> 
    transmute(
      match,
      team,
      map,
      round,
      is_offense = side == 'O',
      win_round = coalesce(w == '1', FALSE),
      plant
    )
  
  if(year != '2020') {
    res$earned_fb <- coalesce(long_matches$fb == 'X', FALSE)
  }
  
  qs::qsave(res, path)
  res
}

matches <- sheets |> 
  mutate(
    data = map2(year, sheet, read_snd_sheet, overwrite = TRUE)
  ) |> 
  unnest(data)

fixed_rounds <- tibble(
  year = rep(2020L, 6),
  sheet = c(rep('LON_SnD', 4), rep('LA_SnD', 2)),
  match = c(rep(8L, 2), rep(17L, 2), rep(15L, 2)),
  round = c(rep(11L, 4), rep(9L, 2)),
  team = c('Guerrillas', 'Surge', 'Empire', 'Huntsmen', 'Empire', 'Rokkr'),
  is_offense = c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE),
  win_round = c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE),
  plant = c(NA_character_,NA_character_,'A','A',NA_character_,NA_character_),
  map = c(rep('Arklov Peak', 4), rep('St. Petrograd', 2))
)

clean_matches <- matches |> 
  anti_join(
    fixed_rounds |> 
      distinct(year, sheet, match, team, round), 
    by = c('year', 'sheet', 'match', 'team', 'round')
  ) |> 
  bind_rows(
    fixed_rounds |> 
      mutate(
        event = sprintf('%s - %s', year, sheet) |> 
          factor(levels = sheets$event)
      )
  ) |> 
  mutate(
    across(
      map,
      ~ifelse(.x == 'Arklov PeaK', 'Arklov Peak', .x)
    )
  ) |> 
  arrange(event, match, round, team)

records <- clean_matches |> 
  group_by(year, sheet, match, team) |> 
  mutate(
    cumu_w = cumsum(win_round),
    cumu_l = round - cumu_w,
    pre_cumu_w = lag(cumu_w, default = 0L),
    pre_cumu_l = lag(cumu_l, default = 0L),
    won_prior_round = lag(win_round, default = NA),
    max_cumu_w = max(cumu_w)
  ) |> 
  ungroup() |> 
  inner_join(
    clean_matches |> 
      filter(round == 1L) |> 
      distinct(year, sheet, match, team, starts_as_offense = is_offense), 
    by = c('year', 'sheet', 'match', 'team')
  ) |> 
  inner_join(
    clean_matches |> 
      group_by(year, sheet, match, team) |> 
      slice_max(round, n = 1, with_ties = FALSE) |> 
      ungroup() |> 
      transmute(
        year, 
        sheet, 
        match, 
        team,
        win_match = win_round,
        is_offense_last_round = is_offense,
        n_rounds = round
      ), 
    by = c('year', 'sheet', 'match', 'team')
  )
records

summarize_records <- function(records, ...) {
  records |> 
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
      caption = 'CDL SnD Major and Weekly Matches, 2020 - present',
      tag = '**Viz**: @TonyElHabr | **Data**: @IOUTurtle',
      x = "Offensive Team's # of Pre-Round Wins",
      y = "Defensive Team's # of Pre-Round Wins"
    )
  )
}

## match win %, given round state ---
## need to split this out by who is offense in order to not have a symmetric heatmap
n_records <- records |> summarize_records()
p_offensive_match_win_prop <- n_records |> 
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
    title = 'Offensive Match Win %'
  )
p_offensive_match_win_prop

ggsave(
  p_offensive_match_win_prop,
  filename = file.path(dir_proj, 'offensive_match_win_prop.png'),
  width = 8,
  height = 8
)

## round win %, given round state ---
n_records_side <- records |> summarize_records(is_offense)
offense_round_win_prop <- n_records_side |> 
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
n_records_won_prior_round <- records |> summarize_records(won_prior_round)
n_records_won_prior_round |> 
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

records |> 
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

records |> 
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

records |> 
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
round_seqs <- records |> 
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

records |> 
  filter(pre_cumu_w == 5, pre_cumu_l == 3) |> 
  count(n_rounds, win_match) |> 
  mutate(prop = n / sum(n))
dbinom(3, 3, 0.5)

records |> 
  filter(pre_cumu_w == 5, pre_cumu_l == 3) |> 
  count(n_rounds, win_match, is_offense) |> 
  group_by(is_offense) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup()
dbinom(3, 3, 0.5)
prop.test(
  
)

records |> 
  filter(pre_cumu_w == 5, pre_cumu_l == 4) |> 
  count(n_rounds, win_match) |> 
  mutate(prop = n / sum(n))
dbinom(2, 2, 0.5)
