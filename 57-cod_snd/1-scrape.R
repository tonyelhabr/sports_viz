
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
      is_win = coalesce(w == '1', FALSE),
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
    data = map2(year, sheet, read_snd_sheet)
  ) |> 
  unnest(data)

fixed_rounds <- tibble(
  year = rep(2020L, 6),
  sheet = c(rep('LON_SnD', 4), rep('LA_SnD', 2)),
  match = c(rep(8L, 2), rep(17L, 2), rep(15L, 2)),
  round = c(rep(11L, 4), rep(9L, 2)),
  team = c('Guerrillas', 'Surge', 'Empire', 'Huntsmen', 'Empire', 'Rokkr'),
  is_offense = c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE),
  is_win = c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE),
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
    cumu_w = cumsum(is_win),
    cumu_l = round - cumu_w,
    pre_cumu_w = lag(cumu_w, default = 0L),
    pre_cumu_l = lag(cumu_l, default = 0L),
    max_cumu_w = max(cumu_w)
  ) |> 
  ungroup() |> 
  mutate(
    win_series = max_cumu_w == 6L
  )

n_records <- records |> 
  group_by(pre_cumu_w, pre_cumu_l) |> 
  summarize(
    n = n(),
    win_prop = sum(win_series) / n
  ) |> 
  ungroup()

n_records |> 
  ggplot() +
  aes(x = pre_cumu_w, y = pre_cumu_l) +
  geom_tile(
    alpha = 0.8,
    aes(fill = win_prop)
  ) +
  scale_fill_viridis_c() +
  geom_text(
    aes(label = sprintf('%s\n(%s)', scales::percent(win_prop, accuracy = 0.1), scales::comma(n)))
  )

init_n_records_side <- records |> 
  group_by(pre_cumu_w, pre_cumu_l, is_offense) |> 
  summarize(
    n = n(),
    across(c(is_win, win_series), sum)
  ) |> 
  ungroup() |> 
  mutate(
    win_prop = is_win / n,
    win_series_prop = win_series / n
  )

## Does this make sense?
n_records_side <- bind_rows(
  init_n_records_side,
  init_n_records_side |> 
    filter(pre_cumu_w != pre_cumu_l) |> 
    rename(pre_cumu_w1 = pre_cumu_w, pre_cumu_w = pre_cumu_l) |> 
    rename(pre_cumu_l = pre_cumu_w1)
) |> 
  group_by(pre_cumu_w, pre_cumu_l, is_offense) |> 
  summarize(
    across(c(n, is_win, win_series), sum)
  ) |> 
  ungroup() |> 
  mutate(
    win_prop = is_win / n,
    win_series_prop = win_series / n
  )

n_records_side |> 
  ggplot() +
  aes(x = pre_cumu_w, y = pre_cumu_l) +
  geom_tile(
    alpha = 0.8,
    aes(fill = win_prop)
  ) +
  scale_fill_viridis_c() +
  geom_text(
    aes(label = sprintf('%s\n(%s)', scales::percent(win_prop, accuracy = 0.1), scales::comma(n)))
  ) +
  facet_wrap(~is_offense)

offense_round_win_prop <- init_n_records_side |> 
  filter(is_offense) |> 
  mutate(
    diff_prop = win_prop - 0.5
  )

max_diff_prop <- max(abs(offense_round_win_prop$diff_prop))

p_offensive_round_win_prop <- offense_round_win_prop |> 
  ggplot() +
  aes(x = pre_cumu_w, y = pre_cumu_l) +
  geom_rect(
    # alpha = 0.8,
    color = blackish_background,
    aes(
      fill = win_prop,
      xmin = pre_cumu_w, 
      ymin = pre_cumu_l,
      xmax = pre_cumu_w + 1,
      ymax = pre_cumu_l + 1
    )
  ) +
  scale_fill_gradient2(
    low = '#af8dc3',
    high = '#7fbf7b',
    # mid = '#f7f7f7',
    mid = 'white',
    midpoint = 0.5,
    limits = c(0.5 - max_diff_prop, 0.5 + max_diff_prop)
  ) +
  geom_text(
    family = font,
    color = blackish_background,
    size = 14 / .pt,
    fontface = 'bold',
    aes(
      x = pre_cumu_w + 0.5, 
      y = pre_cumu_l + 0.5,
      label = sprintf('%s\n(%s)', scales::percent(win_prop, accuracy = 1), scales::comma(n))
    )
  ) +
  guides(
    fill = 'none'
  ) +
  scale_x_continuous(
    labels = 0:5,
    breaks = seq(.5, 5.5, by = 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = 0:5,
    breaks = seq(.5, 5.5, by = 1),
    expand = c(0, 0)
  ) +
  theme(
    panel.grid.major = element_blank(),
    axis.title = element_text(hjust = 0.5),
    plot.title = ggtext::element_markdown(hjust = 0.5),
    axis.text = element_text(size = 16, face = 'bold'),
    plot.tag.position = c(0.01, 0.01),
  ) +
  labs(
    title = 'Offensive Round Win %',
    caption = 'CDL SnD Major and Weekly Matches, 2020 - present',
    tag = '**Viz**: @TonyElHabr | **Data**: @IOUTurtle',
    x = "Offensive Team's # of Pre-Round Wins",
    y = "Defensive Team's # of Pre-Round Wins"
  )
p_offensive_round_win_prop

ggsave(
  p_offensive_round_win_prop,
  filename = file.path(dir_proj, 'offensive_round_win_prop.png'),
  width = 8,
  height = 8
)

records |> 
  group_by(map, is_offense) |> 
  summarize(
    n = n(),
    win_prop = sum(win_series) / n
  ) |> 
  ungroup() |> 
  filter(is_offense) |> 
  arrange(desc(win_prop))

