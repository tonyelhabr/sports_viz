
## Data from https://twitter.com/IOUTurtle
library(tidyverse)
library(googlesheets4)
library(magrittr)
library(qs)

dir_proj <- '57-cod_snd'
dir_data <- file.path(dir_proj, 'data')
dir.create(dir_data, showWarnings = FALSE)

sheets <- list(
  '2022' = crossing(
    s = c(1, 2, 3, 4),
    raw_series = c(1, 2, 3, 4)
  ) |> 
    mutate(
      label = sprintf('S%d %s', s, ifelse(raw_series == 4, 'Major', sprintf('Wk%d', raw_series)))
    ) |> 
    pull(label) %>%
    .[1:14],
  '2021' = c(
    'Champs', 
    crossing(
      s = c(1, 2, 3, 4, 5),
      raw_series = c(1, 2, 3, 4)
    ) |> 
      mutate(
        label = sprintf('S%d %s', s, ifelse(raw_series == 4, 'Major', sprintf('HS%d', raw_series)))
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
  
  raw_series <- read_sheet(
    ss = ss,
    sheet = sheet,
    range = range,
    col_names = col_names,
    col_types = col_types
  )
  
  raw_series$series <- rep(1:28, each = 5)
  
  first_na_grp <- raw_series |> 
    group_by(series) |> 
    filter(row_number() == 2) |> 
    ungroup() |> 
    filter(is.na(team)) |> 
    pull(series) |> 
    min()
  
  series <- raw_series |> 
    filter(series < !!first_na_grp) |> 
    relocate(series) |> 
    group_by(series) |> 
    mutate(rn = row_number() - 1L, .before = 1) |> 
    filter(rn %in% c(1L, 2L)) |> 
    ungroup()
  
  long_series <- series |> 
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
    arrange(series, round, rn) |> 
    group_by(series, map, round) |> 
    filter(
      any(!is.na(side)) | any(!is.na(w)) | any(!is.na(plant))
    ) |> 
    ungroup()
  
  res <- long_series |> 
    transmute(
      series,
      team,
      map,
      round,
      is_offense = side == 'O',
      win_round = coalesce(w == '1', FALSE),
      plant
    )
  
  if(year != '2020') {
    res$earned_fb <- coalesce(long_series$fb == 'X', FALSE)
  }
  
  qs::qsave(res, path)
  res
}

raw_series <- sheets |> 
  mutate(
    data = map2(year, sheet, read_snd_sheet, overwrite = T)
  ) |> 
  unnest(data)

fixed_rounds <- tibble(
  year = rep(2020L, 6),
  sheet = c(rep('LON_SnD', 4), rep('LA_SnD', 2)),
  series = c(rep(8L, 2), rep(17L, 2), rep(15L, 2)),
  round = c(rep(11L, 4), rep(9L, 2)),
  team = c('Guerrillas', 'Surge', 'Empire', 'Huntsmen', 'Empire', 'Rokkr'),
  is_offense = c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE),
  win_round = c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE),
  plant = c(NA_character_,NA_character_,'A','A',NA_character_,NA_character_),
  map = c(rep('Arklov Peak', 4), rep('St. Petrograd', 2))
)

series <- raw_series |> 
  anti_join(
    fixed_rounds |> 
      distinct(year, sheet, series, team, round), 
    by = c('year', 'sheet', 'series', 'team', 'round')
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
  arrange(event, series, round, team)

rounds <- series |> 
  group_by(year, sheet, series, team) |> 
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
    series |> 
      filter(round == 1L) |> 
      distinct(year, sheet, series, team, starts_as_offense = is_offense), 
    by = c('year', 'sheet', 'series', 'team')
  ) |> 
  inner_join(
    series |> 
      group_by(year, sheet, series, team) |> 
      slice_max(round, n = 1, with_ties = FALSE) |> 
      ungroup() |> 
      transmute(
        year, 
        sheet, 
        series, 
        team,
        win_series = win_round,
        is_offense_last_round = is_offense,
        n_rounds = round
      ), 
    by = c('year', 'sheet', 'series', 'team')
  )
rounds
qs::qsave(rounds, file.path(dir_proj, 'cod_rounds.qs'))
