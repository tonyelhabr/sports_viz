library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(probably)
packageVersion('probably')

proj_dir <- '65-opta_xg_calib'
data_dir <- file.path(proj_dir, 'data')
footedness_path <- file.path(data_dir, 'footedness.rds')
match_shooting_path <- file.path(data_dir, 'match_shooting.rds')

footedness <- read_rds(footedness_path)
match_shooting <- read_rds(match_shooting_path)

shots <- match_shooting |> 
  transmute(
    # match_url = MatchURL,
    country = Country,
    season = Season_End_Year,
    date = ymd(Date),
    half = Match_Half,
    minute = Minute,
    team = Squad,
    player = str_remove(Player, ' \\(.*$'),
    xg = as.numeric(xG),
    psxg = as.numeric(PSxG),
    outcome = Outcome,
    is_penalty = str_detect(Player, '\\(pen\\)'),
    is_goal = factor(ifelse(outcome == 'Goal', 'yes', 'no')),
    distance = as.integer(Distance),
    body_part = `Body Part`,
    notes = Notes,
    sca1 = Event_SCA_1,
    sca2 = Event_SCA_2
  ) |> 
  left_join(
    footedness |> select(player, primary_foot = foot),
    multiple = 'all',
    by = join_by(player)
  ) |> 
  mutate(
    is_true_open_play = notes == '',
    is_from_deflection = str_detect(notes, 'Deflected'),
    is_from_volley = str_detect(notes, 'Volley'),
    is_free_kick = notes == 'Free kick',
    is_primary_foot = case_when(
      is.na(body_part) ~ NA,
      !(body_part %in% sprintf('%s Foot', c('Left', 'Right'))) ~ NA,
      primary_foot == tolower(str_remove(body_part, ' Foot')) ~ TRUE,
      .default = FALSE
    )
  )

shots |> 
  filter(!is_penalty) |> 
  cal_plot_windowed(
    truth = is_goal,
    estimate = xg,
    window_size = 0.05,
    conf_level = 0.95,
    event_level = 'second'
  )

shots |> 
  filter(!is_penalty) |> 
  cal_plot_breaks(
    truth = is_goal,
    estimate = xg,
    group = is_primary_foot,
    num_breaks = 20,
    conf_level = 0.95,
    event_level = 'second'
  )
  count(sca1) |> 
  mutate(
    is_open_play = case_when(
      sca1 %in% c('Pass (Live)')
    )
  )

shots |> 
  filter(!is.na(is_primary_foot), !is_penalty) |> 
  group_by(is_primary_foot) |> 
  summarize(
    n_shots = n(),
    npxg = sum(xg, na.rm = TRUE),
    npg = sum(as.character(is_goal) == 'yes')
  ) |> 
  ungroup() |> 
  mutate(
    d = npxg - npg
  )


