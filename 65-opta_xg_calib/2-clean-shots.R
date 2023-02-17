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
clean_shots_path <- file.path(data_dir, 'clean_shots.rds')

footedness <- read_rds(footedness_path)
match_shooting <- read_rds(match_shooting_path)

unambiguous_footedness <- footedness |> 
  semi_join(
    footedness |> 
      count(country, tier, gender, season_end_year, player, sort = TRUE) |> 
      filter(n == 1L),
    by = join_by(country, tier, gender, season_end_year, player)
  )

shots <- match_shooting |> 
  transmute(
    # match_url = MatchURL,
    group,
    country = Country,
    gender = Gender,
    tier = Tier,
    season_end_year = Season_End_Year,
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
    unambiguous_footedness |> 
      select(
        country,
        tier,
        gender,
        season_end_year,
        player, 
        primary_foot = foot
      ),
    multiple = 'all',
    by = join_by(
      country, 
      tier, gender, 
      season_end_year, 
      player
    )
  ) |> 
  mutate(
    is_true_open_play = notes == '' & !is_penalty,
    is_from_deflection = str_detect(notes, 'Deflected'),
    is_from_volley = str_detect(notes, 'Volley'),
    is_free_kick = notes == 'Free kick',
    is_open_play = !is_free_kick & !is_penalty,
    is_primary_foot = case_when(
      is.na(body_part) ~ NA,
      is.na(primary_foot) ~ NA,
      !(body_part %in% sprintf('%s Foot', c('Left', 'Right'))) ~ NA,
      primary_foot == tolower(str_remove(body_part, ' Foot')) ~ TRUE,
      .default = FALSE
    )
  )

write_rds(shots, clean_shots_path)
