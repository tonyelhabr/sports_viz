library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(tidyr)

proj_dir <- '65-opta_xg_calib'
data_dir <- file.path(proj_dir, 'data')
footedness_path <- file.path(data_dir, 'footedness.rds')
match_shooting_path <- file.path(data_dir, 'match_shooting.rds')
clean_shots_path <- file.path(data_dir, 'clean_shots.rds')
## for pre- and post-2023-02-08 xG comparison for 2021/22 season (https://twitter.com/fbref/status/1623358271791722502)
clean_shots_compare_path <- file.path(data_dir, 'clean_shots_compare.rds')
source(file.path(proj_dir, 'params.R'))

footedness <- read_rds(footedness_path)
match_shooting <- read_rds(match_shooting_path)

unambiguous_footedness <- footedness |> 
  semi_join(
    footedness |> 
      count(country, tier, gender, season_end_year, player, sort = TRUE) |> 
      filter(n == 1L),
    by = join_by(country, tier, gender, season_end_year, player)
  )

clean_match_shooting <- function(df) {
  df |> 
    transmute(
      match_url = MatchURL,
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
      ## seems that they started to exclusively use Take-On instead of Dribble in 2022
      sca1 = ifelse(Event_SCA_1 == 'Dribble', 'Take-On', Event_SCA_1),
      sca2 = ifelse(Event_SCA_2 == 'Dribble', 'Take-On', Event_SCA_2)
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
}

shots <- clean_match_shooting(match_shooting)
write_rds(shots, clean_shots_path)

## compare ----
pull_old_fb_match_shooting <- function(country, gender, tier) {
  url <- sprintf(
    'https://github.com/JaseZiv/worldfootballR_data/releases/download/old_fb_match_shooting/%s_%s_%s_match_shooting.rds', 
    country,
    gender,
    tier
  )
  readRDS(url(url))
}

old_fb_match_shooting <- params |> 
  filter(group == 'big5') |> 
  mutate(
    data = pmap(
      list(
        country,
        gender,
        tier
      ),
      pull_old_fb_match_shooting
    )
  ) |> 
  unnest(data)

shots_compare <- inner_join(
  shots |> 
    filter(group == 'big5', season_end_year == 2022) |> 
    rename(new_xg = xg),
  old_fb_match_shooting |> 
    filter(Season_End_Year == 2022) |> 
    rename(
      Tier = tier
    ) |> 
    clean_match_shooting() |> 
    select(,
      match_url, 
      half,
      minute,
      team,
      player,
      old_xg = xg
    ),
  by = join_by(match_url, half, minute, team, player),
  multiple = 'first'
) |> 
  mutate(
    xgd = new_xg - old_xg
  ) |> 
  arrange(desc(abs(xgd)))
write_rds(shots_compare, clean_shots_compare_path)
