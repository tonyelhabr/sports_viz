library(dplyr)
library(purrr)
library(worldfootballR)
library(readr)

proj_dir <- '65-opta_xg_calib'
data_dir <- file.path(proj_dir, 'data')
source(file.path(proj_dir, 'params.R'))
match_shooting <- params |> 
  group_by(group, tier, gender) |> 
  summarize(countries = list(country)) |> 
  ungroup() |> 
  mutate(
    data = pmap(
      list(
        countries,
        tier,
        gender,
        group
      ),
      ~{
        first_season_end_year <- ifelse(..4 == 'big5', 2018L, 2019L)
        res <- load_fb_match_shooting(
          country = ..1,
          tier = ..2,
          gender = ..3,
          season_end_year = first_season_end_year:2022L
        )
        res$Tier <- ..2
        res
      }
    )
  ) |> 
  select(group, data) |> 
  unnest(data)
write_rds(match_shooting, file.path(data_dir, 'match_shooting.rds'))
