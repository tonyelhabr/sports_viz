library(dplyr)
library(purrr)
library(worldfootballR)
library(readr)

proj_dir <- '65-opta_xg_calib'
data_dir <- file.path(proj_dir, 'data')

params <- bind_rows(
  'big5' = list(
    country = c('ENG', 'ESP', 'FRA', 'GER', 'ITA'),
    tier = '1st',
    gender = 'M'
  ),
  'other_1st_M' = list(
    country = c('POR', 'NED', 'BRA', 'MEX', 'USA'),
    tier = '1st',
    gender = 'M'
  ),
  '1st_F' = list(
    country = c('ENG', 'USA', 'ESP'),
    tier = '1st',
    gender = 'F'
  ),
  '2nd_M' = list(
    country = c('ENG'),
    tier = '2nd',
    gender = 'M'
  ),
  .id = 'group'
)

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
