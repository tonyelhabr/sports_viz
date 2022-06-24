library(googlesheets4)
library(tidyverse)

dir_proj <- '56-attendance'
dir_data <- file.path(dir_proj, 'data')
dir.create(dir_data, showWarnings = FALSE)

xengagement_team_mapping <- read_sheet(ss = '1nIw1PgozYsb11W-QSzjgPlW-qrsJWLiPB8x6fTTMqiI')

anti_join(
  xengagement_team_mapping |> 
    filter(is.na(is_alternative_fbref)) |> 
    distinct(team_538),
  team_mapping |> 
    distinct(team_538)
  
)
xengagement_team_mapping |> 
  filter(is.na(is_alternative_fbref)) |> 
  count(league, team_fotmob, sort = TRUE)
