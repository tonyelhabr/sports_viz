
# Reference: https://gist.github.com/jthomasmock/b8a1c6e90a199cf72c6c888bd899e84e
library(arrow)
library(tidyverse)

dir_data <- 'data-raw/nflfastr'
dir.create(dir_data)

# create function for partition directories and download parquet files
get_data <- function(year){
  dir.create(file.path('nflfastr', year))
  
  download.file(
    glue::glue('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{year}.parquet?raw=true'),
    file.path(dir_data, year, 'data.parquet'),
    mode = 'wb'
  )
}

walk(2012:2019, get_data)
df <- open_dataset(dir_data, partitioning = 'year')
df %>% collect()
df_agg <-
  df %>% 
  select(year, play_type, yards_gained, epa, penalty, season) %>%
  filter(year %in% c(1999, 2019),
         play_type %in% c("run", "pass"), penalty == 0) %>% 
  collect() %>% 
  group_by(season, play_type) %>% 
  summarize(
    avg_yds = mean(yards_gained, na.rm = TRUE),
    avg_epa = mean(epa, na.rm = TRUE),
    n = n()
  )
df_agg
