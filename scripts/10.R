
library(tidyverse)
df_raw <- 'https://raw.githubusercontent.com/mrcaseb/nfl-data/master/data/nfl_lines_odds.csv' %>% read_csv()
df_raw
df_raw %>% skimr::skim()
df_raw %>% filter(!is.na(opening_lines)) %>% tail()

