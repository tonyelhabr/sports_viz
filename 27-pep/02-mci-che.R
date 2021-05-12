# https://en.wikipedia.org/wiki/List_of_European_Cup_and_UEFA_Champions_League_finals
library(tidyverse)
dir_proj <- '27-pep'

df <- file.path(dir_proj, 'ucl_matches.csv') %>% read_csv()
df

res <-
  df %>% 
  reactable::reactable()
res

# This will be the fifth time since 2007-08 that the #UCL finalists come from the same league. Have the domestic league face-offs during the season foreshadowed the title match result? Sort of.