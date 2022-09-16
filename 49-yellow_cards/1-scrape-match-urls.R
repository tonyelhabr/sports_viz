library(tidyverse)
library(qs)

dir_proj <- '49-yellow_cards'
dir_data <- file.path(dir_proj, 'data')
dir.create(dir_data, showWarnings = FALSE)

params <- crossing(
  country = c('ENG', 'ITA', 'GER', 'FRA', 'ESP', 'USA'),
  gender = 'M',
  season_end_year = c(2022),
  tier = c('1st', '2nd')
)
params

urls <- params |> 
  mutate(
    url = 
      pmap(
        list(country, gender, season_end_year, tier),
        worldfootballR::fb_match_urls
      )
  ) |> 
  unnest(url)
urls
qs::qsave(urls, file.path(dir_data, 'urls.qs'))
