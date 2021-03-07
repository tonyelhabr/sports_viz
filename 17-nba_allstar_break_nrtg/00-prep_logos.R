
library(tidyverse)
.dir_proj <- '17-nba_allstar_break_nrtg'
.dir_logos <- file.path(.dir_proj, 'data')
fs::dir_create(.dir_logos)
download_safely <- safely(download.file)
tms <- 
  nbastatR::nba_teams() %>% 
  janitor::clean_names() %>% 
  filter(is_non_nba_team == 0L & slug_team != 'GLI')
tms

tms_slim <-
  tms %>% 
  select(tm = slug_team, url = url_thumbnail_team)
tms_slim

paths_svg <-
  tms_slim %>% 
  mutate(
    path_local_svg = url %>% str_replace_all('(^.*)([A-Z]{3}_logo[.]svg)', '\\2') %>% file.path(.dir_logos, .),
    already_downloaded = map_lgl(path_local_svg, fs::file_exists)
  ) %>% 
  # filter(!already_downloaded) %>% 
  mutate(
    res = map2(url, path_local_svg, ~download_safely(url = ..1, destfile = ..2, quiet = TRUE, mode = 'wb')))
)
paths_svg

paths_png <-
  paths_svg %>%
  mutate(
    path_local_png = path_local_svg %>% str_replace('svg', 'png')
  ) %>% 
  mutate(already_converted = map_lgl(path_local_png, fs::file_exists)) %>% 
  filter(!already_converted) %>%
  mutate(res = map2(
    path_local_svg,
    path_local_png,
    ~ rsvg::rsvg(..1) %>% png::writePNG(..2,metadata = )
  ))
paths_png