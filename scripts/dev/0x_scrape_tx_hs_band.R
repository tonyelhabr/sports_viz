
# fb rankings source:
# + https://www.6atexasfb.com/forum/main-forum/518558-greatest-texas-hs-fb-programs-of-all-time
library(tidyverse)

import_fb <- memoise::memoise({function() {
  path <- here::here('data-raw', '0x', 'top-50-tx-hs-football.txt')
  lines <- path %>% read_lines()
  res <-
    lines %>% 
    tibble(line = .) %>% 
    filter(line != '') %>% 
    mutate(idx = row_number()) %>% 
    relocate(idx) %>% 
    mutate(header = if_else(str_detect(line, '^[0-9]+[.]'), line, NA_character_)) %>% 
    fill(header) %>% 
    filter(header != line) %>% 
    separate(line, into = c('key', 'value'), sep = '[:]\\s+') %>% 
    separate(header, into = c('rnk', 'school'), sep = '[.]\\s+') %>% 
    mutate(across(rnk, as.integer)) %>% 
    select(-idx) %>% 
    relocate(rnk, school) %>% 
    mutate(across(school, ~sprintf('%s HS', .x))) 
  res
}})

import_bands <- memoise::memoise({
  function(path_export = here::here('data-raw', '0x', 'tx-hs-band.rds')) {
    if(fs::file_exists(path_export)) {
      return(read_rds(path_export))
    }
    
    require(rvest)
    url <- 'https://smbc.uiltexas.org/archives.htm'
    page <- url %>% xml2::read_html()
    # children <- page %>% rvest::html_children()
    main <- page %>% rvest::html_nodes('.main')
    df <- 
      main %>% 
      rvest::html_node('table') %>% 
      rvest::html_table(fill = TRUE) %>% 
      .[[1]] %>% 
      as_tibble() %>% 
      janitor::clean_names()
    res <-
      df %>%
      mutate(idx = row_number()) %>% 
      relocate(idx) %>% 
      filter(conference != 'B') %>% # Drop the 1979 year, where `conference == 'B'`
      filter(round != 'Round') %>% # Extra heading row at the end
      separate(school_isd, into = c('school', 'isd'), sep = '\\s\\(') %>% 
      mutate(
        across(c(place, year), as.integer),
        across(isd, ~str_remove(.x, '\\)$'))
      ) %>% 
      mutate(tier = conference %>% str_sub(1, 1) %>% as.integer())
    write_rds(res, path_export)
    res
  }
})

fb <- import_fb()
fb_points <- fb %>% filter(key == 'TOTAL POINTS') %>% select(-key)

bands <- import_bands()
bands

bands_tier_n <- bands %>% count(tier) %>% mutate(frac = n / sum(n))
bands_tier_n

# # Do some sort of weighted number instead of a percent rank for conference?
# bands_tier_n %>% 
#   mutate(
#     frac_tier = (max(frac) - frac + 1L) / (max(frac) - min(frac) + 1L),
#     frac_tier_2 = ((frac - min(frac))/ (max(frac) - min(frac))) * 0.1
#   )
# bands_tier_n

bands %>% filter(is.na(place))
bands_aug %>% filter(year == 2002L, conference == '3A', round == 'Prelims')

bands_na <-
  bands %>% 
  group_by(year, conference, round) %>% 
  summarize(n_na = sum(is.na(place)), n = n()) %>%
  ungroup() %>% 
  mutate(n_diff = n - n_na)
bands_na %>% filter(n_diff > 0L)
bands_na %>% 
  ggplot() +
  aes(x = year, y = conference, size = n_diff) +
  geom_point()
bands_na %>% filter(n_diff == 0L)

bands %>% filter(year == 2001) %>% filter(!is.na(place))
bands %>% filter(year == 2019, round == 'Prelims', conference == '5A')
bands %>% filter(year == 2003, round == 'Finals', conference == '1A')
bands %>% filter(year == 2019, round == 'Finals', conference == '5A')


bands_aug <-
  bands %>% 
  # filter(!is.na(place)) %>% 
  mutate(
    prnk_conf = (tier - min(tier) + 1L) / (max(tier) - min(tier) + 1L)
  ) %>% 
  group_by(year, conference, round) %>% 
  mutate(n = n()) %>% 
  mutate(
    # Need to coalesce for when `place` is NA.
    prnk_place = ((n - place + 1L) / (n - min(place) + 1L)) %>% coalesce(1 / n)
  ) %>% 
  ungroup() %>% 
  mutate(score = ifelse(round == 'Prelims', 0.5, 1) * prnk_conf * prnk_place)
bands_aug

bands_aug %>% filter(school %>% str_detect('Sund'))
bands_agg <-
  bands_aug %>% 
  group_by(school) %>% 
  summarize(across(score, sum), n = n()) %>% 
  ungroup() %>% 
  mutate(rnk = row_number(desc(score))) %>% 
  relocate(rnk) %>% 
  arrange(rnk)
bands_agg

fb_points %>% 
  rename(rnk_fb = rnk) %>% 
  mutate(across(school, ~sprintf('%s HS', .x))) %>% 
  filter(school %>% str_detect('^Judson'))

hs_join <-
  inner_join(
    bands_agg %>% rename(rnk_band = rnk),
    fb_points %>% rename(rnk_fb = rnk)
  )
hs_join

bands_agg %>% filter(school %>% str_detect('Jud'))

fb_points %>% filter(school == 'Judson HS')

bands_aug
bands %>% count(tier)
bands %>% count(place) %>% tail()
bands %>% filter(place == 41L)

bands %>% count(conference)
bands %>% filter(conference == 'B')
bands
bands %>% skimr::skim()
bands %>% filter(notes != '')
bands %>% count(round)

bands %>% filter(school_isd %>% str_detect('Katy Taylor'))
bands_schools_n <- df %>% count(school_isd, sort = TRUE)
bands %>% count(conference)
bands %>% filter(conference %in% sprintf('%dA', 4:6)) %>% count(school_isd, sort = T)
bands_schools_n %>% filter(school_isd %>% str_detect('Sundown'))
bands_schools_n %>% filter(school_isd %>% str_detect('Carroll'))
bands %>% filter(school_isd %>% str_detect('Sundown'))
