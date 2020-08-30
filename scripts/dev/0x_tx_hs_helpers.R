
library(tidyverse)
import_fb_rnks <- memoise::memoise({function() {
  # fb rankings source:
  # + https://www.6atexasfb.com/forum/main-forum/518558-greatest-texas-hs-fb-programs-of-all-time
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
    relocate(rnk, school)
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
      rename(conf = conference) %>% 
      filter(conf != 'B') %>% # Drop the 1979 year, where `conf == 'B'`
      filter(round != 'Round') %>% # Extra heading row at the end
      separate(school_isd, into = c('school', 'isd'), sep = '\\s\\(') %>% 
      mutate(
        across(c(place, year), as.integer),
        across(isd, ~str_remove(.x, '\\)$'))
      ) %>% 
      mutate(tier = conf %>% str_sub(1, 1) %>% as.integer()) %>% 
      select(idx, year, conf, tier, round, place, school, isd, notes)
    write_rds(res, path_export)
    res
  }
})


# bands <- import_bands() %>% mutate(across(school, ~str_remove(.x, '\\sHS$')), across(isd, ~str_remove(.x, '\\sISD$')))
# bands
# 
# bands_tier_n <- bands %>% count(tier) %>% mutate(frac = n / sum(n))
# bands_tier_n
# 
# # Do some sort of weighted number instead of a percent rank for conf?
# bands_tier_n %>% 
#   mutate(
#     frac_tier = (max(frac) - frac + 1L) / (max(frac) - min(frac) + 1L),
#     frac_tier_2 = ((frac - min(frac))/ (max(frac) - min(frac))) * 0.1
#   )
# bands_tier_n

# # bands %>% filter(is.na(place))
# bands_na <-
#   bands %>% 
#   group_by(year, conf, tier, round) %>% 
#   summarize(n_na = sum(is.na(place)), n = n()) %>%
#   ungroup() %>% 
#   mutate(n_diff = n - n_na)
# bands_na %>% filter(n_diff > 0L)
# 
# bands_na %>% 
#   ggplot() +
#   aes(x = year, y = conf, size = n_diff) +
#   geom_point()
# bands_na %>% filter(n_diff == 0L)

import_scores_old <- function(school) {
  path <- here::here('data-raw', '0x', sprintf('%s.xls', school))
  prefix_record <- c('w', 'l', 't')
  nms <- 
    c('idx', 'year', 'pf', 'pa', sprintf('%s_season', prefix_record), sprintf('%s_district', prefix_record), sprintf('%s_total', prefix_record), 'frac_total', 'blank')
  df_raw <- 
    path %>% 
    readxl::read_excel(skip = 0L) %>% 
    # janitor::clean_names()
    set_names(nms)
  df_raw
  # df_raw %>% filter(is.na(idx) & year %>% str_detect('^[12]', negate = TRUE))
  # df_raw
  df_clean <-
    df_raw %>% 
    # mutate(season = if_else(!is.na(year) & is.na(idx), year, NA_character_)) %>% 
    mutate(season = if_else(pf == 'PF', year, NA_character_)) %>% 
    fill(season) %>% 
    filter(!is.na(idx)) %>% 
    select(-matches('district$'), -blank) %>% 
    # filter(!is.na(idx)) %>% 
    filter(pf != 'PF') %>% 
    mutate(
      across(matches('^[wlt]_|^idx$|^season$'), as.integer),
      across(frac_win, as.double)
    )
  df_clean
  
  df_filt <-
    df_clean %>% 
    filter(season >= 1979) %>% 
    mutate(idx = row_number()) %>% 
    # select(-matches('_total$')) %>% 
    mutate(
      across(matches('_total$'), ~.x - min(.x))
    ) %>% 
    mutate(
      across(w_total, ~if_else(idx == 1L & w_season == 1L, 1L, .x)),
      across(l_total, ~if_else(idx == 1L & l_season == 1L, 1L, .x)),
      across(t_total, ~if_else(idx == 1L & t_season == 1L, 1L, .x))
    ) %>% 
    mutate(frac_total = w_total / (w_total + l_total + t_total))
  df_filt
  # df_filt %>% tail()
}


# google drive fail

#  url_drive <- 'https://onedrive.live.com/View.aspx?resid=86C60DC641D3140!60246&authkey=!AECe-7m6LioN5U0'
# id_drive <- googledrive::as_id(url_drive)
# # download.file(url_drive, 'file.xlsx')
# drive <- googledrive::as_dribble(id_drive)
# instructions:
# 1. Make a copy of https://drive.google.com/open?id=1BRTPCLzc09oW6NpqPoHabXwKwjzwxOVO to your local google drive (link originally found here: https://www.6atexasfootball.com/forum/main-forum/199726-complete-game-by-game-football-histories/page10).
# 2. Download a copy of this workbook.
# 3. Show formulas on the Data sheet for column A.
# 4. Copy-paste formulas to a text file.
.get_fb_score_links_df <- memoise::memoise({function() {
  path_links <- here::here('data-raw', '0x', 'tx-hs-fb-links.txt')
  lines <- path_links %>% read_lines()
  df <- lines %>% tibble(line = .)
  df
  res <-
    df %>% 
    mutate(has_link = line %>% str_detect('^[=]')) %>% 
    mutate(
      link = line %>% str_remove_all('^[=]HYPERLINK\\(|\\,.*\\)$|["]') %>% if_else(has_link, ., NA_character_),
      # This `school` extraction actaully also works fine for those without links
      school = line %>% str_remove_all('^.*\\,.|\\)$|["]'),
    ) %>% 
    select(school, has_link, link)
  res
}})

.download_fb_scores <-
  function(link,
           dir = here::here('data-raw', '0x', 'schools'),
           file = '',
           path = fs::path(dir, sprintf('%s.xlsx', file))) {
    if (fs::file_exists(path)) {
      return(path)
    }
    id <- link %>% googledrive::as_id()
    dr <- id %>% googledrive::as_dribble()
    res <- dr %>% googledrive::drive_download(path = path)
    path
  }

.import_fb_scores <- function(path) {
  res_init <- path %>% readxl::read_excel(col_types = 'text')
  res <-
    res_init %>% 
    select(
      coach = Coach,
      season = Season,
      district = District,
      conf = Classification,
      date = Date,
      week = Week,
      opp = Opponent,
      pf = PF,
      pa = PA,
      mov = `Margin of Victory`, 
      w = Win,
      l = Loss,
      t = Tie,
      gp = `Games Played`, 
      w_cumu = `All Time Wins`,
      l_cumu = `All Time Losses`,
      t_cumu = `All Time Ties`,
      g_cumu = `All Time Games`,
      w_frac_cumu = `Win Percentage`
    ) %>% 
    mutate(
      across(date, lubridate::mdy),
      across(c(season, week, pf, pa, mov, w, l, t, gp, w_cumu, l_cumu, t_cumu, g_cumu), as.integer),
      across(w_frac_cumu, as.double)
    )
  res
}

retrieve_fb_scores <- function(links = NULL, path = here::here('data-raw', '0x', 'fb_scores.rds')) {
  
  if(fs::file_exists(path)) {
    res <- path %>% read_rds()
    return(res)
  }
  
  if(is.null(links)) {
    links <-
      .get_fb_score_links_df() %>% 
      filter(has_link) %>%
      select(-has_link)
  }
  
  links_info <-
    links %>% 
    mutate(path = map2_chr(link, school, ~.download_fb_scores(link = ..1, file = ..2))) %>% 
    select(-link)

  .import_fb_scores_q <- quietly(.import_fb_scores)
  res <-
    links_info %>% 
    mutate(data = map(path, ~.import_fb_scores_q(.x) %>% pluck('result'))) %>% 
    select(-path) %>% 
    unnest(data)
  
  write_rds(res, path)
  res
}

# import_score_links <- function() {
#   path <- here::here('data-raw', '0x', 'Texas High School Football Team Public Links.xlsm')
#   df_raw <- path %>% readxl::read_excel(sheet = 'Data') %>% janitor::clean_names()
#   df_raw
#   link <- df_raw$link[[1]]
#   path_dl <- here::here('data-raw', '0x', 'example.xlsx')
#   download.file(link, destfile = path_dl, quiet = TRUE)
# }

import_scores <- function(school) {
  school <- 'Duncanville'
  path <- here::here('data-raw', '0x', sprintf('%s.xlsx', school))
  sheets <- path %>% readxl::excel_sheets()
  sheets
  df_raw <- path %>% readxl::read_excel(sheet = 'Data') %>% janitor::clean_names()
  df_raw
  df_raw_filt <- df_raw %>% filter(season >= 1979)
  df_raw_filt
  
}
