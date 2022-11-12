library(worldfootballR)
library(dplyr)
library(tibble)
library(stringr)
library(readr)
library(purrr)
library(qs)

dir_proj <- '62-safc'
# team_season_url <- 'https://fbref.com/en/squads/42cc5a38/San-Antonio-FC-Stats'
# match_results <- fb_team_match_results(team_season_url)
# match_results |> as_tibble()

# passing <- fb_team_match_log_stats(team_urls = team_season_url, stat_type = 'passing')

urls_path <- file.path(dir_proj, 'usl_2022_match_urls.rds')
if (!file.exists(urls_path)) {
  usl_2022_urls <- fb_match_urls(
    country = 'USA', 
    gender = 'M', 
    season_end_year = 2022,
    tier = '2nd'
  )
  write_rds(usl_2022_urls, urls_path)
} else {
  usl_2022_urls <- read_rds(urls_path)
}
safc_urls <- str_subset(usl_2022_urls, 'San-Antonio')
safc_urls
match_report <- fb_match_report(
  "https://fbref.com/en/matches/57400488/San-Antonio-FC-Colorado-Springs-Switchbacks-November-6-2022-USL-Championship"
)
match_report |> glimpse()

match_poss <- fb_advanced_match_stats(
  "https://fbref.com/en/matches/57400488/San-Antonio-FC-Colorado-Springs-Switchbacks-November-6-2022-USL-Championship",
  stat_type = "possession",
  team_or_player = "team"
)

match_summary |> as_tibble()
fb_match_results(
  
)
# #team_stats > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(2) > div:nth-child(1) > div:nth-child(1) > strong:nth-child(1)

library(rvest)
page <- read_html("https://fbref.com/en/matches/57400488/San-Antonio-FC-Colorado-Springs-Switchbacks-November-6-2022-USL-Championship")
page |> html_element(xpath = "body/div[2]/div[1]")
# /html/body/div[2]/div[5]/div[7]/div[1]/table/tbody/tr[3]/td[2]/div/div[1]/strong

usl_matches <- load_fotmob_matches_by_date(league_id = 8972) |> 
  mutate(
    across(date, lubridate::date)
  )

safc_2022_matches <- usl_matches |> 
  filter(date >= lubridate::ymd('2022-03-12')) |> 
  filter(home_name == 'San Antonio FC' | away_name == 'San Antonio FC')

get_fotmob_match_team_stats <- function(match_id, overwrite = FALSE) {
  path <- file.path(dir_proj, 'data', sprintf('%s.qs', match_id))
  if (file.exists(path) & isFALSE(overwrite)) {
    return(qs::qread(path))
  }
  res <- fotmob_get_match_team_stats(match_id)
  qs::qsave(path)
  res
}

safc_2022_match_team_stats <- safc_2022_matches$match_id |> 
  map_dfr(fotmob_get_match_team_stats)

match_team_stats %>%
  dplyr::select(match_id, title:dplyr::last_col()) %>%
  dplyr::glimpse()