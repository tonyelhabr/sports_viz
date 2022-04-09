
library(tidyverse)
library(rvest)

page <- 'https://en.wikipedia.org/wiki/List_of_current_Premier_League_and_English_Football_League_managers' %>% 
  read_html()
managers <- page %>% 
  html_table() %>% 
  pluck(1) %>% 
  select(
    manager = `Manager`,
    league = `Division`,
    club = `Club`
  )
managers %>% clipr::write_clip()
managers %>% 
  filter(league == 'Premier League')

res <- worldfootballR::tm_player_bio(
  'https://www.transfermarkt.us/sean-dyche/profil/spieler/13785'
)
res <- worldfootballR::tm_player_bio('https://www.transfermarkt.us/jurgen-klopp/profil/spieler/1041')
res$foot

tm_league_abbrvs <- sprintf('%s1', c('GB', 'ES', 'IT', 'L', 'FR'))
scrape_manager_bios <- function(league_abbrv) {
  page <- sprintf('https://www.transfermarkt.us/premier-league/trainer/pokalwettbewerb/%s', league_abbrv) %>% 
    read_html()
  els <- page %>% html_elements('tr > td.zentriert > a')
  idx <- els %>% html_text2() %>% str_which('to profile')
  managers <- tibble(
    player_url = els[idx] %>% html_attr('href') %>% sprintf('https://www.transfermarkt.us%s', .),
    manager = els[idx] %>% html_attr('title')
  )
  managers$bio <- worldfootballR::tm_player_bio(manager$player_url)
  managers
}

