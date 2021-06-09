
library(tidyverse)
library(rvest)
url <- 'https://www.skysports.com/football/news/19692/12309182/euro-2020-squads-confirmed-squad-lists-and-premier-league-players-at-tournament'
page <- url %>% xml2::read_html()

h3 <- page %>% html_nodes('h3')
h3
countrys <-
  h3 %>% 
  html_text() %>% 
  # .[1:20] %>% 
  str_subset('GROUP|See|Win|Get', negate = TRUE)
countrys
page %>% html_nodes('p') %>% html_nodes('strong')
nodes_p <- page %>% html_nodes('p')
text_p <- nodes_p %>% html_text()
idx_filt <- 7:(7+98)
nodes_filt <- nodes_p[idx_filt]

lst <-
  nodes_filt %>% 
  html_text2(preserve_nbsp = TRUE) %>% 
  str_split(pattern = '\n')
lst[2]

f_strip <- function(x, rgx = 'Goalkeepers') {
  rgx <- sprintf('%s[:] ', rgx)
  res_init <- x %>% keep(~str_detect(.x, rgx))
  if(length(res_init) == 0L) {
    return(NA_character_)
  }
  res_init %>% str_remove(rgx)
}

do_f_strip <- function(rgx) {
  res <- lst %>% map_chr(~f_strip(.x, rgx))
  res[!is.na(res)]
}

agg <-
  tibble(
    g = do_f_strip('Goalkeepers'),
    d = do_f_strip('Defenders'),
    m = do_f_strip('Midfielders'),
    f = do_f_strip('Forwards')
  ) %>% 
  bind_cols(tibble(country = countrys), .)
agg

f_separate <- function(pos) {
  pos_sym <- sym(pos)
  f_replace <- function(x, i) {
    str_replace(x, '(^.*)\\s\\((.*)\\)$', sprintf('\\%s', i))
  }
  agg %>% 
    select(country, !!pos_sym) %>% 
    separate_rows(!!pos_sym, sep = '\\,\\s+') %>% 
    set_names(c('country', 'player')) %>% 
    mutate(
      across(
        player,
        list(
          name = ~f_replace(.x, 1),
          team = ~f_replace(.x, 2)
        ),
        .names = '{fn}'
      ),
      pos = !!pos
    ) %>% 
    select(country, team_sky = team, name, pos)
}

players <- c('g', 'm', 'd', 'f') %>% map_dfr(f_separate)

team_mapping <-
  tibble(
    team_sky = c('Arsenal', 'Aston Villa', 'Brighton', 'Burnley', 'Chelsea', 'Crystal Palace', 'Everton', 'Fulham', 'Leeds', 'Leicester', 'Liverpool', 'Man City', 'Manchester City', 'Manchester United', 'Newcastle', 'Newcastle United', 'Sheffield United', 'Southampton', 'Tottenham', 'West Brom', 'West Ham', 'Wolves'),
    team = c('Arsenal', 'Aston Villa', 'Brighton', 'Burnley', 'Chelsea', 'Crystal Palace', 'Everton', 'Fulham', 'Leeds', 'Leicester', 'Liverpool', 'Man City', 'Man City', 'Man United', 'Newcastle Utd', 'Newcastle Utd', 'Sheffield Utd', 'Southampton', 'Tottenham', 'West Brom', 'West Ham', 'Wolves')
  )

players_filt <-
  players %>% 
  inner_join(team_mapping)
players_filt

teams_n <-
  players_filt %>% 
  count(team, sort = TRUE)
teams_n

countrys_n <-
  players_filt %>% 
  count(country, sort = TRUE)
countrys_n
