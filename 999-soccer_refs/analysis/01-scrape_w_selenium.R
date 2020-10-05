
dr <- .initialize_driver()
seasons_links <- 
  .get_leagues_valid() %>% 
  tibble(league = .) %>% 
  # tail(1) %>% 
  mutate(links = map(league, ~.retrieve_seasons_links(dr = dr, league = .x, overwrite = FALSE))) %>% 
  unnest(links)
# dr$navigate(.url_base)
# dr$screenshot(display = TRUE)
res <-
  seasons_links %>% 
  filter(season >= '2009-10') %>% 
  crossing(suffix = 'Overall') %>% 
  filter(league == 'Seria A') %>% 
  mutate(
    data = 
      pmap(
        list(league = league, link = link, suffix = suffix), 
        ~scrape_season_possibly(dr = dr, league = ..1, link = ..2, suffix = ..3, overwrite = FALSE)
      )
  )
# url<- glue::glue('{url_base}/Regions/252/Tournaments/2/England-Premier-League')
# dr$navigate(url)
# dr$screenshot(display = TRUE)

# direct httr attempt ----
# https://www.whoscored.com/Regions/206/Tournaments/4/Seasons/2596/Stages/4624/RefereeStatistics/Spain-LaLiga-2010-2011
# res <- httr::GET('https://www.whoscored.com/RefereesFeed/4624/TournamentStatsByReferee?fieldString=Away')
