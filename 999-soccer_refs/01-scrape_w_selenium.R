
source('999-soccer_refs/R/functions.R')

# main ----

dr <- .initialize_driver()
seasons_links <- .retrieve_seasons_links(dr = dr, league = 'EPL')

# url<- glue::glue('{url_base}/Regions/252/Tournaments/2/England-Premier-League')
# dr$navigate(url)
# dr$screenshot(display = TRUE)