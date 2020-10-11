
.get_paths_possible <- memoise::memoise({function() {
  res <-
    seasons_links %>% 
    filter(season >= '2009-10') %>% 
    crossing(suffix = 'Overall') %>% 
    mutate(
      path = sprintf('%s/%s_%s_%s.json', .dir_data, league, str_replace(season, '\\/', '-'), suffix)
    )
}})


.get_paths_existing <- function() {
  res <-
    fs::dir_ls(
      .dir_data,
      regexp = 'Overall[.]json$'
    ) %>% 
    fs::file_info() %>% 
    mutate(across(size, as.integer)) %>% 
    filter(size > 2000) %>% 
    select(path)
  res
}

.check_paths <- function(paths_possible = .get_paths_possible(), paths_existing = .get_paths_existing()) {
  res <- paths_possible %>% anti_join(paths_existing)
}

# NOTE: Would need to pass `dr` into this if these hadn't all been scraped yet.
seasons_links <- 
  .get_leagues_valid() %>% 
  tibble(league = .) %>% 
  # tail(1) %>% 
  mutate(links = map(league, ~.retrieve_seasons_links(league = .x, overwrite = FALSE))) %>% 
  unnest(links)
keep_going <- TRUE
i <- 1

while(keep_going) {
  if(exists('dr')) {
    dr$closeall()
    rm('dr')
  }
  dr <- .initialize_driver()
  paths_todo_before <- .check_paths()
  n_paths_todo_before <- paths_todo_before %>% nrow()
  cat(cli::cli_text(glue::glue('{n_paths_todo_before} urls to scrape (loop {i}).')))
  res <-
    paths_todo_before %>% 
    # slice(1) %>% 
    mutate(
    data = 
      pmap(
        list(league = league, link = link, suffix = suffix), 
        ~scrape_season_possibly(dr = dr, league = ..1, link = ..2, suffix = ..3, overwrite = FALSE)
      )
  )
  invisible(dr$close())
  paths_todo_after <- .check_paths()
  n_paths_todo_after <- paths_todo_after %>% nrow()
  n_paths_diff <- n_paths_todo_before - n_paths_todo_after
  if(n_paths_todo_after == 0L) {
    keep_going <- FALSE
    cat(cli::cli_alert_success('No more urls to scrape.'))
  } else if(n_paths_diff == 0L) {
    cat(cli::cli_alert_warning(glue::glue('Stopping even though there are {n_paths_todo_after} more urls to scrape. because no urls were scraped in loop {i}.')))
    keep_going <- FALSE
  } else {
    cat(cli::cli_text(glue::glue('{n_paths_todo_after} more urls to scrape. (Successfully scraped {n_paths_diff} urls.)')))
  }
  i <- i + 1
}
# url<- glue::glue('{url_base}/Regions/252/Tournaments/2/England-Premier-League')
# dr$navigate(url)
# dr$screenshot(display = TRUE)

# direct httr attempt ----
# https://www.whoscored.com/Regions/206/Tournaments/4/Seasons/2596/Stages/4624/RefereeStatistics/Spain-LaLiga-2010-2011
# res <- httr::GET('https://www.whoscored.com/RefereesFeed/4624/TournamentStatsByReferee?fieldString=Away')
