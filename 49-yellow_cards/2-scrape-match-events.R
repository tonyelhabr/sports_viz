library(tidyverse)
library(janitor)
library(glue)
library(qs)

dir_proj <- '49-yellow_cards'
dir_data <- file.path(dir_proj, 'data')

urls <- qs::qread(file.path(dir_data, 'urls.qs'))

scrape_f <- function(url, f, prefix, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('%s-%s.rds', prefix, basename(url)))
  suffix <- glue::glue('for `url = "{url}"`.')
  if(file.exists(path) & !overwrite) {
    cat(glue::glue('{Sys.time()}: Returning early {suffix}'), sep = '\n')
    return(read_rds(path))
  }
  res <- f(url)
  # cat(glue::glue('{Sys.time()}: Retrieved data {suffix}'), sep = '\n')
  write_rds(res, path)
  res
}

scrape_match_summary <- partial(
  scrape_f,
  f = worldfootballR::fb_match_summary,
  prefix = 'summary',
  ... = 
)

scrape_match_lineups <- partial(
  scrape_f,
  f = worldfootballR::fb_match_lineups,
  prefix = 'lineups',
  ... = 
)

possibly_scrape_match_summary <- possibly(scrape_match_summary, otherwise = tibble())
possibly_scrape_match_lineups <- possibly(scrape_match_lineups, otherwise = tibble())

summaries <- urls$url |> 
  set_names() |> 
  map_dfr(possibly_scrape_match_summary, .id = 'match_url') |>
  janitor::clean_names() |> 
  as_tibble()
  
lineups <- urls$url |> 
  set_names() |> 
  map_dfr(possibly_scrape_match_lineups, .id = 'match_url') |>
  janitor::clean_names() |> 
  as_tibble() |> 
  transmute(
    link = match_url,
    matchday, 
    team, 
    player = player_name,
    pos,
    min,
    yellow_cards = crd_y,
    red_cards = crd_r,
    is_starter = ifelse(starting == 'Pitch', TRUE, FALSE)
  )
qs::qsave(summaries, file.path(dir_data, 'summeries.qs'))
qs::qsave(lineups, file.path(dir_data, 'lineups.qs'))

