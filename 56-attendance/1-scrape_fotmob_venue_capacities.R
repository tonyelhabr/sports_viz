library(googlesheets4)
library(worldfootballR)
library(tidyverse)
library(httr)

dir_proj <- '56-attendance'
dir_data <- file.path(dir_proj, 'data')
dir.create(dir_data, showWarnings = FALSE)

sheet <- read_sheet(ss = '1nIw1PgozYsb11W-QSzjgPlW-qrsJWLiPB8x6fTTMqiI')
mapping_us_teams <- sheet |> 
  filter(league %in% c('mls', 'usl')) |> 
  distinct(team = team_fotmob)
fotmob_us_teams <- fotmob_get_league_tables(league_id = c(130, 8972, 9296)) |> 
  filter(table_type == 'all') |> 
  distinct(league_id, team = name, id, team_page_url)

fotmob_us_team_ids <- mapping_us_teams |> 
  inner_join(fotmob_us_teams) |> 
  arrange(league_id, team)

unmatched_fotmob_us_team_ids <- mapping_us_teams |> 
  anti_join(fotmob_us_teams) |> 
  arrange(team)

fotmob_build_id <- worldfootballR:::.fotmob_get_build_id()
scrape_fotmob_capacity <- function(team_id, team, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('fotmob_capacity-%s.qs', team))
  if(file.exists(path) & !overwrite) {
    cli::cli_text('Returning early for {team}.')
    return(qs::qread(path))
  }

  team_url <- sprintf(
    'https://www.fotmob.com/_next/data/%s/teams/%s/overview/%s.json',
    fotmob_build_id, team_id, str_replace_all(tolower(team), ' ', '-')
  )
  team_json <- team_url |> jsonlite::fromJSON()
  team_element <- team_json$pageProps$initialState$team
  team_id <- names(team_element)
  venue_element <- team_element[[team_id]]$data$venue$statPairs
  capacity <- venue_element |>
    as_tibble() |> 
    filter(V1 == 'Capacity') |> 
    pull(V2) |> 
    parse_number()
  
  res <- tibble(
    team_id = team_id,
    team = team,
    capacity = capacity
  )
  
  qs::qsave(res, path)
  cli::cli_text('Returning data for {team}.')
  res
}

possibly_scrape_fotmob_capacity <- possibly(scrape_fotmob_capacity, otherwise = tibble(), quiet = FALSE)
slowly_scrape_fotmob_capacity <- slowly(possibly_scrape_fotmob_capacity)

search_fotmob_capacity <- function(team, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('fotmob_search_capacity-%s.qs', team))
  if(file.exists(path) & !overwrite) {
    cli::cli_text('Returning early for {team}.')
    return(qs::qread(path))
  }
  
  resp <- sprintf('https://apigw.fotmob.com/searchapi/suggest?term=%s', tolower(team)) |> 
    URLencode() |> 
    httr::GET()
  
  team_id <- resp |> 
    httr::content() |> 
    enframe() |> 
    filter(name == 'teamSuggest') |> 
    select(value) |> 
    unnest(value) |> 
    unnest_wider(value) |> 
    select(options) |> 
    unnest(options) |> 
    unnest_wider(options) |> 
    slice(1) |> 
    pull(text) |> 
    str_remove_all('(.*\\|)')

  res <- scrape_fotmob_capacity(team_id = team_id, team)
  qs::qsave(res, path)
  res
}

possibly_search_fotmob_capacity <- possibly(search_fotmob_capacity, otherwise = tibble(), quiet = FALSE)
slowly_search_fotmob_capacity <- slowly(possibly_search_fotmob_capacity)

fotmob_us_capacities <- setNames(fotmob_us_team_ids$id, fotmob_us_team_ids$team) |> 
  imap_dfr(~slowly_scrape_fotmob_capacity(.x, .y))

searched_fotmob_us_capacities <- unmatched_fotmob_us_team_ids$team |> 
  map_dfr(~slowly_search_fotmob_capacity(.x))

all_fotmob_us_capacities <- bind_rows(
  fotmob_us_capacities,
  searched_fotmob_us_capacities
) |> 
  arrange(team)

write_csv(
  all_fotmob_us_capacities,
  file.path(dir_data, 'fotmob_venue_capacities.csv'),
  na = ''
)
          