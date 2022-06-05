
library(tidyverse)
library(glue)
library(cli)
library(qs)
library(rvest)

dir_proj <- '56-attendance'
dir_data <- file.path(dir_proj, 'data')
path_attendance <- file.path(dir_proj, 'attendance.qs')
path_importance <- file.path(dir_proj, 'importance.qs')

## output
path_venue_capacities <- file.path(dir_proj, 'venue_capacities.csv')

attendance <- path_attendance |> qs::qread()
importance <- path_importance |> qs::qread()
# venues <- attendance |> 
#   group_by(home_team, venue) |> 
#   summarize(
#     n = n(),
#     across(attendance, list(max = max), na.rm = TRUE)
#   ) |> 
#   ungroup() |> 
#   arrange(desc(max_attendance))

venues <- attendance |> 
  filter(venue |> str_detect('[Nn]eutral', negate = TRUE)) |> 
  group_by(venue) |> 
  summarize(
    n = n(),
    max_attendance = max(attendance, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  arrange(desc(max_attendance))

.parse_coord <- function(page, xpath) {
  degree <- page |> 
    rvest::html_elements(xpath = xpath) |> 
    rvest::html_text2()
  if(length(degree) == 0) {
    return(NA_integer_)
  }
  integral <- degree |> str_remove('°.*$') |> as.integer()
  decimal <- degree |> str_remove('^.*°') |> str_remove_all('[^0-9]') |> as.integer()
  sprintf('%s.%s', integral, decimal) |> as.double()
}

parse_coords <- function(info, page) {
  res <- info |> 
    filter(name == 'Coordinates')
  
  if(nrow(res) != 1) {
    lat <- page |> .parse_coord(xpath = '//*[@id="coordinates"]/span/span/a/span[1]/span/span[1]')
    long <- page |> .parse_coord(xpath = '//*[@id="coordinates"]/span/a/span[1]/span/span[2]')
    if(is.na(lat) | is.na(long)) {
      return( c(NA_real_, NA_real_))
    }
    c(lat, long)
  }
  res |> 
    pull(value) |> 
    str_replace_all('(^.*\\/)(.*)', '\\2') |> 
    str_squish() |> 
    str_split('\\; ') |> 
    pluck(1) |> 
    parse_number() |> 
    as.double()
}
possibly_parse_coords <- possibly(parse_coords, otherwise = c(NA_real_, NA_real_), quiet = FALSE)

parse_capacity <- function(info) {
  res <- info |> 
    filter(name == 'Capacity')
  
  if(nrow(res) != 1) {
    return(NA_integer_)
  }
  
  res |> 
    pull(value) |> 
    parse_number() |> 
    pluck(1) |> 
    as.integer()
}
possibly_parse_capacity <- possibly(parse_capacity, otherwise = NA_integer_, quiet = FALSE)

scrape_wiki_for_venue <- function(url) {
  
  page <- url |> rvest::read_html()
  info <- page |> 
    rvest::html_elements(css = '.infobox.vcard') |> 
    rvest::html_table() |> 
    pluck(1) |> 
    rlang::set_names(c('name', 'value'))
  
  coords <- info |> possibly_parse_coords(page)
  capacity <- info |> possibly_parse_capacity()
  
  tibble(
    lat = coords[1],
    long = coords[2],
    capacity = capacity
  )
}
possibly_scrape_wiki_for_venue <- possibly(scrape_wiki_for_venue, otherwise = tibble())
search_and_scrape_wiki_for_venue <- function(venue, overwrite = FALSE){
  
  path <- file.path(dir_data, sprintf('%s.qs', venue))
  if(file.exists(path) & !overwrite) {
    cli::cli_inform(glue::glue('{Sys.time()}: Returning early for {venue}'))
    return(qs::qread(path))
  }
  
  cli::cli_text('Searching for {venue}.')
  url <- sprintf(
    'http://en.wikipedia.org/w/api.php?action=query&list=search&srsearch=%s&format=json',
    URLencode(venue)
  )
  results <- jsonlite::fromJSON(url)
  best_page <- results$query$search |> 
    as_tibble() |> 
    slice(1) |> 
    pull(title)
  
  url <- sprintf('https://en.wikipedia.org/wiki/%s', str_replace_all(best_page, ' ', '_'))
  res <- bind_cols(
    venue = venue,
    url = url,
    possibly_scrape_wiki_for_venue(url)
  )
  qs::qsave(res, path)
  res
}

possibly_search_and_scrape_wiki_for_venue <- possibly(search_and_scrape_wiki_for_venue, otherwise = tibble(), quiet = FALSE)
slowly_search_and_scrape_wiki_for_venue <- slowly(possibly_search_and_scrape_wiki_for_venue)

wiki_capacities <- venues |> 
  filter(n > 1) |> 
  pull(venue) |> 
  unique() |> 
  setdiff(
    c('', 'ESPN Wide World of Sports Complex', 'City Stadium')
  ) %>%
  set_names(., .) %>%
  # .[1:3] |> 
  map_dfr(
    slowly_search_and_scrape_wiki_for_venue, overwrite = T
    # possibly_search_and_scrape_wiki_for_venue
  )

# unmatched_venues <- c(
#   'Maureen Hendricks Field Maryland SoccerP...', ## 1 DC United match
#   'Navy-Marine Corps Memorial Stadium', ## 1 DC United match
#   'Lamport Stadium', ## 1 Toronto FC II match
#   'StubHub Center Track & Field Stadium', ## 1 LA Galaxy match
#   'Capelli Sport Stadium', ## 1 N Caolina match
#   'ESPN Wide World of Sports Complex', ## used for MLS covid ball
#   'Las Positas College Turf Field', ## 5 Oakland matches in 2021
#   'FIU Soccer Stadium' ## 3 Miami FC matches in 2021
# )

manually_scrape_wiki_for_venue <- function(url, name, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('%s.qs', name))
  if(file.exists(path) & !overwrite) {
    cli::cli_text('Returning early for {name}.')
    return(qs::qread(path))
  }
  res <- bind_cols(
    venue = name,
    scrape_wiki_for_venue(url)
  )
  cli::cli_text('Returning data for {name}.')
  qs::qsave(res, path)
  res
}
possibly_manually_scrape_wiki_for_venue <- possibly(manually_scrape_wiki_for_venue, otherwise = tibble(), quiet = FALSE)
slowly_manually_scrape_wiki_for_venue <- slowly(possibly_manually_scrape_wiki_for_venue)

manual_capacities <- c(
  'City Stadium' = 'https://en.wikipedia.org/wiki/City_Stadium_(Richmond)',
  'Olympic Stadium' = 'https://en.wikipedia.org/wiki/Olympic_Stadium_(Montreal)',
  'Laney College Field' = 'https://en.wikipedia.org/wiki/Laney_College_Football_Stadium',
  'Orange Country Great Park Soccer Field' = 'https://en.wikipedia.org/wiki/Championship_Soccer_Stadium',
  'Phoenix Rising Soccer Complex' = 'https://en.wikipedia.org/wiki/Casino_Arizona_Field',
  'Stamford Bridge' = 'https://en.wikipedia.org/wiki/Stamford_Bridge_(stadium)',
  'Pratt & Whitney Stadium at Rentschler Fi...' = 'https://en.wikipedia.org/wiki/Pratt_%26_Whitney_Stadium_at_Rentschler_Field',
  'The Valley' = 'https://en.wikipedia.org/wiki/The_Valley_(London)',
  'Dr. Mark & Cindy Lynn Soccer Stadium' = 'https://en.wikipedia.org/wiki/Dr._Mark_%26_Cindy_Lynn_Stadium'
) |> 
  imap_dfr(
    # ~possibly_manually_scrape_wiki_for_venue(.x, .y, overwrite = T)
    ~slowly_manually_scrape_wiki_for_venue(.x, .y, overwrite = T)
  )


manual_coords <- list(
  'American Legion Memorial Stadium' = c('lat' = 35.2182, 'long' = -80.8283), 
  'Audi Field' = c('lat' = 38.869048, 'long' = -77.013001),
  'Brentford Community Stadium' = c('lat' = 51.292697, 'long' = -0.171932),
  'City Stadium' = c('lat' = 37.549697, 'long' = -77.486781),
  'Fifth Third Bank Stadium' = c('lat' = 34.028967, 'long' = -84.567626), 
  'Goodman Stadium' = c('lat' = 40.3520, 'long' = -75.2119),
  'H-E-B Park' = c('lat' = 26.2826, 'long' = -98.1352),
  'London Stadium' = c('lat' = 51.3219, 'long' = -0.059),
  'Merlo Field' = c('lat' = 45.3428, 'long' = -122.4338), 
  'New York Stadium' = c('lat' = 53.4279, 'long' = -1.362),
  'Old Trafford' = c('lat' = 53.2747, 'long' = -2.1729),
  'Papa Murphy's Park' = c('lat' = 38.3528, 'long' = -121.2617),
  'Shawnee Mission District Stadium' = c('lat' = 39.021358, 'long' = -94.67113), 
  'WakeMed Soccer Park' = c('lat' = 35.471019, 'long' = -78.451838),
  'Wembley Stadium' = c('lat' = 51.556158, 'long' = 0.279607)
) |>
  enframe('venue', 'coords') |>
  unnest_wider(coords)

venue_capacities <- venues |> 
  select(
    venue,
    max_attendance
  ) |> 
  mutate(
    across(
      max_attendance, 
      ~ifelse(is.infinite(.x), NA_integer_, .x)
    )
  ) |>
  left_join(
    bind_rows(
      wiki_capacities,
      manual_capacities
    ),
    by = 'venue'
  ) |> 
  arrange(venue) |> 
  mutate(
    max_capacity = ifelse(max_attendance > capacity, max_attendance, capacity)
  ) |> 
  filter(!is.na(max_capacity)) |> 
  left_join(
    manual_coords |> 
      rename(lat2 = lat, long2 = long),
    by = 'venue'
  ) |> 
  mutate(
    across(lat, ~coalesce(lat, lat2)),
    across(long, ~coalesce(long, long2))
  ) |> 
  select(-matches('2$'))
venue_capacities |> write_csv(path_venue_capacities, na = '')

## make a manual mapping with these
venue_teams <- attendance |> 
  group_by(team = home_team) |> 
  summarize(
    n = n()
  ) |> 
  ungroup() |> 
  arrange(team)
venue_teams |> write_csv(file.path(dir_proj, 'fbref_teams.csv'), na = '')

importance_teams <- importance |> 
  group_by(team = home_team) |> 
  summarize(
    n = n()
  ) |> 
  ungroup() |> 
  arrange(team)
importance_teams |> write_csv(file.path(dir_proj, '538_teams.csv'), na = '')

