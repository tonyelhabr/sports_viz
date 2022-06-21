library(tidyverse)
library(rvest)

dir_proj <- '56-attendance'
path_venue_capacities <- file.path(dir_proj, 'venue_capacities.csv')

## outpout
path_venue_capacities_compared <- file.path(dir_proj, 'venue_capacities_compared.csv')

parse_capacity <- function(x, i) {
  x |>
    str_replace('(^.*)\\((.*)\\)$', sprintf('\\%d', i)) |> 
    parse_number() |> 
    as.integer()
}

parse_coord <- function(x, i) {
  x |> 
    str_replace("(^.*)([0-9]{2,3}[.][0-9]{4,7})(; )(-?[0-9]{1,3}[.][0-9]{4,7})(.*$)", sprintf('\\%d', i)) |> 
    as.double()
}

mls_url <- 'https://en.wikipedia.org/wiki/List_of_Major_League_Soccer_stadiums'
mls_page <- mls_url |> rvest::read_html()

mls_venue_capacities <- mls_page |>
  rvest::html_table() |> 
  pluck(2) |> 
  transmute(
    venue = Stadium,
    team = Franchise,
    year_opened = Opened,
    raw_capacity = Capacity |> str_remove_all('\\[.*$'),
    raw_coords = Coordinates
  ) |> 
  mutate(
    # reduced_capacity = raw_capacity |> parse_capacity(1),
    capacity = raw_capacity |> parse_capacity(2),
    lat = raw_coords |> parse_coord(2),
    long = raw_coords |> parse_coord(4)
  ) |> 
  select(-matches('^raw_'))

# mls_venues |>
#   select(venue, team2 = team, long2 = long, lat2 = lat, reduced_capacity, full_capacity) |>
#   inner_join(
#     latest_venue_capacities |> 
#       filter(league == 'Major League Soccer') |> 
#       select(venue, team, long, lat, capacity)
#   ) |> 
#   filter(full_capacity != capacity)
# 
# mls_venues |>
#   select(venue, team) |>
#   anti_join(
#     venue_capacities |>
#       distinct(venue)
#   )
# venue_capacities |> filter(venue |> str_detect('BBVA'))

usl_url <- 'https://en.wikipedia.org/wiki/Soccer-specific_stadium'
usl_page <- usl_url |> rvest::read_html()
usl_venue_capacities <- usl_page |>
  rvest::html_table() |> 
  pluck(4) |> 
  transmute(
    venue = Stadium,
    team = `Club(s)`,
    capacity = Capacity |> parse_number() |> as.integer()
  )

# usl_venues |> 
#   select(venue, capacity2 = capacity) |> 
#   inner_join(
#     latest_venue_capacities |>
#       filter(league == 'USL Championship') |> 
#       select(venue, long, lat, capacity)
#   )
# 
# usl_venues |> 
#   select(venue, team) |> 
#   anti_join(
#     latest_venue_capacities |>
#       distinct(venue)
#   )

corrected_efl_venue_capacities <- tibble(
  venue = 'New York Stadium',
  lat = 53.4279,
  long = 1.362
)

retrieve_english_venues <- function(league) {
  i <- switch(
    league,
    'EFL' = 2,
    'EPL' = 1
  )
  
  league_name <- switch(
    league,
    'EFL' = 'EFL_Championship',
    'EPL' = 'Premier_League'
  )
  url <- sprintf('https://en.wikipedia.org/wiki/List_of_%s_stadiums', league_name)
  page <- url |> rvest::read_html()
  page |>
    rvest::html_table() |> 
    pluck(i) |> 
    filter(is.na(Closed) | Closed >= 2016) |> 
    transmute(
      venue = Stadium,
      team = Club,
      year_opened = Opened |> str_sub(1, 4) |> as.integer(),
      year_closed = Closed |> as.integer(),
      capacity = Capacity |> parse_number() |> as.integer(),
      raw_coords = Coordinates
    ) |> 
    mutate(
      lat = raw_coords |> parse_coord(2),
      long = raw_coords |> parse_coord(4)
    ) |> 
    select(-raw_coords) 
}

epl_venue_capacities <- efl_venues <- retrieve_english_venues('EPL')
efl_venue_capacities <- retrieve_english_venues('EFL') |> 
  left_join(
    corrected_efl_venue_capacities |> rename(lat2 = lat, long2 = long)
  ) |> 
  mutate(
    across(lat, ~coalesce(lat2, .x)),
    across(long, ~coalesce(long2, .x))
  ) |> 
  select(-matches('2$'))

venue_capacities2 <- bind_rows(
  epl_venue_capacities |> mutate(league = 'EPL'),
  efl_venue_capacities |> mutate(league = 'EFL'),
  mls_venue_capacities |> mutate(league = 'MLS'),
  usl_venue_capacities |> mutate(league = 'USL')
)

## TODO: split this into a second script
league_mapping <- tibble(
  league_fbref = c('Premier League', 'EFL Championship', 'Major League Soccer', 'USL Championship'),
  league_538 = c('Barclays Premier League', 'English League Championship', 'Major League Soccer', 'United Soccer League'),
  league_abbrv = c('EPL', 'EFL', 'MLS', 'USL')
)

latest_venue_capacities <- venue_capacities |>
  arrange(venue, season) |>
  group_by(venue) |>
  slice_max(season, n = 1, with_ties = FALSE) |>
  ungroup()

venue_capacities_compared <- latest_venue_capacities |> 
  inner_join(league_mapping |> select(league = league_fbref, league_abbrv), by = 'league') |> 
  select(-league) |> 
  rename(league = league_abbrv) |> 
  select(venue, league, team1 = team, capacity1 = capacity, n1 = n, lat1 = lat, long1 = long) |> 
  full_join(
    venue_capacities2 |> 
      select(venue, league, team2 = team, capacity2 = capacity, year_opened2 = year_opened, year_closed2 = year_closed, lat2 = lat, long2 = long),
    by = c('venue', 'league')
  ) |> 
  mutate(
    team = coalesce(team1, team2)
  ) |> 
  arrange(team) |> 
  select(-team) |> 
  select(venue, league, team1, team2, capacity1, capacity2, lat1, lat2, long1, long2, n1, year_opened2, year_closed2) |> 
  mutate(which = case_when(is.na(team1) ~ "second", is.na(team2) ~ "first", TRUE ~ "both"), .before = 1)
venue_capacities_compared |> write_csv(path_venue_capacities_compared, na = '')
