library(rvest)
url <- 'https://en.wikipedia.org/wiki/List_of_Major_League_Soccer_stadiums'
page <- url |> read_html()

parse_capacity <- function(x, i) {
  x |>
    str_replace('(^.*)\\((.*)\\)$', sprintf('\\%d', i)) |> 
    parse_number() |> 
    as.integer()
}

parse_coord <- function(x, i) {
  x |> 
    str_replace("(^.*)([0-9]{2,3}[.][0-9]{4,7})(; )(-[0-9]{1,3}[.][0-9]{4,7})(.*$)", sprintf('\\%d', i)) |> 
    as.double()
}

mls_venues <- page |>
  html_table() |> 
  pluck(2) |> 
  transmute(
    venue = Stadium,
    team = Franchise,
    year_opened = Opened,
    raw_capacity = Capacity |> str_remove_all('\\[.*$'),
    raw_coords = Coordinates
  ) |> 
  mutate(
    reduced_capacity = raw_capacity |> parse_capacity(1),
    full_capacity = raw_capacity |> parse_capacity(2),
    lat = raw_coords |> parse_coord(2),
    long = raw_coords |> parse_coord(4)
  )

# mls_venues |> 
#   select(venue, long2 = long, lat2 = lat, reduced_capacity, full_capacity) |> 
#   anti_join(
#     venue_capacities |> 
#       slice_max(season) |> 
#       select(venue, long, lat, capacity)
#   )
# venue_capacities |> filter(venue |> str_detect('BBVA'))
