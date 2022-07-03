library(tidyverse)
library(worldfootballR)
library(glue)
library(cli)
library(qs)
library(janitor)
library(readr)
library(lubridate)
library(httr)
library(googlesheets4)

dir_proj <- '56-attendance'
dir_data <- file.path(dir_proj, 'data')
dir_img <- file.path(dir_proj, 'img')
dir.create(dir_data, showWarnings = FALSE)
dir.create(dir_img, showWarnings = FALSE)

## outputs ----
path_attendance <- file.path(dir_proj, 'attendance.qs')
path_importance <- file.path(dir_proj, 'importance.qs')
path_team_logos <- file.path(dir_proj, 'team_logos.qs')
path_venue_capacities <- file.path(dir_proj, 'venue_capacities.qs')

## start ----
params <- crossing(
  country = c('USA', 'ENG'),
  gender = 'M',
  season_end_year = c(2016:2022),
  tier = c('1st', '2nd')
)

## attendance ----
get_team_urls <- function(...) {
  res <- worldfootballR::fb_league_urls(...)
  
  if(length(res) == 0) {
    return(tibble())
  }
  urls <- res |> 
    worldfootballR::fb_teams_urls()
  tibble(url = urls)
}
possibly_get_team_urls <- possibly(get_team_urls, otherwise = tibble(), quiet = TRUE)

scrape_team_urls <- function(country, gender, season_end_year, tier, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('team_urls-%s-%s-%s-%s.qs', country, gender, season_end_year, tier))
  suffix <- glue::glue('for `country = {country}`, `gender = "{gender}"`, `season_end_year = "{season_end_year}"`, `tier = "{tier}"`.')
  if(file.exists(path) & !overwrite) {
    cli::cli_text('{Sys.time()}: Returning early {suffix}')
    return(qs::qread(path))
  }
  res <- possibly_get_team_urls(country, gender, season_end_year, tier)
  cli::cli_text('{Sys.time()}: Retrieved data {suffix}')
  qs::qsave(res, path)
  res
}
possibly_scrape_team_urls <- possibly(scrape_team_urls, otherwise = tibble(), quiet = FALSE)

team_urls <- params |> 
  mutate(
    urls = 
      pmap(
        list(country, gender, season_end_year, tier), 
        ~possibly_scrape_team_urls(..1, ..2, ..3, ..4)
      )
  ) |> 
  unnest(urls)

scrape_fbref_team_img <- function(url, name, overwrite = FALSE) {
  path_img <- file.path(dir_img, sprintf('%s.png', name))
  path <- file.path(dir_data, sprintf('fbref_team_img-%s.qs', name))
  suffix <- sprintf('for %s.', name)
  if(file.exists(path) & !overwrite) {
    cli::cli_text('{Sys.time()}: Returning early {suffix}')
    return(qs::qread(path))
  }
  page <- rvest::read_html(url)
  team <- page |> 
    rvest::html_element(xpath = '//*[@id="inner_nav"]/ul/li[1]/a') |> 
    rvest::html_text2() |> 
    str_remove('\\sStats & History')
  img_url <- page |> 
    rvest::html_element(xpath = '//*[@id="meta"]/div[1]/img') |>
    rvest::html_attr('src')
  if(!file.exists(path_img)) {
    download.file(img_url, destfile = path_img, mode = 'wb', quiet = TRUE)
  }
  res <- tibble(
    url = url,
    name = name,
    team = team,
    path = path_img
  )
  cli::cli_text('{Sys.time()}: Retrieved data {suffix}')
  qs::qsave(res, path)
  res
}

possibly_scrape_fbref_team_img <- possibly(scrape_fbref_team_img, otherwise = tibble(), quiet = FALSE)
slowly_scrape_fbref_team_img <- slowly(possibly_scrape_fbref_team_img)

## logic used in worldfootballR
extract_team_name <- function(url) {
  sub('.*\\/', '', url) %>% gsub('-Stats', '', .) %>% gsub('-', ' ', .)
}

team_logos <- team_urls |> 
  mutate(
    season = url |> dirname() |> basename(),
    name = url |> extract_team_name(),
    .before = 1
  ) |> 
  group_by(name) |> 
  slice_max(season, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  distinct(name, url) |> 
  deframe() |> 
  imap_dfr(
    ~possibly_scrape_fbref_team_img(.x, .y)
    # ~slowly_scrape_fbref_team_img(.x, .y)
  )
# team_logos |> select(url, name) |> write_csv(file.path(dir_data, 'fbref_logo_teams.csv'), na = '')
team_logos |> qs::qsave(path_team_logos)

possibly_get_match_results <- possibly(worldfootballR::get_match_results, otherwise = tibble())

scrape_results <- function(country, gender, season_end_year, tier, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('results-%s-%s-%s-%s.qs', country, gender, season_end_year, tier))
  suffix <- glue::glue('for `country = {country}`, `gender = "{gender}"`, `season_end_year = "{season_end_year}"`, `tier = "{tier}"`.')
  if(file.exists(path) & !overwrite) {
    cli::cli_text('{Sys.time()}: Returning early {suffix}')
    return(qs::qread(path))
  }
  res <- possibly_get_match_results(country, gender, season_end_year, tier)
  cli::cli_text('{Sys.time()}: Retrieved data {suffix}')
  qs::qsave(res, path)
  res
}

possibly_scrape_results <- possibly(scrape_results, otherwise = tibble(), quiet = FALSE)

results <- params %>% 
  mutate(
    data = 
      pmap(
        list(country, gender, season_end_year, tier), 
        possibly_scrape_results
      )
  )

attendance <- results %>% 
  unnest(data) |> 
  janitor::clean_names() |> 
  filter(venue |> str_detect('Neutral Site', negate = TRUE)) |> 
  mutate(
    # across(venue, ~str_remove(.x, '\\s+\\(Neutral Site\\)')),
    across(
      venue,
      ~case_when(
        home == 'Saint Louis' & venue == 'Toyota Stadium' ~ 'World Wide Technology Soccer Park',
        TRUE ~ .x
      )
    ),
    across(
      attendance,
      ~case_when(
        venue == 'Al Lang Stadium' & date == lubridate::ymd('2022-03-26') ~ round(.x / 10),
        venue == 'Cheney Stadium' & date == lubridate::ymd('2019-08-09') ~ round(.x / 10),
        TRUE ~ .x
      )
    )
  ) |> 
  select(
    country,
    gender,
    league = competition_name,
    season = season_end_year,
    date,
    wk,
    day,
    home_team = home,
    away_team = away,
    home_goals,
    away_goals,
    attendance,
    venue
  ) |> 
  arrange(season, league, date, home_team)
attendance |> qs::qsave(path_attendance)

## importance ----
matches_538 <-  read_csv(
  'https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv',
  col_types = cols(
    .default = col_double(),
    date = col_date(format = ''),
    league = col_character(),
    team1 = col_character(),
    team2 = col_character()
  )
)

importance <- matches_538 |> 
  filter(
    league %in% c(
      'Barclays Premier League',
      'English League Championship',
      'Major League Soccer',
      'United Soccer League'
    )
  ) |> 
  rename_with(
    ~str_replace(.x, '(^.*)1$', 'home_\\1'), 
    matches('1$')
  ) %>% 
  rename_with(
    ~str_replace(.x, '(^.*)2$', 'away_\\1'), 
    matches('2$')
  ) %>% 
  select(
    season,
    date,
    league,
    home_team,
    away_team,
    home_importance,
    away_importance
  ) |> 
  arrange(season, league, date, home_team) 
importance |> qs::qsave(path_importance)

## fbref_venue_capacities ----
fbref_venues <- attendance |> 
  group_by(venue) |> 
  summarize(
    n_matches = n(),
    max_attendance = max(attendance, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  left_join(
    attendance |> 
      count(venue, team = home_team, name = 'n_matches_team') |> 
      group_by(venue) |> 
      slice_max(n_matches_team, n = 1, with_ties = FALSE) |> 
      ungroup(),
    by = 'venue'
  ) |> 
  select(venue, team, n_matches, n_matches_team, max_attendance) |> 
  arrange(desc(max_attendance))

.parse_coord <- function(page, xpath) {
  degree <- page |> 
    rvest::html_elements(xpath = xpath) |> 
    rvest::html_text2()
  if(length(degree) == 0) {
    return(NA_integer_)
  }
  parts <- str_split(degree, '[^0-9]')
  sprintf('%s.%s', parts[1], parts[2] / 60 + parts[3] / 3600) |> as.double()
}

parse_coords <- function(info, page) {
  res <- info |> 
    filter(name == 'Coordinates')
  
  if(nrow(res) != 1) {
    lat <- page |> .parse_coord(xpath = '//*[@id="coordinates"]/span/span/a/span[1]/span/span[1]')
    long <- page |> .parse_coord(xpath = '//*[@id="coordinates"]/span/a/span[1]/span/span[2]')
    if(is.na(lat) | is.na(long)) {
      return(c(NA_real_, NA_real_))
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
    cli::cli_text('Returning early for {venue}.')
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

manually_scrape_wiki_for_venue <- function(url, name, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('%s.qs', name))
  if(file.exists(path) & !overwrite) {
    cli::cli_text('Returning early for {name}.')
    return(qs::qread(path))
  }
  res <- bind_cols(
    venue = name,
    url = url,
    scrape_wiki_for_venue(url)
  )
  cli::cli_text('Returning data for {name}.')
  qs::qsave(res, path)
  res
}
possibly_manually_scrape_wiki_for_venue <- possibly(manually_scrape_wiki_for_venue, otherwise = tibble(), quiet = FALSE)
slowly_manually_scrape_wiki_for_venue <- slowly(possibly_manually_scrape_wiki_for_venue)

manual_wiki_url_capacities <- c(
  'City Stadium' = 'https://en.wikipedia.org/wiki/City_Stadium_(Richmond)',
  'Olympic Stadium' = 'https://en.wikipedia.org/wiki/Olympic_Stadium_(Montreal)',
  'Laney College Field' = 'https://en.wikipedia.org/wiki/Laney_College_Football_Stadium',
  'Orange Country Great Park Soccer Field' = 'https://en.wikipedia.org/wiki/Championship_Soccer_Stadium',
  'Phoenix Rising Soccer Complex' = 'https://en.wikipedia.org/wiki/Casino_Arizona_Field',
  'Stamford Bridge' = 'https://en.wikipedia.org/wiki/Stamford_Bridge_(stadium)',
  'Pratt & Whitney Stadium at Rentschler Fi...' = 'https://en.wikipedia.org/wiki/Pratt_%26_Whitney_Stadium_at_Rentschler_Field',
  'The Valley' = 'https://en.wikipedia.org/wiki/The_Valley_(London)',
  'Dr. Mark & Cindy Lynn Soccer Stadium' = 'https://en.wikipedia.org/wiki/Dr._Mark_%26_Cindy_Lynn_Stadium',
  'Toyota Stadium' = 'https://en.wikipedia.org/wiki/World_Wide_Technology_Soccer_Park' ## same name as FC Dallas
) |> 
  imap_dfr(
    ~possibly_manually_scrape_wiki_for_venue(.x, .y)
    # ~slowly_manually_scrape_wiki_for_venue(.x, .y, overwrite = TRUE)
  )

manual_wiki_venues <- tibble(
  venue = 'Toyota Stadium (Missouri)',
  # league = 'Major League Soccer',
  team = 'Saint Louis'
)

wiki_capacities <- fbref_venues |> 
  filter(n_matches > 1) |> 
  pull(venue) |> 
  unique() |> 
  setdiff(
    c('', manual_wiki_url_capacities |> distinct(venue) |> pull(venue))
  ) %>%
  set_names(., .) %>%
  map_dfr(
    possibly_search_and_scrape_wiki_for_venue
    # slowly_search_and_scrape_wiki_for_venue, overwrite = T
  )

# unmatched_venues <- c(
#   'Maureen Hendricks Field Maryland SoccerP...', ## 1 DC United match
#   'Navy-Marine Corps Memorial Stadium', ## 1 DC United match
#   'Lamport Stadium', ## 1 Toronto FC II match
#   'StubHub Center Track & Field Stadium', ## 1 LA Galaxy match
#   'Capelli Sport Stadium', ## 1 n_matches Caolina match
#   'ESPN Wide World of Sports Complex', ## used for MLS covid ball
#   'Las Positas College Turf Field', ## 5 Oakland matches in 2021
#   'FIU Soccer Stadium' ## 3 Miami FC matches in 2021
# )

corrected_capacities <- list(
  'Bold Stadium' = 5036,
  'First Tennessee Park' = 10000,
  'Orange Country Great Park Soccer Field' = 5000
) |> 
  enframe('venue', 'capacity') |> 
  unnest(capacity)

changed_capacities <- list(
  'Highmark Stadium' = tibble(
    'season' = c(2018, 2019),
    'capacity' = c(3500, 5000)
  ),
  'Nippert Stadium' = tibble(
    'season' = c(2018, 2021),
    'capacity' = c(37978, 40000)
  ),
  'Phoenix Rising Soccer Complex' = tibble(
    'season' = c(2018, 2021),
    'capacity' = c(6000, 10000)
  )
) |> 
  enframe('venue', 'capacity') |> 
  unnest(capacity)

manual_wiki_coords <- list(
  'American Legion Memorial Stadium' = c('lat' = 35.2182, 'long' = -80.8283), 
  'Audi Field' = c('lat' = 38.869048, 'long' = -77.013001),
  'Brentford Community Stadium' = c('lat' = 51.49083, 'long' = -0.2886111),
  'City Stadium' = c('lat' = 37.549697, 'long' = -77.486781),
  'Fifth Third Bank Stadium' = c('lat' = 34.028967, 'long' = -84.567626), 
  'Goodman Stadium' = c('lat' = 40.3520, 'long' = -75.2119),
  'H-E-B Park' = c('lat' = 26.2826, 'long' = -98.1352),
  'London Stadium' = c('lat' = 51.3219, 'long' = -0.059),
  'Merlo Field' = c('lat' = 45.3428, 'long' = -122.4338), 
  'New York Stadium' = c('lat' = 53.4279, 'long' = -1.362),
  'Old Trafford' = c('lat' = 53.2747, 'long' = -2.1729),
  "Papa Murphy's Park" = c('lat' = 38.3528, 'long' = -121.2617),
  'Shawnee Mission District Stadium' = c('lat' = 39.021358, 'long' = -94.67113), 
  'WakeMed Soccer Park' = c('lat' = 35.471019, 'long' = -78.451838),
  'Wembley Stadium' = c('lat' = 51.556158, 'long' = 0.279607)
) |>
  enframe('venue', 'coords') |>
  unnest_wider(coords)

# fix_coord <- function(x) {
#   p0 <- round(x)
#   p1 <- as.integer(str_sub(round(x * 100), -2, -1))
#   p2 <- as.integer(str_sub(round(x * 10000), -2, -1))
#   p0 + p1 / 60 + p2 / 3600
# }
# 
# manual_wiki_coords |> 
#   select(venue, lat) |> 
#   deframe() |> 
#   map_dbl(fix_coord)


fbref_venue_capacities <- fbref_venues |> 
  select(
    venue,
    # league,
    team,
    n_matches,
    n_matches_team,
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
      manual_wiki_url_capacities
    ),
    by = 'venue'
  ) |> 
  left_join(
    manual_wiki_coords |> 
      rename(lat2 = lat, long2 = long),
    by = 'venue'
  ) |> 
  mutate(
    across(lat, ~coalesce(lat, lat2)),
    across(long, ~coalesce(long, long2))
  ) |> 
  select(-matches('2$')) |> 
  left_join(
    corrected_capacities |> rename(capacity2 = capacity),
    by = 'venue'
  ) |> 
  mutate(
    across(capacity, ~coalesce(capacity2, capacity))
  ) |> 
  select(-capacity2) |> 
  filter(venue != '', !is.na(venue)) |> 
  left_join(
    changed_capacities |> rename(capacity2 = capacity),
    by = 'venue'
  ) |> 
  mutate(
    across(capacity, ~coalesce(capacity2, capacity))
  ) |> 
  select(-capacity2) |> 
  arrange(venue)
fbref_venue_capacities

## fotmob venue capacities ----
read_mapping <- function(sheet) {
  read_sheet(ss = '1nIw1PgozYsb11W-QSzjgPlW-qrsJWLiPB8x6fTTMqiI', sheet = sheet)
} 

team_mapping <- read_mapping('teams')
venue_mapping <- read_mapping('venues')

fotmob_team_mapping <- team_mapping |> 
  distinct(team = team_fotmob)

fotmob_teams <- fotmob_get_league_tables(league_id = c(47, 48, 108, 130, 8972, 9296)) |> 
  filter(table_type == 'all') |> 
  distinct(league_id, team = name, id, team_page_url)

fotmob_team_ids <- fotmob_team_mapping |> 
  inner_join(fotmob_teams) |> 
  arrange(league_id, team)

unmatched_fotmob_team_ids <- fotmob_team_mapping |> 
  anti_join(fotmob_teams) |> 
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
  
  venue_element <- team_element[[team_id]]$data$venue
  widget_element <- venue_element$widget
  
  stat_df <- venue_element$statPairs |>
    as_tibble() |> 
    pivot_wider(names_from = V1, values_from = V2) |> 
    janitor::clean_names() |> 
    mutate(
      across(matches('capacity|opened'), parse_number)
    )
  
  res <- tibble(
    team_id = team_id,
    team = team,
    venue = widget_element$name,
    lat = parse_number(widget_element$location[1]),
    long = parse_number(widget_element$location[2]),
    city = widget_element$city
  ) |> 
    bind_cols(stat_df)
  
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
  
  res <- scrape_fotmob_capacity(team_id = team_id, team = team, overwrite = TRUE)
  qs::qsave(res, path)
  res
}

possibly_search_fotmob_capacity <- possibly(search_fotmob_capacity, otherwise = tibble(), quiet = FALSE)
slowly_search_fotmob_capacity <- slowly(possibly_search_fotmob_capacity)

fotmob_capacities <- setNames(fotmob_team_ids$id, fotmob_team_ids$team) |> 
  imap_dfr(
    ~possibly_scrape_fotmob_capacity(.x, .y)
    # ~slowly_scrape_fotmob_capacity(.x, .y, overwrite = TRUE)
  )

searched_fotmob_capacities <- unmatched_fotmob_team_ids$team |> 
  map_dfr(
    ~possibly_search_fotmob_capacity(.x)
    # ~slowly_search_fotmob_capacity(.x, overwrite = TRUE)
  )

fotmob_venue_capacities <- bind_rows(
  fotmob_capacities,
  searched_fotmob_capacities
) |> 
  arrange(team)

joined_venue_mapping <- venue_mapping |> 
  # filter(venue_fbref |> str_detect('Coolray')) |> 
  left_join(
    fbref_venue_capacities |> 
      filter(is.na(season)) |> 
      distinct(venue_fbref = venue, team_fbref = team, capacity_wiki = capacity, lat_wiki = lat, long_wiki = long) |> 
      left_join(
        fbref_venue_capacities |>
          filter(is.na(season)) |>
          group_by(team) |>
          slice_max(n_matches, n = 1, with_ties = FALSE) |>
          ungroup() |>
          transmute(venue_fbref = venue, team_fbref = team, is_primary = TRUE),
        by = c('venue_fbref', 'team_fbref')
      ),
    by = 'venue_fbref'
  ) |> 
  left_join(
    fbref_venue_capacities |> 
      filter(!is.na(season)) |> 
      distinct(season, venue_fbref = venue, capacity_wiki_season = capacity), 
    by = 'venue_fbref'
  ) |> 
  left_join(
    fbref_venue_capacities |> 
      mutate(prop_matches_team = n_matches_team / n_matches) |> 
      distinct(venue_fbref = venue, team_fbref = team, n_matches, prop_matches_team),
    by = c('venue_fbref', 'team_fbref')
  ) |> 
  left_join(
    fotmob_venue_capacities |> 
      distinct(venue_fotmob = venue, capacity_fotmob = capacity, lat_fotmob = lat, long_fotmob = long, opened),
    by = 'venue_fotmob'
  ) |> 
  transmute(
    season,
    team_538,
    venue_fbref,
    venue_fotmob,
    lat = coalesce(lat_fotmob, lat_wiki),
    long = coalesce(long_fotmob, long_wiki),
    capacity = coalesce(capacity_wiki_season, capacity_fotmob, capacity_wiki),
    opened,
    n_matches,
    prop_matches_team,
    is_primary = ifelse(
      !is.na(season),
      NA,
      coalesce(is_primary, FALSE)
    ),
    coord_source = ifelse(!is.na(lat_fotmob), 'fotmob', 'wiki'),
    capacity_source = case_when(
      !is.na(season) ~ 'wiki',
      capacity == capacity_fotmob ~ 'fotmob',
      TRUE ~ 'wiki'
    )
  )
joined_venue_mapping |> arrange(prop_matches_team)
qs::qsave(joined_venue_mapping, path_venue_capacities)
