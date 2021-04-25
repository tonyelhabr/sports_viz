
library(tidyverse)
dir_proj <- '24-202021_game_state_fouls'
dir_data <- file.path('..', 'whoscraped', 'data')
path_events <- file.path(dir_proj, 'events.rds')
path_meta <- file.path(dir_proj, 'meta.rds')
paths <- 
  fs::dir_ls(dir_data, regexp = 'js$') %>% 
  str_subset('2019-2020|2020-2021')
paths
n_path <- paths %>% length()

# Reference: https://adv-r.hadley.nz/function-operators.html
.print_every <- function(f, n = 1, max = NULL, format = 'Function call {cli::bg_black(i)} of {max}.') {
  force(f)
  force(n)
  
  i <- 0
  function(...) {
    i <<- i + 1
    if (i %% n == 0) {
      cat(glue::glue(format), sep = '\n')
    }
    f(...)
  }
}

.parse_init <- function(path) {
  text_trim <- 
    path %>% 
    read_lines() %>% 
    paste0(collapse = '', sep = '')
  
  lst_match_id <-
    text_trim %>% 
    str_replace_all('^.*matchId[:]\\s?', '') %>% 
    str_replace_all('\\,\\s+matchCentreData.*$', '') %>% 
    jsonlite::parse_json()
  
  lst_event <-
    text_trim %>% 
    str_replace_all('^.*matchCentreData[:]\\s+', '') %>% 
    str_replace_all('\\,\\s+matchCentreEventTypeJson.*$', '') %>% 
    jsonlite::parse_json()
  list(match_id = lst_match_id, events_init = lst_event %>% enframe())
}

.parse_meta <- function(path) {
  lst <- .parse_init(path)
  
  meta_init <-
    lst$events_init %>% 
    rowwise() %>% 
    filter(length(value) == 1L) %>% 
    ungroup() %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    janitor::clean_names()
  
  meta <- 
    meta_init %>% 
    unnest(cols = names(meta_init)) %>% 
    mutate(match_id = lst$match_id) %>% 
    relocate(match_id)
  meta
}

.parse_events <- function(path) {
  # path <- '../whoscraped/data/England-Premier-League-2020-2021-Arsenal-Wolverhampton-Wanderers.js'
  lst <- .parse_init(path)
  
  events <-
    lst$events_init %>% 
    filter(name == 'events') %>% 
    select(value) %>% 
    unnest(value) %>% 
    unnest_wider(value)
  
  players <-
    lst$events_init %>%
    filter(name == 'playerIdNameDictionary') %>%
    select(value) %>%
    unnest_longer(value, indices_to = 'player_id', values_to = 'player_name') %>% 
    mutate(across(player_id, as.integer))
  
  .unnest_side <- function(side) {
    lst$events_init %>%
      filter(name == !!side) %>%
      select(value) %>% 
      unnest_wider(value) %>%
      janitor::clean_names() %>% 
      select(team_id, team_name = name, side = field)
  }
  
  teams <- c('home', 'away') %>% map_dfr(.unnest_side)
  
  events_clean_init <-
    events %>%
    janitor::clean_names() %>% 
    hoist(period, 'period' = 'value', 'period_name' = 'displayName') %>% 
    hoist(type, 'type' = 'value', 'type_name' = 'displayName') %>% 
    hoist(outcome_type, 'outcome_type' = 'value', 'outcome_type_name' = 'displayName')
  
  if(any('cardType' == names(events))) {
    # There's some weird thing here with `cardType` compared to the other elements that are hoisted.
    # Since `cardType` is NULL by default while the others are always named lists, automatically renaming
    # a hoisted element to the same name of the column that is being hoisted drops the column; one way
    # to avoid this is to temporarily name the hoisted column to something different (in this case `cardTypex`).
    events_clean_init <-
      events_clean_init %>%
      hoist(card_type, 'card_typex' = 'value', 'card_type_name' = 'displayName') %>% 
      rename(cardtype = card_typex)
  }
  
  events_clean_init <-
    events_clean_init %>% 
    left_join(teams, by  = 'team_id') %>% 
    left_join(players, by = 'player_id') %>% 
    mutate(match_id = lst$match_id) %>% 
    relocate(match_id)
  
  events_clean <- 
    events_clean_init %>% 
    select(-c(qualifiers, satisfied_events_types)) %>% 
    arrange(minute, event_id)
  events_clean
}

.postprocess_parsed_data <- function(data) {
  rgx <- '(^.*)[-]([0-9]{4}[-][0-9]{4})[-](.*$)'
  .f_replace <- function(x, i) {
    x %>% str_replace(rgx, sprintf('\\%d', i))
  }
  data %>% 
    mutate(file = path %>% basename() %>% tools::file_path_sans_ext()) %>% 
    mutate(
      across(
        file,
        list(
          league = ~.f_replace(.x, 1),
          season = ~.f_replace(.x, 2)
        ),
        .names = '{fn}'
      )
    ) %>% 
    select(-c(path, file)) %>% 
    relocate(season, league, match_id)
}

# main ----
do_f <- function(f, path) {
  f_v <- .print_every(f, max = n_path)
  res_init <- 
    paths %>% 
    tibble(path = .) %>% 
    mutate(data = map(path, f_v)) %>% 
    unnest(data)
  res <- res_init %>% .postprocess_parsed_data()
  write_rds(res, path)
  res
}

events <- do_f(.parse_events, path_events)
meta <- do_f(.parse_meta, path_meta)

beepr::beep(3)

