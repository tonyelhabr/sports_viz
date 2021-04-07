
library(tidyverse)
dir_proj <- '21-202021_epl_heatmap'
dir_data <- file.path('..', 'whoscraped', 'data')
path_events <- file.path(dir_proj, 'events.rds')
path_meta <- file.path(dir_proj, 'meta.rds')
path_forms <- file.path(dir_proj, 'forms.rds')
paths <- fs::dir_ls(dir_data, regexp = 'js$')
paths

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

.do_parse_init <- function(path) {
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
  lst <- .do_parse_init(path)
  
  meta_init <-
    lst$events_init %>% 
    rowwise() %>% 
    filter(length(value) == 1L) %>% 
    ungroup() %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    janitor::clean_names()
  
  meta <- meta_init %>% unnest(cols = names(meta_init))
  meta
}

.parse_events <- function(path) {
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
  
  events_init <- lst_event %>% enframe()

  # events_init %>% 
  #   filter(name == 'periodMinuteLimits') %>% 
  #   select(value) %>% 
  #   unnest_wider(value)
  
  events <-
    events_init %>% 
    filter(name == 'events') %>% 
    select(value) %>% 
    unnest(value) %>% 
    unnest_wider(value)
  
  players <-
    events_init %>%
    filter(name == 'playerIdNameDictionary') %>%
    select(value) %>%
    unnest_longer(value, indices_to = 'player_id', values_to = 'player_name') %>% 
    mutate(across(player_id, as.integer))
  
  .unnest_side <- function(side) {
    events_init %>%
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
    hoist(outcome_type, 'outcome_type' = 'value', 'outcome_type_name' = 'displayName') %>% 
    left_join(teams, by  = 'team_id') %>% 
    left_join(players, by = 'player_id') %>% 
    mutate(match_id = !!lst_match_id)
  
  events_clean <- 
    events_clean_init %>% 
    select(-c(qualifiers, satisfied_events_types)) %>% 
    arrange(minute, event_id)
  
  # events_foul <-
  #   events_clean %>% 
  #   filter(type_name == 'Foul') # %>% 
  #   # This corresponds with `event_type_name == "foulCommited"`, so no need to join `satisfied_events_types`.
  #   # filter(outcome_type_name == 'Unsuccessful')
  # events_foul
  events_clean
}

.parse_forms <- function(path) {
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
  
  events_init <- lst_event %>% enframe()
  
  events <-
    events_init %>% 
    filter(name == 'events') %>% 
    select(value) %>% 
    unnest(value) %>% 
    unnest_wider(value)
  
  players <-
    events_init %>%
    filter(name == 'playerIdNameDictionary') %>%
    select(value) %>%
    unnest_longer(value, indices_to = 'player_id', values_to = 'player_name') %>% 
    mutate(across(player_id, as.integer))
  
  .unnest_side <- function(side) {
    team <-
      events_init %>%
      filter(name == !!side) %>%
      select(value) %>% 
      unnest_wider(value) %>%
      janitor::clean_names()
    
    form_init <-
      team %>% 
      select(formations) %>% 
      unnest(formations) %>% 
      unnest_wider(formations) %>% 
      mutate(idx = row_number()) %>%
      janitor::clean_names()
    
    form_grid <-
      form_init %>% 
      select(idx) %>% 
      crossing(formation_slot = seq.int(1, 11))
    
    form_slots <-
      form_init %>% 
      select(-formation_positions) %>% 
      unnest(c(jersey_numbers, formation_slots, player_ids)) %>% 
      unnest(c(jersey_numbers, formation_slots, player_ids)) %>% 
      rename_with(~str_remove(.x, 's$'), c(jersey_numbers, formation_slots, player_ids)) %>% 
      filter(formation_slot != 0L) %>% 
      full_join(form_grid, by = c('idx', 'formation_slot'))
    
    form_pos <-
      form_init %>% 
      select(idx, formation_positions) %>% 
      unnest(formation_positions) %>% 
      unnest_wider(formation_positions)
    
    form <-
      bind_cols(form_slots, form_pos %>% select(-idx)) %>%
      relocate(idx) %>%
      left_join(players, by = 'player_id') %>%
      bind_cols(team %>% select(team_id, team_name = name, side = field)) %>% 
      select(
        idx,
        team_id,
        team_name,
        side,
        formation_id,
        formation_name,
        formation_slot,
        jersey_number,
        player_id,
        player_name,
        x = horizontal,
        y = vertical
      )
    form
  }
  
  forms <- 
    c('home', 'away') %>%
    map_dfr(.unnest_side) %>% 
    mutate(match_id = !!lst_match_id)
  forms
}

.postprocess_parsed_data <- function(data) {
  rgx <- '(^.*)[-]([0-9]{4}[-][0-9]{4})[-](.*$)'
  .f_replace <- function(x, i) {
    x %>% str_replace(rgx, sprintf('\\%d', i))
  }
  data %>% 
    mutate(file = path %>% basename() %>% tools::file_path_sans_ext()) %>% 
    # select(-path) %>% 
    mutate(
      across(
        file,
        list(
          # match = ~.f_replace(3),
          league = ~.f_replace(1),
          season = ~.f_replace(2)
        ),
        .names = '{fn}'
      )
    ) %>% 
    select(-c(path, file)) %>% 
    relocate(season, league, match_id)
}

# do_f <- function(paths, f) {
#   data <- paths %>% tibble(path = .)
#   n_row <- data %>% nrow()
#   res <-
#     purrr::map_dfr(1:n_row, function(row) {
#       cat(glue::glue('Row {cli::bg_black(row)} (of {cli::bg_black(n_row)})'), sep = '\n')
#       paths[row] %>% f()
#     })
#   data %>% select(path) %>% bind_cols(res)
# }

# main ----
n_path <- paths %>% length()
parse_events_verbosely <- .print_every(.parse_events, max = n_path)
parse_forms_verbosely <- .print_every(.parse_forms, max = n_path)
# events_init <- paths %>% do_f(.parse_events)
# forms_init <- paths %>% do_f(.parse_forms)

events_init <- paths %>% map_dfr(parse_events_verbosely, .id = 'path')
forms_init <- paths %>% map_dfr(parse_forms_verbosely, .id = 'path')

events <- events_init %>% .postprocess_parsed_data()
forms <- forms_init %>% .postprocess_parsed_data()

write_rds(events, path_events)
write_rds(forms, path_forms)
beepr::beep(3)

