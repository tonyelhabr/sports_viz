
library(tidyverse)
library(rvest)
library(janitor)

get_event_url <- function(year = 2021, split = 1, region = 'North_America', series_type = 'Open_Series', index = 1) {
  
  base_url <- 'https://liquipedia.net/halo/Halo_Championship_Series'
  if(series_type == 'Open_Series') {
    
    # 'https://liquipedia.net/halo/Halo_Championship_Series/2021-22/Split_1/North_America/Open_Series/1',
    year <- sprintf('%s-%s', year, str_sub(as.integer(year) + 1, 3, 4))
    
    url <- sprintf(
      '%s/%s/Split_%s/%s/%s/%s',
      base_url,
      year,
      split,
      region,
      series_type,
      index
    )
    
  } else if (series_type == 'Kickoff_Major') {
    # 'https://liquipedia.net/halo/Halo_Championship_Series/2021/Kickoff_Major/North_America'
    url <- sprintf(
      '%s/%s/%s/%s',
      base_url,
      year,
      series_type,
      region
    )
  }
  url
}

.get_team_names <- function(series_element) {
  names <- series_element %>% html_elements('.name')
  idx_team_names <- names %>% html_attr('style') %>% str_which('overflow:hidden;text-overflow:ellipsis;white-space:pre')
  names[idx_team_names] %>% html_text2()
}

parse_series_result <- function(series_element) {
  team_names <- .get_team_names(series_element)
  
  series_matches_won <- series_element %>% html_elements(".brkts-opponent-score-inner") %>% html_text2()
  
  tibble(
    side = c('home', 'away'),
    team = team_names,
    w = series_matches_won
  ) %>% 
    pivot_wider(
      names_from = side,
      values_from = c(team, w),
      names_glue = '{side}_{.value}'
    )
}

parse_series_matches <- function(series_element) {
  team_names <- .get_team_names(series_element)
  
  popup <- series_element %>% html_elements('.brkts-popup.brkts-match-info-popup')
  popup_matches <- popup %>% html_elements('.brkts-popup-body-element.brkts-popup-body-game')
  popup_divs <- popup_matches %>% html_elements('div > div')
  match_text <- popup_divs %>% 
    html_text() %>% 
    str_trim()
  match_modes <- popup_divs %>%
    html_element('a') %>%
    html_attr('title') %>% 
    discard(is.na)
  
  n_matches <-  length(match_text) / 3
  matches_wide <- tibble(
    match = rep(1:n_matches, each = 3),
    home_team = team_names[1],
    away_team = team_names[2],
    text = match_text,
    name = rep(c('home_score', 'map', 'away_score'), n_matches)
  ) %>% 
    pivot_wider(
      names_from = name,
      values_from = text
    )
  
  # if(length(setdiff(team_names, c('Up Next', 'Oxygen Esports'))) == 0) {
  #   browser()
  # }
  # 
  # if(length(setdiff(team_names, c('OpTic Gaming', 'Sentinels'))) == 0) {
  #   browser()
  # }
  
  # will get warnings if results are just "W" and "L"
  suppressWarnings(
    matches_wide <- matches_wide %>% 
      mutate(
        is_integerish = str_detect(home_score, '[0-9]'),
        winner = case_when(
          home_score == 'W' ~ home_team,
          home_score == 'L' ~ away_team,
          home_score == '' ~ NA_character_,
          as.integer(home_score) > as.integer(away_score) ~ home_team,
          as.integer(home_score) < as.integer(away_score) ~ away_team,
          TRUE ~ NA_character_
        ),
        across(c(home_score, away_score), as.integer)
      )
  )
  matches_wide <- matches_wide %>% 
    select(match, map, home_team, away_team, winner, home_score, away_score)

  first_match_w_result <- matches_wide %>% 
    filter(!is.na(winner)) %>% 
    slice_min(match, n = 1, with_ties = FALSE) %>% 
    pull(match)
  
  matches_wide %>% 
    mutate(
      # there are some cases when a game 1 or 2 is missing, and a later result in the series is not missing
      # a series always has a row for all possible matches, so we can infer if a game should have a result
      # by first checking if the match index is less than the total possible number of matches divided by 2
      # and rounded up, or by checking if a later game has a result
      missing_result = case_when(
        match < ceiling(n_matches / 2) & is.na(winner) ~ TRUE,
        !is.na(!!first_match_w_result) & match < !!first_match_w_result ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>% 
    bind_cols(tibble(mode = match_modes)) %>% 
    # now drop unplayed matches (because series is already clinched)
    filter(!(is.na(winner) & !missing_result))
}

parse_infobox <- function(page) {
  infobox_element <- page %>% html_elements('.fo-nttax-infobox-wrapper.infobox-halo')
  title <- infobox_element %>% 
    html_elements('.infobox-header') %>%
    pluck(1) %>% 
    html_text2() %>% 
    ## remove "[e][h]" before "HCS..."
    str_replace_all('\\[.*\\]H', 'H')
  labels_and_values <- infobox_element %>% html_elements('.infobox-cell-2') %>% html_text2()
  n_text <- length(labels_and_values)
  idx_labels <- seq(1, n_text, by = 2)
  idx_values <- seq(2, n_text, by = 2)
  
  info <- tibble(
    label = labels_and_values[idx_labels] %>% str_remove('[:]$'),
    value = labels_and_values[idx_values] %>% str_trim() %>% str_replace_all('\\n', ', ')
  ) %>% 
    pivot_wider(
      names_from = label,
      values_from = value
    ) %>% 
    janitor::clean_names()
  
  tibble('event_name' = title) %>% 
    mutate(info = list(info))
}

scrape_bracket <- function(url) {
  page <- url %>% read_html()
  body <- page %>% html_element('body')
  infobox <- parse_infobox(page)
  bracket_elements <- page %>% html_elements('.brkts-match.brkts-match-popup-wrapper.brkts-match-has-details')
  
  do_possibly_map_dfr <- function(f) {
    possibly_f <- possibly(f, otherwise = tibble())
    bracket_elements %>% 
      map_dfr(possibly_f, .id = 'i') %>% 
      mutate(
        across(i, as.integer)
      )
  }
  
  series_matches <- do_possibly_map_dfr(parse_series_matches)
  series_results <- do_possibly_map_dfr(parse_series_result)
  infobox %>% 
    mutate(
      series = list(series_results),
      matches = list(series_matches)
    )
}

urls <- 
  c(
    1:2 %>% 
      map_chr(
        ~get_event_url(
          year = 2021, split = 1, region = 'North_America', series_type = 'Open_Series', index = .x 
        )
      ),
    get_event_url(
      year = 2021, split = 1, region = 'North_America', series_type = 'Kickoff_Major'
    )
  )

brackets <- urls[3] %>% 
  map_dfr(scrape_bracket)

all_matches <- brackets %>% 
  select(event_name, matches) %>% 
  unnest(matches)


# all_teams <- bind_rows(
#   all_matches %>% 
#     distinct(team = home_team),
#   all_matches %>% 
#     distinct(team = away_team)
# ) %>% 
#   distinct(team) %>% 
#   arrange(team)
# all_teams

redux_matches <- bind_rows(
  all_matches %>% 
    rename(
      team = home_team,
      opponent = away_team
    ),
  all_matches %>% 
    rename(
      team = away_team,
      opponent = home_team
    )
) %>% 
  mutate(
    w = team == winner,
    l = team != winner,
    across(c(w, l), as.integer)
  )

agg_matches <- redux_matches %>% 
  # filter(team == 'OpTic Gaming') %>% 
  group_by(team, map, mode) %>% 
  summarize(
    across(c(w, l), ~sum(.x, na.rm = TRUE))
  ) %>% 
  ungroup()
agg_matches

all_matches %>% 
  filter(event_name == 'Halo Championship Series 2021: Kickoff Major - North America Qualifier') %>% 
  filter(match == 1) %>% 
  filter(is.na(winner)) %>% 
  arrange(desc(i))

redux_matches %>% 
  filter(
    team == 'Sentinels',
    opponent == 'OpTic Gaming'
  )
