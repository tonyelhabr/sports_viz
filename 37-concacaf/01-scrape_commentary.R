
library(tidyverse)
library(fcscrapR)

dir_proj <- '37-concacaf'
dir_data <- file.path(dir_proj, 'data')

results <-
  fs::dir_ls(dir_data, regexp = 'results-') %>% 
  tibble(path = .) %>% 
  mutate(
    data = map(path, ~read_rds(.x) %>% mutate(across(matches('xG$'), as.numeric)))
  ) %>% 
  select(-path) %>% 
  unnest(data) %>% 
  janitor::clean_names() %>% 
  select(-matches('_2$')) %>% 
  mutate(
    across(home, ~str_remove(.x, '\\s[a-z]+$')),
    across(away, ~str_remove(.x, '^[a-z]+\\s'))
  )
results
# results %>% count(competition_name)
# results_conc <- results %>% filter(competition_name == 'CONCACAF Gold Cup')
# dates_conc <- results_conc %>% distinct(date)
# dates_conc

dates <-
  results %>% 
  select(competition_name, date, home, away) %>% 
  mutate(
    scoreboard_name = 
      case_when(
        competition_name == 'CONCACAF Gold Cup' ~ 'concacaf gold cup',
        competition_name == 'Copa America' ~ 'copa america',
        TRUE ~ 'uefa european championship'
      )
  )
dates %>% count(scoreboard_name)
dates %>% count(competition_name)
results %>% count(competition_name)

# fcscrapR::league_url_data %>% 
#   as_tibble() %>% 
#   filter(name %>% str_detect('uefa\\seuro|gold\\scup'))

.display_info <- function(..., .envir = parent.frame(), .stem = '') {
  x <- glue::glue_collapse(..., sep = '\n')
  x <- glue::glue(x, .envir = .envir)
  cli::cat_line(sprintf('%s: %s%s.', Sys.time(), .stem, x))
}

.display_info_early <- function(..., .envir = parent.frame()) {
  .display_info(..., .envir = .envir, .stem = 'Returning early ')
}

.display_info_after <- function(..., .envir = parent.frame()) {
  .display_info(..., .envir = .envir, .stem = 'Returning data ')
}

do_ids <- function(overwrite = FALSE) {
  path <- file.path(dir_proj, 'game_ids_espn.rds')
  suffix <- glue::glue('ESPN game ids')
  
  if(file.exists(path) & !overwrite) {
    .display_info_early(suffix)
    return(read_rds(path))
  }
  
  scrape_scoreboard_ids2 <- function(scoreboard_name, game_date, .verbose = TRUE) {
    .display_info('Scraping for `scoreboard_name = "{scoreboard_name}"`, `game_date = "{game_date}"`')
    suffix <- 
      switch(
        scoreboard_name,
        'concacaf gold cup' = 'concacaf.gold',
        'copa america' = 'conmebol.america',
        'uefa european championship' = 'uefa.euro'
      )
    url <- paste(
      'https://www.espn.com/soccer/fixtures/_/date',
      stringr::str_remove_all(game_date, '-'),
      'league',
      suffix,
      sep = '/'
    )
    # url <- 'https://www.espn.com/soccer/schedule/_/league/concacaf.gold'
    # url <- 'https://www.espn.com/soccer/fixtures/_/date/20210729/league/concacaf.gold'
    # url <- 'https://www.espn.com/soccer/fixtures/_/date/20150611/league/conmebol.america'
    # url <- 'https://www.espn.com/soccer/fixtures/_/date/20150630/league/conmebol.america'
    page <- url %>% xml2::read_html()
    abbrs <-
      page %>% 
      rvest::html_node('#sched-container') %>% 
      rvest::html_nodes('abbr')
    codes <- abbrs %>% rvest::html_text('text')
    teams <- abbrs %>% rvest::html_attr('title')
    ids_init <-
      page %>% 
      rvest::html_node('#sched-container') %>% 
      rvest::html_nodes('td > a') %>% 
      rvest::html_attr('href')
    # rgx <- '\\/soccer\\/report\\?gameId[=]'
    rgx <- '\\/soccer\\/(matchstats|report)\\?gameId[=]'
    ids <-
      ids_init %>% 
      str_subset(rgx) %>% 
      str_remove(rgx)

    if(length(ids) == 0L | (2 * length(ids) != length(codes))) {
      return(tibble())
    }
    tibble(
      code = codes,
      team = teams,
      id = rep(ids, each = 2)
    )

  }
  
  f_ids <- possibly(function(x, y) { scrape_scoreboard_ids2(scoreboard_name = x, game_date = as.character(y)) }, otherwise = tibble())
  ids_init <- 
    dates %>% 
    distinct(scoreboard_name, date) %>% 
    mutate(data = map2(scoreboard_name, date, f_ids)) %>% 
    unnest(data)
  ids_init
  
  # # NOTE: Have to run some dates twice (not always the same dates), probably due to timing out.
  # dates %>% anti_join(ids) -> dates2
  # ids2 <- 
  #   dates2 %>% 
  #   distinct(scoreboard_name, date) %>% 
  #   mutate(data = map2(scoreboard_name, date, f_ids)) %>% 
  #   unnest(data)
  # ids2
  # ids <- binds_rows(ids, ids2) %>% arrange(date)
  codes <- ids_init %>% distinct(team, code)
  ids <-
    ids_init %>% 
    mutate(
      across(team, ~case_when(.x == 'Curacao' ~ 'Curaçao', TRUE ~ .x))
    ) %>% 
    left_join(dates) %>% 
    filter(team == home | team == away) %>% 
    left_join(codes %>% select(code_home = code, home = team)) %>% 
    left_join(codes %>% select(code_away = code, away = team)) %>% 
    select(-c(team, code)) %>% 
    distinct(date, home, away, .keep_all = TRUE)
  ids
  .display_info_after(suffix)
  write_rds(ids, path)
  ids
}
ids <- do_ids(F)

scrape_commentary2 <- function(game_id, overwrite = FALSE) {
  
  path <- file.path(dir_data, sprintf('commentary-%s.rds', game_id))
  suffix <- glue::glue('for `game_id = "{game_id}"`')
  
  if(file.exists(path) & !overwrite) {
    # .display_info_early(suffix)
    return(read_rds(path))
  }
  res <- fcscrapR::scrape_commentary(game_id)
  .display_info_after(suffix)
  write_rds(res, path)
  res
}

f_scrape <- possibly(scrape_commentary2, otherwise = data.frame())
comm <- ids %>% mutate(data = map(id, f_scrape))

comm_clean <-
  comm %>% 
  mutate(
    cls = map_chr(data, class)
  ) %>% 
  filter(cls == 'data.frame') %>%
  select(-cls) %>% 
  mutate(
    n_row = map_int(data, nrow),
    is_bad = ifelse(n_row == 0, TRUE, FALSE)
  ) %>%
  filter(!is_bad) %>% 
  select(-c(n_row, is_bad))
comm_clean

comm_wide <- 
  comm_clean %>% 
  mutate(data = map(data, ~mutate_all(.x, as.character))) %>%
  unnest(data)

comm_wide %>% 
  filter(home != team_one) %>% 
  distinct(id, date, home, away, team_one, team_two)

comm <-
  comm_wide %>%
  # Weird edge case where this shows up
  # filter(!(home == 'Peru' & away == 'Paraguay' & date == lubridate::ymd('2015-06-30'))) %>%
  filter(half_end == '1') %>%
  select(
    comp = scoreboard_name,
    date,
    id,
    half,
    match_time,
    team_h = home,
    team_a = away,
    code_h = code_home,
    code_a = code_away,
    g_h = team_one_score,
    g_a = team_two_score
  ) %>%
  mutate(across(
    matches('team_[ah]'),
    ~ case_when(.x == 'Curacao' ~ 'Curaçao', TRUE ~ .x)
  ),
  across(c(matches('^g_[ah]'), half), as.integer)) %>%
  distinct() %>%
  mutate(
    # warning is for things like 45' (where there is not minute for some reason)
    stoppage_time = match_time %>% str_replace("^(45|90)\\'[+]([0-9]+)\\'$", '\\2') %>% as.integer() %>% coalesce(0L)
  )
comm

path_comm <- file.path(dir_proj, 'comm.rds')
write_rds(comm, path_comm)


