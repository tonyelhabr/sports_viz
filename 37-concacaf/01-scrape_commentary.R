
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
    # url <- 'https://www.espn.com/soccer/fixtures/_/date/20150713/league/concacaf.gold'
    page <- url %>% xml2::read_html()
    teams <-
      page %>% 
      rvest::html_node('#sched-container') %>% 
      rvest::html_nodes('abbr') %>% 
      rvest::html_attr('title')
    ids <-
      page %>% 
      rvest::html_node('#sched-container') %>% 
      rvest::html_nodes('td > a') %>% 
      rvest::html_attr('href') %>% 
      str_subset('\\/soccer\\/report\\?gameId') %>% 
      str_remove('\\/soccer\\/report\\?gameId[=]')
    tibble(
      team = teams,
      id = rep(ids, each = 2)
    )

  }
  
  f_ids <- possibly(function(x, y) { scrape_scoreboard_ids2(scoreboard_name = x, game_date = as.character(y)) }, otherwise = tibble())
  ids <- 
    dates %>% 
    # filter(competition_name == 'Copa America') %>% 
    distinct(scoreboard_name, date) %>% 
    mutate(data = map2(scoreboard_name, date, f_ids)) %>% 
    unnest(data)

  ids_aug <-
    ids %>% 
    mutate(
      across(team, ~case_when(.x == 'Curacao' ~ 'Curaçao', TRUE ~ .x))
    ) %>% 
    # group_by(id) %>% 
    # filter(date == max(date)) %>% 
    # ungroup() %>% 
    left_join(dates) %>% 
    filter(team == home | team == away) %>% 
    select(-team) %>% 
    distinct(date, home, away, .keep_all = TRUE)
  ids_aug
  .display_info_after(suffix)
  write_rds(ids_aug, path)
  ids_aug
}
ids <- do_ids()
ids
dates %>% anti_join(ids)

scrape_commentary2 <- function(game_id, overwrite = FALSE) {
  
  path <- file.path(dir_data, sprintf('commentary-%s.rds', game_id))
  suffix <- glue::glue('for `game_id = "{game_id}"`')
  
  if(file.exists(path) & !overwrite) {
    .display_info_early(suffix)
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
comm_wide <- comm_clean %>% mutate(data = map(data, ~mutate_all(.x, as.character))) %>% unnest(data)

comm_slim <-
  comm_wide %>% 
  filter(half_end == '1') %>% 
  select(date, id, half, match_time, team_one, team_two, team_one_score, team_two_score) %>% 
  mutate(
    across(c(team_one, team_two), ~case_when(.x == 'Curacao' ~ 'Curaçao', TRUE ~ .x)),
    across(c(team_one_score, team_two_score, half), as.integer)
  ) %>% 
  distinct() %>% 
  mutate(
    score_diff_abs = abs(team_one_score - team_two_score)
  )
comm_slim
comm_slim %>% count(id, date, team_one, team_two, sort = TRUE)

comm_final <-
  comm_slim %>% 
  left_join(ids) %>% 
  filter((team_one == home & team_two == away) | (team_two == home & team_one == away)) %>% 
  mutate(
    stoppage_time = match_time %>% str_replace("^(45|90)\\'[+]([0-9]+)\\'$", '\\2') %>% as.integer() %>% coalesce(0L)
  )
comm_final
comm_final_long <-
  bind_rows(
    comm_final %>% 
      filter(team_one == home) %>%
      mutate(side = 'h') %>% 
      select(comp = scoreboard_name, date, half, side, team = team_one, team_opp = team_two, g = team_one_score, g_opp = team_two_score, stoppage_time),
    comm_final %>% 
      filter(team_one == away) %>%
      mutate(side = 'a') %>% 
      select(comp = scoreboard_name, date, half, side, team = team_two, team_opp = team_one, g = team_two_score, g_opp = team_one_score, stoppage_time)
  ) %>% 
  mutate(
    g_diff = g - g_opp,
    across(g_diff, list(g_state = ~case_when(.x > 0L ~ 'ahead', .x < 0L ~ 'behind', TRUE ~ 'neutral')), .names = '{fn}')
  )
comm_final_long
path_comm <- file.path(dir_proj, 'comm.rds')
write_rds(comm_final_long, path_comm)

by_team <-
  comm_final_long %>% 
  group_by(comp, team, g_state) %>% 
  summarize(
    n = n(),
    across(stoppage_time, list(max = max, mean = mean))
  ) %>% 
  ungroup() %>% 
  group_by(g_state) %>% 
  mutate(rnk_state = row_number(desc(stoppage_time_mean))) %>% 
  ungroup() %>% 
  arrange(desc(stoppage_time_mean))

by_comp <-
  by_team %>% 
  group_by(comp) %>% 
  summarize(
    stoppage_time_mean = sum(n * stoppage_time_mean) / sum(n)
  ) %>% 
  ungroup()
by_comp


by_team %>% 
  filter(g_state == 'neutral') %>% 
  filter(n > 2) %>% 
  head(20)

by_team %>% 
  filter(g_state == 'neutral') %>% 
  filter(n > 2) %>% 
  group_by(comp) %>% 
  summarize(
    n_team = n_distinct(team),
    across(n, sum)
  )

by_team %>% 
  filter(team == 'United States')
  filter(g_state == 'behind')


comm_slim %>% 
  anti_join(comm_final)
results %>% filter(home %>% str_detect('Cur')) %>% distinct(home)
comm_final %>% 
  arrange(desc(date)) %>% 
  mutate(
    year = date %>% lubridate::year(),
    across(stoppage_time, list(trunc = ~ifelse(.x > 10, 10, .x)))
  ) %>% 
  ggplot() +
  aes(x = stoppage_time_trunc, y = competition_name) +
  ggbeeswarm::geom_quasirandom(
    aes(color = half),
    width = 0.2,
    groupOnX = FALSE,
    alpha = 1
  ) +
  # facet_wrap(~year) +
  theme(
    legend.position = 'top'
  )
