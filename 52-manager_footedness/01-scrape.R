
library(tidyverse)
library(rvest)

dir_proj <- '52-manager_footedness'
dir_data <- file.path(dir_proj, 'data')
dir.create(dir_data, showWarnings = FALSE)
countries <- c('England', 'Spain', 'Italy', 'Germany', 'France')
tm_league_abbrvs <- sprintf('%s1', c('GB', 'ES', 'IT', 'L', 'FR'))
leagues <- setNames(tm_league_abbrvs, countries)

scrape_bio <- function(url, overwrite = FALSE) {
  parts <- str_split(url, '\\/') %>% unlist()
  path <- file.path(dir_data, sprintf('%s-%s.rds', parts[4], parts[7]))
  if(file.exists(path) & !overwrite) {
    cli::cli_inform(sprintf('Reading %s.', parts[4]))
    return(read_rds(path))
  }
  cli::cli_inform(sprintf('Scraping %s.', parts[4]))
  res <- worldfootballR::tm_player_bio(url)
  write_rds(res, path)
  res
}
possibly_scrape_bio <- possibly(scrape_bio, otherwise = tibble(), quiet = FALSE)

scrape_manager_bios <- function(league_abbrv, ...) {
  page <- sprintf('https://www.transfermarkt.us/premier-league/trainer/pokalwettbewerb/%s', league_abbrv) %>% 
    read_html()
  els <- page %>% html_elements('tr > td.zentriert > a')
  idx <- els %>% html_text2() %>% str_which('to profile')
  managers <- tibble(
    player_url = els[idx] %>% html_attr('href') %>% sprintf('https://www.transfermarkt.us%s', .),
    manager = els[idx] %>% html_attr('title')
  )
  managers %>% 
    mutate(
      bio = map(player_url, ~scrape_bio(url = .x, overwrite = FALSE))
    )
}

managers <- leagues %>% 
  imap_dfr(
    ~scrape_manager_bios(
      .x
    ) %>% 
      mutate(country = .y, .before = 1)
  ) %>% 
  unnest_wider(bio)

scrape_league_team_urls <- function(country_name, start_year = '2021', overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('league_team_urls-%s-%s.rds', country_name, start_year))
  if(file.exists(path) & !overwrite) {
    cli::cli_inform(sprintf('Reading %s, %s.', country_name, start_year))
    return(read_rds(path))
  }
  cli::cli_inform(sprintf('Scraping %s, %s.', country_name, start_year))
  res <- worldfootballR::tm_league_team_urls(country_name = country_name, start_year = start_year)
  write_rds(res, path)
  res
}

scrape_team_url <- function(url, overwrite = FALSE) {
  parts <- str_split(url, '\\/') %>% unlist()
  path <- file.path(dir_data, sprintf('%s-%s.rds', parts[4], parts[7]))
  if(file.exists(path) & !overwrite) {
    cli::cli_inform(sprintf('Reading %s.', parts[4]))
    return(read_rds(path))
  }
  cli::cli_inform(sprintf('Scraping %s.', parts[4]))
  res <- worldfootballR::tm_squad_stats(url)
  write_rds(res, path)
  res
}
possibly_scrape_team_url <- possibly(scrape_team_url, otherwise = tibble(), quiet = FALSE)

scrape_league_player_bios <- function(country, ...) {
  team_urls <- scrape_league_team_urls(country_name = country, start_year = '2021', overwrite = FALSE)
  teams <- team_urls %>% map_dfr(possibly_scrape_team_url, overwrite = FALSE)
  teams %>% 
    # select(country, player_url) %>% 
    mutate(
      bio = map(player_url, possibly_scrape_bio, overwrite = FALSE)
    )
}

# https://www.transfermarkt.com/mehdi-zerkane/profil/spieler/460654
players <- leagues %>% 
  names() %>% 
  map_dfr(scrape_league_player_bios)

players_unnested <- players %>% 
  as_tibble() %>% 
  filter(minutes_played >= 1000) %>% 
  select(country, team_name, minutes_played, bio) %>% 
  unnest(bio)

footedness <- bind_rows(
  managers %>% 
    rename(team_name = current_club) %>% 
    mutate(group = 'manager'),
  players_unnested %>% 
    mutate(group = ifelse(position == 'Goalkeeper', 'keeper', 'outfielder'))
) %>% 
  as_tibble() %>% 
  transmute(
    country,
    team_name,
    # curent_club, 
    across(player_name, str_squish), 
    group,
    minutes_played,
    foot
  )
footedness %>% 
  filter(group == 'keeper') %>% 
  filter(country != 'England') %>% 
  count(foot)

n_footedness <- footedness %>% 
  filter(country %in% c('England', 'Spain')) %>% 
  count(group, foot) %>% 
  group_by(group) %>% 
  mutate(
    prop = n / sum(n),
    n2 = ifelse(is.na(foot), NA_integer_, n),
    prop2 = n2 / sum(n2, na.rm = TRUE)
  ) %>% 
  ungroup()
n_footedness
n_footedness %>% 
  filter(foot == 'left') %>% 
  select(country, group, prop2) %>% 
  pivot_wider(names_from = group, values_from = prop2)

df <- n_footedness %>% 
  # filter(group == 'outfielder') %>% 
  filter(foot %in% c('left', 'right', 'both')) %>% 
  group_by(group) %>% 
  mutate(
    area = n / sum(n),
    r = sqrt(area)
  ) %>% 
  ungroup()

df2 <- n_footedness %>% 
  # filter(group == 'outfielder') %>% 
  filter(foot %in% c('left', 'right')) %>% 
  group_by(group) %>% 
  mutate(
    area = n / sum(n),
    r = sqrt(area)
  ) %>% 
  ungroup()


coords <- tibble(
  group = rep(c('outfielder', 'keeper', 'manager'), each = 3),
  foot = rep(c('left', 'right', 'both'), times = 3),
  y0 = rep(c(-1, 1, 1), each = 3),
  x0 = c(
    c(-0.5, 0.5,  0),
    c(-2,  -1,   -1.5),
    c( 1,   2,    1.5)
  )
) %>% 
  left_join(df2)

coords %>% 
  filter(foot != 'both') %>% 
  ggplot() +
  ggforce::geom_circle(
    aes(x0 = x0, y0 = y0, r = 1.1 * area)
  ) +
  geom_text(
    data = coords,
    aes(x = x0, y = y0, label = foot)
  )
