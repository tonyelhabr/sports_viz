library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(rvest)
library(xml2)
library(purrr)
library(tibble)

proj_dir <- '65-opta_xg_calib'
data_dir <- file.path(proj_dir, 'data')
footedness_data_dir <- file.path(data_dir, 'footedness')
players_meta_data_dir <- file.path(data_dir, 'players', 'meta')
dir.create(footedness_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(players_meta_data_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(proj_dir, 'params.R'))
all_competitions <- read_csv('https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv')

league_urls <- all_competitions |> 
  filter(str_detect(.data[['competition_type']], 'Leagues')) |> 
  inner_join(
    params |> 
      mutate(
        first_season_end_year = ifelse(group == 'big5', 2018L, 2019L),
        last_season_end_year = 2022L,
        n = 1L + last_season_end_year - first_season_end_year
      ) |> 
      uncount(n) |> 
      group_by(group, country, tier, gender) |> 
      mutate(
        season_end_year = first_season_end_year + row_number() - 1L
      ) |> 
      ungroup() |> 
      select(
        group,
        country,
        tier,
        gender,
        season_end_year
      ),
    by = join_by(country, gender, tier, season_end_year)
  ) |> 
  distinct(
    group,
    country,
    tier,
    gender,
    season_end_year,
    url = seasons_urls
  ) |> 
  mutate(
    url = sprintf('%s/stats/%s', dirname(url), basename(url))
  )

retrieve_league_players <- function(url) {
  page <- read_html_live(url)
  cells <- page |>
    html_elements('tbody') |> 
    pluck(3) |> 
    html_elements('tr > td > a')
  hrefs <- html_attr(cells, 'href')
  text <- html_text2(cells)
  stopifnot(length(hrefs) == length(text))
  urls <- hrefs |> str_subset("players") |> str_subset("summary", negate = TRUE)
  idx <- str_which(hrefs, 'players(?!(.*summary))')
  tibble(
    player = text[idx],
    team = text[idx + 2],
    url = paste0('https://fbref.com/', hrefs[idx])
  )
}

insistently_retrieve_league_players <- insistently(
  retrieve_league_players, 
  rate = rate_backoff(max_times = 3, pause_min = 3)
  quiet = FALSE
)

possibly_insistently_retrieve_league_players <- possibly(
  insistently_retrieve_league_players,
  otherwise = tibble(),
  quiet = FALSE
)

retrieve_and_save_league_players <- function(url, country, tier, gender, season_end_year) {
  path <- file.path(footedness_data_dir, sprintf('%s-%s-%s-%s.rds', country, tier, gender, season_end_year))
  if (file.exists(path)) {
    message(sprintf('Reading in from "%s".', path))
    return(read_rds(path))
  }
  message(sprintf('Collecting data at "%s".', url))
  
  res <- possibly_insistently_retrieve_league_players(url)
  write_rds(res, path)
  res
}

league_players <- league_urls |> 
  mutate(
    player = pmap(
      list(
        url, 
        country, 
        tier, 
        gender, 
        season_end_year
      ),
      ~{
        retrieve_and_save_league_players(
          url = ..1, 
          country = ..2, 
          tier = ..3, 
          gender = ..4, 
          season_end_year = ..5 
        )
      }
    )
  )

league_players |> 
  unnest(player, names_sep = '_') |> 
  write_rds(file.path(data_dir, 'footedness_players.rds'))

retrieve_player_meta <- function(url) {
  page <- read_html(url)
  page |> 
    html_elements('#meta > div > p') |> 
    html_text2()
}

possibly_retrieve_player_meta <- possibly(
  retrieve_player_meta,
  quiet = FALSE,
  otherwise = character()
)

retrieve_and_save_players_meta <- function(url) {
  path <- file.path(players_meta_data_dir, paste0(basename(dirname(url)), '.txt'))
  if (file.exists(path)) {
    message(sprintf('Reading in from "%s".', path))
    return(read_lines(path))
  }
  message(sprintf('Collecting data at "%s".', url))
  Sys.sleep(3)
  res <- possibly_retrieve_player_meta(url)
  write_lines(res, path)
  res
}

players_meta <- league_players |> 
  select(player) |> 
  unnest(player) |> 
  pull(url) |> 
  unique() |> 
  map_dfr(
    ~{
      res <- retrieve_and_save_players_meta(.x)
      tibble(text = res) |> 
        mutate(
          url = .x,
          .before = 1
        )
    }
  )

players_meta |>
  filter(str_detect(text, 'Footed')) |> 
  transmute(
    url,
    foot = tolower(str_remove(text, '.*Footed[:] '))
  ) |> 
  inner_join(
    league_players |> distinct(player, url),
    by = 'url',
    multiple = 'all'
  ) |> 
  arrange(player) |> 
  write_rds(file.path(data_dir, 'footedness.rds'))