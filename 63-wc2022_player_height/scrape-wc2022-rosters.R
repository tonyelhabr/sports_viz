library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(janitor)
library(qs)
library(httr)
library(lubridate)
library(countrycode)
library(gt)

dir_proj <- '63-wc2022_player_height'
rosters_path <- file.path(dir_proj, 'rosters.qs')
matches_path <- file.path(dir_proj, 'matches.qs')

if (!file.exists(rosters_path)) {
  table_url <- 'https://www.espn.com/soccer/table/_/league/fifa.world'
  table_page <- rvest::read_html(table_url)
  
  team_hrefs <- table_page |>
    rvest::html_element('table') |>
    rvest::html_elements('tr > td .AnchorLink') |>
    rvest::html_attr('href') |>
    unique() |>
    sort() |> 
    str_replace('team/', 'team/squad/')
  
  scrape_roster <- function(team_href) {
    Sys.sleep(runif(1))
    message(sprintf('Scraping at %s', team_href))
    # team_href <- '/soccer/team/squad/_/id/212/uruguay'
    url <- sprintf('https://www.espn.com%s', team_href)
    page <- read_html(url)
    tbs <- page |> 
      html_table() |> 
      ## uruguay has an empty column names that makes it impossible to do a mutate, even on just one column.
      ##   thus, use clean_names
      map(
        ~.x |> janitor::clean_names() |> mutate(across(everything(), as.character))
      ) |> 
      bind_rows()
  }
  
  rosters <- team_hrefs |>
    map_dfr(scrape_roster)
  qs::qsave(rosters, rosters_path)
} else {
  rosters <- qs::qread(rosters_path)
}

heights <- rosters |>
  filter(as.integer(app) > 0) |> 
  select(country = nat, name, ht) |> 
  mutate(
    across(
      ht, 
      list(
        feet = ~str_remove(.x, "['].*$") |> as.integer(), 
        inches = ~str_replace_all(.x, "(.*\\s)([0-9]+)(.*$)", '\\2') |> as.integer()
      ), 
      .names = '{fn}'
    ),
    total_inches = 12 * feet + inches
  )

agg_heights <- heights |>
  group_by(country) |> 
  summarize(
    across(
      total_inches,
      mean
    )
  ) |> 
  arrange(desc(total_inches))

if (!file.exists(matches_path)) {
  matches_resp <- httr::GET('https://api.fifa.com/api/v3/calendar/matches?language=en&count=500&idSeason=255711')
  results <- content(matches_resp) |> pluck('Results')
  
  ## for incomplete matches (anything beyond the group stage at the moment), there will be `NULL`s
  ##   which causes `pluck()` to throw an error. using a `.default` of `NA` fixes the issue.
  pluck2 <- partial(pluck, .default = NA_character_, ... = )
  
  map_pluck_chr <- function(x, ...) {
    map_chr(x, pluck2, ...)
  }
  
  map_pluck_results_chr <- function(...) {
    results |> map_pluck_chr(...)
  }
  
  matches <- tibble(
    date = map_pluck_results_chr('Date') |> ymd_hms() |> date(),
    results_id = map_pluck_results_chr('Properties', 'IdIFES') |> as.integer(),
    home_ioc = map_pluck_results_chr('Home', 'IdCountry'),
    away_ioc = map_pluck_results_chr('Away', 'IdCountry')
  )
  qs::qsave(matches, matches_path)
} else {
  matches <- qs::qread(matches_path)
}

country_mapping <- countrycode::codelist |> 
  select(ioc, country = country.name.en) |> 
  bind_rows(
    tibble(
      ioc = c('ENG', 'WAL', 'IRN'),
      country = c('England', 'Wales', 'Iran')
    )
  )

matches |> 
  distinct(ioc = home_ioc) |> 
  anti_join(
    country_mapping
  )

agg_heights_with_ioc <- agg_heights |> 
  inner_join(country_mapping, by = 'country')

matches |> 
  filter(!is.na(home_ioc)) |> 
  inner_join(
    agg_heights_with_ioc |> 
      select(home_country = country, home_ioc = ioc, home_inches_mean = total_inches),
    by = 'home_ioc'
  ) |> 
  inner_join(
    agg_heights_with_ioc |> 
      select(away_country = country, away_ioc = ioc, away_inches_mean = total_inches),
    by = 'away_ioc'
  ) |> 
  mutate(
    d = home_inches_mean - away_inches_mean
  ) |> 
  arrange(desc(abs(d)))

