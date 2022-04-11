
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
  read_rds(path)
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
  )

footedness <- bind_rows(
  managers %>% mutate(group = 'manager')
) %>% 
  as_tibble() %>% 
  unnest_wider(bio) %>% 
  select(country, group, player_name, foot) %>%
  count(foot) %>% 
  mutate(
    prop = n / sum(n),
    n2 = ifelse(is.na(foot), NA_integer_, n),
    prop2 = n2 / sum(n2, na.rm = TRUE)
  )
footedness
footedness