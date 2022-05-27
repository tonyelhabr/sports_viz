
library(tidyverse)
library(worldfootballR)
library(glue)
library(cli)
library(janitor)

dir_proj <- '54-202122_ucl_final'
dir_data <- file.path(dir_proj, 'data')
path_urls <- file.path(dir_proj, 'urls.rds')
path_data <- file.path(dir_proj, 'data.rds')
dir.create(dir_data, showWarnings = FALSE)

if(!file.exists(path_urls)){
  urls <- tibble(
    url = c(
      'https://fbref.com/en/squads/822bd0ba/Liverpool-Stats',
      'https://fbref.com/en/squads/53a2f082/Real-Madrid-Stats'
    )
  ) |> 
    unnest(url) |> 
    mutate(url_player = map(url, worldfootballR::fb_player_urls)) |>
    unnest(url_player) |> 
    select(-url) |> 
    rename(url = url_player)
  write_rds(urls, path_urls)
} else {
  urls <- read_rds(path_urls)
}

scrape_player <- function(url, stat_type, overwrite = FALSE) {
  player <- url |> basename()
  path <- file.path(dir_data, sprintf('%s-%s.rds', stat_type, player))
  suffix <- glue::glue('for `stat_type = "{stat_type}"`, `player = "{player}"`.')
  if(file.exists(path) & !overwrite) {
    cli::cli_inform(glue::glue('{Sys.time()}: Returning early {suffix}'), sep = '\n')
    return(read_rds(path))
  }
  res <- worldfootballR::fb_player_season_stats(url, stat_type)
  cli::cli_inform(glue::glue('{Sys.time()}: Retrieved data {suffix}'), sep = '\n')
  write_rds(res, path)
  res
}

scrape_player_profile <- function(url, overwrite = FALSE) {
  player <- url |> basename()
  path <- file.path(dir_data, sprintf('%s-%s.rds', stat_type, player))
  suffix <- glue::glue('for `player = "{player}"`.')
  if(file.exists(path) & !overwrite) {
    cli::cli_inform(glue::glue('{Sys.time()}: Returning early {suffix}'), sep = '\n')
    return(read_rds(path))
  }
  res <- worldfootballR::fb_player_scouting_report(url, 'primary')
  cli::cli_inform(glue::glue('{Sys.time()}: Retrieved data {suffix}'), sep = '\n')
  write_rds(res, path)
  res
}

possibly_scrape_player <- possibly(scrape_player, otherwise = tibble(), quiet = FALSE)
poss <- setNames(
  urls$url,
  basename(urls$url)
) |> 
  map_dfr(
    possibly_scrape_player,
    stat_type = 'possession',
    .id = 'player_name'
  )

misc <- setNames(
  urls$url,
  basename(urls$url)
) |> 
  map_dfr(
    possibly_scrape_player,
    stat_type = 'misc',
    .id = 'player_name'
  )

pass <- setNames(
  urls$url,
  basename(urls$url)
) |> 
  map_dfr(
    possibly_scrape_player,
    stat_type = 'passing',
    .id = 'player_name'
  )

teams <- poss |> 
  distinct(player_name, team_name = Squad) |> 
  filter(team_name %in% c('Liverpool', 'Real Madrid')) |> 
  as_tibble()

df <- inner_join(
  poss %>% 
    janitor::clean_names() |> 
    filter(season == '2021-2022') |> 
    group_by(player_name) |> 
    summarize(
      across(megs_dribbles, sum, na.rm = TRUE)
    ) |> 
    ungroup() |> 
    rename(megs = megs_dribbles),
  misc %>% 
    janitor::clean_names() |> 
    filter(season == '2021-2022') |> 
    group_by(player_name) |> 
    summarize(
      across(c(mins_per_90, fls, crd_y, crd_r, x2crd_y), sum, na.rm = TRUE)
    ) |> 
    ungroup() |> 
    transmute(
      player_name,
      m90 = mins_per_90,
      fouls = fls,
      cards = crd_y + crd_r + x2crd_y
    ),
  by = 'player_name'
) |> 
  inner_join(teams, by = 'player_name') |> 
  relocate(team_name, .after = 'player_name') |> 
  filter(
    !(player_name %in% c('Alisson', 'Thibaut-Courtois'))
  ) |> 
  mutate(
    megs_p90 = megs / m90,
    fouls_per_card = fouls / ifelse(cards == 0, 1, cards),
    fouls_p90 = fouls / m90
  ) |> 
  filter(m90 >= 15) |> 
  mutate(
    across(
      c(megs_p90, fouls_p90),
      list(prnk = percent_rank)
    )
  )
df |> arrange(desc(megs_p90))
df |> filter(m90 > 10) |> arrange(desc(megs_p90))
df |> filter(m90 > 10) |> arrange(fouls_per_card)
df |> 
  ggplot() +
  aes(
    x = megs_p90_prnk,
    y = fouls_per_card
  ) +
  geom_point() +
  ggrepel::geom_text_repel(
    aes(label = player_name)
  )
write_rds(path_data)
