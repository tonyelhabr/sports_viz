
library(tidyverse)
library(httr)
library(jsonlite)
library(janitor)

get_all_teams <- memoise::memoise({
  function() {
    resp <- GET('https://backend-prod.rib.gg/v1/teams/all')
    resp |> 
      content(as = 'text') |> 
      fromJSON() |> 
      as_tibble() |> 
      clean_names()
  }
})

all_teams <- get_all_teams()
all_teams

get_completed_events <- function(q, n = 50) {
  resp <- sprintf('https://backend-prod.rib.gg/v1/events?query=%s&sort=startDate&sortAscending=false&hasSeries=true&take=%s', URLencode(q), n) |> 
    GET()
  res <- resp |> 
    content(as = 'text') |> 
    fromJSON()
  res$data |> 
    as_tibble() |> 
    clean_names()
}

vct_events <- get_completed_events('VALORANT Champions 2022')
vct_events

get_completed_series <- function(event_id, n = 50) {
  resp <- sprintf('https://backend-prod.rib.gg/v1/series?take=%s&eventIds[]=%s&completed=true', n, event_id) |> 
    GET()
  res <- resp |> 
    content(as = 'text') |> 
    fromJSON()
  res$data |> 
    as_tibble() |> 
    clean_names()
}

vct_series <- get_completed_series(vct_events$id[1])
vct_series

get_matches <- function(series_id) {
  resp <- sprintf('https://backend-prod.rib.gg/v1/series/%s', series_id) |> 
    GET()
  
  res <- resp |> 
    content(as = 'text') |> 
    fromJSON()
  
  for(nm in names(res)) {
    el <- res[[nm]]
    if (is.data.frame(el)) {
      res[[nm]] <- el |> 
        as_tibble() |> 
        clean_names()
    }
  }
  res
}

get_player <- function(player_id) {
  resp <- sprintf('https://backend-prod.rib.gg/v1/players/%s', player_id) |> 
    GET()
  
  res <- resp |> 
    content(as = 'text') |> 
    fromJSON()
  
  for(nm in names(res)) {
    el <- res[[nm]]
    if (is.data.frame(el)) {
      res[[nm]] <- el |> 
        as_tibble() |> 
        clean_names()
    }
  }
  
  res
}

vct_matches <- get_matches(vct_series$id[2])
vct_matches$matches

vct_players <- vct_matches$playerStats |> 
  distinct(player_id) |> 
  mutate(
    data = map(player_id, get_player)
  )

vct_player_names <- vct_players |> 
  hoist(
    data,
    'ign',
    'first_name' = 'firstName',
    'last_name' = 'lastName',
    'team_id' = list('team', 'id'),
    'team_name' = list('team', 'name')
  ) |>
  select(-data) |>
  mutate(
    across(c(first_name, last_name), str_squish)
  )
  
get_match_details <- function(match_id) {
  resp <- sprintf('https://backend-prod.rib.gg/v1/matches/%s/details', match_id) |> 
    GET()
  
  res <- resp |> 
    content(as = 'text') |> 
    jsonlite::fromJSON()
  
  for(nm in names(res)) {
    el <- res[[nm]]
    if (is.data.frame(el)) {
      res[[nm]] <- el |> 
        as_tibble() |> 
        clean_names()
    }
  }
  
  res
}

vct_match_details <- get_match_details(vct_matches$matches$id[3])
vct_match_details$locations

library(magick)
library(ggpath)

map_png_path <- '999-dev/ascent.png'
map_png_path2 <- '999-dev/Ascent_contour.png'

df <- vct_match_details |> 
  pluck('locations') |> 
  inner_join(
    vct_player_names |> select(player_id, ign, team_name),
    by = 'player_id'
  ) |> 
  mutate(
    round_sec = round(round_time_millis / 1000, 0)
    # round_sec_remaining = round(((100 * 1000) - round_time_millis) / 1000, 1)
  ) |> 
  filter(round_number == 1) |> 
  filter(round_sec == min(round_sec)) |> 
  mutate(
    # across(location_x, ~.x - 53.5),
    # across(location_y, ~-.x + 12)
    across(location_y, ~-.x)
  )

p <- df |> 
  ggplot() +
  aes(x = location_x, y = location_y) +
  geom_point(
    aes(color = team_name)
  ) +
  ggrepel::geom_text_repel(
    aes(label = ign)
  ) +
  geom_from_path(
    inherit.aes = FALSE,
    data = data.frame(),
    alpha = 0.5,
    hjust = 0,
    vjust = 1,
    aes(x = 0, y = 0, path = map_png_path2)
  ) +
  coord_cartesian(
    xlim = c(+53.5, 955+53.5),
    ylim = c(-1025, 12),
    expand = FALSE
  ) +
  # facet_wrap(~round_sec,scales = 'fixed') +
  theme(
    legend.position = 'top'
  )
p
