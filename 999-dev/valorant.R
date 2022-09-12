
library(tidyverse)
library(httr)
library(jsonlite)
library(janitor)

get_ribgg <- function(x, ...) {
  sprintf('https://backend-prod.rib.gg/v1/%s', x) |> 
    GET(...) |> 
    content(as = 'text') |> 
    fromJSON()
}

prettify_df <- function(df) {
  df |> 
    as_tibble() |> 
    clean_names()
}

prettify_nested_dfs <- function(x) {
  
  is_list <- is.list(x)
  if (!is_list) {
    return(x)
  }
  
  is_df <- is.data.frame(x)
  if (is_df) {
    ## TODO: Check if has any nested columns and then recurse if so
    return(prettify_df(x))
  }
  
  res <- list()
  for(nm in names(x)) {
    pretty_nm <- make_clean_names(nm)
    el <- x[[nm]]
    # if (is.data.frame(el)) {
    if (is.list(el)) {
      if (is.data.frame(el)) {
        el <- el |> prettify_df()
      }
      res[[pretty_nm]] <- el
      ## TODO: Something recursive?
    } else {
      res[[pretty_nm]] <- x[[nm]]
    }
  }
  res
}

get_ribgg_data <- function(...) {
  get_ribgg(...) |> 
    pluck('data') |> 
    prettify_df()
}

get_nested_ribgg_data <- function(...) {
  get_ribgg(...) |> 
    prettify_nested_dfs()
}

get_all_teams <- memoise::memoise({
  function() {
    get_ribgg('teams/all') |> 
      prettify_df()
  }
})

get_player <- function(player_id) {
  sprintf('players/%s', player_id) |> 
    get_nested_ribgg_data()
}

get_completed_events <- function(q, n = 50) {
  sprintf('events?query=%s&sort=startDate&sortAscending=false&hasSeries=true&take=%s', URLencode(q), n) |> 
    get_ribgg_data()
}

get_completed_series <- function(event_id, n = 50) {
  sprintf('series?take=%s&eventIds[]=%s&completed=true', n, event_id) |> 
    get_ribgg_data()
}

get_matches <- function(series_id) {
  sprintf('series/%s', series_id) |> get_nested_ribgg_data()
}

get_match_details <- function(match_id) {
  sprintf('matches/%s/details', match_id) |> get_nested_ribgg_data()
}

all_teams <- get_all_teams()

# vct_events <- get_completed_events('VALORANT Champions 2022')
vct_champs_playoffs_events <- get_completed_events('VALORANT Champions 2022 Playoffs') |> 
  filter(name == 'VALORANT Champions 2022 - Playoffs')

vct_champs_playoffs_series <- get_completed_series(vct_champs_playoffs_events$id) |> 
  unnest_wider(c(team1, team2), names_sep = '_') |> 
  clean_names()

optic_liquid_series <- vct_champs_playoffs_series |> 
  filter(team1_short_name == 'OPTC', team2_short_name == 'TL')

optic_liquid_matches <- get_matches(optic_liquid_series$id)

optic_liquid_players <- optic_liquid_matches$player_stats |> 
  distinct(player_id) |> 
  mutate(
    data = map(player_id, get_player)
  )

optic_liquid_player_names <- optic_liquid_players |> 
  hoist(
    data,
    'ign',
    'first_name',
    'last_name',
    'team_id' = list('team', 'id'),
    'team_short_name' = list('team', 'shortName')
  ) |>
  select(-data) |>
  mutate(
    across(c(first_name, last_name), str_squish)
  )

optic_liquid_match_details <- optic_liquid_matches$matches$id |> 
  set_names() |> 
  map(get_match_details)
optic_liquid_match_details

optic_liquid_match_details_m3 <- optic_liquid_match_details |> pluck(3)

library(magick)
library(ggpath)

map_png_path <- '999-dev/ascent.png'
map_png_path2 <- '999-dev/Ascent_contour.png'

optic_liquid_m3_coords <- optic_liquid_match_details_m3 |> 
  pluck('locations') |> 
  inner_join(
    optic_liquid_player_names |> select(player_id, ign, team_short_name),
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

p <- optic_liquid_m3_coords |> 
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
