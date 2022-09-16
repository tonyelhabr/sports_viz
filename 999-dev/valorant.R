
library(tidyverse)
library(httr)
library(jsonlite)
library(janitor)
library(magick)
library(ggpath)

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
  
  if (isFALSE(is.list(x))) {
    return(x)
  }
  
  is_df <- is.data.frame(x)
  if (isTRUE(is.data.frame(x))) {
    
    clss <- purrr::map(x, class)
    clss_w_df <- clss |> purrr::keep(~any(.x == 'data.frame'))
    
    if (length(clss_w_df) > 0) {
      cols <- names(clss_w_df)
      for(col in cols) {
        x[[col]] <- prettify_nested_dfs(col)
      }
    }
    
    # clss <- purrr::map(x, class)
    # clss_w_lst <- clss |> purrr::keep(~any(.x == 'list'))
    # 
    # if (length(clss_w_lst) > 0) {
    #   cols <- names(clss_w_lst)
    #   for(col in cols) {
    #     x[[col]] <- prettify_nested_dfs(col)
    #   }
    # }
    
    x <- prettify_nested_dfs(x)
    
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

df <- tibble(
  a = 1,
  bad = 'c',
  dIck = tibble(abcDef = 'ghi', jK = 'lmn', op = tibble(q = 'rstuv', wX = 'y')),
  z = list('a' = 2, 'b' = 3)
)
prettify_nested_dfs(df)

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

optic_liquid_match_details_m3 |> 
  pluck('locations') |> 
  summarize(
    across(
      c(location_x, location_y),
      list(min = min, max = max)
    )
  )

raw_map_img <- '999-dev/ascent.png' |> image_read()
map_img_info <- raw_map_img |> image_info()
map_img <- raw_map_img |> 
  image_fx(
    expression = paste0(0.5, '*a'),
    channel = 'alpha'
  ) |>
  image_quantize(colorspace = 'gray')

optic_liquid_m3_coords <- optic_liquid_match_details_m3 |> 
  pluck('locations') |> 
  inner_join(
    optic_liquid_player_names |> select(player_id, ign, team_short_name),
    by = 'player_id'
  ) |> 
  mutate(
    round_sec = round(round_time_millis / 1000, 0)
  ) |> 
  filter(round_number == 2) |> 
  filter(round_sec == 28)


optic_liquid_m3_coords |> 
  ggplot() +
  aes(x = location_x, y = -location_y) +
  annotation_raster(
    map_img,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf
  ) +
  scale_x_continuous(
    limits = c(0, map_img_info$width),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(-map_img_info$height, 0),
    expand = c(0, 0)
  ) +
  geom_point(
    aes(color = team_short_name)
  ) +
  ggrepel::geom_text_repel(
    aes(label = ign)
  ) +
  guides(
    fill = guide_legend('', override.aes = list(size = 3))
  ) +
  theme_void() +
  theme(
    legend.position = 'top'
  )

