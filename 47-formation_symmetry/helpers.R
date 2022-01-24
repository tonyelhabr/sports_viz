library(dplyr)
library(tibble)
library(purrr)
library(sf)
library(concaveman)
library(tidygraph)
library(igraph)
library(ggplot2)
library(ggsoccer)
library(tidyr)
library(sdpt3r)

## convex ----
flip_df <- function(df, y_center = 34) {
  df %>% 
    mutate(
      y = (2 * y_center) - y
    )
}

.get_convex_hull_idx <- function(df) {
  chull(df$x, df$y)
}

get_convex_hull <- function(df) {
  ch_idx <- .get_convex_hull_idx(df)
  df[ch_idx, ] %>% 
    add_row(head(., 1))
}

get_inner_pts <- function(df) {
  ch_idx <- .get_convex_hull_idx(df)
  df[-ch_idx, ]
}

.get_polygon_sf <- function(df) {
  df %>%
    select(x, y) %>% 
    as.matrix() %>% 
    list() %>% 
    st_polygon()
}

get_polygon_sfc <- function(df) {
  df %>% 
    .get_polygon_sf() %>% 
    st_sfc()
}

possibly_st_intersection <- possibly(
  st_intersection,
  otherwise = NULL,
  quiet = TRUE
)

poly_to_pts_df <- function(p) {
  p %>% 
    st_coordinates() %>% 
    as_tibble() %>%
    rename_all(tolower) # %>% 
  ## drop the redundant (closing) point
  # distinct()
}

get_convex_hull_areas <- function(df, y_center = 34) {
  if(nrow(df) <= 1) {
    return(tibble())
  }
  ch1 <- df %>% get_convex_hull()
  p1 <- ch1 %>% get_polygon_sfc()
  a1 <- p1 %>% st_area()
  
  df2 <- df %>% flip_df(y_center = y_center)
  ch2 <- df2 %>% get_convex_hull()
  p2 <- ch2 %>% get_polygon_sfc()
  a2 <- p2 %>% st_area()
  
  pi <- possibly_st_intersection(p1, p2)
  if(is.null(pi)) {
    warning('`st_intersection()` failed.')
    return(tibble())
  }
  
  not_has_intersection <- pi %>% pluck(1) %>% is.null()
  if(not_has_intersection) {
    warning('Returning early since there is no intersection.')
    return(
      res <- tibble(
        area_inner = 0,
        area_outer = a1,
        data_orig = df %>% list(),
        data_flip = df2 %>% list(),
        n_players_orig = ch1 %>% distinct() %>% nrow(),
        xy_hull_orig = ch1 %>% list(),
        xy_hull_flip = ch2 %>% list(),
        polygon_hull_orig = p1,
        polygon_hull_flip = p2,
        xy_inner = NULL,
        xy_outer = NULL,
        polygon_inner = NULL,
        polygon_outer = NULL
      )
    )
  }
  
  pd <- st_sym_difference(p1, p2)
  
  ai <- st_area(pi)
  ad <- st_area(pd)
  
  res <- tibble(
    area_inner = ai,
    area_outer = ad,
    data_orig = df %>% list(),
    data_flip = df2 %>% list(),
    n_players_orig = ch1 %>% distinct() %>% nrow(),
    xy_hull_orig = ch1 %>% list(),
    xy_hull_flip = ch2 %>% list(),
    polygon_hull_orig = p1,
    polygon_hull_flip = p2,
    xy_inner = pi %>% poly_to_pts_df() %>% list(),
    xy_outer = pd %>% poly_to_pts_df() %>% list(),
    polygon_inner = pi,
    polygon_outer = pd
  )
  
  ## recursion if there are more than 2 points left (inside the hull just found)
  ip <- df %>% get_inner_pts()
  if(nrow(ip) <= 2) {
    return(res)
  }
  bind_rows(
    res,
    get_convex_hull_areas(ip)
  )
}

## concave ----
.get_poly_as_sfg <- function(df) {
  df %>% .get_polygon_sf()
}

.get_pts_as_sfc <- function(df) {
  poly <- df %>% .get_poly_as_sfg()
  n <- df %>% distinct(x, y) %>% nrow()
  ## easiest way for me to do the conversion to points for some reason
  poly %>% st_sample(size = n)
}

# .get_intersection_idx <- function(df1, df2) {
#   poly <- df1 %>% .get_poly_as_sfg()
#   pts <- df2 %>% .get_pts_as_sfc()
#   st_intersects(pts, poly, sparse = FALSE) %>% as.vector()
# }
# 
# ## see help docs for st_intersects
# get_intersection <- function(df1, df2) {
#   idx <- .get_intersection_idx(df1, df2)
#   df1[idx, ]
# }
# 
# ## df is the points df, p is the polygon in which we want to find the points
# do_get_intersection <- function(df, p, ...) {
#   p_df <- p %>% .poly_to_pts_df()
#   get_intersection(p_df, df)
# }


get_concave_hull_points <- function(df) {
  df %>% st_as_sf(coords = c('x', 'y'))
}

get_concave_hull_polygon <- function(df) {
  sdf <- df %>% get_concave_hull_points()
  sdf %>% concaveman() %>% pluck('polygons')
}

.round_xy <- function(df, digits = 1) {
  df %>% 
    mutate(across(c(x, y), round, digits))
}

get_concave_hull_inner_points <- function(df1, df2, ...) {
  df1 %>% 
    .round_xy(...) %>% 
    anti_join(
      df2 %>% .round_xy(...),
      by = c('x', 'y')
    )
}

get_concave_hull_areas <- function(df, y_center = 34) {
  if(nrow(df) <= 1) {
    return(tibble())
  }
  
  p1 <- df %>% get_concave_hull_polygon()
  a1 <- p1 %>% st_area()
  
  df2 <- df %>% flip_df(y_center = y_center)
  p2 <- df2 %>% get_concave_hull_polygon()
  a2 <- p2 %>% st_area()
  
  pi <- possibly_st_intersection(p1, p2)
  if(is.null(pi)) {
    warning('`st_intersection()` failed.')
    return(tibble())
  }
  
  p1_df <- p1 %>% poly_to_pts_df()
  not_has_intersection <- pi %>% pluck(1) %>% is.null()
  if(not_has_intersection) {
    warning('Returning early since there is no intersection.')
    return(
      res <- tibble(
        area_inner = 0,
        area_outer = a1,
        data_orig = df %>% list(),
        data_flip = df2 %>% list(),
        n_players_orig = p1_df %>% distinct() %>% nrow(),
        xy_hull_orig = p1_df %>% list(),
        xy_hull_flip = p2 %>% poly_to_pts_df() %>% list(),
        polygon_hull_orig = p1,
        polygon_hull_flip = p2,
        xy_inner = NULL,
        xy_outer = NULL,
        polygon_inner = NULL,
        polygon_outer = NULL
      )
    )
  }
  
  pd <- st_sym_difference(p1, p2)
  
  ai <- st_area(pi)
  ad <- st_area(pd)
  
  if(any(class(pi) == 'sfc_GEOMETRYCOLLECTION')) {
    pi <- pi %>% st_collection_extract('POLYGON')
  }
  
  res <- tibble(
    area_inner = ai,
    area_outer = ad,
    data_orig = df %>% list(),
    data_flip = df2 %>% list(),
    n_players_orig = p1_df %>% distinct() %>% nrow(),
    xy_hull_orig = p1_df %>% list(),
    xy_hull_flip = p2 %>% poly_to_pts_df() %>% list(),
    polygon_hull_orig = p1,
    polygon_hull_flip = p2,
    xy_inner = pi %>% poly_to_pts_df() %>% list(),
    xy_outer = pd %>% poly_to_pts_df() %>% list(),
    polygon_inner = pi,
    polygon_outer = pd
  )
  
  ## this is not actually doing a great job... it's still returning points on the hull
  ## apparently rounding x,y is not enough
  ip <- get_concave_hull_inner_points(df, p1_df)
  
  if(nrow(ip) <= 2) {
    return(res)
  }
  
  bind_rows(
    res,
    get_concave_hull_areas(ip)
  )
}

## network ----
compute_network_stats <- function(n, e) {
  # n <- n %>% select(id = player_id, name)
  e <- e %>% select(from = player_id_start, to = player_id_end, n)
  g <- graph_from_data_frame(
    # vertices = n,
    e
  ) %>% 
    as_tbl_graph()

  e_b <- g %>% 
    activate(edges) %>% 
    mutate(
      z = centrality_edge_betweenness()
    ) %>% 
    as_tibble() %>% 
    pull(z)
  
  do <- degree(g, mode = 'out')
  di <- degree(g, mode = 'in')
  b <- betweenness(g)
  tibble(
    reciprocity = reciprocity(g),
    transitivity = transitivity(g),
    mean_distance = mean_distance(g),
    density = edge_density(g),
    median_node_degree_out = median(do),
    median_node_degree_in = median(di),
    median_node_betweenness = median(b),
    # n_edge_betweeness = length(e_b),
    median_edge_betweenness = median(e_b),
    mean_node_degree_out = mean(do),
    mean_node_degree_in = mean(di),
    mean_node_betweenness = mean(b),
    # n_edge_betweeness = length(e_b),
    mean_edge_betweenness = mean(e_b), 
  )
}

## plot ----
## Reference: https://github.com/Torvaney/ggsoccer/blob/master/R/dimensions.R
# .pitch_international <- list(
#   length = 105,
#   width = 68,
#   penalty_box_length = 16.5,
#   penalty_box_width = 40.32,
#   six_yard_box_length = 5.5,
#   six_yard_box_width = 18.32,
#   penalty_spot_distance = 11,
#   goal_width = 7.32,
#   origin_x = 0,
#   origin_y = 0
# )
# 
# .common_gg <- function(data, ...) {
#   list(
#     ...,
#     aes(x = x, y = y),
#     ggsoccer::annotate_pitch(
#       dimensions = .pitch_international,
#       colour = 'black',
#       fill = 'white'
#     ),
#     # coord_flip(xlim = c(1, 99), ylim = c(4, 96), clip = 'on') +
#     theme(
#       axis.title = element_text(size = 12, hjust = 0),
#       axis.ticks = element_blank(),
#       axis.text = element_blank(),
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank()
#     ),
#     labs(
#       y = NULL,
#       x = NULL
#     ),
#     coord_flip(ylim = c(0, 68), xlim = c(105, 0))
#   )
# }
# 
# 
# select_unnest <- function(df, ...) {
#   df %>% 
#     select(...) %>% 
#     unnest(where(is.list))
# }

compute_max_cut <- function(df, weighted = TRUE) {
  player_ids <- bind_rows(
    df %>% distinct(player_id = player_id_start),
    df %>% distinct(player_id = player_id_end)
  ) %>% 
    distinct(player_id) %>% 
    arrange(player_id) %>% 
    pull(player_id)
  
  if(!weighted) {
    df <- df %>% mutate(n = 1)
  }
  
  df_wide <- tidyr::crossing(
    player_id_start = player_ids,
    player_id_end = player_ids
  ) %>% 
    left_join(
      df %>% 
        select(player_id_start, player_id_end, n),
      by = c('player_id_start', 'player_id_end')
    ) %>% 
    mutate(
      across(n, ~coalesce(.x, 0L))
    ) %>% 
    pivot_wider(
      names_from = player_id_end,
      values_from = n
    ) %>% 
    mutate(
      across(player_id_start, ~ordered(.x, player_ids))
    ) %>% 
    arrange(player_id_start) %>% 
    column_to_rownames('player_id_start')
  
  m <- as.matrix(df_wide)
  m_uni <- m + t(m)
  if(!weighted) {
    m_uni <- m_uni / 2
  }
  mc <- maxcut(m_uni)
  mc$pobj
  
}