library(dplyr)
library(tibble)
library(purrr)
library(sf)
library(concaveman)

flip_df <- function(df, y_center = 34) {
  df %>% 
    mutate(
      y = (2 * y_center) - y
    )
}

.get_ch_idx <- function(df) {
  chull(df$x, df$y)
}

get_convex_hull <- function(df) {
  ch_idx <- .get_ch_idx(df)
  df[ch_idx, ]
}

get_inner_pts <- function(df) {
  ch_idx <- .get_ch_idx(df)
  df[-ch_idx, ]
}

.get_polygon <- function(df) {
  df %>%
    select(x, y) %>% 
    add_row(head(., 1)) %>% 
    as.matrix() %>% 
    list() %>% 
    st_polygon()
}

get_polygon <- function(df) {
  df %>% 
    .get_polygon() %>% 
    st_sfc()
}

get_concave_hull_points <- function(df) {
  df %>% st_as_sf(coords = c('x', 'y'))
}

get_concave_hull_polygon <- function(df) {
  sdf <- df %>% get_concave_hull_points()
  sdf %>% concaveman() %>% pluck('polygons')
}

possibly_st_intersection <- possibly(
  st_intersection,
  otherwise = NULL,
  quiet = TRUE
)

.mutate_xy <- function(df) {
  df %>% 
    mutate(across(c(x, y), round, 1))
}

.get_poly_as_sfg <- function(df) {
  df %>% .get_polygon()
}

.get_pts_as_sfc <- function(df) {
  poly <- df %>% .get_poly_as_sfg()
  n <- df %>% distinct(x, y) %>% nrow()
  ## easiest way for me to do the conversion to points for some reason
  poly %>% st_sample(size = n)
}

.get_intersection_idx <- function(df1, df2) {
  poly <- df1 %>% .get_poly_as_sfg()
  pts <- df2 %>% .get_pts_as_sfc()
  st_intersects(pts, poly, sparse = FALSE) %>% as.vector()
}

poly_to_df <- function(p) {
  p %>% 
    st_coordinates() %>% 
    as_tibble() %>%
    select(x = X, y = Y)
}

## see help docs for st_intersects
get_intersection <- function(df1, df2) {
  idx <- .get_intersection_idx(df1, df2)
  df1[idx, ]
}

## df is the points df, p is the polygon in which we want to find the points
do_get_intersection <- function(df, p, ...) {
  p_df <- p %>% poly_to_df()
  get_intersection(p_df, df)
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
  not_has_intersection <- pi %>% pluck(1) %>% is.null()
  if(not_has_intersection) {
    warning('Returning early since there is no intersection.')
    return(tibble())
  }
  pd <- st_sym_difference(p1, p2)
  p1_df <- p1 %>% poly_to_df() %>% distinct()
  ai <- st_area(pi)
  ad <- st_area(pd)
  
  res <- tibble(
    area_inner = ai,
    area_outer = ad,
    data_orig = list(df),
    data_flip = list(df2),
    points_hull_orig = list(p1_df),
    polygon_hull_orig = p1,
    polygon_hull_flip = p2,
    polygon_inner = pi,
    polygon_outer = pd
  )
  
  # ip <- do_get_intersection(df, p1)
  # if(nrow(ip) <= 2) {
  #   return(res)
  # }
  # bind_rows(
  #   res,
  #   get_concave_hull_areas(ip)
  # )
  res
}

get_convex_hull_areas <- function(df, y_center = 50) {
  if(nrow(df) <= 1) {
    return(tibble())
  }
  ch1 <- df %>% get_convex_hull()
  p1 <- ch1 %>% get_polygon()
  a1 <- p1 %>% st_area()
  
  df2 <- df %>% flip_df(y_center = y_center)
  ch2 <- df2 %>% get_convex_hull()
  p2 <- ch2 %>% get_polygon()
  a2 <- p2 %>% st_area()
  
  pi <- possibly_st_intersection(p1, p2)
  if(is.null(pi)) {
    warning('`st_intersection()` failed.')
    return(tibble())
  }
  not_has_intersection <- pi %>% pluck(1) %>% is.null()
  if(not_has_intersection) {
    warning('Returning early since there is no intersection.')
    return(tibble())
  }
  pd <- st_sym_difference(p1, p2)
  
  ai <- st_area(pi)
  ad <- st_area(pd)

  res <- tibble(
    area_inner = ai,
    area_outer = ad,
    data_orig = list(df),
    data_flip = list(df2),
    chull_orig = list(ch1),
    chull_flip = list(ch2),
    polygon_orig = p1,
    polygon_flip = p2,
    polygon_inner = pi,
    polygon_outer = pd
  )
  ip <- df %>% get_inner_pts()
  if(nrow(ip) <= 2) {
    return(res)
  }
  bind_rows(
    res,
    get_convex_hull_areas(ip)
  )
}
