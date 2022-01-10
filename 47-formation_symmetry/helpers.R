library(dplyr)
library(tibble)
library(purrr)
library(sf)
library(concaveman)

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
  
  ai <- st_area(pi)
  ad <- st_area(pd)
  
  res <- tibble(
    area_inner = ai,
    area_outer = ad,
    data_orig = df %>% list(),
    data_flip = df2 %>% list(),
    xy_hull_orig = p1 %>% poly_to_pts_df() %>% list(),
    xy_hull_flip = p2 %>% poly_to_pts_df() %>% list(),
    polygon_hull_orig = p1,
    polygon_hull_flip = p2,
    xy_inner = pi %>% poly_to_pts_df() %>% list(),
    xy_outer = pd %>% poly_to_pts_df() %>% list(),
    polygon_inner = pi,
    polygon_outer = pd
  )
  ## no recursion since this is highly likely to get at least 9 points
  res
}
