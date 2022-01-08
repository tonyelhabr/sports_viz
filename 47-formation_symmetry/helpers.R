library(dplyr)
library(tibble)
library(sf)

flip_df <- function(df, x_center = 5) {
  df %>% 
    mutate(
      x = (2 * x_center) - x
    )
}

.get_ch_idx <- function(df, col_x = 'x', col_y = 'y') {
  chull(df[[col_x]], df[[col_y]])
}

get_ch <- function(df, ...) {
  ch_idx <- .get_ch_idx(df, ...)
  df[ch_idx, ]
}

get_inner_pts <- function(df, ...) {
  ch_idx <- .get_ch_idx(df, ...)
  df[-ch_idx, ]
}

get_polygon <- function(df) {
  df %>%
    select(x, y) %>% 
    add_row(head(., 1)) %>% 
    as.matrix() %>% 
    list() %>% 
    st_polygon() %>% 
    st_sfc()
}

get_areas <- function(df) {
  if(nrow(df) <= 1) {
    return(tibble())
  }
  ch1 <- df %>% get_ch()
  p1 <- ch1 %>% get_polygon()
  a1 <- p1 %>% st_area()
  
  df2 <- df %>% flip_df()
  ch2 <- df2 %>% get_ch()
  p2 <- ch2 %>% get_polygon()
  a2 <- p2 %>% st_area()
  
  pi <- st_intersection(p1, p2)
  pd <- st_sym_difference(p1, p2)
  
  ai <- st_area(pi)
  ads <- st_area(pds)

  res <- tibble(
    area_inner = ai,
    area_outer = ads,
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
    get_areas(ip)
  )
}