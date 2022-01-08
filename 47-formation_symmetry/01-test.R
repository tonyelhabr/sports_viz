library(tidyverse)
library(sf)

df1 <- tibble::tribble(
  ~node,  ~y,  ~x,
    'b',   3, 1.5,
    'c',   3, 8.5,
    'd', 2.5,   3,
    'e', 2.5,   7,
    'f',   4,   2,
    'g',   4,   8,
    'h',   4,   4,
    'i',   4,   6,
    'j',   6, 3.5,
    'k',   7,   6
  )

flip_df <- function(df, x_center = 5) {
  df %>% 
    mutate(
      x = (2 * x_center) - x
    )
}

get_ch <- function(df, col_x = 'x', col_y = 'y') {
  ch_idx <- chull(df[[col_x]], df[[col_y]])
  df[ch_idx, ]
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

ch1 <- df1 %>% get_ch()
p1 <- ch1 %>% get_polygon()
a1 <- p1 %>% st_area()

df2 <- df1 %>% flip_df()
ch2 <- df2 %>% get_ch()
p2 <- ch2 %>% get_polygon()
a2 <- p2 %>% st_area()

p1 %>% plot(axes = TRUE)
points(df1$x, df1$y, col = 'red')
points(ch1$x, ch1$y, col = 'red')

p2 %>% plot(add = TRUE)
points(df2$x, df2$y, col = 'blue')
points(ch2$x, ch2$y, col = 'blue')
sf::st_overlaps(p1, p2)
