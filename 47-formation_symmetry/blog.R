

library(tibble)
library(dplyr)
library(tidyr)
library(sdpt3r)
library(ggnetwork)
library(network)
library(ggplot2)
library(grid)

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

df <- tibble::tribble(
   ~from, ~to, ~n,
     "a", "b", 1L,
     "a", "c", 2L,
     "a", "d", 6L,
     "a", "e", 3L,
     "b", "a", 1L,
     "b", "c", 5L,
     "b", "d", 1L,
     "b", "e", 7L,
     "c", "a", 1L,
     "c", "b", 3L,
     "c", "d", 1L,
     "c", "e", 2L,
     "d", "a", 4L,
     "d", "b", 5L,
     "d", "c", 4L,
     "d", "e", 4L,
     "e", "a", 6L,
     "e", "b", 4L,
     "e", "c", 2L,
     "e", "d", 3L
   )

wide_df <- df %>% 
  pivot_wider(
    names_from = to,
    values_from = n,
    values_fill = 0
  ) %>% 
  select(from, a, b, c, d, e) %>% 
  arrange(from) %>% 
  select(-from)
wide_df
## # A tibble: 3 x 3
##       a     b     c
##   <dbl> <dbl> <dbl>
## 1     0     2     3
## 2     5     0     4
## 3     1     3     0

m <- as.matrix(wide_df)
symmetric_m <- m + t(m) ## must be symmetric
mc <- maxcut(symmetric_m)
max_cut <- -round(mc$pobj, 0)
max_cut

upper_symmetric_m <- symmetric_m
upper_symmetric_m[lower.tri(upper_symmetric_m)] <- 0
network <- network(upper_symmetric_m)
upper_symmetric_m[upper.tri(upper_symmetric_m, diag = TRUE)] %>% length()
set.edge.attribute(
  network, 
  'n',
  upper_symmetric_m[upper.tri(upper_symmetric_m, diag = FALSE)]
)

# set.seed(42)
gg_network <- network %>% 
  ggnetwork() %>% 
  as_tibble() %>% 
  mutate(
    rn = row_number()
  )
11+8+3+10+5+7
# a-e (9) 10
# b-e (5) 8
# a-d (10) 9
# a-c (8) 7
# b-d (3) 6
# b-c (4) 8
rns <- c(9, 5, 10, 8, 3, 4)
filt_edges <- gg_network %>%
  filter(
    rn %in% rns
  ) %>% 
  mutate(
    across(rn, ~factor(.x, levels = rns))
  ) %>% 
  arrange(rn)
filt_edges
midpoints <- filt_edges %>% 
  mutate(
    x_mid = (x + xend) / 2,
    y_mid = (y + yend) / 2
  ) %>% 
  mutate(
    x_next = coalesce(lead(x_mid), x_mid - 0.1),
    y_next = coalesce(lead(y_mid), y_mid + 0.1)
  )
midpoints
gg_network %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(curvature = 0.1) +
  geom_nodes(color = 'black', size = pts(36)) +
  geom_nodetext(
    data = gg_network %>% filter(is.na(n)),
    aes(label = vertex.names), 
    size = pts(16), 
    color = 'white',
    fontface = 'bold', 
    family = 'Karla'
  ) +
  # geom_segment(
  #   data = midpoints,
  #   aes(
  #     x = x_mid,
  #     y = y_mid,
  #     xend = x_next,
  #     yend = y_next
  #   ),
  #   color = 'red'
  # ) +
  geom_edgetext(aes(label = n), color = 'black')

