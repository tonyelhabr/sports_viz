

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
  "a", "c", 0L,
  "a", "d", 3L,
  "b", "a", 1L,
  "b", "c", 1L,
  "b", "d", 1L,
  "c", "a", 0L,
  "c", "b", 2L,
  "c", "d", 1L,
  "d", "a", 1L,
  "d", "b", 5L,
  "d", "c", 4L
)

wide_df <- df %>% 
  pivot_wider(
    names_from = to,
    values_from = n,
    values_fill = 0
  ) %>% 
  select(from, a, b, c, d) %>% 
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
  upper_symmetric_m[upper.tri(upper_symmetric_m) & upper_symmetric_m > 0]
)

set.seed(42)
gg_network <- network %>% 
  ggnetwork() %>% 
  as_tibble() %>% 
  mutate(
    rn = row_number()
  )
rns <- c(5, 4, 2)
filt_edges <- gg_network %>%
  filter(
    rn %in% rns
  ) %>% 
  mutate(
    across(rn, ~factor(.x, levels = rns))
  ) %>% 
  arrange(rn) %>% 
  mutate(
    x_mid = (x + xend) / 2,
    y_mid = (y + yend) / 2
  ) %>% 
  mutate(
    x_next = lead(x_mid),
    y_next = lead(y_mid)
  ) %>% 
  select(rn, vertex.names, n, x_mid, y_mid, x_next, y_next)

adj_filt_edges <- filt_edges %>% 
  add_row(
    filt_edges %>%
      slice(1) %>% 
      mutate(
        x_next = x_mid,
        y_next = y_mid,
        across(x_mid, ~.x + 0.2)
      ),
    .before = 1
  ) %>% 
  mutate(
    across(
      x_next,
      ~ifelse(
        rn == rev(rns)[1],
        x_mid - 0.2,
        .x
      )
    ),
    across(
      y_next,
      ~ifelse(
        rn == rev(rns)[1],
        y_mid,
        .x
      )
    )
  )
adj_filt_edges

p_ex <- gg_network %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(curvature = 0) +
  geom_nodes(color = 'black', size = pts(36)) +
  geom_nodetext(
    data = gg_network %>% filter(is.na(n)),
    aes(label = vertex.names), 
    size = pts(16), 
    color = 'white',
    fontface = 'bold', 
    family = 'Karla'
  ) +
  geom_edgetext(
    aes(label = n), 
    size = pts(16),
    fill = NA,
    hjust = 1,
    vjust = 2,
    color = 'black',
    fontface = 'bold', 
    family = 'Karla'
  ) +
  geom_curve(
    data = adj_filt_edges,
    aes(
      x = x_mid,
      y = y_mid,
      xend = x_next,
      yend = y_next
    ),
    curvature = 0.1,
    size = 2,
    linetype = 2,
    color = 'red'
  ) +
  annotate(
    geom = 'text',
    x = 0.2,
    y = 0.5,
    hjust = 0,
    size = pts(16),
    color = 'red',
    label = 'max cut: 15',
    family = 'Karla',
    fontface = 'bold'
  ) +
  theme_blank() +
  labs(
    title = 'Example max cut formulation'
  ) +
  theme(
    plot.title = element_text('Karla', face = 'bold', size = 24, color = 'black', hjust = 0.5)
  )

