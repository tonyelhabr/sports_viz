
#- load, echo=F, include=F, eval=T ----
dir_proj <- '47-formation_symmetry'
.f_import <- function(name) {
  file.path(dir_proj, sprintf('%s.rds', name)) %>% read_rds()
}

#- example, echo=F, include=F, eval=T ----
library(tibble)
library(dplyr)
library(tidyr)
library(sdpt3r)

df <- tibble(
  from = c('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd'),
  to   = c('b', 'c', 'd', 'a', 'c', 'd', 'a', 'b', 'd', 'a', 'b', 'c'),
  n    = c( 1L,  0L,  3L,  1L,  1L,  1L,  0L,  2L,  1L,  1L,  5L,  4L)
)

wide_df <- df %>% 
  pivot_wider(
    names_from = to,
    values_from = n,
    values_fill = 0L
  ) %>% 
  select(from, a, b, c, d) %>% 
  arrange(from) %>% 
  select(-from)
wide_df
## # A tibble: 4 x 4
##       a     b     c     d
##   <int> <int> <int> <int>
## 1     0     1     0     3
## 2     1     0     1     1
## 3     0     2     0     1
## 4     1     5     4     0

m <- as.matrix(wide_df)
symmetric_m <- m + t(m) ## must be symmetric
mc <- maxcut(symmetric_m)
max_cut <- -round(mc$pobj, 0)
max_cut
## [1] 15

#- plot-example, echo=F, include=F, eval=T ----
library(network)

upper_symmetric_m <- symmetric_m
upper_symmetric_m[lower.tri(upper_symmetric_m)] <- 0
net <- network(upper_symmetric_m)
set.edge.attribute(
  net, 
  'value',
  upper_symmetric_m[upper.tri(upper_symmetric_m) & upper_symmetric_m > 0]
)

set.seed(42)
gg_net <- net %>% 
  ggnetwork() %>% 
  as_tibble() %>% 
  mutate(
    rn = row_number()
  )
rns <- c(5, 4, 2)
filt_edges <- gg_net %>%
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

#- plot-example, echo=F, include=F, eval=T ----
library(ggnetwork)
library(ggplot2)
library(grid)
pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}
p_ex <- gg_net %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(curvature = 0) +
  geom_nodes(color = 'black', size = pts(36)) +
  geom_nodetext(
    data = gg_net %>% filter(is.na(n)),
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

ggsave(
  plot = p_ex,
  filename = file.path(dir_proj, 'example_max_cut.png'),
  width = 6,
  height = 6
)

#- example-pass-network
team_mapping <-
  xengagement::team_accounts_mapping %>% 
  select(team, team_understat, team_opta = team_whoscored) %>% 
  add_row(
    team = 'Brentford',
    team_understat = 'Brentford',
    team_opta = 'Brentford'
  )
team_mapping

