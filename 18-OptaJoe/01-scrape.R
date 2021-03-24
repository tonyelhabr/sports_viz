
library(tidyverse)
user <- 'OptaJoe'
dir_proj <- sprintf('18-%s', user)
grps <-
  file.path(dir_proj, 'grps.csv') %>% 
  read_csv() %>% 
  fill(catg) %>% 
  rename(suffix = word)
path <- file.path(dir_proj, sprintf('%s_timeline.rds', user))
overwrite <- FALSE
if(!file.exists(path) & !overwrite) {
  token <- xengagement::get_twitter_token()
  tweets <- 
    xengagement::retrieve_tweets(
      method = 'all',
      token = token,
      user = user,
      path = path
    )
  tweets
} else {
  tweets <- path %>% read_rds()
}

tweets_slim <-
  tweets %>% 
  select(status_id, created_at, text) %>%
  distinct(text, .keep_all = TRUE) %>% 
  # select(text) %>% 
  mutate(
    text_trim = 
      text %>% 
      tolower() %>% 
      # Remove leading and trailing spaces.
      str_trim() %>% 
      # Remove ew line characters.
      str_replace_all('[\\r\\n]', ' ') %>% 
      # Remove urls.
      str_remove_all('(ht)tp(s?)://\\S+\\b') %>% 
      # Remove hashtags and twitter handles at the end.
      str_remove_all('\\s+(#[@])[A-z]+\\s?$') %>% 
      # # Remove twitter handles at the end.
      # str_remove_all('\\s+[@][A-z_]+\\s?$') %>% 
      # Pad a character at the end.
      paste0('.') %>% 
      # Remove if there are more than one (especially if they have been padded).
      str_replace_all('[.]{1,2}', '.'),
    # don't technically need this number but it could be interesting to do analysis on
    number = 
      text_trim %>% 
      str_sub(1, 10) %>% 
      str_replace_all('(^[-:.\\\\\\/0-9]+)([ ][--][ ])(.*$)', '\\1'),
    suffix1 = 
      text_trim %>% 
      str_replace_all('(^.*)([.][\\s]+?\\w+[.!?])(.*$)', '\\2') %>% 
      str_remove_all('[.]|\\s'),
    suffix2 = 
      text_trim %>% 
      str_replace_all('(^.*)([.]?[\\s]+?\\w+[.!?])(.*$)', '\\2') %>% 
      str_remove_all('[.]|\\s'),
    across(matches('^suffix'), list(str_length), .names = 'n_char_{col}'),
    is_good1 = if_else(n_char_suffix1 <= 20, TRUE, FALSE),
    is_good2 = if_else(n_char_suffix2 <= 20, TRUE, FALSE),
    suffix = case_when(
      suffix1 == suffix2 & is_good1 ~ suffix1,
      !is_good1 & !is_good2 ~ NA_character_,
      is_good1 ~ suffix1,
      is_good2 ~ suffix2,
      TRUE ~ NA_character_
    )
  ) %>% 
  # Keep text for debugging.
  select(-text_trim) %>% 
  relocate(matches('text'), .after = last_col())
tweets_slim

n_tweet <-
  tweets_slim %>% 
  count(suffix, sort = TRUE) %>% 
  drop_na(suffix) %>% 
  left_join(grps)
n_tweet

n_tweet_filt <- 
  n_tweet %>%
  filter(n >= 8L) 

# colors <- c('orange' = '#E62600', 'red' = '#ee0831')
colors <-
  c(
    '#003f5c',
    '#58508d',
    '#bc5090',
    '#ff6361',
    '#ffa600'
  )

catgs <-
  n_tweet_filt %>% 
  group_by(from = catg) %>% 
  summarize(
    across(n, sum)
  ) %>% 
  ungroup() %>% 
  add_row(from = 'root', n = 0)
catgs

df <-
  catgs %>% 
  filter(from != 'root') %>% 
  rename(to = from) %>% 
  mutate(from = 'root') %>% 
  select(from, to, n) %>% 
  bind_rows(n_tweet_filt %>% select(from = catg, to = suffix, n))
df

nodes <- 
  df %>% 
  distinct(node = to, n) %>% 
  add_row(node = 'root', n = 0) %>% 
  select(node, n)
nodes

igraph::graph_from_data_frame(
  vertices = nodes,
  d = df
)

g <-
  tbl_graph(
    nodes = nodes,
    edges = df
  )

# debugonce(as_tbl_graph)
g <-
  df %>% 
  as_tbl_graph()
g

library(tidygraph)
library(ggraph)
.idx_center <- 1L
n_tweet_filt <- 
  n_tweet %>%
  filter(n >= 8L) %>% 
  mutate(
    idx = row_number(desc(n)) + .idx_center,
    to = idx
  ) %>% 
  group_by(catg) %>% 
  arrange(to, .by_group = TRUE) %>% 
  mutate(
    # to = row_number(desc(n)),
    from = dplyr::lag(to),
    # across() is breaking
    from = coalesce(from, .idx_center )
  ) %>% 
  ungroup() %>% 
  # arrange(to)
  arrange(catg, idx) %>% 
  relocate(from, to)
n_tweet_filt
# idx_from_center <-
#   n_tweet_filt %>% 
#   filter(from == idx_center) %>% 
#   pull(idx)
# idx_from_center
# 
# idx_center <-
#   tibble(
#     suffix = 'none',
#     n = 2 * max(n_tweet_filt$n),
#     catg = 'None',
#     idx = !!idx_center
#   )
# 
# graph_init <-
#   crossing(idx_center, tibble(to = idx_from_center)) %>%
#   mutate(from = idx) %>% 
#   bind_rows(n_tweet_filt) %>%
#   relocate(suffix, from, to)
# graph_init

g <-
  tbl_graph(
    nodes = 
      n_tweet_filt %>% 
      distinct(name = idx, suffix, catg, n) %>% 
      relocate(name) %>% 
      add_row(name = .idx_center, suffix = 'none', n = max(n_tweet_filt$n) + 1L) %>% 
      arrange(name),
    edges = n_tweet_filt %>% select(from, to)
  )
g

g %>% 
  ggraph('circlepack', weight = n^2) + 
  geom_node_circle(
    # aes(fill = node)
    # aes(area = n)
  ) +
  geom_node_text(
    aes(
      label = node,
      filter = leaf,
      fill = node,
    ),
  ) +
  theme_void() +
  theme(
    legend.position = 'none'
  ) +
  coord_fixed()

ggraph(g, 'unrooted') + 
  geom_edge_link()

ggraph(g, 'tree') + 
  geom_edge_diagonal()

g %>% 
  ggraph(layout = 'eigen') +
  geom_node_label(aes(label = suffix)) +
  # geom_node_point(aes(fill = n))
  geom_edge_link()

g2 <-
  igraph::graph_from_data_frame(
    vertices = graph_init %>% 
      distinct(name = idx, suffix, n, catg) %>% 
      relocate(name),
    d = graph_init %>% select(from, to)
  )
g2

g2 %>% 
  ggraph('circlepack') + 
  # geom_node_circle(aes(fill = catg)) + 
  geom_node_circle(aes(fill = factor(catg))) +
  geom_node_text(aes(label=catg, filter=leaf, fill=factor(catg), size=n)) +
  # theme_void() +
  # theme(
  #   legend.position = 'none'
  # ) +
  coord_fixed()



graph <- tbl_graph(flare$vertices, flare$edges)
set.seed(1)
graph
n_tweet_filt %>% 
  as_tbl_graph()

edges <- flare$edges %>% 
  filter(to %in% from) %>% 
  droplevels()
vertices <- flare$vertices %>% 
  filter(name %in% c(edges$from, edges$to)) %>% 
  droplevels()
vertices$size <- runif(nrow(vertices))

# Rebuild the graph object
mygraph <- igraph::graph_from_data_frame( edges, vertices=vertices )
mygraph

ggraph(mygraph, layout = 'circlepack') + 
  geom_node_circle(aes(fill = depth)) +
  geom_node_text( aes(label=shortName, filter=leaf, fill=depth, size=size)) +
  theme_void() + 
  theme(legend.position="FALSE") + 
  scale_fill_viridis()
