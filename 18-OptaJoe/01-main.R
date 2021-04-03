
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
      str_remove_all('[.]|\\s')
  ) %>% 
  # Can't refer to a column within across created in the same `mutate()` (https://github.com/tidyverse/dplyr/issues/5734)
  mutate(
    across(matches('^suffix'), list(str_length), .names = 'n_char_{col}')
  ) %>% 
  mutate(
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

# Find last opener tweet (since this is the most common word) for the tweet that I'll make.
tweets_slim %>% 
  filter(suffix == 'opener') %>%
  slice(1) %>% 
  pull(status_id) %>% 
  paste0('https://twitter.com/OptaJoe/status/', .)

# Arbitrarily choosing this cutoff, trying to choose a good balance between number of words and over-plotting.
n_tweet_filt <- 
  n_tweet %>%
  filter(n >= 8L) 

# colors <- c('orange' = '#E62600', 'red' = '#ee0831')
# colors <- paletteer::paletteer_d('LaCroixColoR::PeachPear') %>% rev()
# colors <- paletteer::paletteer_d('vapeplot::jazzcup') %>% rev() %>% c('#FF3200FF') # c('#ffffff')
colors <-
  c(
    '#003f5c',
    '#58508d',
    '#bc5090',
    '#ff6361',
    '#ffa600',
    # '#E62600',
    '#ffffff' # Don't even end up using this.
  )

.root <- 'a_root'
catgs <-
  n_tweet_filt %>% 
  group_by(from = catg) %>% 
  summarize(
    across(n, sum)
  ) %>% 
  ungroup() %>% 
  add_row(from = .root, n = Inf) %>% 
  mutate(across(from, ~fct_reorder(.x, -n)))
catgs

df <-
  catgs %>% 
  filter(from != .root) %>% 
  rename(to = from) %>% 
  mutate(from = .root) %>% 
  select(from, to, n) %>% 
  bind_rows(n_tweet_filt %>% select(from = catg, to = suffix, n))
df

nodes <- 
  df %>% 
  distinct(node = to, n) %>% 
  add_row(node = .root, n = 0) %>% 
  select(node, n) %>% 
  left_join(grps %>% rename(node = suffix)) %>% 
  mutate(across(catg, ~coalesce(.x, .root)))
nodes

graph <-
  tidygraph::tbl_graph(
    nodes = nodes,
    edges = df
  )
graph

colors_named <-
  colors %>% 
  setNames(., as.character(catgs$from))
colors_named

set.seed(42)
viz <-
  graph %>% 
  ggraph::ggraph('circlepack', weight = n) + 
  ggraph::geom_node_circle(
    alpha = 0.7,
    aes(filter = leaf, fill = catg)
  ) +
  ggraph::geom_node_text(
    aes(
      filter = leaf,
      label = node,
      size = n
    ),
    family = 'Karla'
  ) +
  scale_size(range = c(3.5, 5)) +
  scale_fill_manual(values = colors_named) +
  theme_void(base_family = 'Karla') +
  theme(
    legend.position = 'none',
    text = element_text('Karla')
  ) +
  theme(
    plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0),
    plot.caption = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 1),
    plot.title = ggtext::element_markdown(size = 22, hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10),
    plot.tag.position = c(.01, 0.01)
  ) +
  labs(
    title = 'Most Comon @OptaJoe One-Word Summaries',
    # Improvement would be to not hard-code this date range.
    caption = '**Time period**: Aug. 5, 2020 - Mar. 21, 2021<br/>Colored categories defined manually.',
    tag = '**Viz**: @TonyElHabr'
  ) +
  coord_fixed()
viz

gb <- viz %>% ggplot2::ggplot_build()
centers <-
  gb$data[[2]] %>% 
  as_tibble() %>% 
  select(suffix = label, x, y) %>% 
  left_join(grps) %>% 
  group_by(catg) %>% 
  summarize(across(c(x, y), list(min = min, max = max, mean = mean))) %>% 
  # summarize(across(c(x, y), mean)) %>% 
  ungroup() # %>% 
# mutate(across(c(x, y), round, 1))
centers

# centers %>% clipr::write_clip()

catgs_labs <-
  colors_named %>% 
  enframe('catg', 'color') %>% 
  mutate(lab = str_replace(catg, '&', '&<br/>')) %>% 
  mutate(
    lab = glue::glue("<b><span style='color:{color}'>{lab}</span></b>")
  )
catgs_labs

centers_coords <-
  catgs_labs %>% 
  left_join(
    tibble(
      catg = c('In-game Events', 'Player Archetypes', 'Player Superlatives', 'Team Events & History', 'Transfers & Roster Construction'),
      x = c(7, 5, -12, 5, 2),
      y = c(11, -6, 5, 14, -8)
    )
  ) %>% 
  filter(catg != .root)
centers_coords

viz_w_labs <-
  viz +
  ggtext::geom_richtext(
    data = centers_coords,
    fill = NA, label.color = NA,
    family = 'Karla',
    hjust = 0,
    size = 5,
    aes(x = x, y = y, label = lab)
  )
viz_w_labs

h <- 10
path_viz <- file.path(dir_proj, 'bubble_optajoe_wide.png')
ggsave(
  plot = viz_w_labs,
  filename = path_viz,
  type = 'cairo',
  width = 1.5 * h,
  height = h
)

# img ----
path_img <- file.path(dir_proj, 'optajoe-cropped.png')
# img <- magick::image_read()
# img %>% magick::image_resize(geometry = c('300x400'))
# viz_w_img <-
#   viz_w_labs +
#   ggimage::geom_image(
#     size = 0.15,
#     aes(x = 11, y = -12, image = path_img)
#   )
# viz_w_img
# ggsave(
#   plot = viz_w_img,
#   filename = file.path(dir_proj, 'bubble_optajoe_w_logo.png'),
#   type = 'cairo',
#   width = w,
#   height = w
# )

# Reference: https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/
plot <- path_viz %>% magick::image_read()
logo_raw <- path_img %>% magick::image_read() # %>% magick::image_resize(geometry = c('300x400'))

# get dimensions of plot for scaling
plot_height <- magick::image_info(plot)$height
plot_width <- magick::image_info(plot)$width

logo_scale <- 8
logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))

logo_width <- magick::image_info(logo)$width
logo_height <- magick::image_info(logo)$height

x_pos <- plot_width - logo_width - 0.26 * plot_width
y_pos <- plot_height - logo_height - 0.08 * plot_height
offset <- paste0('+', x_pos, '+', y_pos)

res <- plot %>% magick::image_composite(logo, offset = offset)
magick::image_write(
  res,
  path_viz %>% str_replace('[.]png', '_w_logo.png')
)

