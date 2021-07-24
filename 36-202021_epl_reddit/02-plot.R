
library(tidyverse)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 24, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown('Karla', face = 'bold', size = 18, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  # plot.caption = ggtext::element_markdown('Karla', size = 16, color = 'gray20', hjust = 1),
  plot.caption = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.016),
  # legend.text = element_text(color = 'gray20', size = 14),
  # strip.text = element_text(color = 'gray20', size = 14),
  # strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

dir_proj <- '36-tv_ratings'
path_con <- file.path(dir_proj, 'reddit_epl_posts_controversial.csv')
subs <- 
  file.path(dir_proj, 'reddit_epl_subscribers.csv') %>% 
  read_csv() %>% 
  mutate(subreddit_lo = tolower(subreddit))
subs

dir_img <- file.path('24-202021_game_state_fouls', 'img')
team_mapping <-
  subs %>% 
  left_join(
    xengagement::team_accounts_mapping %>%
      # mutate(href = .link_to_img(url_logo_espn)) %>% 
      mutate(path_local = file.path(dir_img, sprintf('%s.png', team))) %>% 
      select(team, path_local, subreddit, color_pri)
  )
team_mapping

con <- 
  path_con %>% 
  read_csv() %>% 
  filter(created < lubridate::ymd('20210601')) %>% 
  filter(title %>% str_detect('Match Thread|FPL', negate = TRUE)) %>% 
  mutate(subreddit_lo = tolower(subreddit)) %>% 
  left_join(
    team_mapping %>% 
      select(-subreddit)
  ) %>% 
  group_by(subreddit) %>% 
  mutate(rnk_intra = row_number(desc(num_comments + (1 - upvote_ratio)))) %>% 
  ungroup() %>% 
  arrange(desc(num_comments))
con

con_filt <-
  con %>% 
  # filter(created < lubridate::ymd('20210601')) %>% 
  group_by(subreddit) %>% 
  slice_max(num_comments, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(
    rnk_inter = row_number(desc(num_comments + (1 - upvote_ratio))),
    rnk_inter_wt = row_number(desc(num_comments / subscribers))
  ) %>% 
  arrange(rnk_inter) %>% 
  relocate(matches('^rnk')) %>% 
  mutate(
    across(title, ~str_remove(.x, '\\[?Unpopular [Oo]pinion\\]?\\:?\\s+')),
    # adjust for clarity
    across(upvote_ratio, ~case_when(subreddit == 'avfc' ~ 0.44, TRUE ~ .x)),
    across(title, ~str_replace(.x, 'uck', '***'))
  )
con_filt

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

library(ggforce)
viz_controversial <-
  con_filt %>% 
  ggplot() +
  aes(y = num_comments, x = upvote_ratio) +
  geom_point() +
  # scale_y_log10() +
  scale_y_continuous(
    # limits = c(0, 401),
    trans = scales::log10_trans()
  ) +
  # ylim(0, 500) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  # coord_trans() +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  # ggforce::geom_mark_circle(
  #   label.family = 'Karla',
  #   # label.colour = NA,
  #   expand = 0,
  #   label.hjust = 0,
  #   label.fontsize = 10,
  #   aes(
  #     # label.hjust = ifelse(upvote_ratio <= 0.5, 0, 1),
  #     # label.fontsize = -rnk_inter,
  #     label = str_replace(strftime(created, '%b %d, %Y'), ', 20', ', \''),
  #     description = sprintf('%s', str_wrap(title, 35))
#   )
# ) +
ggrepel::geom_text_repel(
  family = 'Karla',
  # size = pts(10),
  # linewidth = 0,
  # hjust = 0,
  force_pull = 0,
  max.time = 0.5,
  max.iter = 1e5,
  max.overlaps  = Inf,
  lineheight = 0.8,
  # max.overlaps = 10,
  min.segment.length = 0,
  seed = 42,
  box.padding = 1,
  segment.curvature = -0.1,
  segment.ncp = 3,
  segment.angle = 30,
  segment.colour = 'grey50',
  show.legend = FALSE,
  # fontface = 'italic',
  aes(
    # label = sprintf('%s\n%s', str_wrap(title, 35), lubridate::date(created)),
    hjust = ifelse(upvote_ratio <= 0.5, 1, 0),
    label = sprintf('%s (%s)', str_wrap(title, 35), str_replace_all(strftime(created, '%b %d, %Y'), c(', 20' =', \'', ' 0' = ' '))),
    # alpha = -rnk_inter
    size = -rnk_inter
  )
) +
  scale_size(range = c(3, 5)) +
  ggimage::geom_image(
    size = 0.05,
    aes(image = path_local),
  ) +
  labs(
    title = 'Most controversial posts on each team\'s subreddit',
    subtitle = '2020/21 Premier League',
    caption = 'The percentage of upvotes to total votes (upvotes + downvotes) on the post.<br/>"Controversial" is roughly defined as having a similar number of upvotes and downvotes, per reddit.',
    tag = '**Viz**: Tony ElHabr',
    x = 'Upvote Ratio',
    y = 'log(# of Comments)'
  )
viz_controversial

h <- 10
asp_ratio <- 1 #  16/9
path_export <- file.path(dir_proj, 'viz_reddit_controversial.png')
ggsave(
  plot = viz_controversial,
  filename = path_export,
  height = h,
  width = h * asp_ratio,
  type = 'cairo'
)

add_logo_epl(
  path_viz = path_export,
  path_suffix = '',
  delete = FALSE,
  logo_scale = 0.1,
  adjust_y = FALSE,
  idx_x = 0.01,
  idx_y = 0.99
)

add_logo(
  path_logo = file.path(dir_proj, 'reddit.png'),
  path_viz = path_export, # str_replace(path_export, '.png', '_w_logo.png'),
  # path_suffix = '',
  delete = FALSE,
  logo_scale = 0.06,
  adjust_y = FALSE,
  idx_x = 0.03,
  idx_y = 0.88
)

