
library(tidyverse)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = element_text('Karla', face = 'bold', size = 18, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', face = 'bold', size = 14, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  # axis.title = element_text(size = 24, face = 'bold'),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  # axis.text = element_text('Karla', size = 12, face = 'bold', color = 'gray20'),
  # axis.title.x = element_text(hjust = 0.95),
  # axis.title.y = element_text(hjust = 0.95),
  # axis.line = element_line(color = 'gray80'),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  # plot.margin = margin(25, 25, 25, 25),
  plot.margin = margin(10, 10, 10, 10),
  # plot.background = element_rect(fill = '#F3F4F6', color = '#F3F4F6'),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  # plot.caption = element_text(size = 15, face = 'italic'),
  plot.caption = element_text('Karla', size = 14, color = 'gray50', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = element_text('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.02),
  # legend.text = element_text(size = 14),
  legend.text = element_text(size = 14),
  # legend.background = element_rect(fill = '#F3F4F6', color = '#F3F4F6'),
  # legend.position = c(.85, .85))
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

.dir_proj <- '17-nba_allstar_break_nrtg'
.dir_logos <- file.path(.dir_proj, 'data')
df <- file.path(.dir_proj, 'nrtg.csv') %>% read_csv()
df

tms <- 
  nbastatR::nba_teams() %>% 
  janitor::clean_names() %>% 
  filter(is_non_nba_team == 0L & slug_team != 'GLI')

# Reference: https://themockup.blog/posts/2020-10-11-embedding-images-in-ggplot/
link_to_img <- function(x, width = 50) {
  glue::glue("<img src='{x}' width='{width}'/>")
}

tms_slim <-
  tms %>% 
  # select(tm = slug_team, url = url_thumbnail_team) %>% 
  select(tm = slug_team) %>% 
  mutate(url = file.path(.dir_logos, sprintf('%s_logo.png', tm))) %>% 
  mutate(href = link_to_img(url))
tms_slim

.ordered <- function(x) {
  ordered(x, levels = c('2019-20', '2020-21'))
}

df_wide <-
  df %>% 
  pivot_wider(
    names_from = side,
    values_from = c(gp, poss, pts),
    names_glue = '{side}{.value}'
  ) %>% 
  mutate(
    ortg = 100 * opts / oposs,
    drtg = 100 * dpts / dposs,
    nrtg = ortg - drtg,
    across(season, .ordered)
  ) %>% 
  group_by(season) %>% 
  mutate(rnk_inv = row_number(nrtg)) %>% 
  ungroup() %>% 
  arrange(season, desc(nrtg)) %>% 
  left_join(tms_slim)
df_wide

# df_wide %>% 
#   filter(season == '2020-21') %>% 
#   filter(tm %in% c('MEM', 'MIA'))

asp_ratio <- 1.618
pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

viz <-
  df_wide %>% 
  # head(5) %>% 
  # filter(season == '2020-21') %>% 
  ggplot() +
  # aes(y = tidytext::reorder_within(tm, -rnk, season), x = nrtg) +
  aes(y = rnk_inv, x = nrtg) +
  geom_vline(aes(xintercept = 0)) +
  geom_rect(
    data = tibble(season = .ordered('2020-21')),
    inherit.aes = FALSE,
    aes(ymin = 8, ymax = 23, xmin = -2, xmax = 2),
    fill = 'blue',
    alpha = 0.3
  ) +
  geom_rect(
    data = tibble(season = .ordered('2019-20')),
    inherit.aes = FALSE,
    aes(ymin = 5, ymax = 18, xmin = -5, xmax = 0),
    fill = 'red',
    alpha = 0.3
  ) +
  ggimage::geom_image(
    aes(image = url)
  ) +
  ggtext::geom_richtext(
    data = tibble(season = .ordered('2020-21'), lab = "<b><span style='font-size:18pt; color:blue'>2020-21</span></b>"),
    fill = NA, label.color = NA,
    family = 'Karla',
    hjust = 1,
    aes(x = -0.5, y = 28, label = lab)
  ) +
  geom_text(
    data = tibble(season = .ordered('2020-21'), lab = '14 teams with a net rating\nbetween -2 and +2'),
    color = 'black',
    size = pts(12),
    family = 'Karla',
    fontface = 'bold',
    hjust = 1,
    aes(x = -0.5, y = 25, label = lab)
  ) +
  ggtext::geom_richtext(
    data = tibble(season = .ordered('2019-20'), lab = "<b><span style='font-size:18pt; color:red'>2019-20</span></b>"),
    fill = NA, label.color = NA,
    family = 'Karla',
    hjust = 1,
    aes(x = -0.5, y = 28, label = lab)
  ) +
  geom_text(
    data = tibble(season = .ordered('2019-20'), lab = '12 teams with a net rating\nbetween -5 and 0'),
    color = 'black',
    size = pts(12),
    family = 'Karla',
    fontface = 'bold',
    hjust = 0,
    aes(x = 0.5, y = 12, label = lab)
  ) +
  facet_wrap(~season, scales = 'free_y', ncol = 2) +
  theme(
    # aspect.ratio = asp_ratio,
    # axis.text.y = element_markdown(),
    strip.text = element_blank(),
    axis.text.y = element_blank(),
    plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0),
    plot.subtitle = ggtext::element_markdown('Karla', size = 12, color = 'gray20'),
    plot.caption = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 1),
    panel.grid.major.y = element_blank(),
    plot.title = ggtext::element_markdown(size = 22),
    plot.margin = margin(10, 10, 10, 10),
    plot.tag.position = c(.01, 0.01),
  ) +
  labs(
    title = 'NBA Team Net Rating at <b><span style="color:blue">2020-21</span></b> All-Star Break compared to <b><span style="color:red">2019-20</span></b>',
    subtitle = 'There\'s a notable cluster of average teams in <b><span style="color:blue">2020-21</span></b>.',
    tag = '**Viz**: @TonyElHabr | **Data**: pbpstats',
    caption = '**Net Rating**: An estimate of point differential per 100 possessions.', 
    x = 'Net Rating',
    y = NULL
  )
viz

height = 8
ggsave(
  plot = viz,
  filename = file.path(.dir_proj, 'viz_nba_allstar_break_2021.png'),
  height = height,
  width = height * asp_ratio,
  type = 'cairo'
)
