
extrafont::loadfonts(device = 'win', quiet = TRUE)
library(tidyverse)
require(here)
require(readxl)
require(ggtext)

theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Fira Mono'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = element_text('Karla', face = 'bold', size = 18, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', face = 'bold', size = 14, color = 'gray50'),
  axis.text = element_text(size = 14),
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
  plot.background = element_rect(fill = '#fffaf0', color = NA),
  # plot.caption = element_text(size = 15, face = 'italic'),
  plot.caption = element_text('Karla', size = 14, color = 'gray50', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = element_text('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.02),
  # legend.text = element_text(size = 14),
  legend.text = element_text(size = 14),
  # legend.background = element_rect(fill = '#F3F4F6', color = '#F3F4F6'),
  # legend.position = c(.85, .85))
  panel.background = element_rect(fill = '#fffaf0', color = NA)
)
update_geom_defaults('text', list(family = 'Fira Mono', size = 3.5))

iris %>%
  ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 4, alpha = 0.6) +
  theme(plot.tag.position = c(0.01, 0.01), plot.tag = element_text(hjust = 0)) +
  labs(
    title = 'Sepal Dimensions of Iris Flowers',
    subtitle = 'by Species',
    x = 'Sepal length',
    y = 'Sepal width',
    caption = 'Data: iris dataset',
    tag = 'Tony ElHabr'
  )

arw <- arrow(length = unit(5, 'pt'), type = 'closed')
arw_annotate <- arrow(length = unit(3, 'pt'), type = 'closed')
# References:
# + https://rud.is/b/2019/06/11/makeover-jumbalaya-beating-dumbbells-into-slopegraphs-whilst-orchestrating-ethercalc/
# + https://www.economist.com/graphic-detail/2020/07/25/empty-stadiums-have-shrunk-football-teams-home-advantage
dir_proj <- '04-2020_ucl_npg90_adj'
rnks <- here::here(dir_proj, 'data-raw', '04_ucl_npg90_adj.xlsx') %>% readxl::read_excel()
rnks

rnks_mini <-
  rnks %>% 
  filter(min_ucl >= 2000) %>% 
  mutate(
    idx = row_number(ucl_adj)
  ) %>% 
  mutate(
    lab = sprintf('%.02f (%s%s%%)', ucl_adj, ifelse(npg90_change_perc_adj > 0, '+', ''), npg90_change_perc_adj)
  ) %>% 
  add_row(player = NA_character_, domestic_adj = 1.12, ucl_adj = 1.28, idx = 5L, npg90_change_perc_adj = 1.28 - 1.12) %>% 
  mutate(
    # dir = case_when(
    #   npg90_change_perc_adj > 0 ~ 'Increase',
    #   npg90_change_perc_adj < 0 ~ 'Decrease',
    #   TRUE ~ 'No change'
    # ) %>%
    #   ordered(levels = c('Increase', 'Decrease', 'No change'))
    dir = factor(sign(npg90_change_perc_adj)) 
  )
rnks_mini

viz <-
  rnks_mini %>%
  ggplot() +
  aes(x = domestic_adj, y = idx) +
  geom_vline(aes(xintercept = 0)) +
  geom_segment(
    aes(xend = ucl_adj, yend = idx, color = dir),
    size = 1.25,
    arrow = arw,
    show.legend = FALSE
  ) +
  # geom_segment(
  #   data = rnks_lgnd,
  #   aes(xend = ucl_adj, yend = idx, color = dir),
  #   size = 1.25,
  #   arrow = arw,
  #   show.legend = FALSE
  # ) +
  geom_text(
    data = rnks_mini %>% filter(npg90_change_perc_adj > 0) %>% filter(!is.na(player)),
    aes(x = domestic_adj, y = idx, label = player, color = dir),
    hjust = 1.05
  ) +
  geom_text(
    data = rnks_mini %>% filter(npg90_change_perc_adj > 0) %>% filter(!is.na(player)),
    aes(x = ucl_adj, y = idx, label = lab, color = dir),
    hjust = -0.05
  ) +
  geom_text(
    data = rnks_mini %>% filter(npg90_change_perc_adj <= 0),
    aes(x = domestic_adj, y = idx, label = player, color = dir),
    hjust = -0.05,
  ) +
  geom_text(
    data = rnks_mini %>% filter(npg90_change_perc_adj <= 0),
    aes(x = ucl_adj, y = idx, label = lab, color = dir),
    hjust = 1.05
  ) +
  scale_color_manual(
    # values = c('Increase' = 'steelblue', 'Decrease' = '#808080', 'No change' = '#808080')
    values = c('1' = 'steelblue', '0' = 'grey50', '-1' = 'grey50')
  ) +
  scale_x_continuous(
    limits = c(0, 1.5)
  ) +
  annotate(
    geom = 'curve',
    x = 1.14,
    y = 3.2,
    xend = 1.13,
    yend = 4.7,
    size = 1,
    # angle = -75,
    curvature = -0.1,
    arrow = arw_annotate
  ) +
  geom_text(
    inherit.aes = FALSE,
    aes(x = 1.15, y = 3),
    size = 3.5,
    hjust = 0,
    # family = 'Arial',
    # fontface = 'bold',
    family = 'Karla',
    color = 'grey20',
    label = glue::glue('Adjusted domestic league rate')
  ) +
  annotate(
    geom = 'curve',
    x = 1.32,
    y = 7.1,
    xend = 1.27,
    yend = 5.6,
    size = 1,
    # angle = -75,
    curvature = -0.1,
    arrow = arw_annotate
  ) +
  geom_text(
    inherit.aes = FALSE,
    aes(x = 1.42, y = 8),
    size = 3.5,
    hjust = 1,
    # family = 'Arial',
    # fontface = 'bold',
    family = 'Karla',
    color = 'grey20',
    label = glue::glue('Adjusted Champions League rate')
  ) +
  theme(
    legend.position = 'none',
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    plot.subtitle = element_text(size = 14, face = 'plain')
    # axis.text.x = element_text(size = 14, face = 'bold', color = 'black')
  ) +
  labs(
    title = 'Domestic League vs. Champions League Competition-Adjusted Goals Per 90 Min.',
    subtitle = glue::glue(
      'Although raw goal scoring rates are generally lower in Champions League competion play,
      many of the best attacking players actually have stronger rates when adjusting for competition.'
      ),
    caption = glue::glue('Players with >2k minutes played in UCL prior to 2020.'),
    tag = 'Viz: @TonyElHabr | Data: 21stClub',
    y = NULL,
    x = 'Competition-Adjusted Goals Per 90 Min.'
  )
viz
ggsave(plot = viz, filename = here::here(dir_proj, 'ucl_npg90_adj.png'), width = 10, height = 10.5, type = 'cairo')

# x1_chr <- 'Domestic'
# x2_chr <- 'UCL'
# 
# rnks_mini %>% filter(player %>% str_detect('ero$')) %>% pull(player)
# players_filt <-
#   c(
#     'L. Messi',
#     'R. Lewandowski',
#     # 'S. Agüero',
#     'Neymar',
#     'K. Mbappé',
#     'R. Sterling' # ,
#     # 'T. Müller',
#     # 'A. Griezmann'
#   )
# rnks_mini %>% filter(player %>% str_detect('Lew')) %>% pull(player)
# df_aug_filt <- rnks_mini %>% filter(player %in% players_filt)
# 
# viz <-
#   rnks_mini %>% 
#   ggplot() +
#   # aes(x = 'Domestic', y = 'UCL') +
#   geom_segment(
#     aes(
#      x = x1_chr, y = domestic_adj, 
#      xend = x2_chr, yend = ucl_adj # , color = dir
#     ),
#     # alpha = 0.5,
#     color = '#dddddd'
#   ) +
#   geom_segment(
#     data = df_aug_filt,
#     aes(
#       x = x1_chr, y = domestic_adj, 
#       xend = x2_chr, yend = ucl_adj # , color = dir
#     ),
#     size = 1,
#     color = 'black'
#   ) +
#   geom_vline(aes(xintercept = 1), color = '#808080') +
#   geom_vline(aes(xintercept = 2), color = '#808080') +
#   # ggrepel::geom_text_repel(
#   geom_text(
#     data = df_aug_filt %>% mutate(lab = sprintf('%s: %.02f', player, domestic_adj)),
#     aes(x = x1_chr, y = domestic_adj, label = lab),
#     family = 'Fira Mono', size = 4, hjust = 1, nudge_x = -0.01, lineheight = 0.875
#   ) +
#   geom_text(
#     data = df_aug_filt %>% mutate(lab = sprintf('%.02f (%s)', ucl_adj, scales::percent(npg90_change_perc_adj / 100, accuracy = 1))),
#     aes(x = x2_chr, y = ucl_adj, label = lab),
#     family = 'Fira Mono', size = 4, hjust = 0, nudge_x = 0.01, lineheight = 0.875
#   ) +
#   # geom_curve(
#   #   data = data.frame(), 
#   #   aes(x = 1.2, y = -1, xend = 1.05, yend = -1.125), 
#   #   # color = ft_cols$red, 
#   #   color = 'blue',
#   #   arrow = arw
#   # ) +
#   # geom_segment(
#   #   data = data.frame(), aes(x = 1.6, xend = 1.6, yend = -12.1, y = -12.9), 
#   #   color = '#2b2b2b', arrow = arw
#   # ) +
#   scale_x_discrete(position = 'top') +
#   theme_minimal() +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.text.x = element_text(size = 14, face = 'bold', color = 'black'),
#     axis.text.y = element_blank()
#   ) +
#   labs(
#     x = NULL,
#     y = NULL,
#     title = '',
#     subtitle = '',
#     caption = ''
#   )
# viz
