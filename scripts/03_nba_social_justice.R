
extrafont::loadfonts(device = 'win', quiet = TRUE)
library(tidyverse)
library(here)
library(readxl)
# remotes::install_github('abresler/nbastatR')
# library(nbastatR)
library(ggtext)

theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Fira Code'),
  # title = element_text('Karla', size = 22, color = 'gray20'),
  title = element_text('IBM Plex Mono SemiBold', size = 14, color = 'gray20'),
  # plot.title = element_text('IBM Plex Mono SemiBold', face = 'bold', size = 22, color = 'gray20'),
  plot.title = element_text('IBM Plex Mono SemiBold', face = 'bold', size = 18, color = 'gray20'),
  plot.title.position = 'plot',
  # plot.subtitle = element_text('IBM Plex Mono SemiBold', face = 'bold', size = 22, color = 'gray50'),
  plot.subtitle = element_text('Fira Code', face = 'bold', size = 14, color = 'gray50'),
  axis.text = element_text(size = 14),
  # axis.title = element_text(size = 24, face = 'bold'),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
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
  plot.caption = element_text('Fira Code', size = 14, color = 'gray50', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = element_text('Fira Code', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.02),
  # legend.text = element_text(size = 14),
  legend.text = element_text(size = 14),
  # legend.background = element_rect(fill = '#F3F4F6', color = '#F3F4F6'),
  # legend.position = c(.85, .85))
  panel.background = element_rect(fill = '#fffaf0', color = NA)
)

msgs <- 
  here::here('data-raw', '2020-restart-nba-social-justice-jersey-messages.xlsx') %>% 
  readxl::read_excel() %>% 
  mutate(
    message = message %>% 
      str_replace_all(c('(^.*)aka \\"(.*)\\"\\)$' = '\\2', '(^.*)for \\"(.*)\\"\\)$' = '\\2'))
  )
msgs

msgs_n <- msgs %>% count(message, sort = T) %>% mutate(rnk = row_number(desc(n)))
# msgs %>% count(team) %>% arrange(n)
# msgs %>% filter(player %>% str_detect('James'))
# msgs %>% filter(message %>% str_detect('Power'))

# teams <- nbastatR::nba_teams() %>% janitor::clean_names() %>% filter(is_non_nba_team == 0L)
# teams
# players <- nbastatR::nba_players() %>% janitor::clean_names() %>% rename(player = name_player)
# players
# 
# players_active <-
#   players %>% 
#   filter(is_active)
# players_active
# players_active %>% filter(player == 'LeBron James')
# 
# msgs_anti <-
#   msgs %>% 
#   anti_join(players_active)
# players_active_anti
# 
# players_active_anti <-
#   players_active %>% 
#   anti_join(msgs)
# players_active_anti

msgs_n_filt <- msgs_n %>% filter(rnk <= 20) 
msgs_n_filt

msgs_n_filt_pos <-
  msgs_n_filt %>% 
  mutate(
    y = case_when(
      rnk <= 2 ~ n - 1L,
      TRUE ~ n + 1L
    )
  )

viz_msgs <-
  msgs_n_filt %>% 
  ggplot() +
  aes(x = -rnk, y = n) +
  geom_col(fill = 'grey20') +
  geom_text(
    data = msgs_n_filt_pos %>% filter(rnk <= 2),
    aes(x = -rnk, y = y, label = message),
    hjust = 1,
    family = 'Fira Code',
    size = 5,
    fontface = 'bold',
    color = 'white'
  ) +
  geom_text(
    data = msgs_n_filt_pos %>% filter(rnk > 2),
    aes(x = -rnk, y = y, label = message),
    hjust = 0,
    family = 'Fira Code',
    size = 5,
    fontface = 'bold',
    color = 'grey20'
  ) +
  annotate(
    # geom = 'segment',
    geom = 'curve',
    x = -4,
    y = 59.5,
    xend = -1.7,
    yend = 65,
    size = 1,
    # angle = -75,
    curvature = 0.25,
    arrow = arrow(length = unit(2, 'mm')),
    lineend = 'round'
  ) +
  geom_text(
    aes(x = -4.1, y = 59),
    size = 5,
    hjust = 1,
    # family = 'Arial',
    # fontface = 'bold',
    family = 'Fira Code',
    color = 'grey20',
    label = glue::glue('All Dallas Mavericks players
                       elected to wear "Equality"')
  ) +
  coord_flip() +
  theme(
    # axis.text.y = element_markdown(),
    axis.text.y = element_blank(),
    # plot.subtitle = element_markdown(),
    # panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    plot.tag.position = c(.01, 0.01),
  ) +
  labs(
    title = 'Most Prevalent Social Justice Messages',
    subtitle = 'NBA, 2019-20 Restart',
    tag = 'Data: NBA | Viz: @TonyElHabr',
    caption = '~30 players elected not to have a message.', 
    x = NULL, 
    y = NULL
  )
viz_msgs
ggsave(plot = viz_msgs, filename = here::here('plots', 'nba_social_justice.png'), width = 10.5, height = 10.5, type = 'cairo')
