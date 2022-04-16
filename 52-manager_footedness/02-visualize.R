
library(tidyverse)
dir_proj <- '52-manager_footedness'
gray_wv <- rgb(24, 24, 24, maxColorValue = 255)
gray_grid_wv <- rgb(64, 64, 64, maxColorValue = 255)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  title = element_text('Karla', size = 22, color = 'white'),
  plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 22, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown('Karla', size = 18, color = '#f1f1f1'),
  axis.text = element_text('Karla', color = 'white', size = 14),
  axis.title = element_text('Karla', size = 14, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = gray_grid_wv),
  panel.grid.minor = element_line(color = gray_grid_wv),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = gray_wv, color = gray_wv),
  plot.caption = element_text('Karla', color = 'white', hjust = 1, size = 12, face = 'italic'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'white', hjust = 0),
  plot.tag.position = c(0.01, 0.01),
  panel.background = element_rect(fill = gray_wv, color = gray_wv)
)
footedness <- read_csv(file.path(dir_proj, 'footedness.csv'))

pal <- c(
  'both' = '#ffa600', 
  'left' = '#7a5195', 
  'right' = '#ef5675'
)

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

n_footedness <- footedness %>% 
  # filter(!is.na(foot)) %>% 
  filter(country %in% c('England', 'Spain')) %>% 
  count(group, foot) %>% 
  group_by(group) %>% 
  mutate(
    prop = n / sum(n),
    n2 = ifelse(is.na(foot), NA_integer_, n),
    prop2 = n2 / sum(n2, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    across(
      group,
      factor,
      levels = c('outfielder', 'keeper', 'manager')
    ),
    across(
      foot,
      factor,
      levels = c('left', 'both', 'right')
    )
  ) %>% 
  arrange(group, foot) %>% 
  group_by(group) %>% 
  mutate(
    prop2_cumu = cumsum(prop2),
    prop2_cumu_lag1 = lag(prop2_cumu, default = 0, n = 1)
  ) %>% 
  ungroup()
n_footedness

n_footedness %>% 
  filter(!is.na(foot)) %>% 
  ggplot() +
  aes(
    y = group,
    x = prop2,
    fill = foot
  ) +
  geom_bar(
    stat = 'identity',
    width = 0.5
  ) +
  theme(
    axis.text.x = element_blank(),
    panel.grid.major = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL
  )
library(dplyr)
summary = df %>% group_by(Brand, Category) %>% dplyr::summarize(USD = sum(USD)) %>% dplyr::group_by(Brand) %>%  mutate(percent = USD/sum(USD))
ggplot(summary, aes(x=reorder(Brand, USD, sum), y=percent, fill=Category)) +
  geom_bar(width = .7, colour="black", lwd=0.1)

df <- n_footedness %>% 
  # filter(group == 'outfielder') %>% 
  # filter(foot %in% c('left', 'right')) %>% 
  filter(!is.na(foot)) %>% 
  group_by(group) %>% 
  mutate(
    area = n / sum(n),
    r = area ^ (1/2), # square instead of sqrt since we want more overlap and proportion doesn't matter too too much
    across(r, ~(.x / sum(.x)))
  ) %>% 
  ungroup()
df

coords <- tibble(
  group = rep(c('outfielder', 'keeper', 'manager'), each = 3),
  foot = rep(c('left', 'right', 'both'), times = 3),
  # x0 = c(
  #   c(-0.5, 0.5,  0),
  #   c(-2,  -1,   -1.5),
  #   c( 1,   2,    1.5)
  # ),
  y0 = rep(c(-1, 1, 1), each = 3),
) %>% 
  left_join(df) %>% 
  mutate(
    r_both = ifelse(foot == 'both', r, NA_real_)
  ) %>% 
  group_by(group) %>% 
  fill(r_both, .direction = 'downup') %>% 
  ungroup() %>% 
  mutate(
    x0 = case_when(
      group == 'outfielder' ~ 0,
      group == 'keeper' ~ -1,
      group == 'manager' ~ +1
    ) + 
      case_when(
        foot == 'both' ~ 0,
        foot == 'left' ~ -1,
        foot == 'right' ~ +1
      ) * (r_both / 2 + r / 2)
  )
coords

paths <- sprintf(
  'c:/users/antho/downloads/%s.png',
  c(
    'orange-feet',
    'pink-left-foot',
    'purple-right-foot'
  )
)
paths
paths_df <- tibble(
  path = paths,
  x = c(0, -1, 1),
  y = c(0, 0, 0)
)

ggplot() +
  nflplotR::geom_from_path(
    data = paths_df,
    width = .08,
    aes(
      x = x,
      y = y,
      path = path
    )
  ) +
  coord_cartesian(
    xlim = c(-2, 2),
    ylim = c(-2, 2)
  ) +
  theme(
    axis.text = element_blank(),
    panel.grid.major = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL
  )

p <- coords %>% 
  filter(foot != 'both') %>% 
  ggplot() +
  ggforce::geom_circle(
    alpha = 0.5,
    show.legend = FALSE,
    aes(x0 = x0, y0 = y0, r = 1 * r, fill = foot, color = foot)
  ) +
  scale_fill_manual(
    values = pal
  ) +
  scale_color_manual(
    values = pal
  ) +
  geom_text(
    data = coords %>% filter(foot == 'left'),
    color = 'white',
    family = 'Karla',
    size = pts(18),
    hjust = 1.2,
    aes(x = x0 - r, y = y0, label = sprintf('%d\n%s', n, scales::percent(prop2, accuracy = 1)))
  ) +
  geom_text(
    data = coords %>% filter(foot == 'right'),
    color = 'white',
    family = 'Karla',
    size = pts(18),
    hjust = -1.2,
    aes(x = x0 + r, y = y0, label = sprintf('%d\n%s', n, scales::percent(prop2, accuracy = 1)))
  ) +
  theme(
    axis.text = element_blank(),
    panel.grid.major = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL
  )
p
ggsave(
  plot = last_plot(),
  filename = file.path(dir_proj, 'temp.svg'),
  width = 6,
  height = 6
)
