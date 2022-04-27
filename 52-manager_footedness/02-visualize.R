
library(tidyverse)
library(ggchicklet)
dir_proj <- '52-manager_footedness'
gray_wv <- rgb(24, 24, 24, maxColorValue = 255)
gray_grid_wv <- rgb(64, 64, 64, maxColorValue = 255)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  title = element_text('Karla', size = 20, color = 'white'),
  plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 20, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown('Karla', size = 16, color = '#f1f1f1'),
  axis.text = element_text('Karla', color = 'white', size = 14),
  axis.title = element_text('Karla', size = 14, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = gray_grid_wv),
  panel.grid.minor = element_line(color = gray_grid_wv),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = gray_wv, color = gray_wv),
  plot.caption = ggtext::element_markdown('Karla', color = 'white', hjust = 1, size = 12, face = 'italic'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'white', hjust = 0),
  plot.tag.position = c(0.01, 0.03),
  panel.background = element_rect(fill = gray_wv, color = gray_wv)
)
footedness <- read_csv(file.path(dir_proj, 'footedness.csv'))

pal <- c(
  'Both' = '#ffa600', 
  'Right' = '#7a5195', 
  'Left' = '#ef5675'
)

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

upper1 <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

lvls_foot <- c('Right', 'Both', 'Left')
lvls_group <- c('Outfielder', 'Keeper', 'Manager')
clean_footedness <- footedness %>% 
  mutate(
    across(
      c(group, foot),
      upper1
    )
  ) %>% 
  filter(!is.na(foot)) %>% 
  filter(
    (group == 'Manager') | (group != 'Manager' & minutes_played >= 1000)
  )

count_footedness <- function(df) {
  df %>% 
    count(group, foot) %>% 
    group_by(group) %>% 
    mutate(
      prop = n / sum(n)
    ) %>% 
    ungroup() %>% 
    mutate(
      across(
        group,
        factor,
        levels = lvls_group
      ),
      across(
        foot,
        factor,
        levels = lvls_foot
      )
    ) %>% 
    arrange(group, desc(foot)) %>% 
    group_by(group) %>% 
    mutate(
      across(
        prop,
        list(cumu = cumsum)
      ),
      across(
        prop_cumu,
        list(lag1 = ~dplyr::lag(.x, n = 1, default = 0))
      )
    ) %>% 
    ungroup() %>% 
    mutate(
      x_lab = (prop_cumu + prop_cumu_lag1) / 2
    )
}

add_text <- function(..., .data, .size) {
  list(
    ...,
    geom_text(
      data = .data,
      color = 'white',
      family = 'Karla',
      size = pts(.size),
      fontface = 'bold',
      hjust = 0.5,
      aes(
        y = x_lab,
        label = sprintf('%.0f%% (%d)', 100 * prop, n)
      )
    )
  )
}

lab_subtitle <- 'Current managers and players for La Liga and the English Premier League'
lab_caption <- '12 managers did not have proper player data.<br>Outfielders and keepers must have >1k minutes played.'
lab_tag <-  '**Viz**: Tony ElHabr<br>**Data**: transfermarkt (2021/22 season up through 2022-04-09)'
plot_footedness <- function(df) {
  df %>% 
    ggplot() +
    aes(
      x = group,
      y = prop,
      fill = foot
    ) +
    scale_fill_manual(
      name = NULL,
      values = pal,
      breaks = rev(lvls_foot)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      expand = c(0, 0)
    ) +
    geom_chicklet(
      width = 0.8, 
      colour = gray_wv,
      radius = unit(6, units = 'pt'), 
      position = position_stack(reverse = FALSE)
    ) +
    coord_flip() +
    theme(
      legend.position = 'top',
      legend.text = ggtext::element_markdown(color = 'white', size = 14, face = 'bold', family = 'Karla'),
      axis.text.x = element_blank(),
      axis.text.y = element_text(face = 'bold'),
      panel.grid.major = element_blank()
    ) +
    labs(
      title = glue::glue('Are <span style="color:{pal["Left"]}">Left</span>-Footed Managers out of Favor?'),
      subtitle = lab_subtitle,
      tag = lab_tag,
      caption = lab_caption,
      x = NULL,
      y = NULL
    )
}

filt_footedness <- clean_footedness %>% 
  filter(country %in% c('England', 'Spain'))

n_footedness <- filt_footedness%>% 
  count_footedness()

p <- n_footedness %>% 
  plot_footedness() +
  add_text(
    .data = n_footedness %>% 
      filter(
        (group != 'Manager' & foot != 'Both') |
          (group == 'Manager' & foot == 'Right')
      ),
    .size = 16
  ) +
  add_text(
    .data = n_footedness %>% filter(group == 'Manager', foot != 'Right'),
    .size = 12
  )
p

n_footedness_all <- clean_footedness %>% 
  count_footedness()

p_all <- n_footedness_all %>% 
  plot_footedness() +
  add_text(
    .data = n_footedness_all %>% 
      filter(
        !((group == 'Keeper' & foot == 'Both') |
          (group == 'Outfielder' & foot == 'Both'))
      ),
    .size = 16
  ) +
  theme(
    plot.tag.position = c(0.01, 0.025)
  ) +
  labs(
    subtitle = lab_subtitle %>% str_replace('for.*', 'for Big 5 leagues'),
    caption = lab_caption %>% str_remove('.*br\\>')
  )
p_all

save_viz <- function(p, file) {
  path_viz <- file.path(dir_proj, sprintf('%s.png', file))
  ggsave(
    plot = p,
    filename = path_viz,
    width = 10,
    height = 5
  )
  
  add_logo(
    path_viz = path_viz,
    path_logo = file.path(dir_proj, 'epl-logo-white.png'),
    logo_scale = 0.13,
    path_suffix = '',
    idx_x = 0.01,
    idx_y = 0.98,
    adjust_y = FALSE,
    delete = FALSE
  )
  
  add_logo(
    path_viz = path_viz,
    path_logo = file.path(dir_proj, 'la-liga-white.png'),
    logo_scale = 0.04,
    path_suffix = '_w_logo',
    idx_x = 0.15,
    idx_y = 0.98,
    adjust_y = FALSE,
    delete = TRUE
  )
  
}

save_viz(p, 'manager_footedness')

ggsave(
  plot = p_all,
  filename = file.path(dir_proj, 'manager_footedness_big5.png'),
  width = 10,
  height = 5
)

## ci ----
resample_footedness <- function(.group) {
  df <- filt_footedness %>% filter(group == .group)
  rerun_df <- rerun(
    1000,
    df %>% 
      slice_sample(prop = 1, replace = TRUE) %>% 
      count(foot) %>% 
      mutate(prop = n / sum(n))
  ) %>%
    bind_rows()
  
  rerun_df %>% 
    group_by(foot) %>% 
    summarize(
      across(
        prop,
        list(
          mean = mean,
          median = median,
          ci_lo = ~quantile(.x, 0.025),
          ci_hi = ~quantile(.x, 0.975)
        ),
        .names = '{fn}'
      )
    ) %>% 
    ungroup()
}

set.seed(1)
cis <- lvls_group %>% 
  setNames(., .) %>% 
  map_dfr(resample_footedness, .id = 'group') %>% 
  mutate(
    across(
      group,
      factor,
      lvls_group
    )
  )
cis

p_cis <- cis %>% 
  filter(foot == 'Left') %>% 
  ggplot() +
  aes(x = median, y = group) +
  geom_point(
    size = 5,
    color = pal[['Left']]
  ) +
  geom_errorbarh(
    height = 0.5,
    size = 2,
    color = pal[['Left']],
    aes(
      xmin = ci_lo,
      xmax = ci_hi
    )
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme(
    plot.tag.position = c(0.01, 0.025)
  ) +
  labs(
    title = 'Uncertainty in Relative Left-Footed Share',
    subtitle = lab_subtitle,
    tag = lab_tag,
    caption = lab_caption %>% str_remove('.*\\<br\\>') %>% paste0('<br/>Based on 1k resamples.'),
    y = NULL,
    x = '% of Group that is Left-Footed'
  )
p_cis

save_viz(p_cis, 'manager_footedness_left_cis')
