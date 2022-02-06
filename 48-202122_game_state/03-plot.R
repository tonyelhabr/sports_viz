
library(tidyverse)
library(extrafont)
library(ggtext)
library(cowplot)

gray_wv <- rgb(24, 24, 24, maxColorValue = 255)
gray_grid_wv <- rgb(64, 64, 64, maxColorValue = 255)

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla', color = 'white'),
  title = element_text('Karla', size = 24, color = 'white'),
  plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 24, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', size = 18, color = '#f1f1f1'),
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
  plot.tag = ggtext::element_markdown('Karla', size = 14, color = 'white', hjust = 0),
  plot.tag.position = c(0.01, 0.01),
  strip.text = element_text('Karla', color = 'white', face = 'bold', size = 12, hjust = 0),
  panel.background = element_rect(fill = gray_wv, color = gray_wv),
  legend.text = element_text('Karla', color = 'white', hjust = 1, size = 12)
)
update_geom_defaults('text', list(family = 'Karla', size = pts(10), fontface = 'plain', color = 'white'))

dir_proj <- '48-202122_game_state'
xg <- file.path(dir_proj, 'xg.rds') %>% read_rds()
# standings <- file.path(dir_proj, 'standings.rds') %>% read_rds()

team_mapping <- xengagement::team_accounts_mapping %>% 
  select(team, team_abbrv, url_logo_espn)

state_pal <- c(
  'Behind <1 xG' = '#d01c8b',
  'Behind 0-1 xG' = '#f1b6da',
  'Neutral' = '#f7f7f7',
  'Ahead 0-1 xG' = '#b8e186',
  'Ahead >1 xG' ='#4dac26'
)

states <- xg %>% 
  group_by(team, match_id) %>% 
  mutate(
    cumu_xg_diff = cumu_xg - cumu_xg_opp
  ) %>% 
  ungroup() %>% 
  mutate(
    across(
      cumu_xg_diff, 
      list(
        ~case_when(
          .x < -1 ~ names(state_pal)[1], 
          .x < 0 ~ names(state_pal)[2], 
          .x > 1 ~ names(state_pal)[5], 
          .x > 0 ~ names(state_pal)[4], 
          TRUE ~ names(state_pal)[3]
        ) %>% 
          factor(names(state_pal))
      ),
      .names = 'xg_state'
    )
  ) %>%
  group_by(team) %>% 
  mutate(
    idx_match = dense_rank(date)
  ) %>% 
  ungroup() %>% 
  mutate(
    idx_match = max(idx_match) - idx_match
  ) %>% 
  group_by(match_id, team) %>% 
  mutate(
    across(
      minute,
      ~(.x - 0) / (max(minute) - 0)
    ),
    across(
      minute,
      list(
        prior = ~dplyr::lag(.x, n = 1, default = 0)
      )
    ),
    dur = minute -  minute_prior
  ) %>% 
  ungroup() %>% 
  arrange(season, date, match_id, minute, team)

states %>% count(xg_state)
states %>% 
  filter(
    minute_prior == 0
  ) %>% 
  select(
    match_id, 
    team, 
    team_opp, 
    minute, 
    minute_prior, 
    cumu_xg, cumu_xg_opp, cumu_xg_diff, xg_state
  )
states %>% count(xg_state)

matches <- states %>% 
  distinct(team, match_id, idx_match, date)
matches

results <- states %>% 
  # filter(match_id == 16576) %>% 
  group_by(match_id, team) %>% 
  slice_max(minute, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  transmute(
    match_id, 
    team,
    result = case_when(
      cumu_g > cumu_g_opp ~ 'W',
      cumu_g < cumu_g_opp ~ 'L',
      TRUE ~ 'D'
    )
  )
results

## just the past 5 matches
matches_filt <- states %>% 
  distinct(team, match_id, idx_match) %>% 
  group_by(team) %>% 
  slice_min(idx_match, n = 5, with_ties = FALSE) %>% 
  mutate(
    across(idx_match, row_number)
  ) %>% 
  ungroup()
matches_filt

states_filt <- states %>% 
  select(-idx_match) %>% 
  inner_join(matches_filt)

result_pal <- c(
  'L' = unname(state_pal[1]),
  'W' = unname(state_pal[5]),
  'D' = unname(state_pal[3])
)

ahead_dur <- states_filt %>% 
  filter(
    xg_state %>% str_detect('Ahead')
  ) %>% 
  group_by(team) %>% 
  summarize(
    across(dur, sum)
  ) %>% 
  ungroup() %>% 
  arrange(desc(dur))

states_arr <- states_filt %>% 
  mutate(
    across(
      team,
      factor,
      levels = ahead_dur$team
    )
  )

labs <- states_filt %>% 
  distinct(match_id, idx_match, date, side, team, team_opp) %>% 
  left_join(
    team_mapping %>% 
      select(
        team_opp = team, team_opp_abbrv = team_abbrv
      )
  ) %>% 
  left_join(
    results
  ) %>% 
  mutate(
    across(
      team,
      factor,
      levels = ahead_dur$team
    ),
    result_lab = sprintf('<span style="color:%s; font-size:10pt">%s %s %s</span>', result_pal[result], result, ifelse(side == 'h', 'v', '@'), team_opp_abbrv),
    date_lab = format(date, '%b %d')
  )

p <- states_arr %>% 
  ggplot() +
  geom_rect(
    aes(
      fill = xg_state,
      xmin = minute_prior,
      xmax = minute,
      ymin = idx_match - 0.4,
      ymax = idx_match + 0.4
    )
  ) +
  ggtext::geom_richtext(
    fill = NA_character_,
    label.color = NA_character_,
    hjust = 1,
    data = labs,
    aes(
      x = -0.05,
      y = idx_match,
      label = result_lab
    )
  ) +
  geom_text(
    hjust = 0,
    data = labs,
    aes(
      x = 1.05,
      y = idx_match,
      label = date_lab
    )
  ) +
  scale_fill_manual(
    values = state_pal
  ) +
  scale_x_continuous(
    limits = c(-1, 1.5)
  ) +
  guides(
    fill = guide_legend(title = '')
  ) +
  facet_wrap(~team) +
  theme(
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = 'top',
  ) +
  labs(
    x = NULL,
    y = NULL,
    tag = '**Viz**: @TonyElHabr | **Data**: understat',
    caption = 'Teams shown in descending order of total proportion of match time spent ahead.',
    title = sprintf('Proportion of match spent <span style="color:%s">ahead</span> or <span style="color:%s">behind</span> in xG', state_pal[5], state_pal[1]),
    subtitle = '2021/22 Premier League, last 5 matches'
  )
p

## Reference: https://github.com/ajreinhard/data-viz/blob/master/ggplot/plot_SB.R
p_bld <- ggplot_gtable(ggplot_build(p))
grob_strip_index <- which(sapply(p_bld$grob, function(x) x$name)=='strip')
facet_id <- sapply(grob_strip_index, function(grb) {
  p_bld$grobs[[grb]]$grobs[[1]]$children[[2]]$children[[1]]$label
})
# p_bld$layout$z[grob_strip_index] <- 0 ## not sure what the point of this is...

for (i in 1:length(facet_id)) {
  id <- facet_id[i]
  url <- team_mapping %>% filter(team == !!id) %>% pull(url_logo_espn)
  lab <-
    grid::textGrob(
      id,
      x = unit(0, 'npc'),
      gp = grid::gpar(
        col = 'white',
        fontfamily = 'Karla',
        fontface = 'bold',
        fontsize = 11
      ),
      hjust = 0
    )
  img <-
    grid::rasterGrob(
      image = magick::image_read(url),
      # just = 'right',
      hjust = 1,
      x = unit(1, 'npc'),
      ## 1 and 0.75 is also fine
      vp = grid::viewport(height = 0.8, width = 0.6)
    )
  tot_tree <- grid::grobTree(lab, img)
  p_bld$grobs[[grob_strip_index[i]]] <- tot_tree
}
p <- cowplot::ggdraw(p_bld)
base_size <- 8
asp <- 1.5
path_viz <- file.path(dir_proj, '202122_xg_game_state.png')
ggsave(
  filename = path_viz, 
  plot = cowplot::ggdraw(p), 
  height = base_size, 
  width = base_size * asp
)

tonythemes:::add_logo(
  path_viz = path_viz,
  path_logo = file.path(dir_proj, 'epl-logo-white.png'),
  delete = TRUE,
  logo_scale = 0.1,
  idx_x = 0.01,
  idx_y = 0.98,
  adjust_y = FALSE
)

