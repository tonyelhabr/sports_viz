
library(tidyverse)
dir_proj <- '29-202021_u2x'
dir_data <- file.path(dir_proj, 'data')

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 24, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown('Karla', face = 'bold', size = 18, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  axis.title = element_text(size = 18, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  plot.caption = ggtext::element_markdown('Karla', size = 16, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 16, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.01),
  legend.text = element_text(color = 'gray20', size = 14),
  strip.text = element_text(color = 'gray20', size = 14),
  strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

stats <-
  fs::dir_ls(dir_data, regexp = 'rds$') %>% 
  tibble(path = .) %>% 
  # slice(1) %>% 
  mutate(
    data = map(path, ~read_rds(.x)) #  %>% mutate(across(Wk, as.integer), across(matches('xG$'), as.numeric)))
  ) %>% 
  select(-path) %>% 
  unnest(data) %>% 
  janitor::clean_names()
stats

stats_filt <-
  stats %>% 
  filter(season %in% c('2020-2021') & comp == '1. Premier League' & country == 'eng ENG')
stats_filt
# stats_agg %>% pull(squad) %>% clipr::write_clip()

.do_agg <- function(data) {
  data %>% 
    select(squad, player_name, mp, starts, min) %>% 
    group_by(squad) %>% 
    summarize(
      across(c(mp:min), sum, na.rm = TRUE),
      n_player = n_distinct(player_name)
    ) %>% 
    ungroup() 
}

stats_agg <-
  stats_filt %>% 
  .do_agg() # %>% 
# rename_with(~sprintf('%s_total', .x), c(mp:n_player))
stats_agg_long <- stats_agg %>% pivot_longer(-squad, names_to = 'stat', values_to = 'total')
stats_agg_long

age_seq <- seq.int(20, 23, by = 1L)

do_agg <- function(.age) {
  res <-
    stats_filt %>% 
    filter(!is.na(age) & age <= .age) %>% 
    .do_agg() %>% 
    pivot_longer(-squad, names_to = 'stat', values_to = 'value') %>% 
    left_join(stats_agg_long) %>% 
    mutate(frac = value / total)
  res
}

# TODO: Maybe add some labels?
do_agg_players <- function(.age) {
  res <-
    stats_filt %>% 
    filter(!is.na(age) & age <= .age) %>% 
    filter(!is.na(starts) & starts > 0L) %>% 
    mutate(lab = sprintf('%s (%s)', player_name, age)) %>% 
    group_by(squad) %>% 
    arrange(desc(starts), .by_group = TRUE) %>% 
    mutate(n = n()) %>% 
    mutate(
      across(
        lab,
        ~case_when(
          n == 1L ~ .x,
          TRUE ~ paste0(.x, collapse = ', ', sep = '')
        )
      )
    ) %>% 
    ungroup() %>% 
    distinct(squad, lab)
  res
}

age_seq_named <-
  age_seq %>% 
  setNames(., sprintf('U%d', .))
age_seq_named

grps_init <-
  age_seq_named %>% 
  map_dfr(do_agg, .id = 'grp')
grps_init

grps <-
  age_seq_named %>% 
  map_dfr(do_agg, .id = 'grp') %>% 
  complete(
    crossing(
      stats_filt %>% distinct(squad), 
      grps_init %>% distinct(stat),
      grp = age_seq_named %>% names()
    )
  ) %>% 
  mutate(
    across(where(is.integer), ~coalesce(.x, 0L)),
    across(where(is.numeric), ~coalesce(.x, 0))
  ) %>%
  arrange(grp, stat, desc(frac))
grps

grps_filt <-
  grps %>% 
  filter(stat == 'starts') %>% 
  select(-stat) %>% 
  group_by(grp) %>% 
  mutate(
    rnk_inv = row_number(value)
  ) %>% 
  ungroup()
grps_filt

lab_grps <-
  tibble(
    age_cutoff = age_seq,
    grp = age_seq_named %>% names(),
    lab_grp = c(
      '"<span style=\'color:#FDB913\'>Wolves</span>\' youth tanked them this season!"',
      '"<span style=\'color:#1b458f\'>Chelsea</span> are surely going to win the title next year now that they have more experience!"',
      '"<span style=\'color:#cc0000\'>Fulham</span>\'s dependence on young players got them relegated!"',
      '"<span style=\'color:#1358a1\'>Leicester</span> are ready to repeat 2015-16 title run with their young talent!"'
    )
  ) %>%
  # gplots::col2hex('grey20')
  mutate(
    across(lab_grp, ~sprintf('Age cut-off: <b><span style="color:#333333;font-size:24pt">%d</span><b/><br/>%s', age_cutoff, .x))
  )
lab_grps

# Source this locally, even though could also get this from espn
dir_img <- file.path('24-202021_game_state_fouls', 'img')
grps_viz <-
  grps_filt %>% 
  # left_join(grps_top) %>% 
  left_join(
    xengagement::team_accounts_mapping %>% 
      mutate(
        across(
          color_pri,
          ~case_when(
            team_abbrv == 'WOL' ~ color_sec,
            TRUE ~ .x
          )
        )
      ) %>% 
      mutate(path_local = file.path(dir_img, sprintf('%s.png', team))) %>% 
      select(squad = squad_fbref, color_pri, path_local, url = url_logo_espn)
  ) %>% 
  # filter(grp == 'U23') %>% 
  left_join(lab_grps) %>% 
  relocate(squad)
grps_viz

grps_top <-
  grps_filt %>% 
  group_by(grp) %>% 
  slice_max(value, with_ties = FALSE) %>% 
  ungroup() %>%  
  arrange(grp)
grps_top

squads_top <- grps_top %>% pull(squad)

p <-
  grps_viz %>%
  ggplot() +
  aes(x = value, y = rnk_inv) +
  geom_tile(
    data = grps_viz %>% filter(!(squad %in% squads_top)),
    aes(
      x = value / 2,
      width = value,
      height = 0.1
    ),
    color = NA,
    fill = 'black'
  ) +
  geom_tile(
    data = grps_viz %>% filter((squad %in% squads_top)),
    aes(
      x = value / 2,
      width = value,
      height = 0.9,
      fill = I(color_pri)
    ),
    color = NA
  ) +
  ggimage::geom_image(
    data = grps_viz %>% filter(!(squad %in% squads_top)),
    size = 0.05,
    aes(image = url)
    # aes(image = path_local)
  ) +
  ggimage::geom_image(
    data = grps_viz %>% filter((squad %in% squads_top)),
    size = 0.08,
    aes(image = url)
    # aes(image = path_local)
  ) +
  # facet_wrap(~grp, scales = 'free') +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = 'Which Team Has Started the Most "Young" Players?',
    # subtitle = '{} is {}',
    # subtitle = 'Change the narrative based on your age cut-off',
    caption = 'Player ages based on latest player listing on fbref.com<br/>2020/21 Premier League, through Matchweek 36',
    tag = '**Viz**: Tony ElHabr | **Data**: fbref',
    y = NULL, 
    x = 'Total Starts'
  )
p

transition_length <- 0.5
state_length <- 1.5
anim <-
  p +
  gganimate::transition_states(
    grp,
    transition_length = !!transition_length,
    state_length = !!state_length,
    wrap = FALSE
  ) +
  gganimate::view_follow(fixed_y = TRUE) +
  labs(
    subtitle = '{lab_grps %>% filter(grp == closest_state) %>% pull(lab_grp)}',
    x = 'Total Starts by {closest_state} Players'
  )
# anim

path_gif <- fs::path(dir_proj, 'viz_ux.gif')
n_sec <- nrow(lab_grps) * (1.5 + 0.5)
fps <- 20
n_sec_end <- 2
height <- 800
width <- 800
n_frame <- (n_sec + n_sec_end) * fps

gganimate::animate(
  anim,
  nframe = n_frame,
  fps = fps,
  height = height,
  width = width,
  type = 'cairo',
  renderer = gganimate::gifski_renderer(path_gif),
  end_pause = n_sec_end * fps
)
