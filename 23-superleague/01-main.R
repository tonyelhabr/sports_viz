
library(tidyverse)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = element_text('Karla', face = 'bold', size = 24, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', face = 'bold', size = 16, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  plot.caption = element_text('Karla', size = 14, color = 'gray20', hjust = 0),
  plot.caption.position = 'plot',
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

dir_proj <- '23-superleague'
path_gif <- fs::path(dir_proj, 'superleague_timeline.gif')
n_sec <- 10
fps <- 20
n_sec_end <- 3
height <- 400
width <- 800
n_frame <- (n_sec + n_sec_end) * fps

# main ----
# Decided not to use these other files.
paths <- fs::dir_ls(dir_proj, regexp = 'csv$')
paths


labs_grp <-
  tibble(
    grp = seq.int(1L, 4L),
    lab_grp = c('Super League, eh?', 'Mourinho\'s sacking peaked interest like the Super League', 'UEFA was discussed along with the Super League', 'Discussion of Champions League matches (last Wed.) was much bigger')
  )

df <-
  paths %>% 
  tibble(path = .) %>% 
  arrange(path) %>% 
  mutate(grp = row_number(path)) %>% 
  mutate(
    data = map(path, ~read_csv(.x, skip = 2, col_names = TRUE))
  ) %>% 
  select(-path) %>% 
  unnest(data) %>%
  rename_all(~str_remove_all(.x, '[:]\\s\\(Worldwide\\)')) %>% 
  janitor::clean_names() %>% 
  select(-c(chelsea, florentino_perez)) %>% 
  mutate(
    across(-c(grp, time), ~ifelse(.x == '<1', 0, as.integer(.x) / 100)),
    across(time, ~.x + lubridate::hours(5))
  ) %>% 
  left_join(labs_grp) %>% 
  mutate(across(lab_grp, ~fct_reorder(.x, grp))) %>% 
  mutate(drop = case_when(grp <= 3L & time <= lubridate::ymd('2021-04-18') ~ TRUE, TRUE ~ FALSE)) %>% 
  filter(!drop) %>% 
  select(-drop)

# df %>% count(super_league, sort = TRUE)
pal <- c('#003f5c', '#bc5090', '#ffa600')
nms_actual <- c('super_league', 'jose_mourinho', 'uefa_champions_league')
nms_pretty <- c('Super League', 'José Mourinho', 'UEFA Champions League')
nms <-
  tibble(
    name = nms_actual,
    name_pretty = nms_pretty,
  )

df_long <-
  df %>% 
  pivot_longer(super_league:uefa_champions_league) %>% 
  drop_na(value) %>% 
  left_join(nms) %>% 
  select(-name) %>% 
  rename(name = name_pretty) %>% 
  mutate(
    across(name, ~ordered(.x, nms_pretty))
  )
df_long

require(gganimate)
.slice_f <- function(col, f = slice_min) {
  df_long %>% 
    group_by(grp) %>% 
    f({{col}}, with_ties = FALSE) %>% 
    ungroup() %>% 
    pull({{col}})
}
x_mins <- .slice_f(time, slice_min)
x_maxs <- .slice_f(time, slice_max)
y_mins <- .slice_f(value, slice_min)
y_maxs <- .slice_f(value, slice_max)

labs_init <-
  df_long %>% 
  group_by(grp, name) %>% 
  mutate(across(value, list(prnk = ~percent_rank(.x)), .names = '{fn}')) %>% 
  filter(prnk == 1) %>% 
  # filter(prnk >= 0.99) %>% 
  arrange(prnk) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-prnk)
labs_init

times <-
  df_long %>% 
  group_by(grp) %>% 
  mutate(
    across(time, list(min = min, max = max))
  ) %>% 
  ungroup()
times

labs <-
  labs_init %>%
  left_join(times) %>% 
  mutate(
    time_diff1 = as.numeric(time - time_min, 'hours'),
    time_diff2 = as.numeric(time_max - time, 'hours'),
    side = case_when(
      time_diff1 < time_diff2 ~ 'right',
      TRUE ~ 'left'
    )
  ) %>% 
  mutate(
    across(
      value,
      ~case_when(
        .x == 1 ~ 0.9,
        .x < 0.2 ~ .x + 0.1,
        # .x > 0.7 ~ .x + 0.05,
        TRUE ~ .x + 0.05
      )
    ),
    across(
      time,
      list(new = ~case_when(
        side == 'right' ~.x + lubridate::hours(4),
        side == 'left' ~.x - lubridate::hours(4)
      )
      )
    ),
    # Corner cases
    across(
      value,
      ~case_when(
        grp %in% c(2L, 3L, 4L) & name == 'José Mourinho' ~ .x + 0.06,
        grp == 3L & name == 'UEFA Champions League' ~ .x + 0.35,
        TRUE ~ .x
      )
    )
  ) %>% 
  select(-time) %>% 
  rename(time = time_new)
labs

f_text <- function(..., .side = 'left') {
  list(
    ...,
    geom_text(
      data = labs %>% filter(side == .side), #  %>% filter(grp == 4L),
      show.legend = FALSE,
      check_overlap = TRUE,
      size = 5,
      fontface = 'bold',
      hjust = 0,
      aes(x = time, y = value, label = name, color = name)
    )
  )
}

p <-
  df_long %>% 
  # filter(grp != 5L) %>% 
  # filter(grp == 4) %>% 
  ggplot() +
  aes(x = time, y = value, color = name) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  f_text(.side = 'left') +
  f_text(.side = 'right') +
  scale_x_datetime(date_breaks = '1 day', date_labels = '%a (%dth)') +
  scale_color_manual(values = setNames(pal, nms_pretty)) +
  geom_line(size = 1.25, show.legend = FALSE) +
  guides(color = FALSE) +
  theme(plot.caption = ggtext::element_markdown()) +
  labs(
    title = 'Timeline of Super League Interest',
    caption = '**Viz**: Tony ElHabr | **Source**: Google Trends',
    x = NULL,
    y = 'Relative Interest Over Time'
  )
# p

anim <-
  p + 
  transition_states(lab_grp, transition_length = 0.5, state_length = 1.5, wrap = FALSE) + 
  labs(subtitle = '{closest_state}') +
  # ease_aes(x = 'cubic-in-out', y = 'cubic-in-out') + 
  view_follow(fixed_y = TRUE)
# anim

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
