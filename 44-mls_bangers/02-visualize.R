
library(tidyverse)
library(qs)
library(janitor)
library(lubridate)
library(scales)
library(extrafont)
library(ggtext)
library(ggsoccer)
library(ggimage)
library(ebbr)

dir_proj <- '44-mls_bangers'

gray_wv <- rgb(24, 24, 24, maxColorValue = 255)
gray_grid_wv <- rgb(64, 64, 64, maxColorValue = 255)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla', color = 'white'),
  title = element_text('Karla', size = 14, color = 'white'),
  plot.title = element_text('Karla', face = 'bold', size = 16, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', size = 14, color = '#f1f1f1'),
  axis.text = element_text('Karla', color = 'white', size = 14),
  axis.title = element_text('Karla', size = 14, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = gray_grid_wv),
  panel.grid.minor = element_line(color = gray_grid_wv),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = gray_wv, color = gray_wv),
  plot.caption = element_text('Karla', color = 'white', hjust = 1, size = 11, face = 'italic'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'white', hjust = 0),
  plot.tag.position = c(0.01, 0.01),
  strip.text = element_text('Karla', color = 'white', face = 'bold', size = 12, hjust = 0),
  panel.background = element_rect(fill = gray_wv, color = gray_wv)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))


## max on goal y (with on_goal_shot$zoomRatio == 1):
## 0.0317 <= on_goal_shot$x <= 1.97, or 37.7 <= y_g <= 30.3, # 7.4 / 1.94
## 0.0519 <= on_goal_shot$x <= 1.95 or 37.6 <= y_g <= 30.4 if z_g is not NA # 7.2 / 1.9
## max on goal y (with on_goal_shot$zoomRatio == 1):
## 0.0102 <= on_goal_shot$y <= 0.613, 0.0385 <= z_g <= 2.32  if z_g is not NA
## actual dims are 7.3152 m wide and 2.4384 m height

convert_ogs_y_to_z_g <- function(y) {
  y * 2.32 / 0.613
}

df <- file.path(dir_proj, 'data.qs') %>% 
  qs::qread() %>% 
  # filter(!is_own_goal) %>% 
  distinct(match_id, id, .keep_all = TRUE) %>% 
  mutate(
    is_g = event_type == 'Goal',
    league_grp = case_when(
      league_name == 'MLS' ~ 'MLS',
      # league_name == 'Ligue 1' ~ 'Ligue 1',
      TRUE ~ 'Big 5'
    ),
    is_penalty = (xg == 0.7884 & round(x) == 94 & round(y) == 34),
    xg_tier = case_when(
      is_penalty ~ NA_character_,
      is.na(xg) ~ NA_character_,
      xg >= 0.33 ~ 'great',
      xg >= 0.15 ~ 'good',
      xg >= 0.05 ~ 'average',
      TRUE ~ 'poor'
    ) %>% 
      ordered(c('great', 'good', 'average', 'poor', NA_character_)),
    xg_diff = xgot - xg,
    across(
      z_g,
      ~case_when(
        !is.na(xgot) ~ coalesce(.x, convert_ogs_y_to_z_g(on_goal_shot$y)),
        TRUE ~ .x
      )
    )
  )

## how many matches per league?
df %>% 
  group_by(league_name, match_id, situation, date) %>% 
  summarize(
    shots = n()
  ) %>% 
  ungroup() %>% 
  group_by(league_name, situation) %>% 
  summarize(
    across(shots, sum),
    min_date = min(date)
  )

w_crossbar <- 0.12
w_g <- 7.3152
h_g <- 2.4384
max_y <- 68
max_x <- 105
half_y <- max_y / 2
half_w_g <- w_g / 2
is_in_outer_w <- function(y, buffer = 1) {
  case_when(
    y >= (half_y - half_w_g) & y <= (half_y - half_w_g + buffer) ~ TRUE,
    y <= (half_y + half_w_g) & y >= (half_y + half_w_g - buffer) ~ TRUE,
    TRUE ~ FALSE
  )
}

is_in_outer_h <- function(z, buffer = 0.5) {
  case_when(
    z <= h_g & z >= (h_g - buffer) ~ TRUE,
    TRUE ~ FALSE
  )
}

is_in_w <- function(y) {
  y >= (half_y - half_w_g) & y <= (half_y + half_w_g)
}

is_in_h <- function(z) {
  z >= 0 & z <= h_g
}

df_outer <- df %>% 
  drop_na(z_g) %>% 
  mutate(
    is_on_frame = is_in_w(y_g) & is_in_h(z_g),
    is_in_w = is_on_frame & is_in_outer_w(y_g),
    is_in_h = is_on_frame & is_in_outer_h(z_g),
    is_in_outer_frame = is_in_w | is_in_h
  )

w_g_buffer <- 1
gg_base <-
  tibble(
    x = c(half_y - half_w_g, half_y - half_w_g, half_y + half_w_g, half_y + half_w_g),
    y = c(0, h_g, h_g, 0)
  ) %>% 
  ggplot() +
  aes(x = x, y = y) + 
  geom_path(
    color = 'white',
    size = 2
  ) +
  coord_fixed(
    xlim = c(half_y - half_w_g - w_g_buffer, half_y + half_w_g + w_g_buffer), 
    ylim = c(0, h_g + 0.1)
  ) +
  theme(
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  labs(x = NULL, y = NULL)

# Reference: https://github.com/Torvaney/ggsoccer/blob/master/R/dimensions.R
.pitch_international <- list(
  length = 105,
  width = 68,
  penalty_box_length = 16.5,
  penalty_box_width = 40.32,
  six_yard_box_length = 5.5,
  six_yard_box_width = 18.32,
  penalty_spot_distance = 11,
  goal_width = 7.32,
  origin_x = 0,
  origin_y = 0
)

.xlim <- c(max_x / 2 + 20, max_x + 1)
.ylim <- c(0 - 1, max_y + 1)
.asp <- (.xlim[2] - .xlim[1]) / (.ylim[2] - .ylim[1])
gg_pitch <-
  function(pitch = .pitch_international,
           xlim = .xlim,
           ylim = .ylim,
           aspect_ratio = .asp,
           fill = gray_wv,
           color = 'white',
           limits = FALSE,
           ...) {
    list(
      ...,
      ggsoccer::annotate_pitch(
        dimensions = pitch,
        fill = fill,
        colour = color,
        limits = limits
      ),
      coord_flip(xlim = xlim, ylim = ylim),
      ggsoccer::theme_pitch(aspect_ratio = aspect_ratio),
      theme(legend.position = 'none')
    )
  }


date_start_mls_2021 <- lubridate::ymd('2021-04-16')

df_outer_fk <- df_outer %>% 
  drop_na(y_g, xgot) %>% 
  filter(
    is_in_outer_frame & 
      !is_penalty &
      # league_name == 'MLS' & 
      situation == 'FreeKick' & 
      date >= !!date_start_mls_2021
  )

## code from 202021_soccer_refs
pal <- c(scales::hue_pal()(5), 'white')
league_logos <-
  tibble(
    league_name =  c('EPL', 'LaLiga', 'Ligue 1', 'Bundesliga', 'Serie A', 'MLS'),
    path_logo = file.path('25-202021_soccer_refs', sprintf('%s.png', c('epl-150px', 'la-liga-150px', 'ligue-1', 'bundesliga', 'serie-a', 'mls'))),
    idx_logo = c(1L, 2L, 4L, 3L, 5L, 6L),
    color = c(pal[5], pal[2], pal[3], pal[1], pal[4], pal[6])
  ) %>% 
  mutate(img = glue::glue("<img src={path_logo} width='40' height='40'/>")) %>% 
  arrange(idx_logo)
league_logos

lab_tag <- '**Viz**: Tony ElHabr'
lab_suffix_base <- ' direct free kicks on target and near goal frame'
lab_suffix_2 <- sprintf(' of%s', lab_suffix_base)
lab_explainer <- '"Near goal frame" = within 1 meter of goal posts or 0.5 meter within goal crossbar.'
lab_subtitle <- 'MLS 2021 season'
lab_subtitle_2 <- sprintf('%s and 2020/21-21/22 seasons for Big 5 leagues', lab_subtitle)

p_frame <-
  gg_base +
  geom_point(
    data = df_outer_fk %>% filter(league_name != 'MLS'),
    alpha = 0.25,
    aes(x = y_g, y = z_g, color = league_name, size = xg_diff)
  ) +
  geom_point(
    data = df_outer_fk %>% filter(league_name == 'MLS'),
    aes(x = y_g, y = z_g, color = league_name, size = xg_diff)
  ) +
  guides(
    size = guide_legend('xGoT - xG', override.aes = list(color = 'gray50')),
    color = guide_legend('League', override.aes = list(size = 3))
  ) +
  scale_color_manual(
    values = league_logos %>% select(league_name, color) %>% deframe()
  ) +
  scale_radius(range = c(1, 8)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = 'bold'),
    legend.position = 'top'
  ) +
  labs(
    title = sprintf('Shots from%s', lab_suffix_base),
    tag = lab_tag,
    caption = sprintf('%s\nData: %s.', lab_explainer, lab_subtitle_2)
  )
p_frame

h_frame <- 5
ggsave(
  filename = file.path(dir_proj, 'frame.png'),
  plot = p_frame,
  height = h_frame,
  width = h_frame * 2
)

df_outer_filt <- df_outer %>% 
  drop_na(y_g, xgot) %>% 
  filter(
    is_g &
      !is_penalty &
      is_in_outer_frame & 
      league_name == 'MLS' & 
      situation == 'FreeKick' & 
      date >= !!date_start_mls_2021
  ) %>% 
  mutate(
    footedness = map_chr(player_id, retreive_footedness),
    path_png = file.path(dir_proj, 'players', sprintf('%d.png', player_id)),
    dist = sqrt((max_x - x)^2 + (y_g - y)^2),
    sign = ifelse(footedness == 'right', 1, -1),
    curvature = 0.2 * sign * dist / max(abs(dist))
  )

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

p_shotmap <- df_outer_filt %>% 
  ggplot() +
  gg_pitch() +
  aes(x = x, y = y) +
  map(
    df_outer_filt %>% 
      group_split(row_number()), ~{
        geom_curve(
          data = .x,
          color = '#f8de7f', # 'yellow',
          aes(
            color = event_type,
            xend = max_x,
            yend = y_g
          ),
          curvature = .x$curvature
        )
      }
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    title = glue::glue('Goals from{lab_suffix_base}'),
    subtitle = lab_subtitle,
    tag = lab_tag,
    caption = lab_explainer
  )

w_shotmap <- 10
ggsave(
  filename = file.path(dir_proj, 'shotmap.png'),
  plot = p_shotmap,
  width = w_shotmap,
  height = w_shotmap * .asp
)

agg_by_spot <- df_outer %>% 
  group_by(league_name, situation, is_on_frame, is_in_outer_frame) %>% 
  summarize(
    n = n()
  ) %>% 
  ungroup() %>% 
  group_by(league_name, situation) %>% 
  mutate(
    total = sum(n),
    prop = n / total
  ) %>% 
  ungroup() %>% 
  group_by(situation, is_on_frame, is_in_outer_frame) %>% 
  mutate(
    rnk = row_number(desc(prop))
  ) %>% 
  ungroup()
agg_by_spot

agg_by_spot_filt <- agg_by_spot %>%
  add_ebb_estimate(n, total, method = 'mle', prior_subset = n > 100) %>%
  filter(situation == 'FreeKick') %>% 
  filter(
    is_on_frame & is_in_outer_frame
  ) %>% 
  mutate(
    across(league_name, ~fct_reorder(.x, -rnk))
  )
agg_by_spot_filt

p_bars <- 
  agg_by_spot_filt %>% 
  ggplot() +
  aes(x = .raw, y = league_name, color = league_name) +
  scale_y_discrete(name = '', labels = league_logos %>% select(league_name, img) %>% deframe()) +
  scale_color_manual(values = league_logos %>% select(league_name, color) %>% deframe()) +
  guides(color = 'none') +
  geom_errorbarh(
    aes(xmin = .low, xmax = .high),
    size = 1.5,
    height = 0.4
  ) +
  geom_point(
    size = 4
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.2, 0.5)) +
  theme(
    plot.title = element_text(size = 16),
    axis.text.y = ggtext::element_markdown(size = 10.5),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    y = NULL,
    title = glue::glue('MLS has the highest proportion{lab_suffix_2}'),
    subtitle = lab_subtitle_2,
    tag = lab_tag,
    caption = sprintf('%s\nError bars indicate 95%% credible interval for emperical Bayes adjusted percentage.\nOn frame shots does not have to result in a goal to qualify.', lab_explainer),
    x = glue::glue('%{lab_suffix_2}')
  )
p_bars

h_bars <- 7
ggsave(
  filename = file.path(dir_proj, 'prop.png'),
  plot = p_bars,
  width = h_bars * 1.5,
  height = h_bars
)

