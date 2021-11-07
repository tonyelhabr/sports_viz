
library(tidyverse)
library(qs)
library(janitor)
library(lubridate)
library(ggplot2)
library(extrafont)
library(nflplotR)
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
  plot.title = element_text('Karla', face = 'bold', size = 24, color = 'white'),
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
    xg_diff = xgot - xg
  )

## how many matches per league?
df %>% 
  distinct(league_name, match_id, date) %>% 
  group_by(league_name) %>% 
  summarize(
    n = n(),
    min_date = min(date)
  )

## max on goal y (with on_goal_shot$zoomRatio == 1):
## 0.0317 <= on_goal_shot$x <= 1.97, or 37.7 <= y_g <= 30.3, # 7.4 / 1.94
## 0.0519 <= on_goal_shot$x <= 1.95 or 37.6 <= y_g <= 30.4 if z_g is not NA # 7.2 / 1.9
## max on goal y (with on_goal_shot$zoomRatio == 1):
## 0.0102 <= on_goal_shot$y <= 0.613, 0.0385 <= z_g <= 2.32  if z_g is not NA
## actual dims are 7.3152 m wide and 2.4384 m height

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

is_in_outer_h <- function(z, buffer = 1) {
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
  geom_path() +
  coord_fixed(
    xlim = c(half_y - half_w_g - w_g_buffer, half_y + half_w_g + w_g_buffer), 
    ylim = c(0, h_g + 1)
  ) +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  labs(x = NULL, y = NULL)

df_outer_grp <- 
  df_outer %>% 
  select(date, league_name, match_id, id, xg, xgot, is_on_frame, is_in_w, is_in_h, x, y, y_g, z_g) %>% 
  mutate(
    grp = case_when(
      !is_on_frame ~ 'miss',
      is_on_frame & !is_in_w & !is_in_h ~ 'easy',
      is_in_w & !is_in_h ~ 'w only',
      !is_in_w & is_in_h ~ 'h only',
      is_in_w & is_in_h ~ 'w+h'
    )
  )

gg_base +
  geom_point(
    data = df_outer_grp,
    alpha = 0.5,
    aes(x = y_g, y = z_g, color = grp)
  )

gg_base +
  geom_point(
    data = df_outer %>% 
      # filter(is_on_frame & is_in_outer_frame) %>% 
      filter(!is_penalty) %>% 
      filter(situation == 'FreeKick'),
    aes(x = y_g, y = z_g, color = league_name, size = xg_diff)
  )

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

fs::dir_create(dir_proj, 'players')
date_start_mls_2021 <- lubridate::ymd('2021-04-16')
df_outer_filt %>% 
  count(player_id, player_name)

base_url <- 'https://www.fotmob.com/'
x <- paste0(base_url, 'playerData?id=789068') %>% jsonlite::fromJSON()
footedness <- x$playerProps %>% filter(title == 'Preferred foot') %>% pull(value)


retreive_footedness <- function(id) {
  base_url <- 'https://www.fotmob.com/'
  res <- paste0(base_url, 'playerData?id=', id) %>% jsonlite::fromJSON()
  value <- res$playerProps %>% 
    filter(title == 'Preferred foot') %>% 
    pull(value)
  ifelse(length(value) == 0, 'right', value)
}

df_outer_filt <- df_outer %>% 
  drop_na(y_g, xgot) %>% 
  # filter(is.na(is_penalty) | !is_penalty) %>% 
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
    ## exists after download is done one
    path_png = file.path(dir_proj, 'players', sprintf('%d.png', player_id)),
    dist = sqrt((max_x - x)^2 + (y_g - y)^2),
    # sign = case_when(
    #   y <= (max_y / 2) & y_g <= (max_y / 2) ~ -1,
    #   y <= (max_y / 2) & y_g >= (max_y / 2) ~ 1,
    #   y >= (max_y / 2) & y_g <= (max_y / 2) ~ -1,
    #   y >= (max_y / 2) & y_g >= (max_y / 2) ~ 1,
    #   TRUE ~ NA_real_
    # ),
    sign = ifelse(footedness == 'right', 1, -1),
    curvature = 0.2 * sign * dist / max(abs(dist))
  )

## https://twitter.com/NYCFC/status/1450998669252104198?s=20
# df_outer_filt %>% 
#   arrange(desc(date)) %>% 
#   head(1) %>% 
#   glimpse()

## donwload player images once
if(FALSE) {
  df_outer_filt$player_id %>% 
    unique() %>% 
    walk(
      ~sprintf('https://images.fotmob.com/image_resources/playerimages/%d.png', .x) %>% 
        download.file(destfile = file.path(dir_proj, 'players', sprintf('%d.png', .x)), mode = 'wb', quiet = TRUE)
    )
}

lab_suffix_base <- ' direct free kicks on target and within 1 meter of the frame'
lab_suffix_2 <- sprintf(' of%s', lab_suffix_base)
lab_subtitle <- 'MLS 2021 season'
p_shotmap <- df_outer_filt %>% 
  ggplot() +
  gg_pitch() +
  aes(x = x, y = y) +
  ggimage::geom_image(
    size = 0.05,
    asp = 1 / .asp,
    aes(x = x - 3, y = y, image = path_png)
  ) +
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
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14)
  ) +
  labs(
    title = glue::glue('Goals from{lab_suffix_base}'),
    subtitle = lab_subtitle
  )
p_shotmap
w_shotmap <- 8
# nflplotR::ggpreview(p, width = w_shotmap, height = w_shotmap * .asp)
ggsave(
  filename = file.path(dir_proj, 'shotmap.png'),
  plot = p_shotmap,
  width = w_shotmap,
  height = w_shotmap * .asp
)

## match sample sizes?
# ns_outer <- df_outer %>% 
#   filter(league_name == 'MLS' & date >= !!date_start_mls_2021) %>% 
#   group_by(league_name, situation, is_on_frame, is_in_outer_frame) %>% 
#   summarize(
#     n = n()
#   ) %>% 
#   ungroup()
# ns_outer

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

## code from 202021_soccer_refs
pal <- c(scales::hue_pal()(5), 'white')
league_logos <-
  tibble(
    league_name =  c('EPL', 'LaLiga', 'Ligue 1', 'Bundesliga', 'Serie A', 'MLS'),
    path_logo = file.path('25-202021_soccer_refs', sprintf('%s.png', c('epl-150px', 'la-liga-150px', 'ligue-1', 'bundesliga', 'serie-a', 'mls'))),
    # path_logo = file.path(dir_proj, sprintf('%s.png', c('epl-150px', 'mls'))),
    idx_logo = c(1L, 2L, 4L, 3L, 5L, 6L),
    color = c(pal[5], pal[2], pal[3], pal[1], pal[4], pal[6])
  ) %>% 
  mutate(img = glue::glue("<img src={path_logo} width='40' height='40'/>")) %>% 
  # mutate(across(c(path_logo, img), ~fct_reorder(.x, idx_logo)))
  arrange(idx_logo)
league_logos

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
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, .5)) +
  theme(
    plot.title = element_text(size = 16),
    axis.text.y = ggtext::element_markdown(size = 10.5),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    y = NULL,
    title = glue::glue('MLS has the highest proportion{lab_suffix_2}'),
    subtitle = glue::glue('{lab_subtitle} and 2020/21-21/22 seasons for Big 5'),
    caption = 'Error bars indicate 95% credible interval for emperical Bayes adjusted percentage.\nOn frame shots does not have to result in a goal to qualify.',
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

