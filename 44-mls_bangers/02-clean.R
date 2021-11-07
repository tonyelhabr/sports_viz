
library(tidyverse)
library(qs)
library(janitor)
library(lubridate)
library(ebbr)

dir_proj <- '44-mls_bangers'

df <- file.path(dir_proj, 'data.qs') %>% 
  qs::qread() %>% 
  # filter(!is_own_goal) %>% 
  distinct(match_id, id, .keep_all = TRUE) %>% 
  mutate(
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
## actual dims are 7.3152 m wide and 2.4384 m height, 
# df %>%
#   drop_na(xgot, z_g) %>%
#   select(xg, xgot, y_g, z_g, on_goal_shot) %>% 
#   arrange(desc(on_goal_shot$x))

w_crossbar <- 0.12
w_g <- 7.3152 # - w_crossbar
h_g <- 2.4384 # - w_crossbar
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

gg_base +
  geom_point(
    data = df_outer %>% filter(!is_on_frame),
    color = 'grey50',
    alpha = 0.5,
    aes(x = y_g, y = z_g)
  ) +
  geom_point(
    data = df_outer %>% filter(is_on_frame & !is_in_w & !is_in_h),
    color = 'black',
    alpha = 0.5,
    aes(x = y_g, y = z_g)
  ) +
  geom_point(
    data = df_outer %>% filter(is_in_w & !is_in_h),
    color = 'blue',
    alpha = 0.5,
    aes(x = y_g, y = z_g)
  ) +
  geom_point(
    data = df_outer %>% filter(!is_in_w & is_in_h),
    color = 'red',
    alpha = 0.5,
    aes(x = y_g, y = z_g)
  ) +
  geom_point(
    data = df_outer %>% filter(is_in_w & is_in_h),
    color = 'green',
    alpha = 0.5,
    aes(x = y_g, y = z_g)
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


.xlim <- c(max_x / 2 + 25, max_x + 1)
.ylim <- c(0 - 1, max_y + 1)
.asp <- (.xlim[2] - .xlim[1]) / (.ylim[2] - .ylim[1])
gg_pitch <-
  function(pitch = .pitch_international,
           xlim = .xlim,
           ylim = .ylim,
           aspect_ratio = .asp,
           fill = 'white',
           color = 'grey80',
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


df_outer_filt <- df_outer %>% 
  drop_na(y_g, xgot) %>% 
  # filter(is.na(is_penalty) | !is_penalty) %>% 
  filter(
    # event_type == 'Goal' &
      league_name == 'MLS' & 
      situation == 'FreeKick' & 
      !is_penalty & 
      is_in_outer_frame & 
      date >= lubridate::ymd('2021-04-16')
  ) %>% 
  mutate(
    dist = sqrt((max_x - x)^2 + (y_g - y)^2),
    sign = case_when(
      y <= (max_y / 2) & y_g <= (max_y / 2) ~ -1,
      y <= (max_y / 2) & y_g >= (max_y / 2) ~ 1,
      y >= (max_y / 2) & y_g <= (max_y / 2) ~ -1,
      y >= (max_y / 2) & y_g >= (max_y / 2) ~ 1,
      TRUE ~ NA_real_
    ),
    curvature = 0.2 * sign * dist / max(abs(dist))
  )

# https://twitter.com/NYCFC/status/1450998669252104198?s=20
df_outer_filt %>% 
  arrange(desc(date)) %>% 
  head(1) %>% 
  glimpse()

p <- df_outer_filt %>% 
  ggplot() +
  gg_pitch() +
  aes(x = x, y = y) +
  map(
    df_outer_filt %>% 
      group_split(row_number()), ~{
        geom_curve(
          data = .x,
          # color = 'grey20',
          aes(
            color = event_type,
            xend = max_x,
            yend = y_g
          ),
          curvature = .x$curvature
        )
      })
p
w_shotmap <- 8
ggsave(
  filename = file.path(dir_proj, 'shotmap.png'),
  plot = p,
  width = w_shotmap,
  height = w_shotmap * .asp
)

agg_by_spot <- df_outer %>% 
  group_by(
    league_name,
    situation,
    is_on_frame,
    is_in_outer_frame
  ) %>% 
  summarize(
    n = n()
  ) %>% 
  ungroup() %>% 
  group_by(league_name) %>% 
  mutate(
    total = sum(n),
    prop = n / total
  ) %>% 
  ungroup() %>% 
  group_by(
    situation, is_on_frame, is_in_outer_frame
  ) %>% 
  mutate(
    rnk = row_number(desc(prop))
  ) %>% 
  ungroup()
agg_by_spot



p_bars <- agg_by_spot %>%
  add_ebb_estimate(n, total, method = 'mle', prior_subset = n > 100) %>%
  filter(situation == 'FreeKick') %>% 
  filter(
    is_on_frame & is_in_outer_frame
  ) %>% 
  mutate(
    across(league_name, ~fct_reorder(.x, -rnk))
  ) %>% 
  ggplot() +
  aes(x = .fitted, y = league_name) +
  geom_errorbarh(aes(xmin = .low, xmax = .high)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  expand_limits(x = 0) +
  labs(
    y = NULL,
    title = 'MLS has the highest proportion of free kicks on target and within 1m of the frame',
    caption = 'Error bars indicate 95% credible interval for emperical Bayes adjusted proportion',
    x = '% of free kicks on target and within 1m of the frame'
  )

w_bars <- 8
ggsave(
  filename = file.path(dir_proj, 'prop.png'),
  plot = p_bars,
  width = w_bars,
  height = w_bars * 1
)

agg_by_spot %>%
  filter(n > 100) %>% 
  # arrange(desc(prop)) %>% 
  add_ebb_estimate(n, total) %>% 
  arrange(prop) %>% 
  filter(situation == 'FreeKick') %>% 
  filter(league_name == 'MLS')

agg_by_spot %>% 
  filter(league_name == 'MLS') %>% 
  # filter(situation == 'FreeKick')
  filter(is_in_outer_frame)

df_outer_filt %>% 
  filter(league_name == 'EPL' & season == '2021/2022') %>% 
  arrange(desc(date)) %>% 
  filter(event_type == 'Post')

df_outer_filt %>% 
  count(event_type, sort = TRUE)
df_outer_filt %>% filter(is_in_w | is_in_h) %>% count(event_type)

## max on goal y (on_goal_shot$zoomRatio == 1): on_goal_shot$y: .613, z_g: 2.32

df %>% 
  drop_na(y_g) %>% 
  # mutate(across(y_g, ~.x - 34)) %>% 
  lm(formula(y_g ~ on_goal_shot$y + 0), data = .)

df %>% 
  drop_na(xgot, z_g)  %>% 
  arrange(desc(on_goal_shot$y)) %>% 
  select(xg, xgot, y_g, z_g, on_goal_shot)

df %>% 
  drop_na(z_g) %>% 
  filter(league_name == 'EPL', season == '2021/2022') %>% 
  slice_max(date) %>%
  select(home_team, away_team, player_name, min, x, y, y_g, z_g, xg, xgot, on_goal_shot)
# mutate(across(y_g, ~.x - 34)) %>% 
lm(formula(z_g ~ on_goal_shot$x + 0), data = .)

## why is there missing z_g for shots where there is non-na xgot?
## can impute with on_goal_shot vars
df %>%
  filter(!is.na(xgot)) %>% 
  filter(is.na(z_g))
df %>% 
  filter(!is.na(xgot)) %>% 
  filter(is.na(on_goal_shot$x))

# mutate(
#   frac_conv_lo = qbeta(0.025, g + 0.5, shots - g + 0.5),
#   frac_conv_hi = qbeta(0.975, g + 0.5, shots - g + 0.5)
# )
stats <-
  df %>% 
  filter(!is_own_goal) %>% 
  filter(!is_penalty) %>% 
  group_by(league_grp, situation, xg_tier) %>% 
  summarize(
    g = sum(event_type == 'Goal'),
    shots = n(),
    frac_conv = g / shots,
    shots_blocked = sum(is_blocked),
    across(c(xg, xgot), mean, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  group_by(league_grp) %>% 
  mutate(
    frac_blocked = shots_blocked / shots,
    frac_shots = shots / sum(shots),
    frac_g = g / sum(g)
  ) %>% 
  ungroup()
stats

df %>% 
  mutate(
    diff = xgot - xg
  ) %>% 
  filter(league_name == 'Bundesliga') %>% 
  filter(situation == 'FreeKick')
arrange(desc(diff))

df %>% 
  mutate(
    mutate(across(xgot, ifelse(!is_blocked & is.na(xgot))))
    diff = xgot - xg
  ) %>% 
  arrange(desc(diff)) %>% 
  # filter(situation %in% c('FastBreak', 'FreeKick', 'FromCorner', 'RegularPlay')) %>% 
  filter(situation %in% c('FreeKick')) %>% 
  group_by(league_name, situation) %>% 
  mutate(
    prnk = percent_rank(desc(diff))
  ) %>% 
  ungroup() %>% 
  # filter(prnk <= 0.5) %>% 
  ggplot() +
  aes(x = prnk, y = diff, group = league_name, color = league_name) +
  geom_step() +
  facet_wrap(~situation)

stats %>% 
  group_by(situation) %>% 
  mutate(rnk = row_number(desc(xgot))) %>% 
  ungroup() %>% 
  filter(league_grp == 'MLS' & rnk == 1)

df %>% 
  filter(event_type == 'Goal' & !is_own_goal & !is_penalty) %>% 
  mutate(
    across(c(x, y), round, 1)
  ) %>% 
  count(x, y, sort = TRUE)

df %>% 
  filter(event_type == 'Goal' & !is_own_goal & !is_penalty) %>% 
  filter(round(x, 1) == 98 & round(y, 1) == 37.7)

df %>% 
  filter(event_type == 'Goal' & !is_own_goal & !is_penalty) %>% 
  filter(xg_tier == 'poor') %>% 
  filter(league_grp != 'Ligue 1') %>% 
  ggplot() +
  aes(x = xgot, y = league_grp) +
  ggbeeswarm::geom_quasirandom(
    alpha = 0.3,
    groupOnX = FALSE
  )
geom_point(aes(color = league_grp))
