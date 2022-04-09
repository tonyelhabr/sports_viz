
library(tidyverse)
library(qs)
library(janitor)
library(lubridate)

dir_proj <- '51-goal_coords'
shots <- file.path(dir_proj, '201522_epl_shots.parquet') %>% arrow::read_parquet()
shots_filt <- shots %>% 
  filter(game_id == max(game_id))
shots_filt %>% write_csv(file.path(dir_proj, 'shots_1549727.csv'))
devtools::source_url('https://raw.githubusercontent.com/larsmaurath/keine-mathematik/master/content/data/standardize_coordinates.R')
shots_standardized <- shots %>% 
  standardize_opta_x(cols = c('x')) %>% 
  standardize_opta_y(cols = c('y', 'goal_mouth_y')) %>% 
  standardize_opta_z(cols = c('goal_mouth_z'))
shots_standardized %>% 
  filter(game_id == max(game_id)) -> x

convert_ogs_y_to_z_g <- function(y) {
  y * 2.32 / 0.613
}

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

w_g_buffer <- 1
gg_base <- tibble(
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

gg_pitch <- function(
  xlim = .xlim,
  ylim = .ylim,
  aspect_ratio = .asp,
  fill = gray_wv,
  color = 'white',
  limits = FALSE,
  ...
) {
  list(
    ...,
    ggsoccer::annotate_pitch(
      dimensions = ggsoccer::pitch_international,
      fill = fill,
      colour = color,
      limits = limits
    ),
    coord_flip(xlim = xlim, ylim = ylim),
    ggsoccer::theme_pitch(aspect_ratio = aspect_ratio),
    theme(legend.position = 'none')
  )
}

df <- file.path(dir_proj, 'data.qs') %>% 
  qs::qread() %>% 
  filter(league_name == 'EPL') %>% 
  # filter(!is_own_goal) %>% 
  distinct(match_id, id, .keep_all = TRUE) %>% 
  mutate(
    is_g = event_type == 'Goal',
    is_penalty = (xg == 0.7884 & round(x) == 94 & round(y) == 34),
    xg_diff = xgot - xg,
    across(
      z_g,
      list(`2` = ~case_when(
        !is.na(xgot) ~ coalesce(.x, convert_ogs_y_to_z_g(on_goal_shot$y)),
        TRUE ~ .x
      )
      )
    )
  )

# team_mapping <- xengagement::team_accounts_mapping %>% 
#   select(team_fotmob, team = team_whoscored)
# team_mapping

df_filt <- df %>% 
  # drop_na(z_g) %>% 
  # filter(
  #   # date >= ymd('2021-09-01'),
  #   away_team == 'Arsenal',
  #   home_team == 'Brentford'
  # ) %>% 
  mutate(
    period_id = case_when(
      period == 'FirstHalf' ~ 1L,
      period == 'SecondHalf' ~ 2L,
      TRUE ~ NA_integer_
    )
  ) %>% 
  mutate(
    is_on_frame = is_in_w(y_g) & is_in_h(z_g)
  )
df_filt %>% count(team)

df_slim <- df_filt %>% 
  # drop_na(y_g, xgot) %>% 
  filter(
    !is_penalty
  ) %>% 
  select(
    match_id,
    period_id,
    min,
    team, 
    player_name, 
    is_g,
    is_on_target,
    x, 
    y, 
    y_g, 
    z_g,
    z_g_2,
    on_goal_shot
  )
df_slim %>% filter(on_goal_shot$zoomRatio == 1)

p_frame <- gg_base +
  geom_point(
    data = df_slim,
    aes(x = y_g, y = z_g, size = xg_diff)
  ) +
  guides(
    size = guide_legend('xGoT - xG', override.aes = list(color = 'gray50')),
    color = guide_legend('League', override.aes = list(size = 3))
  ) +
  scale_radius(range = c(1, 8)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = 'bold'),
    legend.position = 'top'
  )
p_frame
