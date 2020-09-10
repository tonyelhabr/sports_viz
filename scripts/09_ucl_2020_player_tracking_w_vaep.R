
# setup ----
library(tidyverse)
library(xgboost) # for prediction
file <- 'ucl_2020_psg_mun'
path_export_gif <- here::here('plots', sprintf('%s.gif', file))
path_export_pc <- here::here('data', sprintf('09_%s_pc.rds', file))
play_filt <- 'PSG 0-[1] Bayern Munich'
fps <- 25
# pitch_fill <- '#7fc47f'
# pitch_color <- 'white'
pitch_fill <- 'white'
pitch_color <- 'black'
pitch <-
  ggsoccer::annotate_pitch(
    fill = pitch_fill, 
    colour = pitch_color,
    limits = FALSE
  )
xlim_pitch <- c(40, 101)
ylim_pitch <- c(112, -12)

color_low <- 'blue'
color_high <- 'red'
arw_v <- arrow(length = unit(3, 'pt'), type = 'closed')

pitch_gg <- function(...) {
  list(
    pitch,
    coord_flip(
      xlim = xlim_pitch,
      ylim = ylim_pitch
    ),
    ggsoccer::theme_pitch(aspect_ratio = 0.5), # changing the default aspect ratio cuz it looks weird
    theme(legend.position = 'none')
  )
}


get_config <- function(sheet) {
  path_config <- here::here('data-raw', '09', 'config.xlsx')
  res <- 
    path_config %>% 
    readxl::read_excel(sheet = sheet) %>% 
    mutate(across(matches('_id$'), as.integer))
  res
}

types <- 'types' %>% get_config()
bodyparts <- 'bodyparts' %>% get_config()
results <- 'results' %>% get_config()
spadl_vaep_features <- 'spadl_vaep_features' %>% get_config() %>% pull()

.rescale <- function(x, rng1, rng2) {
  rng2[1] + ((x - rng1[1]) * (rng2[2] - rng2[1])) / (rng1[2] - rng1[1])
}

.case_when_vendor <- function(coord, vendor) {
  
  case_when(
    coord == 'x' & vendor == 'opta' ~ c(0, 100), 
    coord == 'x' & vendor == 'spadl' ~ c(0, 105),
    coord == 'y' & vendor == 'opta' ~ c(0, 100), 
    coord == 'y' & vendor == 'spadl' ~ c(0, 68)
  )
}

.vendors_valid <- c('opta', 'spadl')
.coords_valid <- c('x', 'y')
.to_vendor_coord <- function(value, vendor1 = .vendors_valid, vendor2 = .vendors_valid, coord = .coords_valid) {
  vendor1 <- match.arg(vendor1)
  vendor2 <- match.arg(vendor2)
  coord <- match.arg(coord)
  rng1 <- .case_when_vendor(coord = coord, vendor = vendor1)
  rng2 <- .case_when_vendor(coord = coord, vendor = vendor2)
  res <- .rescale(value, rng1, rng2)
  res
}
.to_spadl <- partial(.to_vendor_coord, vendor1 = 'opta', vendor2 = 'spadl', ... = )
.to_opta <- partial(.to_vendor_coord, vendor1 = 'spadl', vendor2 = 'opta', ... = )
to_spadl_x <- partial(.to_spadl, coord = 'x', ... = )
to_spadl_y <- partial(.to_spadl, coord = 'y', ... = )
to_opta_x <- partial(.to_opta, coord = 'x', ... = )
to_opta_y <- partial(.to_opta, coord = 'y', ... = )

get_model <- function(x = c('score', 'concede')) {
  x <- match.arg(x)
  path <- fs::path('data-raw', '09', sprintf('vaep.%smodel', x))
  xgboost::xgb.load(path)
}
score_model <- 'score' %>% get_model()
concede_model <- 'concede' %>% get_model()


seq_pitch_dim <- function(dim = c('x', 'y'), length.out = 100L) {
  dim <- match.arg(dim)
  name_origin <- sprintf('origin_%s', dim)
  name_length <- switch(dim, x = 'length', y = 'width')
  start <- ggsoccer::pitch_opta[[name_origin]]
  length <- ggsoccer::pitch_opta[[name_length]]
  seq.int(start, start + length, length.out = length.out)
}

pitch_area <-
  crossing(
    x = seq_pitch_dim('x'),
    y = seq_pitch_dim('y')
  )
pitch_area

pitch_area_filt <-
  pitch_area %>% 
  filter(between(x, xlim_pitch[1], xlim_pitch[2])) %>% 
  filter(between(y, ylim_pitch[2], ylim_pitch[1]))
pitch_area_filt

get_speed <- function(coord, next_coord, time, next_time) {
  (next_coord - coord) / (next_time - time)
}

get_theta <- function(x_speed, y_speed) {
  hypotenuse_speed <- sqrt(x_speed^2 + y_speed^2)
  acos(x_speed / hypotenuse_speed)
}

get_mu <- function(location, speed) {
  location + speed / 2
}

get_srat <- function(speed_x, speed_y) {
  speed <- sqrt(speed_x^2 + abs(speed_y)^2)
  (speed / 13)^2
}

get_ri <- function(x, y, ball_x, ball_y) {
  ball_diff <- sqrt((x - ball_x) ^ 2 + (y - ball_y)^2)
  ri <- 4 + ((ball_diff^3) / ((18^3) / 6))
  min(ri, 10)
}

get_R <- function(theta) {
  matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow = 2)
}

get_S <- function(ri, srat) {
  top_left <- ri * (1 + srat) / 2
  bottom_right <- ri * (1-srat) / 2
  matrix(c(top_left, 0, 0, bottom_right), nrow = 2)
}

get_Sigma <- function(R, S) {
  inv_R <- solve(R)
  R %*% S %*% S %*% inv_R
}

calculate_I <- function(pitch_area, x, y, mu_x, mu_y, Sigma) {
  mu <- c(mu_x, mu_y)
  player_loc <- c(x, y)
  num <- mvtnorm::dmvnorm(as.matrix(pitch_area), mu, Sigma)
  den <- mvtnorm::dmvnorm(t(matrix(player_loc)), mu, Sigma)
  num / den
}

dir_data <- here::here('data', '09')
fs::dir_create(dir_data)
calculate_pc <- 
  function(time, next_time, ball_x, ball_y, x, y, next_x, next_y, team, player, 
           pitch_area, 
           ..., 
           debug = FALSE,
           dir = dir_data, 
           basename = glue::glue('pc_{sprintf("%.02f", time)}_{sprintf("%04d", player)}.rds'), 
           path = fs::path(dir_data, basename), 
           overwrite = FALSE) {

    path_exists <- fs::file_exists(path)
    if(path_exists & !overwrite) {
      return(read_rds(path))
    }
    speed_x <- get_speed(x, next_x, time, next_time)
    speed_y <- get_speed(y, next_y, time, next_time)
    srat <- get_srat(speed_x, speed_y)
    theta <- get_theta(speed_x, speed_y)
    
    mu_x <- get_mu(x, speed_x)
    mu_y <- get_mu(y, speed_y)
    
    ri <- get_ri(x, y, ball_x, ball_y)
    
    R <- get_R(theta)
    S <- get_S(ri, srat)
    
    Sigma <- get_Sigma(R, S)
    I <- calculate_I(as.matrix(pitch_area), x, y, mu_x, mu_y, Sigma)
    # is_bad <- all(sort(I, decreasing = TRUE)[1:10] > 3)
    # if(is_bad) {
    #   I <- rep(0, length(I))
    #   # browser()
    # } else {
    #   I <- case_when(I > 3 ~ 0, TRUE ~ I)
    # }
    pitch_area$I <- I
    write_rds(pitch_area, path)
    pitch_area
  }
do_calculate_pc <- partial(calculate_pc, pitch_area = !!pitch_area, ... = )

do_aggregate_pc <- 
  function(pc,
           suffix,
           dir = dir_data, 
           basename = glue::glue('pc_agg_{suffix}.rds'), 
           path = fs::path(dir_data, basename), 
           overwrite = FALSE) {
    
    path_exists <- fs::file_exists(path)
    if(path_exists & !overwrite) {
      return(read_rds(path))
    }
    
    res <-
      pc %>%
      select(frame, time, player, team, data) %>% 
      unnest(data) %>% 
      # mutate(across(I, ~case_when(.x > 1 ~ 1, TRUE ~ .x))) %>% 
      group_by(frame, time, team, x, y) %>%
      summarise(team_sum = sum(I, na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(names_from = team, values_from = team_sum) %>%
      mutate(pc = 1 / (1 + exp(home - away)))
    # browser()
    write_rds(res, path)
    res
  }

# vaep calc ----
events_nondribble <- 
  here::here('data-raw', '07', 'CL2020_final8_events.csv') %>% 
  read_csv() %>% 
  filter(play == play_filt) %>% 
  mutate(across(id, ~{2 * (.x + 1)}))
events_nondribble

events_dribble <- 
  events_nondribble %>% 
  mutate(
    dframe = from_frame - dplyr::lag(to_frame, default = 0),
    dx = coalesce(from_x - dplyr::lag(to_x), 0),
    dy = coalesce(from_y - dplyr::lag(to_y), 0)
  ) %>% 
  mutate(
    to_frame = from_frame,
    from_frame = from_frame - dframe, 
    to_x = from_x,
    from_x = from_x - dx,
    to_y = from_y,
    from_y = from_y - dy,
    id = id - 1,
    Type = 'Dribble'
  ) %>% 
  # Drop the last frame with a non-existant dribble before the shot.
  filter(dframe != 0) %>% 
  select(-dframe, -dx, -dy) %>% 
  arrange(id)
events_dribble

events <-
  tibble(id = seq(1, nrow(events_nondribble) * 2, by = 1)) %>% 
  left_join(
    bind_rows(events_nondribble, events_dribble)
  ) %>% 
  filter(!is.na(from_x)) %>% 
  mutate(id = row_number(id)) %>% 
  arrange(id)
events

events_spadl_init <-
  events %>% 
  transmute(
    game_id = 1L, # Needed down the line for `vaep_score_actions` called in vaep_get_scores`
    action_id = id,
    period_id = 2L,
    team_id = 1L,
    home_team = TRUE,
    type_name = case_when(id == 11L ~ 'shot', id == 10L ~ 'cross', TRUE ~ tolower(Type)),
    bodypart_name = if_else(id != 4, 'foot', 'head'),
    result_name = 'success',
    time_seconds = from_frame / fps,
    timestamp = sprintf('00:00:0%1.3f', from_frame / fps),
    start_x = to_spadl_x(from_x),
    start_y = to_spadl_y(from_y),
    end_x = to_spadl_x(to_x),
    end_y = to_spadl_y(to_y)
  ) %>% 
  left_join(types) %>% 
  left_join(bodyparts) %>% 
  left_join(results)
events_spadl_init
events_spadl_init1 <- events_spadl_init %>% head(1)

events_spadl_dummy <-
  events_spadl_init1 %>% 
  select(-action_id, -period_id, -team_id, -time_seconds) %>% 
  mutate(dummy = 1) %>% 
  left_join(
    tibble(
      action_id = seq(-5L, -1L, by = 1L),
      period_id = c(rep(1L, 3), rep(2L, 2)),
      team_id = c(rep(2L, 3), rep(1L, 2)),
      time_seconds = seq(1, 5),
      dummy = 1
    )
  )
events_spadl <- bind_rows(events_spadl_dummy, events_spadl_init)
# debugonce(Rteta::vaep_get_features)
vaep_features <- events_spadl %>% Rteta::vaep_get_features() %>% .[spadl_vaep_features]
vaep_labels <- events_spadl %>% Rteta::vaep_get_labels()

score_matrix <-
  xgboost::xgb.DMatrix(
    data = as.matrix(vaep_features),
    label = as.numeric(vaep_labels$scores)
  )

concede_matrix <- 
  xgboost::xgb.DMatrix(
    data = as.matrix(vaep_features),
    label = as.numeric(vaep_labels$concedes)
  )

events_spadl_vaep <- events_spadl
events_spadl_vaep$scores <- score_model %>% predict(newdata = score_matrix)
events_spadl_vaep$concedes <- concede_model %>% predict(newdata = concede_matrix)

events_spadl_vaep <- events_spadl_vaep %>% Rteta::vaep_get_scores('scores', 'concedes')
events_spadl_vaep

frame_last <- 263
events_opta_vaep_init <-
  events_spadl_vaep %>% 
  filter(action_id >= 0)  %>% 
  mutate(
    end_x = to_opta_x(start_x),
    end_y = to_opta_y(start_y),
    to_x = to_opta_x(end_x),
    to_y = to_opta_y(end_y)
  ) %>% 
  select(
    id = action_id,
    scores,
    concedes,
    attack_score,
    defence_score,
    vaep_value
  ) %>% 
  inner_join(events) %>% 
  mutate(across(to_frame, ~if_else(id == max(id), frame_last, to_frame)))
events_opta_vaep_init
n_event <- events_opta_vaep_init %>% nrow()

events_opta_vaep_last_dummy <- 
  events_opta_vaep_init %>% 
  tail(1) %>% 
  transmute(id = id + 1, from_frame = frame_last)
events_opta_vaep <- bind_rows(events_opta_vaep_init, events_opta_vaep_last_dummy)
events_opta_vaep
# events_opta_vaep %>% 
#   filter(id > 0) %>%
#    #filter(action_id < max(action_id)) %>% 
#   pivot_longer(c(scores:vaep_value)) %>% 
#   ggplot() +
#   aes(x = from_frame, y = value, color = name) +
#   geom_step(size = 1.25)
#   # ggforce::geom_bspline(size = 1.25)

# tracking data manip ----
tracking <- 
  here::here('data-raw', '07', 'CL2020_final8_tracking.csv') %>% 
  read_csv() %>% 
  select(-1) %>% 
  filter(play == play_filt)
tracking

frames <- 
  tracking %>% 
  mutate(
    team = case_when(is.na(bgcolor) ~ NA_character_, bgcolor == 'red' ~ 'away', bgcolor == 'darkblue' ~ 'home'),
    time = frame / fps
  ) %>% 
  select(frame, time, player, player_num, team, edgecolor, bgcolor, x, y) %>% 
  group_by(player) %>% 
  mutate(
    across(c(x, y, time), ~dplyr::lead(.x, 1L), .names = 'next_{col}'),
    across(c(x, y), ~dplyr::lead(.x, 10L), .names = 'forward_{col}')
  ) %>% 
  ungroup()
frames

frames_ball <- frames %>% filter(player == 0)
frames_players <- frames %>% anti_join(frames_ball)

frames_redux <- 
  frames_ball %>% 
  select(time, frame, ball_x = x, ball_y = y) %>% 
  inner_join(frames_players)
frames_redux

if(!fs::file_exists(path_export_pc)) {
  
  f <- possibly(do_calculate_pc, otherwise = NULL)
  pc <- 
    frames_redux %>%
    mutate(
      data = 
        pmap(
          list(
            time = time, 
            next_time = next_time, 
            ball_x = ball_x, 
            ball_y = ball_y, 
            x = x, 
            y = y, 
            next_x = next_x, 
            next_y = next_y, 
            team = team, 
            player = player,
            overwrite = TRUE
          ), 
          f
        )
    ) # %>% 
    # Suddenly data is bad for the last frame? (hence why I resorted to possibly)
    # filter(frame != max(frame))
  pc %>% select(frame, player, data) %>% mutate(is_bad = map_lgl(data, is.null)) %>% filter(is_bad) %>% count(frame)
  # pc_frames <- pc %>% count(frame)
  # pc %>% mutate(grp = frame %/% 25) %>% count(grp)
  pc_agg <-
    pc %>%
    mutate(grp = frame %/% 25) %>%
    nest(-c(grp)) %>% 
    mutate(res = map2(data, grp, ~do_aggregate_pc(pc = ..1, suffix = ..2, overwrite = TRUE))) %>% 
    select(res) %>% 
    unnest(res)
  pc_agg
  
  write_rds(pc_agg, path_export_pc)
} else {
  pc_agg <- read_rds(path_export_pc)
}

# viz ----
.filter_frames <- function(data) {
  res <-
    data # %>% 
    # filter(frame >= 80) %>% 
    # filter(frame <= 160) %>% 
    # mutate(is_slice = (frame %% 10 == 0)) %>% 
    # mutate(is_slice = frame %% 25 == 0) %>% 
    # filter(is_slice) # %>% 
    # filter(frame >= 5) # First handful of frames look weird.
  res
}

viz <-
  pc_agg %>%
  .filter_frames() %>% 
  ggplot() +
  aes(x = x, y = y) +
  pitch_gg() +
  geom_tile(aes(x = x, y = y, fill = pc), alpha = 0.7) +
  scale_fill_gradient2(low = color_low, high = color_high, mid = 'white', midpoint = 0.5) +
  geom_segment(
    data = frames_redux %>% .filter_frames() %>% filter(!is.na(forward_x)),
    color = 'black',
    size = 1.5, 
    arrow = arw_v,
    aes(x = x, y = y, xend = forward_x, yend = forward_y)
  ) +
  ggnewscale::new_scale_fill() +
  geom_point(
    data = frames_players %>% .filter_frames(),
    color = 'black',
    aes(fill = team),
    size = 7,
    alpha = 0.8,
    stroke = 1,
    shape = 21
  ) + 
  scale_fill_manual(values = c('away' = color_high, 'home' = color_low)) +
  geom_point(
    data = frames_ball %>% .filter_frames(),
    color = 'black', 
    fill = 'yellow',
    size = 4,
    stroke = 1,
    shape = 21
  ) +
  geom_text(
    data = frames_players %>% .filter_frames() %>% filter(!is.na(player_num)),
    aes(label = player_num),
    fontface = 'bold',
    color = 'black'
  ) +
  gganimate::transition_time(frame) +
  # facet_wrap(~frame) + # for dev +
  theme(
    plot.title = element_text('Karla', face = 'bold', size = 18, color = 'gray20'),
    plot.title.position = 'plot',
    plot.margin = margin(20, 10, 10, 10),
    plot.caption = element_text('Karla', size = 14, color = 'gray20', hjust = 0),
    plot.caption.position = 'plot'
  ) +
  labs(
    title = glue::glue('{play_filt}, UCL 2020 Finals'),
    caption = 'Viz: @TonyElHabr | Data: @lastrowview'
  )
# viz
# ggsave(plot = viz, filename = here::here('viz.png'), width = 12, height = 12, type = 'cairo')
n_frame <- frames_players %>% .filter_frames() %>% pull(frame) %>% max()
viz_anim <- gganimate::animate(viz, width = 900, height = 600, nframes = n_frame, fps = fps)
gganimate::anim_save(animation = viz_anim, path_export_gif)

events_opta_vaep_viz <-
  tibble(from_frame = seq(1, frame_last, by = 1)) %>% 
  left_join(events_opta_vaep) %>%
  fill(vaep_value) %>% 
  mutate(across(vaep_value, ~if_else(id > n_event, 0, .x))) %>% 
  mutate(across(vaep_value, ~coalesce(.x, 0))) %>% 
  mutate(across(vaep_value, list(cumu = cumsum)))
events_opta_vaep_viz

events_opta_vaep_viz_labs <-
  events_opta_vaep_viz %>% 
  # filter(!is.na(vaep_value)) %>% #  & vaep_value > 0.0001) %>% 
  filter(!is.na(id) & !is.na(from_x)) %>% 
  mutate(
    lab = 
      case_when(
        is.na(vaep_value) ~ NA_character_,
        TRUE ~ paste0(ifelse(vaep_value < 0 , '', '+'), scales::number(vaep_value, accuracy = 0.01))
      )
  )
events_opta_vaep_viz_labs %>% relocate(lab)

viz_vaep <-
  events_opta_vaep_viz %>% 
  ggplot() +
  aes(x = from_frame, y = vaep_value_cumu) +
  # gganimate::view_follow() +
  gganimate::transition_reveal(along = from_frame) +
  geom_step(size = 1.25, color = 'gray20') +
  geom_text(
    data = events_opta_vaep_viz_labs, #  %>% filter(from_frame == min(from_frame)),
    aes(x = from_frame - 6, y = vaep_value_cumu + 0.08, label = lab, group = NULL),
    size = 5,
    hjust = 1,
    fontface = 'bold',
    color = 'grey20'
  ) +
  geom_segment(
    data = events_opta_vaep_viz_labs, #  %>% filter(from_frame == min(from_frame)),
    aes(x = from_frame - 5, y = vaep_value_cumu + 0.07, xend = from_frame, yend = vaep_value_cumu + 0.005, group = NULL),
    size = 0.5,
    # curvature = -0.2,
    # arrow = arrow(length = unit(0.03, 'npc'))
  ) +
  geom_text(
    data = tibble(),
    aes(x = 10, y = 0.45, label = glue::glue('VAEP')),
    size = 6,
    hjust = 0,
    fontface = 'bold',
    color = 'grey20'
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = 'Karla'),
    title = element_text('Karla', size = 14, color = 'gray20'),
    plot.title = element_text('Karla', face = 'bold', size = 18, color = 'gray20'),
    plot.title.position = 'plot',
    plot.subtitle = element_text('Karla', face = 'bold', size = 14, color = 'gray50'),
    # axis.text = element_text('Karla', size = 14),
    axis.text = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    x = NULL, y = NULL
  )
viz_vaep

