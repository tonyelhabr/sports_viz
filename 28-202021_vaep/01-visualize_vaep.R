
library(tidyverse)
dir_proj <- '28-202021_vaep'
dir_data <- file.path(dir_proj, 'data-socceraction')

# #1
# I didn't watch every #EPL match this season, so I had a machine do it for me. Here are it's best XI.
# This is based on the VAEP framework, which is basically a method for quantifying how any player action influences the game's outcome.
# #2
# Here's a visual example for a possession in which Lingard scores a net negative VAEP. Note that this is not xG---actions can have negative values!
# #3
# VAEP correlates pretty strongly with transfer market values.
# #4
# VAEP has the strongest variability for forwards.
# #5
# Obligatory links:
# Methodology authors: @TomDecroos, @LotteBransen, @JanVanHaaren, @jessejdavis1
# Paper: # https://arxiv.org/pdf/1802.07127.pdf
# @pwawrzynow's Aug. 2020 thread: https://twitter.com/pwawrzynow/status/1292095872583577600?s=20
# My illustration from Sep. 2020: https://twitter.com/TonyElHabr/status/1304766718468857857?s=20


dir_img <- file.path(dir_proj, 'img')
img_info <-
  fs::dir_ls(
    dir_img,
    regexpr = 'png$'
  ) %>% 
  tibble(path = .) %>% 
  mutate(
    across(path, as.character),
    player_name = path %>% basename() %>% tools::file_path_sans_ext()
  )
img_info

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
  plot.caption = ggtext::element_markdown('Karla', size = 14, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 14, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.02, 0.01),
  legend.text = element_text(color = 'gray20', size = 14),
  strip.text = element_text(color = 'gray20', size = 14),
  strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

do_import <-
  function(path,
           name = tools::file_path_sans_ext(basename(path)),
           ext = 'parquet',
           dir = dir_data,
           f_import = arrow::read_parquet,
           prefix = NULL,
           suffix = NULL,
           sep = '_',
           assign = TRUE) {
    if (is.null(prefix)) {
      prefix <- ''
    } else {
      prefix <- paste0(prefix, sep)
    }
    
    if (is.null(suffix)
    ) {
      suffix <- ''
    } else {
      suffix <- paste0(sep, suffix)
    }
    basename <- sprintf('%s%s%s.%s', prefix, name, suffix, ext)
    path <- file.path(dir, basename)
    res <- path %>% f_import()
    if(!assign) {
      return(res)
    }
    assign(value = res, x = name, envir = .GlobalEnv)
  }

aggregate_av_by_game <- function(av, ...) {
  av %>% 
    select(
      season_id,
      competition_id,
      game_id, 
      matches('_team_name$'),
      team_id,
      team_name,
      player_id,
      player_name,
      off, def, vaep
    ) %>%
    left_join(player_games_clean) %>% 
    group_by_at(vars(matches('_id$'), matches('_name$'), pos, minutes_played)) %>%
    summarize(across(c(off, def, vaep), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      across(vaep, list(p90 = ~90 * .x / minutes_played))
    ) %>% 
    left_join(pos_info) %>% 
    arrange(desc(vaep))
}

aggregate_av_by_season <- function(av_by_game, ...) {
  av_by_game %>%
    select(
      season_id,
      competition_id,
      team_id,
      team_name,
      player_id,
      player_name,
      minutes_played,
      off, def, vaep
    ) %>%
    group_by_at(vars(matches('_id$'), matches('_name$'))) %>%
    summarize(across(c(minutes_played, off, def, vaep), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      across(vaep, list(p90 = ~90 * .x / minutes_played))
    ) %>% 
    left_join(player_pos_filt %>% select(season_id, competition_id, team_id, player_id, pos, pos_grp)) %>% 
    left_join(pos_info) %>% 
    arrange(desc(vaep))
}

pitch_gg <-
  function(pitch = ggsoccer::pitch_international,
           xlim = c(-1, 106),
           ylim = c(-1, 69),
           aspect_ratio = 105 / 68,
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

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

f_text <- partial(
  ggtext::geom_richtext,
  # fill = NA, 
  label.color = NA,
  family = 'Karla',
  fontface = 'bold',
  # inherit.aes = FALSE,
  ...=
)


file.path(dir_data, sprintf('%s.parquet', c('actions_valued', 'games', 'players', 'player_games', 'teams'))) %>% 
  walk(do_import)
# file.path(dir_data, sprintf('%s.parquet', c('actions_valued_atomic'))) %>% 
#   walk(do_import)

av <- 
  actions_valued %>% 
  rename(
    off = offensive_value,
    def = defensive_value,
    vaep = vaep_value
  )
av

# ava <- 
#   actions_valued_atomic %>% 
#   rename(
#     off = offensive_value,
#     def = defensive_value,
#     vaep = vaep_value
#   )
# ava

av_by_game <- av %>% aggregate_av_by_game()
av_by_game

av_by_season <- av_by_game %>% aggregate_av_by_season()
av_by_season

# ava_by_game <- ava %>% aggregate_av_by_game()
# ava_by_game
# 
# ava_by_season <- ava_by_game %>% aggregate_av_by_season()
# ava_by_season
# ava_by_season %>% 
#   filter(season_id == 2020L) %>% 
#   arrange(desc(vaep))
# ava %>% select(off, def) %>% skimr::skim()
# av %>% select(off, def) %>% skimr::skim()
# ava %>% 
#   filter(is.na(off)) %>% 
#   head(10) %>% 
#   left_join(av %>% select(game_id, time_seconds, player_name, off_av = off, def_av = def, vaep_av = vaep)) -> z
# z %>% glimpse()

# main ----
player_games_clean <-
  player_games %>% 
  # This Arsenal - Leicester game has an erroneous number of minutes
  # https://www.whoscored.com/Matches/1485349/Live/England-Premier-League-2020-2021-Leicester-Arsenal
  mutate(
    across(minutes_played, ~case_when(game_id == 1485349L ~ 98L, TRUE ~ .x))
  ) %>% 
  rename(pos = starting_position_name, pos_id = starting_position_id)

pos_info <-
  tibble(
    pos = c('AMC', 'AML', 'AMR', 'DC', 'DL', 'DMC', 'DML', 'DMR', 'DR', 'FW', 'FWL', 'FWR', 'GK', 'MC', 'ML', 'MR', 'Sub'),
    pos_grp = c('F', 'F', 'F', 'D', 'D', 'M', 'M', 'M', 'D', 'F', 'F', 'F', 'G', 'M', 'M', 'M', 'z'),
    pos_11 = c('FWC', 'FWL', 'FWR', 'DC', 'DL', 'MC', 'ML', 'MR', 'DR', 'FWC', 'FWL', 'FWR', 'GK', 'MC', 'ML', 'MR', 'z')
  )
pos_info

pos_info_xy <-
  tibble(
    pos_11 = c('GK', 'DL', 'DCL', 'DCR', 'DR', 'ML', 'MC', 'MR', 'FWL', 'FWC', 'FWR'),
    x = c(5, 30, 25, 25, 30, 55, 50, 55, 75, 70, 75),
    y = c(50, 15, 38, 62, 85, 20, 50, 80, 20, 50, 80)
  ) %>% 
  mutate(across(c(x, y), as.integer))

pos_grp_labs <- c('F', 'M', 'D', 'G', 'z')
player_pos <-
  player_games_clean %>% 
  left_join(teams) %>% 
  left_join(players) %>% 
  left_join(games) %>% 
  group_by(season_id, competition_id, team_id, team_name, player_id, player_name, pos_id, pos) %>% 
  summarize(
    across(c(minutes_played), sum, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  filter(minutes_played > 0L) %>% 
  group_by(season_id, competition_id, team_id, team_name, player_id, player_name) %>% 
  # Don't technically need the coalesce() here if we drop zero minutes played games.
  mutate(total = sum(minutes_played), frac = coalesce(minutes_played / total, 0)) %>% 
  # slice_max(frac, with_ties = FALSE) %>% 
  # filter(pos != 'Sub') %>% 
  ungroup() %>% 
  left_join(pos_info) %>% 
  mutate(
    across(pos_grp, ~ordered(.x, pos_grp_labs))
  )
player_pos

player_pos_filt <-
  player_pos %>% 
  filter(pos != 'Sub') %>% 
  group_by(season_id, competition_id, team_id, team_name, player_id, player_name) %>% 
  slice_max(frac, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(-frac)
player_pos_filt

av_by_season_latest <-
  av_by_season %>% 
  filter(season_id == 2020L) %>% 
  filter(minutes_played > (2 * 90)) %>% 
  left_join(pos_info) %>% 
  arrange(desc(vaep_p90))
av_by_season_latest

# av_by_season_latest %>% 
#   filter(pos_grp == 'G') -> z
# 
# av_by_game %>% 
#   filter(player_name == 'Sam Johnstone')

av_by_game %>% 
  filter(season_id == 2020L) %>% 
  filter(player_name == 'Jesse Lingard') %>% 
  arrange(desc(vaep_p90)) %>% 
  head(3)

av %>% 
  filter(season_id == 2020L) %>% 
  filter(player_name == 'Jesse Lingard') %>% 
  # filter(type_name == 'pass') %>%
  arrange(desc(vaep)) %>% 
  relocate(type_name, off, def, vaep) -> z

event <-
  av %>% 
  filter(season_id == 2020L) %>% 
  filter(player_name == 'Jesse Lingard') %>% 
  # filter(original_event_id == 2262104669L)
  filter(game_id == 1485331 & action_id == 199L)
event  

play <-
  event %>% 
  select(season_id, game_id, period_id, time_seconds_event = time_seconds, action_id_event = action_id) %>% 
  left_join(
    av %>% 
      select(season_id, game_id, period_id, time_seconds, action_id)
  ) %>% 
  # filter((time_seconds >= (time_seconds_event - 10L)) & (time_seconds <= (time_seconds_event + 10L))) %>% 
  filter(action_id >= (action_id_event - 3L) & (action_id <= (action_id_event + 1L))) %>% 
  # select(-matches('_event$')) %>% 
  left_join(av) %>% 
  # left_join(teams) %>% 
  mutate(
    lab = sprintf('%s (%s)', type_name, bodypart_name) %>% fct_inorder(),
    time_dummy = time_seconds - min(time_seconds)
  ) %>% 
  arrange(time_seconds) %>% 
  select(player_name, type_name, action_id, start_x, end_x, start_y, end_y, vaep, lab)
play

lim_x <- 
  play %>% 
  summarize(
    across(c(start_x, end_x), list(min = min, max = max))
  ) %>% 
  rowwise() %>% 
  mutate(
    x_min = min(49, min(start_x_min, end_x_min) - 1L),
    x_max = max(start_x_max, end_x_max) + 1L
  ) %>% 
  select(x_min, x_max)
lim_x

lab_tag <- '**Viz**: Tony ElHabr'
# f_mark <- partial(
#   ggforce::geom_mark_circle,
#   expand = unit(0.01, 'mm'),
#   label.family = 'Karla',
#   # label.colour = '#832424',
#   # label.colour = '#005800',
#   color = 'black',
#   label.buffer = unit(30, 'mm'),
#   # check.overlap = TRUE,
#   aes(
#     group = action_id,
#     # x = end_x, 
#     # y = end_y,
#     # x = (start_x + end_x) / 2,
#     # y = (start_y + end_y) / 2,
#     x = start_x,
#     y = start_y,
#     # description = sprintf('%s %s (%s)', str_replace_all(player_name, '(^.*\\s+)(.*$)', '\\2'), type_name, bodypart_name),
#     description = sprintf('%s %s', str_replace_all(player_name, '(^.*\\s+)(.*$)', '\\2'), type_name),
#     label = sprintf('%+.2f', vaep),
#   ),
#   ... = 
# )

# This is a manual adjustment for this play, since it seems slightly off compared to the film.
# https://www.youtube.com/watch?v=6jizRzA3cbw&t=171s
x_buffer <- -3
f_mark <- partial(
  ggrepel::geom_text_repel,
  family = 'Karla',
  hjust = 0,
  fontface = 'bold',
  size = pts(14),
  aes(
    # group = action_id,
    # hjust = 1,
    # x = end_x, 
    # y = end_y - 1,
    # vjust = 0,
    x = (start_x + end_x) / 2 + x_buffer,
    # y = (start_y + end_y) / 2 - 30,
    y = 18,
    label = sprintf('%+.2f: %s %s', vaep, str_replace_all(player_name, '(^.*\\s+)(.*$)', '\\2'), type_name)
  ),
  ... = 
)

viz_ex <-
  play %>% 
  ggplot() +
  pitch_gg(
    # xlim = c(lim_x$x_min, lim_x$x_max + 10),
    xlim = c(lim_x$x_max, lim_x$x_min),
    aspect_ratio = 0.5 * 105/68
  ) +
  ggforce::geom_link(
    data = play %>% filter(type_name == 'pass'),
    aes(
      x = start_x + x_buffer,
      y = start_y,
      xend = end_x + x_buffer,
      yend = end_y,
      alpha = stat(index),
    ),
    show.legend = FALSE,
    size = 1
  ) +
  geom_segment(
    data = play %>% filter(type_name != 'pass'),
    aes(
      x = start_x + x_buffer,
      y = start_y,
      xend = end_x + x_buffer,
      yend = end_y,
    ),
    linetype = 2,
    show.legend = FALSE,
    size = 1
  ) +
  geom_point(
    aes(
      x = end_x + x_buffer,
      y = end_y,
    ),
    size = 1,
    shape = 19
    # shape = 21
  ) +
  # scales::muted('red') and scales::muted('green')
  f_mark(data = play %>% filter(player_name == 'Jesse Lingard' & vaep >= 0), colour = '#005800') +
  f_mark(data = play %>% filter(player_name == 'Jesse Lingard' & vaep < 0), colour = '#832424') +
  # gplots::col2hex('grey50')
  f_mark(data = play %>% filter(player_name != 'Jesse Lingard' & action_id != min(action_id)), colour = '#7F7F7F') +
  # f_mark(data = play, colour = '#7F7F7F') +
  theme(
    plot.title = ggtext::element_markdown(size = 18),
    plot.subtitle = ggtext::element_markdown(size = 12)
  ) +
  # scale_x_reverse() +
  labs(
    title = 'VAEP Example',
    subtitle = 'Jesse Lingard\'s net VAEP for possession: <span style="color:#832424">-0.04</span><br/>20201-02-21, West Ham v. Tottenham, 10th Minute',
    caption = ' ',
    tag = lab_tag
  )
viz_ex

h <- 10
path_viz_ex <- file.path(dir_proj, 'viz_vaep_ex.png')
ggsave(
  plot = viz_ex,
  filename = path_viz_ex,
  height = h + 2,
  width = h * 68 / 105,
  type = 'cairo'
)

add_logo(
  path_viz = path_viz_ex,
  path_logo = file.path(dir_img, 'Jesse Lingard.png'),
  idx_x = 0.1,
  logo_scale = 0.2,
  # adjust_y = TRUE,
  idx_y = 0.5
)

f_by_pos_11 <- function(av, n = 2) {
  av %>% 
    filter(!is.na(pos_grp)) %>% 
    # filter(minutes_played > (15 * 90)) %>% 
    filter(minutes_played > 1000) %>% 
    group_by(pos_11) %>% 
    slice_max(vaep_p90, n = n) %>% 
    ungroup()
}

pos_11_dual <- c('DC')
# Do this once to assist with creating `team_info`
# av_by_season_latest %>% distinct(team_name) %>% arrange(team_name) %>% pull(team_name) %>% datapasta::vector_paste()

team_info <-
  tibble(
    team_name = c('Arsenal', 'Aston Villa', 'Brighton', 'Burnley', 'Chelsea', 'Crystal Palace', 'Everton', 'Fulham', 'Leeds', 'Leicester', 'Liverpool', 'Man City', 'Man Utd', 'Newcastle', 'Sheff Utd', 'Southampton', 'Tottenham', 'West Brom', 'West Ham', 'Wolves'),
    team_abbrv = c('ARS', 'AVL', 'BRI', 'BUR', 'CHE', 'CRY', 'EVE', 'FUL', 'LEE', 'LEI', 'LIV', 'MCI', 'MUN', 'NEW', 'SOU', 'SHU', 'TOT', 'WBA', 'WHU', 'WOL')
  )

av_by_season_latest_pos <-
  bind_rows(
    av_by_season_latest %>% 
      filter(pos_11 %in% pos_11_dual) %>% 
      f_by_pos_11(n = 4),
    av_by_season_latest %>% 
      filter(!(pos_11 %in% pos_11_dual)) %>% 
      f_by_pos_11(n = 2)
  ) %>% 
  group_by(pos_11) %>% 
  mutate(idx = row_number()) %>% 
  ungroup() %>% 
  mutate(idx2 = idx) %>% 
  mutate(
    across(
      idx, 
      ~case_when(
        pos_11 %in% pos_11_dual & .x == 2L ~ 1L,
        pos_11 %in% pos_11_dual & .x >= 3L ~ 2L,
        TRUE ~ .x
      )
    ),
    across(
      pos_11, 
      ~case_when(
        pos_11 %in% pos_11_dual & idx2 %in% c(2L, 4L) ~ sprintf('%sR', .x),
        pos_11 %in% pos_11_dual ~ sprintf('%sL', .x),
        TRUE ~ .x
      )
    )
  ) %>% 
  select(-idx2) %>% 
  left_join(pos_info_xy) %>% 
  left_join(team_info)%>% 
  left_join(img_info)
av_by_season_latest_pos

viz_team <-
  av_by_season_latest_pos %>% 
  ggplot() +
  aes(x = x, y = y) +
  pitch_gg(pitch = ggsoccer::pitch_opta, xlim = c(-1, 101), ylim = c(-1, 101)) +
  f_text(
    data = 
      av_by_season_latest_pos %>% 
      filter(idx == 2L) %>% 
      mutate(x = x - 2.52) %>% 
      mutate(
        lab = sprintf('<span style="font-size:11px;color:#7F7F7F">%s (%s) </span><span style="font-size:11px;color:#7F7F7F">%.2f</span>', player_name, team_abbrv, vaep_p90)
      ),
    aes(label = lab)
  ) +
  f_text(
    data =
      av_by_season_latest_pos %>%
      filter(idx == 1L) %>% 
      mutate(
        lab = sprintf('<span style="font-size:13px;color:black">%s (%s) </span><span style="font-size:11px;color:black">%.2f</span>', player_name, team_abbrv, vaep_p90)
      ), 
    aes(label = lab)
  ) +
  ggimage::geom_image(
    data =
      av_by_season_latest_pos %>%
      filter(idx == 1L) %>% 
      # slice(1) %>% 
      mutate(x = x + 8L),
    size = 0.1,
    aes(image = path)
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = ggtext::element_markdown(size = 11),
    plot.tag.position = c(.1, 0.01),
    plot.tag = ggtext::element_markdown(size = 11)
  ) +
  labs(
    title = 'VAEP XI of the Season',
    subtitle = '2020/21 Premier League, through Matchweek 34',
    caption = '**VAEP**: Valuing Actions by Estimating Probabilities<br/>Rankings based on best VAEP per 90 minute, minimum 1,000 minutes played.<br/>Positions are based on minutes played but may not be reflective of recent form.',
    # caption = '**VAEP**: Valuing Actions by Estimating Probabilities<br/>Rankings based on best total VAEP, minimum 1,000 minutes played.',
    # tag = '**Viz**: Tony ElHabr<br/>**Data**: 2020-21 Premier League through Matchweek 34'
    tag = '**Viz**: Tony ElHabr'
  )
viz_team

h <- 14
path_viz_team <- file.path(dir_proj, 'viz_team_vaep_p90.png')
ggsave(
  plot = viz_team,
  filename = path_viz_team,
  height = h + 2,
  width = h * 68 / 105,
  type = 'cairo'
)

add_logo_epl(
  path_viz = path_viz_team,
  idx_x = 0.13,
  logo_scale = 0.1,
  # adjust_y = TRUE,
  idx_y = 0.26
)

# idk yet ----
av_by_pos_11 <-
  av_by_season_latest %>% 
  filter(!is.na(pos_grp)) %>% 
  filter(minutes_played > 1000) %>% # (15 * 90)) %>% 
  left_join(pos_info) %>% 
  group_by(pos_11) %>% 
  slice_max(vaep_p90, n = 4) %>% 
  ungroup()
av_by_pos_11

pal <- c('#003f5c', '#7a5195', '#ef5675', '#ffa600')
viz_latest <-
  av_by_season_latest %>% 
  # filter(pos != 'Sub') %>% 
  filter(!is.na(pos_grp)) %>% 
  select(-pos_grp) %>% 
  ggplot() +
  aes(x = minutes_played, y = vaep) +
  geom_point(color = 'grey80') +
  geom_smooth(se = FALSE, color = 'black', size = 1.2) +
  geom_point(
    data =
      av_by_season_latest %>% 
      # filter(pos != 'Sub') %>% 
      filter(!is.na(pos_grp)),
    aes(color = pos_grp),
    show.legend = FALSE
  ) +
  scale_color_manual(values = pal) +
  facet_wrap(~pos_grp, scales = 'fixed') +
  labs(
    title = 'Player Values',
    y = 'VAEP',
    caption = '**VAEP**: Valuing Actions by Estimating Probabilities',
    x = 'Minutes Played'
  )
viz

av_by_season_compare <-
  av_by_season %>% 
  filter(season_id >= 2019L) %>% 
  filter(minutes_played > (2 * 90)) %>% 
  filter(!is.na(pos_grp)) %>% 
  select(season_id, team_id, team_name, player_id, player_name, vaep_p90) %>% 
  pivot_wider(
    names_from = season_id,
    values_from = vaep_p90,
    names_prefix = 'vaep_p90_'
  ) %>% 
  left_join(
    av_by_season %>% 
      filter(season_id == 2020L) %>% 
      select(season_id, team_id, team_name, player_id, player_name, pos_grp)
  ) %>%
  filter(!is.na(pos_grp))
av_by_season_compare

viz <-
  av_by_season_compare %>% 
  select(-pos_grp) %>% 
  ggplot() +
  aes(x = vaep_p90_2019, y = vaep_p90_2020) +
  geom_point(color = 'grey80') +
  geom_point(
    data = av_by_season_compare,
    aes(color = pos_grp)
  ) +
  scale_color_manual(values = pal) +
  facet_wrap(~pos_grp, scales = 'fixed')
viz

retrieve_market_values <-
  function(country_name = 'England',
           start_year = 2020,
           dir = dir_data,
           ext = 'rds',
           file = sprintf('transfer_market_values_%s_%s', country_name, start_year),
           path = NULL,
           f_import = read_rds,
           f_export = write_rds,
           overwrite = FALSE,
           export = TRUE) {
    if (is.null(path)) {
      path <- file.path(dir, sprintf('%s.%s', file, ext))
    }
    
    path_exists <- file.exists(path)
    if (path_exists & !overwrite) {
      # cat(glue::glue('{Sys.time()}: Returning early from `path = "{path}"`.'), sep = '\n')
      return(f_import(path))
    }
    
    res <- worldfootballR::get_player_market_values(country_name = country_name, start_year = start_year)
    
    if(export) {
      f_export(res, path)
    }
    res
  }

mkt <- 
  2017:2020 %>% 
  setNames(., .) %>% 
  map_dfr(~retrieve_market_values(country_name = 'England', start_year = .x), .id = 'season_id') %>% 
  mutate(across(season_id, as.integer)) %>%
  as_tibble()
mkt

# fuzzy-matching ----
.add_z_col <- function(data, ...) {
  data %>% 
    mutate(
      z = player_name %>% 
        stringi::stri_trans_general(id = 'Latin-ASCII') %>% 
        snakecase::to_snake_case()
    )
}

source(file.path(dir_proj, 'helpers.R'))
x <-
  mkt %>% 
  filter(season_id == 2020L) %>% 
  .add_z_col() %>% 
  distinct(z, season_id, player = player_name, team = squad, euro = player_market_value_euro)

res_av_mkt <-
  join_fuzzily(
    x,
    player_pos_filt %>% 
      filter(season_id == 2020L) %>% 
      .add_z_col() %>% 
      distinct(z, season_id, player = player_name, team = team_name, mp = minutes_played),
    suffix = c('mkt', 'opta')
  )
res_av_mkt

av_mkt <-
  x %>% 
  anti_join(res_av_mkt %>% select(z = z_mkt)) %>% 
  bind_rows(
    res_av_mkt %>% 
      # filter(score < 1) %>% 
      select(z = z_opta, score) %>% 
      left_join(x)
  ) %>%
  group_by(season_id) %>% 
  mutate(
    rnk = row_number(desc(euro))
  ) %>% 
  ungroup() %>% 
  arrange(season_id, rnk)
av_mkt

av_mkt %>% filter(!is.na(score))

av_mkt %>% 
  # rename(player_name = player) %>% 
  left_join(av_by_season_latest %>% .add_z_col()) %>% 
  lm(euro ~ vaep, data = .) %>% 
  broom::glance()

av_mkt %>% 
  # rename(player_name = player) %>% 
  left_join(av_by_season_latest %>% .add_z_col()) %>% 
  # filter(is.na(euro)) %>% 
  ggplot() +
  aes(x = euro, y = vaep) +
  geom_point()


# dev ----
market_values %>% 
  filter(season_id == 2020L) %>% 
  select(season_id, player_name, pos_tfmkt = player_position, euro = player_market_value_euro) %>% 
  mutate(
    z = player_name %>% stringi::stri_trans_general(id = 'Latin-ASCII') %>% snakecase::to_snake_case()
  ) %>% 
  full_join(
    av_by_season_latest %>% 
      filter(season_id == 2020L) %>% 
      filter(player_name %>% str_detect('Heung')) %>% 
      mutate(
        z = player_name %>% stringi::stri_trans_general(id = 'Latin-ASCII') %>% snakecase::to_snake_case()
      ) %>% 
      select(-player_name)
  ) %>% 
  arrange(desc(euro)) %>% 
  filter(is.na(minutes_played)) %>% 
  select(player_name, z, euro)
  

av_by_season_latest %>% 
  filter(player_name %>% str_detect('olo Kant'))

write_rds(market_values, )
game_info <-
  av_atomic_by_game %>% 
  filter(season_id == 2020L) %>% 
  slice(1) %>% 
  rename_with(~sprintf('%s_total', .x), matches('_value$'))
game_info
