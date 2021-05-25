
library(tidyverse)
dir_proj <- '28-202021_vaep'
dir_data <- file.path('..', 'whoscraped', 'data-test-socceraction')
source(file.path(dir_proj, 'helpers.R'))
do_save <- TRUE

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
  plot.tag.position = c(0.01, 0.025),
  legend.text = element_text(color = 'gray20', size = 14),
  strip.text = element_text(color = 'gray20', size = 14),
  strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

do_import <-
  function(path = NULL,
           name = tools::file_path_sans_ext(basename(path)),
           ext = 'parquet',
           dir = dir_data,
           f_import = arrow::read_parquet,
           prefix = NULL,
           suffix = NULL,
           sep = '_',
           assign = TRUE) {
    if(is.null(path)) {
      if (is.null(prefix)) {
        prefix <- ''
      } else {
        prefix <- paste0(prefix, sep)
      }
      
      if (is.null(suffix)) {
        suffix <- ''
      } else {
        suffix <- paste0(sep, suffix)
      }
      basename <- sprintf('%s%s%s.%s', prefix, name, suffix, ext)
      path <- file.path(dir, basename)
    }
    res <- path %>% f_import()
    if(!assign) {
      return(res)
    }
    assign(value = res, x = name, envir = .GlobalEnv)
  }

f_import <- function(name) {
  path <- file.path(dir_data, sprintf('%s.parquet', name))
  nm <- str_remove(name, '_local')
  do_import(path, name = nm)
}


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

pitch_gg <-
  function(pitch = .pitch_international,
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

retrieve_market_values <-
  function(country_name = 'England',
           start_year = 2020,
           dir = dir_proj,
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

.add_z_col <- function(data, ...) {
  data %>% 
    mutate(
      z = player_name %>% 
        stringi::stri_trans_general(id = 'Latin-ASCII') %>% 
        snakecase::to_snake_case()
    )
}

c('actions_valued_local', 'actions_valued_atomic_local', 'games', 'players', 'player_games', 'teams') %>% 
  walk(f_import)

# main ----
player_games_clean <-
  player_games %>% 
  # This Arsenal - Leicester game has an erroneous number of minutes
  # https://www.whoscored.com/Matches/1485349/Live/England-Premier-League-2020-2021-Leicester-Arsenal
  mutate(
    across(minutes_played, ~case_when(game_id == 1485349L ~ 98L, TRUE ~ .x))
  ) %>% 
  rename(pos = starting_position_name, pos_id = starting_position_id)

pos_grps <- c('F', 'M', 'D', 'G', 'z')
pos_grp_labs <- c('Forward/Attacker', 'Midfielder', 'Defender', 'Goalkeeper', 'Other')
.factor_pos_grp_col <- function(data) {
  data %>% 
    mutate(
      across(pos_grp, ~ordered(.x, pos_grps))
    )
}

.factor_pos_grp_lab_col <- function(data) {
  data %>% 
    # mutate(
    #   across(pos_grp_lab, ~ordered(.x, pos_grp_labs))
    # ) %>% 
    mutate(pos_grp_lab = ordered(pos_grp_lab, !!pos_grp_labs))
}

pos_grp_labs <-
  tibble(
    pos_grp = pos_grps,
    pos_grp_lab = pos_grp_labs
  ) %>% 
  .factor_pos_grp_col() %>% 
  .factor_pos_grp_lab_col()

pos_info <-
  tibble(
    pos = c('AMC', 'AML', 'AMR', 'DC', 'DL', 'DMC', 'DML', 'DMR', 'DR', 'FW', 'FWL', 'FWR', 'GK', 'MC', 'ML', 'MR', 'Sub'),
    pos_grp = c('F', 'F', 'F', 'D', 'D', 'M', 'M', 'M', 'D', 'F', 'F', 'F', 'G', 'M', 'M', 'M', 'z'),
    pos_11 = c('FWC', 'FWL', 'FWR', 'DC', 'DL', 'MC', 'ML', 'MR', 'DR', 'FWC', 'FWL', 'FWR', 'GK', 'MC', 'ML', 'MR', 'z')
  ) %>% 
  .factor_pos_grp_col() #  %>% 
  # filter(!(pos %in% c('GK', 'Sub')))
pos_info

pos_info_xy <-
  tibble(
    pos_11 = c('GK', 'DL', 'DCL', 'DCR', 'DR', 'ML', 'MC', 'MR', 'FWL', 'FWC', 'FWR'),
    x = c(5, 30, 25, 25, 30, 55, 50, 55, 75, 70, 75),
    y = c(50, 15, 38, 62, 85, 20, 50, 80, 20, 50, 80)
  ) %>% 
  mutate(across(c(x, y), as.integer))

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
  left_join(pos_info)
player_pos

player_pos_filt <-
  player_pos %>% 
  filter(pos != 'Sub') %>% 
  group_by(season_id, competition_id, team_id, team_name, player_id, player_name) %>% 
  slice_max(frac, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(-frac) 
# distinct()
player_pos_filt

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

.add_pos_cols <- function(av) {
  df <-
    av %>% 
    # head(200) %>% 
    group_by(season_id, competition_id, game_id, period_id) %>% 
    mutate(
      poss_change = if_else(team_id != lag(team_id), TRUE, FALSE) %>% coalesce(FALSE)
    ) %>% 
    ungroup()
  
  poss_ids <-
    df %>% 
    filter(poss_change) %>% 
    group_by(season_id, competition_id, game_id, period_id) %>% 
    mutate(idx_inter = row_number()) %>% 
    ungroup()
  poss_ids
  
  suppressMessages(
    df %>% 
      left_join(poss_ids %>% select(-c(scores, concedes, off, def, matches('[xy]$')))) %>% 
      fill(idx_inter) %>% 
      group_by(season_id, competition_id, game_id, period_id, idx_inter) %>% 
      mutate(
        idx_intra = row_number()
      ) %>% 
      ungroup() %>% 
      select(-poss_change) %>% 
      relocate(matches('^idx'))
  )
}

postprocess_av <- function(av) {
  # browser()
  res <-
    av %>% 
    rename(
      off = offensive_value,
      def = defensive_value,
      vaep = vaep_value
    ) %>% 
    mutate(idx = row_number()) %>% 
    .add_pos_cols() %>% 
    relocate(season_id, competition_id, team_name, player_name, matches('^result_name$')) %>% 
    select(-matches('^d[xy]$'), -c(attendance:venue))
}

select_av <- function(av, suffix) {
  av %>% 
    # head(200) %>% 
    select(
      # idx,
      game_id,
      original_event_id,
      period_id,
      time_seconds,
      team_id,
      team_name,
      player_id,
      player_name,
      type_name,
      bodypart_name,
      # start_x, # not easy to join on x/y
      # start_y,
      matches('result_name'),
      matches('idx'),
      action_id,
      # scores,
      # concedes,
      off,
      def,
      vaep
    ) %>% 
    rename_with(~sprintf('%s_%s', .x, suffix), c(matches('idx'), matches('type_name'), action_id:vaep))
}

av <- actions_valued %>% postprocess_av()
av
rm('actions_valued')

ava <- actions_valued_atomic %>% postprocess_av()
ava
rm('actions_valued_atomic')

av_by_game <- av %>% aggregate_av_by_game()
av_by_game

av_by_season <- av_by_game %>% aggregate_av_by_season()
av_by_season

ava_by_game <- ava %>% aggregate_av_by_game()
ava_by_game

ava_by_season <- ava_by_game %>% aggregate_av_by_season()
ava_by_season

av_join_by_season <-
  ava_by_season %>% 
  rename_with(
    ~sprintf('%s_atomic', .x),
    c(off:vaep_p90)
  ) %>% 
  left_join(
    av_by_season %>% 
      filter(season_id == 2020L) %>% 
      rename_with(
        ~sprintf('%s_orig', .x),
        c(off:vaep_p90)
      )
  ) %>% 
  relocate(pos:pos_11, .after = last_col()) %>% 
  arrange(desc(vaep_atomic))
av_join_by_season

av_join_by_season %>% 
  mutate(
    across(
      matches('^(off|def|vaep)_(atomic|orig)$'),
      list(rnk = ~row_number(desc(.x)))
    )
  ) %>% 
  arrange(desc(vaep_orig))

ava_by_season %>% arrange(desc(def))

# av_join <-
#   full_join(
#     av %>% select_av('orig'),
#     ava %>% select_av('atomic')
#   ) %>% 
#   relocate(matches('idx'))
# 
# dups <-
#   av_join %>%
#   filter(!is.na(idx_orig)) %>% 
#   count(idx_orig) %>% 
#   filter(n > 1L) %>% 
#   left_join(av_join %>% select(idx_orig, type_name_orig, type_name_atomic))
# dups
# 
# av_join <-
#   av_join %>% 
#   left_join(
#     dups %>% 
#       group_by(idx_orig) %>% 
#       mutate(idx_dup = row_number()) %>% 
#       ungroup()
#   ) %>% 
#   mutate(
#     across(
#       c(off_orig, def_orig, vaep_orig),
#       ~case_when(
#         !is.na(idx_dup) & idx_dup > 1L ~ NA_real_,
#         TRUE ~ .x
#       )
#     )
#   ) %>%
#   select(-idx_dup) %>% 
#   mutate(
#     time_min = time_seconds / 60,
#     diff_off = off_orig - off_atomic,
#     diff_def = def_orig - def_atomic,
#     diff_vaep = vaep_orig - vaep_atomic
#   ) %>% 
#   relocate(
#     idx_atomic,
#     game_id,
#     period_id,
#     time_min,
#     team_name,
#     player_name, 
#     bodypart_name,
#     result_name,
#     matches('type_name'),
#     vaep_orig,
#     vaep_atomic,
#     diff_vaep
#   ) %>% 
#   distinct(idx_atomic, .keep_all = TRUE) %>% 
#   arrange(idx_atomic)
# av_join

av_by_season_latest <-
  av_join_by_season %>% 
  filter(season_id == 2020L) %>% 
  filter(minutes_played > (2 * 90)) %>% 
  # left_join(pos_info) %>% 
  arrange(desc(vaep_p90_atomic))
av_by_season_latest

# funcs ----
lab_tag <- '**Viz**: Tony ElHabr'
# This is a manual adjustment for this play, since it seems slightly off compared to the film.
# https://youtu.be/96_Rfnd1sKA?t=690
f_text <- partial(
  ggtext::geom_richtext,
  # fill = NA, 
  label.color = NA,
  family = 'Karla',
  fontface = 'bold',
  # inherit.aes = FALSE,
  ...=
)

# team ----
f_by_pos_11 <- function(av, n = 2) {
  av %>% 
    drop_na(pos_grp) %>% 
    # filter(minutes_played > (15 * 90)) %>% 
    filter(minutes_played > 2000) %>% 
    group_by(pos_11) %>% 
    slice_max(vaep_p90_atomic, n = n) %>% 
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
  left_join(team_info) %>% 
  left_join(img_info)
av_by_season_latest_pos %>% filter(idx == 2L)

# best xi ----
lab_subtitle <- '2020/21 Premier League'
lab_caption_vaep <- '**VAEP**: Valuing Actions by Estimating Probabilities'
viz_team <-
  av_by_season_latest_pos %>% 
  ggplot() +
  aes(x = x, y = y) +
  pitch_gg(pitch = ggsoccer::pitch_opta, xlim = c(-1, 101), ylim = c(-1, 101), aspect_ratio = 1) +
  f_text(
    data = 
      av_by_season_latest_pos %>% 
      filter(idx == 2L) %>% 
      mutate(x = x - 3) %>% 
      mutate(
        lab = sprintf('<span style="font-size:12px;color:#7F7F7F">%s (%s) </span><span style="font-size:10px;color:#7F7F7F">%.2f</span>', player_name, team_abbrv, vaep_p90_atomic)
      ),
    aes(label = lab)
  ) +
  f_text(
    data =
      av_by_season_latest_pos %>%
      filter(idx == 1L) %>% 
      mutate(
        lab = sprintf('<span style="font-size:14px;color:black">%s (%s) </span><span style="font-size:10px;color:black">%.2f</span>', player_name, team_abbrv, vaep_p90_atomic)
      ), 
    aes(label = lab)
  ) +
  # ggimage::geom_image(
  #   data =
  #     av_by_season_latest_pos %>%
  #     filter(idx == 1L) %>% 
  #     # slice(1) %>% 
  #     mutate(x = x + 8L),
  #   size = 0.08,
  #   aes(image = path)
  # ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = ggtext::element_markdown(size = 11)
  ) +
  labs(
    title = 'Atomic VAEP XI of the Season',
    subtitle = lab_subtitle,
    caption = glue::glue('{lab_caption_vaep}<br/>Rankings based on best atomic VAEP per 90 minute, minimum 2,000 minutes played.<br/>{lab_tag}')
  )
viz_team

if(do_save) {
  h <- 12
  path_viz_team <- file.path(dir_proj, 'viz_team_vaep_p90_atomic.png')
  ggsave(
    plot = viz_team,
    filename = path_viz_team,
    height = h, #  + 2,
    width = h * 68 / 105, # close enough to 16/9 ratio
    type = 'cairo'
  )
  
  add_logo_epl(
    path_viz = path_viz_team,
    idx_x = 0.08,
    logo_scale = 0.12,
    idx_y = 0.22
  )
}

# scatter by pos ----
# pal <- c('#003f5c', '#7a5195', '#ef5675', '#ffa600') # default tony 4
pal <- c('#ef426f', '#00b2a9', '#ff8200', '#7a5195') %>% rev() # spurs fiesta + purple
# pal <- c('#1e3160', '#f05333', '#0a7ec2', '#fcbb30') # thunder
.f_slice <- function(f = slice_max, lab = 'hi') {
  av_by_season_latest %>% 
    # filter(pos != 'Sub') %>% 
    drop_na(pos_grp) %>% 
    filter(minutes_played >= 2000) %>% 
    group_by(pos_grp) %>% 
    f(vaep_p90_atomic, n = 3) %>% 
    ungroup() %>% 
    mutate(grp = !!lab)
}

av_by_season_labs <-
  bind_rows(
    .f_slice(slice_max, 'hi'),
    .f_slice(slice_min, 'lo')
  )
av_by_season_labs

viz_by_pos <-
  av_by_season_latest %>% 
  drop_na(pos_grp) %>% 
  ggplot() +
  aes(x = minutes_played, y = vaep_atomic) +
  geom_point(color = 'grey80') +
  geom_smooth(
    se = FALSE,
    color = 'black',
    size = 1.2,
    formula = formula(y ~ x),
    # method = 'loess'
    method = 'lm'
  ) +
  geom_point(
    data =
      av_by_season_latest %>% 
      drop_na(pos_grp) %>% 
      left_join(pos_grp_labs),
    size = 2,
    aes(color = pos_grp_lab),
    show.legend = FALSE
  ) +
  ggrepel::geom_label_repel(
    data = 
      av_by_season_labs %>% 
      left_join(pos_grp_labs),
    family = 'Karla',
    fontface = 'bold',
    size = pts(12),
    label.size = NA,
    min.segment.length = 0, seed = 42, box.padding = 1,
    aes(label = player_name)
  ) +
  scale_x_continuous(labels = scales::number_format(big.mark = ',')) +
  scale_color_manual(values = pal) +
  facet_wrap(~pos_grp_lab, scales = 'fixed') +
  theme(
    strip.text = element_text(face = 'bold', size = 16)
  ) +
  labs(
    title = 'Atomic VAEP By Position',
    subtitle = lab_subtitle,
    y = 'Atomic VAEP',
    caption = lab_caption_vaep,
    tag = lab_tag,
    x = 'Minutes Played'
  )
viz_by_pos

if(do_save) {
  path_viz_by_pos <- file.path(dir_proj, 'viz_vaep_atomic_by_pos.png')
  h <- 10
  ggsave(
    plot = viz_by_pos,
    filename = path_viz_by_pos,
    height = h,
    width = h * 4/3,
    type = 'cairo'
  )
  
  add_logo_epl(
    path_viz = path_viz_by_pos,
    idx_x = 0.01,
    logo_scale = 0.08,
    # adjust_y = TRUE,
    idx_y = 0.9
  )
}

# mkt ----
mkt <-
  2017:2020 %>% 
  setNames(., .) %>% 
  map_dfr(~retrieve_market_values(country_name = 'England', start_year = .x), .id = 'season_id') %>% 
  mutate(across(season_id, as.integer)) %>%
  as_tibble() %>% 
  rename(euro = player_market_value_euro) %>% 
  drop_na(euro) %>% 
  group_by(season_id, player_name) %>% 
  slice_max(euro, with_ties = FALSE) %>% 
  ungroup()
mkt

mkt_prep <-
  mkt %>% 
  # filter(season_id == 2020L) %>% 
  .add_z_col() %>% 
  distinct(z, season_id, player_name, euro)
mkt_prep

opta_prep <-
  player_pos_filt %>% 
  # filter(season_id == 2020L) %>% 
  .add_z_col() %>% 
  distinct(z, season_id, player_name)
opta_prep

res_av_mkt <-
  join_fuzzily(
    mkt_prep,
    opta_prep,
    col = 'z',
    suffix = c('mkt', 'opta')
  )
res_av_mkt

av_mkt <-
  mkt_prep %>% 
  anti_join(res_av_mkt %>% select(z = z_mkt)) %>% 
  # Because I do the fuzzy join strictly, there will still be NAs
  select(-player_name) %>% 
  inner_join(opta_prep) %>%
  bind_rows(
    res_av_mkt %>% 
      left_join(mkt_prep %>% select(season_id, player_name, z_mkt = z, euro)) %>% 
      relocate(euro) %>% 
      filter(score < 1) %>% 
      select(season_id, z = z_opta, score, euro) %>% 
      inner_join(opta_prep)
  ) %>%
  inner_join(av_by_season_latest) %>% 
  arrange(desc(season_id), desc(euro))
av_mkt

av_mkt_filt <-
  av_mkt %>% 
  filter(season_id == 2020L) %>% 
  # filter(pos != 'Sub') %>% 
  drop_na(pos_grp)
av_mkt_filt

.f_slice_mkt <- function(f = slice_max, lab = 'hi') {
  av_mkt_filt %>% 
    filter(minutes_played >= 2000) %>% 
    group_by(pos_grp) %>% 
    f(vaep_p90_atomic, n = 3) %>% 
    ungroup() %>% 
    mutate(grp = !!lab)
}

av_mkt_labs <-
  bind_rows(
    .f_slice_mkt(slice_max, 'hi'),
    .f_slice_mkt(slice_min, 'lo')
  )
av_mkt_labs

av_mkt_w_pos <-
  av_mkt_filt %>% 
  left_join(pos_grp_labs)
av_mkt_w_pos

f_text_mkt <- partial(
  ggtext::geom_richtext,
  fill = NA, 
  label.color = NA,
  fontface = 'bold',
  family = 'Karla',
  # size = pts(16),
  aes(x = x, y = y, label = lab),
  ... = 
)

viz_mkt_by_pos <-
  av_mkt_filt %>% 
  ggplot() +
  aes(x = euro, y = vaep_atomic) +
  # geom_point(color = 'grey80') +
  geom_smooth(
    data = av_mkt_w_pos,
    se = FALSE,
    # color = 'black',
    size = 1.2,
    formula = formula(y ~ x),
    method = 'lm',
    show.legend = FALSE,
    aes(color = pos_grp_lab)
  ) +
  geom_point(
    data = av_mkt_w_pos,
    show.legend = FALSE,
    size = 2,
    aes(color = pos_grp_lab)
  ) +
  ggrepel::geom_label_repel(
    data = 
      av_mkt_labs %>% 
      left_join(pos_grp_labs),
    family = 'Karla',
    fontface = 'bold',
    size = pts(12),
    label.size = NA,
    min.segment.length = 0, seed = 42, box.padding = 1,
    aes(label = player_name)
  ) +
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ',',
      scale = 1e-6,
      suffix = 'M',
      prefix = ''
    )
  ) +
  scale_color_manual(values = pal) +
  facet_wrap( ~ pos_grp_lab, scales = 'free') +
  theme(
    strip.text = element_text(face = 'bold', size = 16)
  ) +
  labs(
    title = 'Atomic VAEP & Market Values By Position',
    subtitle = lab_subtitle,
    y = 'Atomic VAEP',
    caption = lab_caption_vaep,
    tag = paste0(lab_tag, ' | **Market Data**: transfermarkt'),
    x = 'Market Value (Euro)'
  ) +
  coord_cartesian(clip = 'off') +
  f_text_mkt(
    hjust = 1,
    # vjust = 1,
    data = tibble(
      x = 125000000,
      y = 3,
      pos_grp_lab = ordered('Forward/Attacker'), # , levels = pos_grp_labs),
      lab = '<b><i><span style="color:red;font-size:18px";>OVERVALUED</span><i></b> <i><span style="color:black;font-size:14px";>(below line)</span></i>'
    )
  ) +
  f_text_mkt(
    hjust = 0,
    data = tibble(
      x = 1000000,
      y = 32,
      pos_grp_lab = ordered('Forward/Attacker'), # , levels = pos_grp_labs),
      lab = '<b><i><span style="color:red;font-size:18px";>UNDERVALUED</span><i></b> <i><span style="color:black;font-size:12px";>(above line)</span></i>'
    )
  )
viz_mkt_by_pos

if(do_save) {
  path_viz_mkt_by_pos <- file.path(dir_proj, 'viz_vaep_atomic_mkt_by_pos.png')
  h <- 10
  ggsave(
    plot = viz_mkt_by_pos,
    filename = path_viz_mkt_by_pos,
    height = h,
    width = h, #  * 4/3,
    type = 'cairo'
  )
  
  add_logo_epl(
    path_viz = path_viz_mkt_by_pos,
    idx_x = 0.01,
    logo_scale = 0.08,
    # adjust_y = TRUE,
    idx_y = 0.9
  )
}

# davies ----
# Source: https://samgoldberg1882.shinyapps.io/ShinyAlph/
davies <- 
  file.path(dir_proj, 'DAVIES_20210525.csv') %>% 
  read_csv() %>%
  janitor::clean_names() %>% 
  filter(league == 'Premier League') %>% 
  mutate(across(season, ~str_sub(.x, 1, 4) %>% as.integer())) %>% 
  rename(player_name = player, season_id = season, xga = x_goals_added) %>% 
  group_by(season_id, player_name) %>% 
  summarize(across(c(xga, davies), sum)) %>% 
  ungroup()
davies

davies_prep <-
  davies %>%
  .add_z_col() %>% 
  distinct(z, player_name, season_id, davies, xga, salary)
davies_prep

res_av_davies <-
  join_fuzzily(
    davies_prep,
    opta_prep,
    col = 'z',
    suffix = c('davies', 'opta')
  )
res_av_davies

av_davies <-
  davies_prep %>% 
  anti_join(res_av_davies %>% select(z = z_davies)) %>% 
  select(-player_name) %>% 
  inner_join(opta_prep) %>% 
  bind_rows(
    res_av_davies %>% 
      left_join(davies_prep %>% select(season_id, player_name, z_davies = z, davies, xga)) %>% 
      # A visual check indicates that I can safely accept >1 but not >2.
      filter(score < 2) %>% 
      select(season_id, z = z_opta, score, davies, xga) %>% 
      # filter(player_name %>% str_detect('Gomez'))
      inner_join(opta_prep)
  ) %>%
  inner_join(av_by_season_latest) %>% 
  # DAVIES data set doesn't have keepers.
  filter(pos_grp %in% c('F', 'M', 'D')) %>% 
  mutate(diff_vaep_davies = vaep_atomic - davies) %>% 
  arrange(desc(season_id), desc(abs(diff_vaep_davies)))
av_davies

av_davies_filt <-
  av_davies %>% 
  filter(season_id == 2020L)
av_davies_filt

fit_davies <-
  av_davies_filt %>% 
  lm(vaep_atomic ~ davies, data = .) 
summ_davies <- fit_davies %>% broom::glance()
summ_davies

preds_davies <- 
  fit_davies %>% 
  broom::augment() %>% 
  bind_cols(av_davies_filt %>% select(-c(vaep_atomic, davies)))
preds_davies

.f_slice_davies <- function(f = slice_max, lab = 'hi') {
  # av_davies %>% 
  preds_davies %>% 
    filter(minutes_played >= 1000) %>% 
    # group_by(pos_grp) %>% 
    f(.resid, n = 10) %>% 
    # ungroup() %>% 
    mutate(grp = !!lab)
}

av_davies_labs <-
  bind_rows(
    .f_slice_davies(slice_max, 'hi'),
    .f_slice_davies(slice_min, 'lo'),
    preds_davies %>% filter(player_name %in% c('Ruben Dias', 'Kevin De Bruyne'))
  ) %>% 
  distinct(player_name, .keep_all = TRUE)
av_davies_labs

f_text_davies <- f_text_mkt

viz_davies_compare <-
  av_davies_filt %>% 
  ggplot() +
  aes(x = davies, y = vaep_atomic) +
  geom_smooth(
    se = FALSE,
    color = 'black',
    size = 1.2,
    formula = formula(y ~ x),
    method = 'lm'
  ) +
  geom_point(
    data = preds_davies,
    # show.legend = FALSE,
    # size = 1,
    aes(color = .resid, size = abs(.resid))
  ) +
  ggrepel::geom_label_repel(
    data = av_davies_labs,
    family = 'Karla',
    fontface = 'bold',
    size = pts(12),
    label.size = NA,
    min.segment.length = 0, seed = 42, box.padding = 1,
    aes(label = player_name)
  ) +
  scale_color_viridis_c(option = 'H', guide = guide_colorbar(title = 'VAEP minus DAVIES')) +
  guides(size = FALSE) +
  theme(
    legend.position = 'top',
    plot.tag = ggtext::element_markdown(size = 10),
    plot.caption = ggtext::element_markdown(size = 10),
    legend.text = element_blank(),
    legend.title = element_text(face = 'bold', size = 14),
    strip.text = element_text(face = 'bold', size = 16)
  ) +
  labs(
    title = 'Atomic VAEP & DAVIES Comparison',
    subtitle = lab_subtitle,
    y = 'Atomic VAEP',
    caption = paste0(lab_caption_vaep, '<br/>**DAVIES**: Determining Added Value of Individual Effectiveness'),
    tag = paste0(lab_tag, '<br/>**Davies Data**: @mimburgio & @SamGoldberg1882'),
    x = 'Davies'
  ) +
  f_text_mkt(
    hjust = 0,
    data = tibble(
      x = -3,
      y = 11,
      lab = glue::glue('<b><i><span style="color:red;font-size:34px";>R<sup>2</sup>: {sprintf("%.2f", summ_davies$r.squared)}</span><i></b>')
    )
  )
viz_davies_compare

if(do_save) {
  path_viz_davies_compare <- file.path(dir_proj, 'viz_vaep_atomic_davies_compare_nonshot.png')
  h <- 8
  ggsave(
    plot = viz_davies_compare,
    filename = path_viz_davies_compare,
    height = h,
    width = h,
    type = 'cairo'
  )
  
  add_logo_epl(
    path_viz = path_viz_davies_compare,
    idx_x = 0.01,
    logo_scale = 0.1,
    idx_y = 0.9
  )
}

res_av <-
  full_join(
    av_mkt %>%
      select(
        season_id,
        player_name,
        team_name,
        pos_grp,
        pos_11,
        minutes_played,
        euro,
        ovaep_orig = off_orig,
        dvaep_orig = def_orig,
        vaep_orig,
        ovaep_atomic = off_atomic,
        dvaep_atomic = def_atomic,
        vaep_atomic
      ),
    av_davies %>%
      select(season_id, player_name, team_name, davies, x_goals_added = xga)
  ) %>%
  arrange(desc(season_id), desc(vaep_atomic))
res_av

# res_av %>% count(season_id, team_name, player_name, sort = TRUE)
if(do_save) {
  write_csv(res_av, file.path(dir_proj, '2017-21_epl_vaep_atomic_davies_mkt.csv'), na = '')
}

# TODO
games_split <- 
  games %>% 
  select(game_id) %>% 
  mutate(idx = row_number()) %>% 
  mutate(
    grp = case_when(
      idx %% 2L == 0L ~ 2L,
      TRUE ~ 1L
    )
  ) %>% 
  select(-idx)
games_split

cors <-
  full_join(
    av_by_game,
    av_by_game
  ) %>% 
  left_join(games_split) %>% 
  relocate(grp) %>% 
  select(-season_id) %>% 
  mutate(
    across(c(vaep_orig, vaep_atomic), ~90 * .x / minutes_played)
  ) %>% 
  select(grp, player_name, team_name, vaep_orig, vaep_atomic, davies, x_goals_added) %>% 
  pivot_longer(
    -c(grp:team_name)
  ) %>% 
  pivot_wider(
    names_from = grp,
    values_from = value
  ) %>% 
  select(-c(player_name, team_name)) %>% 
  group_nest(name) %>% 
  mutate(data = map(data, corrr::correlate)) %>% 
  unnest(data) %>% 
  rename(y1 = term) %>% 
  pivot_longer(
    -c(name, y1),
    names_to = 'y2'
  ) %>% 
  mutate(across(c(y1, y2), as.integer)) %>% 
  filter(y1 == (y2 - 1L))
cors
