library(arrow)
library(httr)
library(readr)

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)

library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtext)
library(ragg)
library(htmltools)
library(ggimage)
library(grid)
library(magick)
library(cropcircles)

input_data_dir <- '../socceraction-streamlined/data/final'
proj_dir <- '70-2023_vaep'
data_dir <- file.path(proj_dir, 'data')
dir.create(data_dir, showWarnings = FALSE)

all_vaep <- arrow::read_parquet(file.path(input_data_dir, 'all_vaep.parquet'))
players_season_games <- arrow::read_parquet(file.path(input_data_dir, 'players_season_games.parquet'))
vaep_by_player_season <- arrow::read_parquet(file.path(input_data_dir, 'vaep_by_player_season.parquet'))
search_and_cache_player_on_fotmob <- function(player_id, term, overwrite = FALSE) {
  path <- file.path(data_dir, paste0(player_id, '.csv'))
  if (file.exists(path) & isFALSE(overwrite)) {
    return(read_csv(path, show_col_types = FALSE))
  }
  params = list(
    'term' = term,
    'lang' = 'en'
  )
  resp <-  GET(
    'https://apigw.fotmob.com/searchapi/suggest',
    query = params
  )
  cont <- content(resp)
  res <- bind_rows(cont$squadMemberSuggest) |> 
    select(options) |> 
    unnest_wider(options) |>
    select(text) |> 
    transmute(
      player_name = str_remove(text, '[|].*$'),
      player_id = as.integer(str_remove(text, '^.*[|]'))
    )
  write_csv(res, path, na = '')
  res
}

generate_player_image_url <- function(player_id) {
  sprintf('https://images.fotmob.com/image_resources/playerimages/%s.png', player_id)
}

## FWL     FW      FWR
## AML     AMC     AMR
## ML      MC      MR
## DL  DML DC  DMR DR
##         DMC
##         GK
position_343_mapping <- list(
  'Left Forward / Attacker' = c('FWL', 'AML'),
  'Center Forward / Attacker' = c('FW', 'AMC'),
  'Right Forward / Attacker' = c('FWR', 'MR'),
  'Left Mid' = c('AML', 'ML'),
  'Center Mid' = c('AMC', 'MC'),
  'Right Mid' = c('AMR', 'MR'),
  'Left Defender' = c('DL', 'DML'),
  'Center Defender' = c('DC', 'DMC'),
  'Right Defender' = c('DR', 'DMR'),
  'Keeper' = 'GK'
) |> 
  enframe('position', 'opta_position') |> 
  unnest_longer(opta_position)

## TODO: Refactor this to go from list to tibble (to improve readability)
position_343_coords <- tibble(
  position = c('Keeper', 'Left Defender', 'Center Defender', 'Right Defender', 'Left Mid', 'Left Center Mid', 'Right Center Mid', 'Right Mid', 'Left Forward / Attacker', 'Center Forward / Attacker', 'Right Forward / Attacker'),
  x = c( 8, 28, 28, 28, 60, 45, 45, 60, 80, 75, 80),
  y = c(50, 15, 50, 85, 14, 32, 68, 86, 20, 50, 80)
) |>
  mutate(across(c(x, y), as.integer))

team_abbrvs <- tibble(
  team_name = c('Bournemouth', 'Arsenal', 'Aston Villa', 'Brighton', 'Brentford', 'Chelsea', 'Crystal Palace', 'Everton', 'Fulham', 'Leeds', 'Leicester', 'Liverpool', 'Man City', 'Man Utd', 'Newcastle', 'Nottingham Forest', 'Southampton', 'Tottenham', 'West Ham', 'Wolves'),
  team_abbrv = c('BOU', 'ARS', 'AVL', 'BRI', 'BRE', 'CHE', 'CRY', 'EVE', 'FUL', 'LEE', 'LEI', 'LIV', 'MCI', 'MUN', 'NEW', 'FOR', 'SOU', 'TOT', 'WHU', 'WOL')
)

filter_data <- function(df) {
  df |> 
    filter(
      competition_id == 8,
      season_id == 2023
    )
}

min_minutes_played <- 2000L
latest_vaep_by_players_positions <- vaep_by_player_season |> 
  filter_data() |> 
  filter(
    minutes_played >= min_minutes_played
  ) |> 
  left_join(
    position_343_mapping,
    ## intentional mulit-join for FW / M (due to AMx being in multiple groups)
    by = join_by(starting_position == opta_position),
    relationship = 'many-to-many'
  ) |> 
  ## Force AMx into either Forward / Attcker role or Mid role
  mutate(
    position = case_when(
      str_detect(position, 'Forward / Attacker$') & substr(starting_position, 1, 2) == 'AM' & starting_position_prop < 0.8 ~ NA_character_,
      TRUE ~ position
    )
  ) |> 
  filter(!is.na(position)) |> 
  add_count(player_id, name = 'n_player_id') |> 
  # filter(n_player_id > 1) |> 
  #A select(player_id, player_name, position, starting_position, starting_position_prop, n_player_id) |> 
  mutate(
    position = case_when(
      n_player_id == 1 ~ position,
      str_detect(position, 'Mid$') & substr(starting_position, 1, 2) == 'AM' & starting_position_prop >= 0.8 ~ NA_character_,
      TRUE ~ position
    )
  ) |> 
  filter(!is.na(position)) |> 
  group_by(position) |> 
  mutate(
    rn = row_number(desc(vaep_atomic_p90))
  ) |> 
  ungroup()

set.seed(42)
best_xi <- latest_vaep_by_players_positions |> 
  ## break ties with players labeled as both left mid and left forward, etc.
  group_by(competition_id, season_id, player_id) |> 
  slice_sample(n = 1) |> 
  ungroup() |> 
  ## randomly assign players to left/right midfielder or left/right forward. any logic that tries
  ##   to assign players to one or the another or to both is susceptible to error.
  group_by(position) |> 
  mutate(
    rn = row_number(desc(vaep_atomic_p90))
  ) |> 
  ungroup() |> 
  # filter(position == 'Left Mid' | position == 'Left Forward / Attacker') |> 
  # select(player_name, position, team_name, vaep_atomic_p90, rn) |> 
  arrange(rn) |> 
  filter(
    rn <= 2L | (position == 'Center Mid' & rn <= 4L)
  ) |>
  mutate(
    orig_rn = rn,
    rn = case_when(
      position == 'Center Mid' & rn == 2L ~ 1L,
      position == 'Center Mid' & rn >= 3L ~ 2L,
      TRUE ~ rn
    ),
    position = case_when(
      position == 'Center Mid' & orig_rn %in% c(2L, 4L) ~ 'Left Center Mid',
      position == 'Center Mid' & orig_rn %in% c(1L, 3L) ~ 'Right Center Mid',
      TRUE ~ position
    )
  ) |> 
  inner_join(
    position_343_coords,
    by = join_by(position)
  ) |> 
  inner_join(
    team_abbrvs,
    by = join_by(team_name)
  ) |> 
  select(
    position,
    rn,
    player_id,
    player_name,
    team_abbrv,
    x,
    y,
    vaep_atomic_p90,
    minutes_played
  )
stopifnot(nrow(best_xi) == 2 * 11)

best_xi_player_names <- best_xi |> 
  filter(rn == 1L)

best_xi_rn1_with_logos <- set_names(
  best_xi_player_names$player_name,
  best_xi_player_names$player_id
) |> 
  imap_dfr(
    ~{
      ## stri_trans_general removes accents
      term <- tolower(stringi::stri_trans_general(.x, id = 'Latin-ASCII'))
      term <- case_when(
        .x == 'Erling Haaland' ~ substr(term, 1, 6),
        .x == 'Son Heung-Min' ~ 'Heung-Min',
        TRUE ~ substr(term, 1, 10)
      )
      
      res <- search_and_cache_player_on_fotmob(
        player_id = .y, 
        term = term
      ) |> 
        rename(
          fotmob_player_id = player_id,
          fotmob_player_name = player_name
        ) |> 
        mutate(
          player_id = as.integer(.y),
          player_name = .x,
          .before = 1
        )
      
      ## specifically for the goalkeeper
      if (.y == 76202) {
        res <- filter(res, fotmob_player_id == 176186)
      }
      res
    }
  ) |>
  group_by(player_id) |> 
  filter(row_number() == 1L) |> 
  ungroup() |> 
  mutate(
    player_url = generate_player_image_url(fotmob_player_id),
    local_player_path = map_chr(
      player_url,
      ~{
        path <- file.path(data_dir, basename(.x))
        if (file.exists(path)) {
          return(path)
        }
        download.file(.x, destfile = path, mode = 'wb')
        path
      }
    ),
    local_player_path = map_chr(
      local_player_path,
      ~{
        whitened <- magick::image_background(
          magick::image_read(.x),
          color = 'white'
        )
        new_path <- gsub('\\.png', '_cropped.png', .x)
        magick::image_write(whitened, new_path)
        cropcircles::circle_crop(
          new_path,
          to = new_path,
          border_colour = 'black'
        )
        new_path
      }
    )
  ) |> 
  inner_join(
    best_xi |> select(player_id, x, y),
    by = join_by(player_id)
  ) |> 
  mutate(
    x = x + 8
  )

## plot ----
font <- 'Titillium Web'
sysfonts::font_add_google(font, font)
## https://github.com/tashapiro/tanya-data-viz/blob/main/chatgpt-lensa/chatgpt-lensa.R for twitter logo
sysfonts::font_add('fb', 'Font Awesome 6 Brands-Regular-400.otf')
showtext::showtext_auto()
plot_resolution <- 300
showtext::showtext_opts(dpi = plot_resolution)

theme_set(theme_minimal())
theme_update(
  text = element_text(family = font, color = 'white'),
  title = element_text(size = 14, color = 'white'),
  plot.title = element_text(face = 'bold', size = 20, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = element_text(size = 14, color = 'white'),
  plot.margin = margin(10, 20, 10, 20),
  plot.caption = ggtext::element_markdown(color = 'white', hjust = 0, size = 10, face = 'plain', lineheight = 1.1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 10, color = 'white', hjust = 1),
  plot.tag.position = c(0.99, 0.01)
)

## https://github.com/tashapiro/tanya-data-viz/blob/1dfad735bca1a7f335969f0eafc94cf971345075/nba-shot-chart/nba-shots.R#L64
tag_lab <- tagList(
  tags$span(HTML(enc2utf8("&#xf099;")), style='font-family:fb'),
  tags$span("@TonyElHabr"),
)

f_text <- partial(
  ggtext::geom_richtext,
  fill = NA,
  label.color = NA,
  family = font,
  fontface = 'bold',
  ...=
)

best_xi_plot <- best_xi |> 
  ggplot() +
  aes(x = x, y = y) +
  annotate_pitch(
    dimensions = ggsoccer::pitch_opta,
    fill = NA,
    colour = 'gray50',
    limits = FALSE
  ) +
  coord_flip(xlim = c(-1, 101), ylim = c(-1, 101)) +
  ggsoccer::theme_pitch(aspect_ratio = 1) +
  theme(legend.position = 'none') +
  f_text(
    data =  best_xi |> 
      filter(rn == 2L) |> 
      mutate(x = x - 3.5) |> 
      mutate(
        lab = sprintf('<span style="font-size:11px;color:#999999">%s (%s) </span><span style="font-size:9px;color:#999999">%.2f</span>', player_name, team_abbrv, vaep_atomic_p90)
      ),
    aes(label = lab)
  ) +
  f_text(
    data = best_xi |>
      filter(rn == 1L) |> 
      mutate(
        lab = sprintf('<span style="font-size:13px;color:white">%s (%s) </span><span style="font-size:11px;color:white">%.2f</span>', player_name, team_abbrv, vaep_atomic_p90)
      ), 
    aes(label = lab)
  ) +
  ggimage::geom_image(
    data = best_xi_rn1_with_logos,
    size = 0.08,
    aes(image = local_player_path)
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.background = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, color = NA)
  ) +
  labs(
    title = 'EPL Team of the Season',
    subtitle = 'Based on atomic VAEP per 90 minutes',
    caption = glue::glue(
      '
      **Data**: 2022/23 season, through May 8.
      <br/>**Criteria**: Minimum {min_minutes_played} minutes played.
      <br/>**VAEP**: Valuing Actions by Estimating Probabilities
      <br/>**VAEP paper DOI**: 10.1145/3292500.3330758
      '
    ),
    tag = tag_lab
  )

path_best_xi <- file.path(proj_dir, 'best_xi_vaep_atomic_p90.png')
size <- 7
w <- size * plot_resolution
h <- round(7 * 105 / 68 * plot_resolution)
ggsave(
  best_xi_plot,
  device = ragg::agg_png,
  res = plot_resolution,
  filename = path_best_xi,
  width = w,
  height = h,
  units = 'px'
)

## add background
path_background <- file.path(proj_dir, 'background.png')
pal <- c('black', '#1f1f1f')
g <- grid::rasterGrob(pal, width = unit(1, 'npc'), height = unit(1, 'npc'), interpolate = TRUE)
ggsave(
  plot = g, 
  filename = path_background,
  width = w,
  height = h,
  units = 'px'
)

main <- image_read(path_best_xi)
background <- image_read(path_background)
composite <- image_composite(
  background,
  main
)

path_best_xi_composite <- gsub('\\.png', '_w_background.png', path_best_xi)
image_write(composite, path_best_xi_composite)
file.remove(path_best_xi)
file.remove(path_background)

path_epl_logo <- file.path(proj_dir, 'epl-logo-white.png')
add_logo(
  path_viz = path_best_xi_composite,
  path_logo = path_epl_logo,
  logo_scale = 0.15,
  idx_x = 0.04,
  idx_y = 0.86,
  adjust_y = FALSE
)


## by-type ----
vaep_by_type <- all_vaep |> 
  filter_data() |> 
  group_by(competition_id, season_id, type_name, player_id, player_name) |> 
  summarize(
    n_type_actions_atomic = n(),
    vaep_atomic_type = sum(vaep_atomic)
  ) |> 
  ungroup() |> 
  arrange(desc(vaep_atomic_type))

# vaep_by_type |> 
#   group_by(type_name) |> 
#   summarize(
#     across(
#       vaep_atomic_type, 
#       sum
#     )
#   ) |> 
#   ungroup() |> 
#   arrange(desc(abs(vaep_atomic_type)))

top_vaep_by_type <- vaep_by_type |> 
  mutate(
    type_name = case_when(
      type_name %in% c('shot', 'dribble') ~ type_name,
      type_name %in% c('receival', 'pass') ~ 'pass or reception',
      TRUE ~ 'other'
    )
  ) |> 
  group_by(competition_id, season_id, type_name, player_id, player_name) |> 
  summarize(
    across(
      c(
        n_type_actions_atomic,
        vaep_atomic_type
      ),
      sum
    )
  ) |> 
  ungroup() |> 
  inner_join(
    vaep_by_player_season |> 
      filter(
        starting_position != 'GK'
      ) |> 
      select(
        competition_id, 
        season_id, 
        player_id, 
        player_name,
        team_name,
        vaep_atomic,
        vaep_atomic_p90
      ) |> 
      filter_data() |> 
      slice_max(
        vaep_atomic,
        n = 20
      ),
    by = join_by(competition_id, season_id, player_id, player_name)
  ) |> 
  inner_join(
    team_abbrvs,
    by = join_by(team_name)
  ) |> 
  mutate(
    player_lab = sprintf('<span style="font-size:13px;color:white">%s </span><span style="font-size:10px;color:#999999">(%s)</span>', player_name, team_abbrv)
  ) |> 
  mutate(
    across(
      player_lab,
      \(x) fct_reorder(x, vaep_atomic)
    ),
    across(
      type_name,
      \(x) factor(x, levels = rev(c('shot', 'pass or reception', 'dribble', 'other')))
    )
  )

type_pal <- c(
  'shot' = '#ef476f',
  'pass or reception' = '#ffd166',
  'dribble' = '#06d6a0',
  'other' = '#118ab2'
)

top_vaep_by_type_plot <- top_vaep_by_type |> 
  ggplot() +
  aes(
    x = vaep_atomic_type,
    y = player_lab
  ) +
  geom_col(
    aes(
      fill = type_name
    )
  ) +
  scale_fill_manual(
    values = type_pal
  ) +
  guides(
    fill = guide_legend(title = '', nrow = 1, reverse = TRUE)
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0),
    plot.background = element_rect(fill = '#1f1f1f', color = '#1f1f1f'),
    panel.background = element_rect(fill = '#1f1f1f', color = '#1f1f1f'),
    axis.title = element_text(color = 'white', size = 14, face = 'bold', hjust = 0.99),
    axis.line = element_blank(),
    axis.text = element_text(color = 'white', size = 12),
    axis.text.y = ggtext::element_markdown(),
    legend.text = element_text(color = 'white', size = 12),
    legend.position = 'top'
  ) +
  labs(
    title = "Players with most atomic VAEP",
    subtitle = 'EPL, 2022/23 season',
    caption = glue::glue(
      '
      **Data**: 2022/23 season, through May 8.
      <br/>**VAEP**: Valuing Actions by Estimating Probabilities
      <br/>**VAEP paper DOI**: 10.1145/3292500.3330758
      '
    ),
    tag = tag_lab,
    y = NULL,
    x = 'Atomic VAEP'
  )

path_top_vaep_by_type <- file.path(proj_dir, 'top_vaep_by_type_plot.png')
ggsave(
  top_vaep_by_type_plot,
  device = ragg::agg_png,
  res = plot_resolution,
  filename = path_top_vaep_by_type,
  width = 7 * plot_resolution,
  height = 7 * plot_resolution,
  units = 'px'
)

add_logo(
  path_viz = path_top_vaep_by_type,
  path_logo = path_epl_logo,
  logo_scale = 0.15,
  idx_x = 0.05,
  idx_y = 0.98,
  adjust_y = FALSE
)

