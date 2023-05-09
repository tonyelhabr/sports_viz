library(arrow)
library(httr)
library(readr)

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)

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

players_season_games <- read_parquet(file.path(input_data_dir, 'players_season_games.parquet'))
vaep_by_player_season <- read_parquet(file.path(input_data_dir, 'vaep_by_player_season.parquet'))
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
  'Center Defender' = c('DML', 'DC', 'DMC', 'DMR'),
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

min_minutes_played <- 2000L
latest_vaep_by_players_positions <- vaep_by_player_season |> 
  filter(
    competition_id == 8L,
    season_id == 2023L,
    minutes_played >= min_minutes_played
  ) |> 
  left_join(
    position_343_mapping,
    ## intentional mulit-join for FW / M (due to AMx being in multiple groups)
    by = join_by(starting_position == opta_position),
    relationship = 'many-to-many'
  ) |> 
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
  text = element_text(family = font),
  title = element_text(size = 14, color = 'black'),
  plot.title = element_text(face = 'bold', size = 20, color = 'black', hjust = 0.5),
  plot.title.position = 'plot',
  plot.subtitle = element_text(size = 14, color = 'black', hjust = 0.5),
  axis.text = element_text(size = 14),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  plot.margin = margin(10, 20, 10, 20),
  plot.background = element_rect(fill = NA, color = NA),
  plot.caption = ggtext::element_markdown(color = 'black', hjust = 0, size = 10, face = 'plain', lineheight = 1.1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 10, color = 'black', hjust = 1),
  plot.tag.position = c(0.98, 0.01),
  panel.spacing.x = unit(2, 'lines'),
  panel.background = element_rect(fill = NA, color = NA)
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
        lab = sprintf('<span style="font-size:12px;color:black">%s (%s) </span><span style="font-size:10px;color:black">%.2f</span>', player_name, team_abbrv, vaep_atomic_p90)
      ),
    aes(label = lab)
  ) +
  f_text(
    data = best_xi |>
      filter(rn == 1L) |> 
      mutate(
        lab = sprintf('<span style="font-size:14px;color:white">%s (%s) </span><span style="font-size:12px;color:white">%.2f</span>', player_name, team_abbrv, vaep_atomic_p90)
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
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    title = 'Premier League Team of the Season',
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
pal <- c('#52B788', '#74C69D') ## https://coolors.co/palette/d8f3dc-b7e4c7-95d5b2-74c69d-52b788-40916c-2d6a4f-1b4332-081c15
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
image_write(
  composite, 
  gsub('\\.png', '_w_background.png', path_best_xi)
)

