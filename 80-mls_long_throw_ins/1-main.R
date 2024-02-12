library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(tidyr)
library(tibble)

library(ggplot2)
library(ggtext)
library(glue)
library(scales)
library(magick)

library(fotmob)

PROJ_DIR <- '80-mls_long_throw_ins'

# https://theathletic.com/4297050/2023/03/11/get-it-launched-explaining-why-long-throw-ins-into-the-box-are-undervalued/
FONT <- 'Oswald'
GRAYISH_COLOR <- '#9f9f9f'
BLACKISH_COLOR <- '#15202B'
theme_asa <- function(...) {
  list(
    ...,
    theme_minimal(base_family = FONT),
    theme(
      text = element_text(color = 'white', size = 20),
      axis.text.y = element_text(color = 'white'),
      axis.text.x = element_text(color = 'white', size = 11),
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = 'top',
      plot.title = element_markdown(size = 20, hjust = 0),
      plot.title.position = 'plot',
      plot.caption.position = 'plot',
      plot.subtitle = element_text(color = 'white', size = 14),
      plot.caption = element_markdown(color = GRAYISH_COLOR, size = 11),
      panel.border = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = GRAYISH_COLOR),
      panel.background = element_rect(fill = BLACKISH_COLOR, color = BLACKISH_COLOR),
      plot.background = element_rect(fill = BLACKISH_COLOR, color = BLACKISH_COLOR)
    )
  )
}

setwd('../socceraction-streamlined')
source(file.path('R', 'helpers.R'))
games <- import_parquet('games')
players <- import_parquet('players')
teams <- import_parquet('teams')
actions <- import_parquet('actions')

x <- import_parquet(add_suffix('x', suffix = '')) |> 
  select(-matches('_a[1-2]$'))

xy <- import_xy(suffix = '', games = games)
setwd(file.path('../sports_viz'))

long_throw_ins <- xy |> 
  filter(type_throw_in_a0 == 1) |> 
  inner_join(
    actions
  ) |> 
  count(
    team_id, 
    player_id,
    attacking_throw_in = start_x_a0 >= (3 * 105 / 4),
    thrown_into_box = (end_x_a0 >= (105 - 16.5)) & (end_y_a0 >= (34 - 20.15) & end_y_a0 <= (34 + 20.15))
  )

n_games <- xy |> 
  distinct(team_id, game_id) |> 
  count(team_id, name = 'n_games')

prop_thrown_into_box <- long_throw_ins |> 
  filter(
    attacking_throw_in 
  ) |> 
  group_by(team_id) |> 
  mutate(
    total = sum(n),
    prop_thrown_into_box = n / sum(n)
  ) |> 
  ungroup() |> 
  filter(thrown_into_box) |> 
  select(
    team_id,
    prop_thrown_into_box,
    n_attacking_throw_ins = total,
    n_attacking_throw_ins_into_box = n
  )

## Debug by looking at player-level
prop_thrown_into_box_by_player <- long_throw_ins |> 
  filter(
    attacking_throw_in 
  ) |> 
  group_by(team_id, player_id) |> 
  mutate(
    total = sum(n),
    prop_thrown_into_box = n / sum(n)
  ) |> 
  ungroup() |> 
  filter(thrown_into_box) |> 
  select(
    team_id,
    player_id,
    prop_thrown_into_box,
    n_attacking_throw_ins = total,
    n_attacking_throw_ins_into_box = n
  ) |> 
  inner_join(
    teams |> select(team_id, team_name),
    by = join_by(team_id)
  ) |> 
  inner_join(
    players |>
      ## There are sometimes dup player names where one name has an accent and another doesn't
      group_by(team_id, player_id) |> 
      mutate(n = n()) |> 
      slice_max(n, n = 1, with_ties = FALSE) |> 
      ungroup() |> 
      select(team_id, player_id, player_name),
    by = join_by(team_id, player_id)
  ) |> 
  arrange(desc(n_attacking_throw_ins))


prop_thrown_into_box_by_player |> 
  filter(n_attacking_throw_ins >= 20) |> 
  arrange(desc(prop_thrown_into_box)) |> 
  slice_max(prop_thrown_into_box, n = 20, with_ties = FALSE) |> 
  select(
    team_name, 
    player_name, 
    n_attacking_throw_ins, 
    n_attacking_throw_ins_into_box, 
    prop_thrown_into_box
  ) |> 
  knitr::kable(digits = 2)

table <- fotmob::fotmob_get_league_tables(league_id = 130, season = '2023') |> 
  filter(table_type == 'all')

team_logos <- table |> 
  distinct(
    fotmob_team_name = table_short_name,
    logo = sprintf('https://images.fotmob.com/image_resources/logo/teamlogo/%s.png', table_id)
  ) |> 
  arrange(fotmob_team_name) |> 
  bind_cols(
    teams |> 
      mutate(
        team_name = case_match(
          team_name,
          'Kansas City' ~ 'Sporting KC',
          .default = team_name
        )
      ) |> 
      arrange(team_name) |> 
      select(team_id, team_name)
  )

df <- prop_thrown_into_box |> 
  inner_join(n_games, by = join_by(team_id)) |> 
  inner_join(team_logos, by = join_by(team_id)) |> 
  arrange(desc(prop_thrown_into_box)) |> 
  mutate(
    n_attacking_throw_ins_per_game = n_attacking_throw_ins / n_games,
    n_attacking_throw_ins_per_game_into_box = prop_thrown_into_box * n_attacking_throw_ins_per_game,
    n_attacking_throw_ins_per_game_not_into_box = n_attacking_throw_ins_per_game - n_attacking_throw_ins_per_game_into_box,
    fotmob_team_name = fct_reorder(fotmob_team_name, n_attacking_throw_ins_per_game)
  )

long <- df |> 
  select(
    fotmob_team_name,
    logo,
    n_attacking_throw_ins_per_game_into_box,
    n_attacking_throw_ins_per_game_not_into_box
  ) |> 
  pivot_longer(
    starts_with('n_'),
    names_to = 'stat',
    values_to = 'value'
  ) |> 
  mutate(
    across(
      stat,
      \(.x) 
      factor(
        .x, 
        levels = c(
          'n_attacking_throw_ins_per_game_not_into_box', 
          'n_attacking_throw_ins_per_game_into_box'
        )
      )
    )
  )

PAL <- c(
  'n_attacking_throw_ins_per_game_into_box' = '#01C4E7',
  'n_attacking_throw_ins_per_game_not_into_box' = '#9f9f9f'
)

long_throw_ins_plot <- long |> 
  ggplot() +
  aes(
    x = value,
    y = fotmob_team_name
  ) +
  geom_col(
    aes(
      fill = stat
    ),
    show.legend = FALSE
  ) +
  geom_text(
    data = df |> filter(prop_thrown_into_box >= 0.12),
    aes(
      x = n_attacking_throw_ins_per_game_into_box,
      label = percent(prop_thrown_into_box, accuracy = 1)
    ),
    color = 'white',
    family = FONT,
    hjust = 1.1
  ) +
  scale_fill_manual(
    values = PAL
  ) +
  scale_x_continuous(
    limits = c(0, 6)
  ) +
  theme_asa() +
  theme(
    axis.text.y = element_markdown(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = glue('Attacking throw-ins <span style="color:{PAL["n_attacking_throw_ins_per_game_into_box"]}">into the box</span> and <span style="color:{PAL["n_attacking_throw_ins_per_game_not_into_box"]}">outside the box</span>'),
    subtitle = 'MLS, 2023',
    y = NULL,
    caption = 'Attacking throw: any throw-in taken in the final quarter of the pitch.<br/>Inspiration: The Athletic',
    x = '# of attacking throw-ins per game'
  ) +
  scale_y_discrete(
    name = '',
    labels = df |>
      mutate(
        label = glue("<span style='font-size:14px;color:white'>{fotmob_team_name}</span>  <img src='{logo}' width='13' height='13'/>")
      ) |> 
      distinct(fotmob_team_name, label) |>
      deframe()
  )

long_throw_ins_plot_path <- file.path(PROJ_DIR, 'long-throw-ins-mls-2023.png')
ggsave(
  long_throw_ins_plot,
  filename = long_throw_ins_plot_path,
  units = 'in',
  width = 6,
  height = 8
)

## https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/
add_logo <- function(
    plot_path,
    logo_path,
    logo_scale = 0.1,
    idx_x = 0.01, ## right-hand side
    idx_y = 0.99, ## top of plot
    adjust_x = ifelse(idx_x < 0.5, TRUE, FALSE),
    adjust_y = ifelse(idx_y < 0.5, TRUE, FALSE)
) {
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  logo <- magick::image_scale(
    logo_raw,
    as.character(round(plot_width * logo_scale))
  )
  
  info <- magick::image_info(logo)
  logo_width <- info$width
  logo_height <- info$height
  
  x_pos <- plot_width - idx_x * plot_width
  y_pos <- plot_height - idx_y * plot_height
  
  if (isTRUE(adjust_x)) {
    x_pos <- x_pos - logo_width
  }
  
  if (isTRUE(adjust_y)) {
    y_pos <- y_pos - logo_height
  }
  
  offset <- paste0('+', x_pos, '+', y_pos)
  
  new_plot <- magick::image_composite(plot, logo, offset = offset)
  ext <- tools::file_ext(plot_path)
  rgx_ext <- sprintf('[.]%s$', ext)
  
  magick::image_write(
    new_plot,
    plot_path
  )
}

asa_logo_path <- file.path(PROJ_DIR, 'ASAlogo.png')
add_logo(
  long_throw_ins_plot_path,
  asa_logo_path,
  logo_scale = 0.3,
  idx_x = 0.98,
  idx_y = 0.07,
  adjust_x = FALSE,
  adjust_y = FALSE
)
