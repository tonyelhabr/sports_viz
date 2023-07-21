library(qs)
library(dplyr)
library(forcats)
library(purrr)

library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtext)
library(ragg)
library(htmltools)
library(ggimage)
library(magick)

proj_dir <- '72-2023_womens_world_cup'
data_dir <- file.path(proj_dir, 'data')
img_dir <- file.path(proj_dir, 'img')
dir.create(img_dir, showWarnings = FALSE, recursive = FALSE)
tourney_logo_path <- file.path(proj_dir, '2023-fifa-womens-world-cup-logo.jpg')

font <- 'Titillium Web'
sysfonts::font_add_google(font, font)
## https://github.com/tashapiro/tanya-data-viz/blob/main/chatgpt-lensa/chatgpt-lensa.R for twitter logo
sysfonts::font_add('fb', 'Font Awesome 6 Brands-Regular-400.otf')
showtext::showtext_auto()
plot_resolution <- 300
showtext::showtext_opts(dpi = plot_resolution)

blackish_background <- '#1f1f1f'
theme_set(theme_minimal())
theme_update(
  text = element_text(family = font, color = 'white'),
  title = element_text(size = 14, color = 'white'),
  plot.title = element_text(face = 'bold', size = 16, color = 'white', hjust = 0),
  plot.title.position = 'plot',
  plot.subtitle = element_text(size = 14, color = 'white', hjust = 0),
  plot.margin = margin(10, 20, 10, 20),
  plot.caption = ggtext::element_markdown(color = 'white', hjust = 0, size = 10, face = 'plain', lineheight = 1.1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 10, color = 'white', hjust = 1),
  plot.tag.position = c(0.99, 0.01),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  plot.background = element_rect(fill = blackish_background, color = blackish_background),
  panel.background = element_rect(fill = blackish_background, color = blackish_background),
  axis.title = element_text(color = 'white', size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  axis.text = element_text(color = 'white', size = 12),
  axis.text.y = ggtext::element_markdown(),
  legend.text = element_text(color = 'white', size = 12),
  legend.position = 'top'
)

## https://github.com/tashapiro/tanya-data-viz/blob/1dfad735bca1a7f335969f0eafc94cf971345075/nba-shot-chart/nba-shots.R#L64
tag_lab <- tagList(
  tags$span(HTML(enc2utf8("&#xf099;")), style='font-family:fb'),
  tags$span("@TonyElHabr"),
)

min_minutes_played <- 0
filt_player_stats_p90 <- read_rds(file.path(data_dir, 'player_stats_p90.rds')) |> 
  filter(time_played >= min_minutes_played) 

download_player_picture <- function(url, player_id) {
  path <- file.path(img_dir, sprintf('%s.png', player_id))
  if (file.exists(path)) {
    return(path)
  }
  download.file(url, destfile = path, mode = 'wb')
  path
}

generate_player_lab <- function(player_name, country) {
  sprintf('<span style="font-size:16px;color:white"><b>%s</b></span><br/><span style="font-size:12px;color:#ffffff">%s</span>', player_name, country)
}

plot_top_10_for_stat <- function(
    col, 
    bar_color = '#ffffff', 
    n_digits = 0, 
    title = 'stat', 
    player_lab_x_offset = 1
) {
  col_sym <- ensym(col)
  top_players <- filt_player_stats_p90 |> 
    slice_max(!!col_sym, n = 10) |> 
    mutate(
      local_player_picture_url = map2(
        player_picture_url,
        player_id,
        download_player_picture
      ),
      player_lab = generate_player_lab(player_name, country),
      across(
        player_lab,
        \(x) fct_reorder(x, !!col_sym)
      )
    )
  
  p <- top_players |> 
    ggplot() +
    aes(
      x = !!col_sym,
      y = player_lab
    ) +
    geom_col(
      fill = bar_color,
      width = 0.9
    ) +
    theme(
      axis.text.y = element_blank()
    ) +
    geom_richtext(
      hjust = 0,
      vjust = 0.5,
      fill = NA,
      family = font,
      label.colour = NA,
      lineheight = 0.8,
      aes(
        x = player_lab_x_offset,
        label = player_lab
      )
    ) +
    geom_text(
      hjust = 1.5,
      vjust = 0.5,
      family = font,
      fontface = 'bold',
      color = 'white',
      size = 14 / .pt,
      aes(
        label = sprintf(paste0('%.', n_digits, 'f'), !!col_sym)
      )
    ) +
    ## passing in more than 1 row to geom_image isn't working for some reason...
    map(
      top_players |>
        group_split(row_number()),
      ~{
        ggimage::geom_image(
          data = .x,
          size = 0.055,
          hjust = 0,
          # vjust = 0.5,
          aes(
            x = 0.5,
            image = local_player_picture_url
          )
        )
      }
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    labs(
      y = NULL,
      x = NULL,
      title = title,
      subtitle = 'FIFA Women\'s World Cup 2023',
      # caption = sprintf('**Source**: FIFA. Data through July 21. Minimum %s minutes played.', min_minutes_played),
      caption = '**Source**: FIFA. Data through July 21.',
      tag = tag_lab
    )
  
  plot_path <- file.path(proj_dir, sprintf('top_%s.png', col))
  ggsave(
    p,
    filename = plot_path,
    width = 7,
    height = 7
  )
  
  add_logo(
    plot_path,
    tourney_logo_path,
    delete = FALSE, 
    path_suffix = '',
    logo_scale = 0.15,
    idx_x = 0.02,
    idx_y = 0.98,
    adjust_y = FALSE
  )
  
  invisible(p)
}

plot_top_10_for_stat(
  'distance_high_speed_sprinting',
  bar_color = '#f27e87',
  n_digits = 0,
  player_lab_x_offset = 30,
  title = 'Most distance (m) covered while high speed sprinting'
)


plot_top_10_for_stat(
  'sprints',
  bar_color = '#13a051',
  n_digits = 0,
  player_lab_x_offset = 6,
  title = 'Most sprints'
)
