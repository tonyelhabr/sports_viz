library(dplyr)
library(tidyr)
library(qs)

## plotting
library(ggplot2)
library(sysfonts)
library(showtext)
library(ggtext)
library(htmltools)
# library(ggpol)
library(scales)
library(glue)

PROJ_DIR <- '82-points_lost_from_winning_positions'

TAG_LABEL <- htmltools::tagList(
  htmltools::tags$span(htmltools::HTML(enc2utf8('&#xf099;')), style = 'font-family:fb'),
  htmltools::tags$span('@TonyElHabr'),
)
PLOT_RESOLUTION <- 300
WHITISH_FOREGROUND_COLOR <- 'white'
COMPLEMENTARY_FOREGROUND_COLOR <- '#cbcbcb'
BLACKISH_BACKGROUND_COLOR <- '#1c1c1c'
COMPLEMENTARY_BACKGROUND_COLOR <- '#4d4d4d'
FONT <- 'Titillium Web'
sysfonts::font_add_google(FONT, FONT)
sysfonts::font_add('fb', 'Font Awesome 6 Brands-Regular-400.otf')
# sysfonts::font_add('fa-brands', 'Font Awesome 6 Brands-Regular-400.otf')
# sysfonts::font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')
showtext::showtext_auto()
showtext::showtext_opts(dpi = PLOT_RESOLUTION)

ggplot2::theme_set(ggplot2::theme_minimal())
ggplot2::theme_update(
  text = ggplot2::element_text(family = FONT),
  title = ggplot2::element_text(size = 16, color = WHITISH_FOREGROUND_COLOR),
  plot.title = ggtext::element_markdown(face = 'bold', size = 32, color = WHITISH_FOREGROUND_COLOR),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown(size = 11, color = COMPLEMENTARY_FOREGROUND_COLOR),
  axis.text = ggplot2::element_text(color = WHITISH_FOREGROUND_COLOR, size = 14),
  axis.title.x = ggtext::element_markdown(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.title.y = ggtext::element_markdown(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.line = ggplot2::element_blank(),
  panel.grid.major = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor.x = ggplot2::element_blank(),
  panel.grid.minor.y = ggplot2::element_blank(),
  plot.margin = ggplot2::margin(10, 20, 30, 20),
  plot.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR),
  plot.caption = ggtext::element_markdown(size = 12, color = WHITISH_FOREGROUND_COLOR, hjust = 0, face = 'plain'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 12, color = WHITISH_FOREGROUND_COLOR, hjust = 1),
  plot.tag.position = c(0.99, 0.01),
  panel.spacing.x = grid::unit(2, 'lines'),
  panel.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR)
)
ggplot2::update_geom_defaults('text', list(color = WHITISH_FOREGROUND_COLOR, size = 14 / .pt))

points_won_and_lost <- qs::qread(file.path(PROJ_DIR, 'points-won-and-lost.qs'))

get_fotmob_standings <- function() {
  url <- 'https://www.fotmob.com/api/leagues?id=47'
  resp <- httr::GET(url)
  cont <- httr::content(resp, as = 'text')
  result <- jsonlite::fromJSON(cont)
  table_init <- result$table$data
  tables <- dplyr::bind_rows(table_init$table)
  tables$all[[1]] |> 
    dplyr::transmute(
      team = name,
      team_id = id,
      pts,
      logo_url = sprintf('https://images.fotmob.com/image_resources/logo/teamlogo/%s.png', team_id)
    )
}

fotmob_standings <- get_fotmob_standings()

team_names <- points_won_and_lost |> 
  dplyr::distinct(old_team = team) |> 
  dplyr::arrange(old_team) |> 
  dplyr::mutate(
    team = dplyr::case_match(
      old_team,
      'Brighton & Hove Albion' ~ 'Brighton',
      'Wolverhampton Wanderers' ~ 'Wolves',
      'Tottenham Hotspur' ~ 'Tottenham',
      'West Ham United' ~ 'West Ham',
      .default = old_team
    )
  )

team_logos <- team_names |> 
  dplyr::bind_cols(
    fotmob_standings |> 
      dplyr::transmute(
        team_fotmob = gsub(
          'AFC Bournemouth',
          'Bournemouth',
          team
        ),
        path = logo_url,
        ## Account for me doing this after the first game in 2/17.
        pts = ifelse(team == 'Liverpool', pts - 3, pts),
        rn = dplyr::row_number()
      ) |> 
      dplyr::arrange(team_fotmob)
  ) |> 
  dplyr::arrange(rn)

agg_points_won_and_lost <- points_won_and_lost |> 
  group_by(team) |> 
  summarize(
    across(
      c(
        points_won,
        points_lost
      ),
      sum
    )
  ) |> 
  arrange(desc(points_won + points_lost))

BUFFER <- 20
long_agg_points_won_and_lost <- agg_points_won_and_lost |> 
  select(
    team,
    `Points Won` = points_won,
    `Points Lost` = points_lost
  ) |> 
  pivot_longer(
    c(`Points Won`, `Points Lost`),
    names_to = 'outcome',
    values_to = 'points'
  ) |> 
  dplyr::rename(old_team = team) |> 
  dplyr::left_join(
    team_names,
    by = dplyr::join_by(old_team)
  ) |> 
  dplyr::inner_join(
    team_logos |> 
      dplyr::select(
        team,
        pts,
        path
      ),
    by = dplyr::join_by(team)
  ) |> 
  dplyr::mutate(
    team_color = ifelse(team == 'Tottenham', '#FFFF00', WHITISH_FOREGROUND_COLOR),
    label = glue::glue("<img src='{path}' width='10' height='10'/> {ifelse(team == 'Tottenham', '<b>', '')}<span style='font-size:18px;color:{team_color}'>{paste0('   ', team)}</span>{ifelse(team == 'Tottenham', '</b>', '')} <span style='font-size:12px;color:{COMPLEMENTARY_FOREGROUND_COLOR}'>{pts} pt{ifelse(pts > 1, 's', '')}</span>")
  ) |> 
  dplyr::select(-path) |> 
  mutate(
    points0 = ifelse(outcome == 'Points Lost', -BUFFER, BUFFER),
    points_buffered = ifelse(outcome == 'Points Lost', -points - BUFFER, BUFFER + points),
    across(team, \(.x) factor(.x, rev(team_logos$team))),
    team_idx = as.integer(team)
  )

RANGE <- 10 * ceiling(max(abs(long_agg_points_won_and_lost$points)) / 10)

# source(file.path(PROJ_DIR, 'facet_share.R'))
long_agg_points_won_and_lost |> 
  ggplot() +
  aes(
    xmin = points0,
    xmax = points_buffered,
    ymin = team_idx - 0.45,
    ymax = team_idx + 0.45
  ) +
  geom_rect(
    aes(
      fill = outcome
    ),
    show.legend = FALSE
  ) +
  geom_text(
    data = long_agg_points_won_and_lost |> 
      dplyr::filter(outcome == 'Points Lost'),
    hjust = 1.25,
    aes(
      x = points_buffered,
      y = team_idx,
      label = points
    )
  ) +
  geom_text(
    data = long_agg_points_won_and_lost |> 
      dplyr::filter(outcome == 'Points Won'),
    hjust = -0.25,
    aes(
      x = points_buffered,
      y = team_idx,
      label = points
    )
  ) +
  scale_fill_manual(
    values = c(
      'Points Lost' = '#f15bb5',
      'Points Won' = '#00bbf9'
    )
  ) +
  scale_x_continuous(
    breaks = c(-RANGE - BUFFER, -BUFFER, BUFFER, BUFFER + RANGE),
    labels = c(RANGE, 0, 0, RANGE),
    limits = c(-RANGE - BUFFER, BUFFER + RANGE)
  ) +
  geom_text(
    inherit.aes = FALSE,
    data = tibble(x = -BUFFER, y = -0.5, label = 'Points Lost'),
    size = 16 / .pt,
    family = FONT,
    fontface = 'bold',
    aes(
      x = x,
      y = y,
      label = label
    ),
    hjust = 1.05
  ) +
  geom_text(
    inherit.aes = FALSE,
    data = tibble(x = BUFFER, y = -0.5, label = 'Points Won'),
    size = 16 / .pt,
    family = FONT,
    fontface = 'bold',
    aes(
      x = x,
      y = y,
      label = label
    ),
    hjust = -0.05
  ) +
  ggtext::geom_richtext(
    inherit.aes = FALSE,
    data = long_agg_points_won_and_lost,
    family = FONT,
    # size = 12 / .pt,
    size = 14 / .pt,
    aes(
      y = team_idx,
      x = 0,
      label = label
    ),
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), 'pt') # remove padding
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(
    title = 'Points Won and Lost, 2023/24 EPL',
    subtitle = glue::glue('A team "wins points" if they trailed in a match at some point and came back to win (+3) or draw (+1).<br/>Likewise, a team "loses points" if they led in a match, but then ended up losing (-3) or drawing (-2).<br/>Strong teams tend to hold on their leads, so they tend to have more points won than lost.<br/><b><span style="color:#FFFF00">Tottenham</span></b> is an outlier, as they\'re currently sit at fourth in the table, but have the 3rd most points lost<br/>and 3rd most points won, indicating that they have won many games where they went down.'),
    tag = TAG_LABEL,
    caption = '**Data**: Updated on 2023-02-16.',
    x = NULL,
    y = NULL
  )

plot_path <- file.path(PROJ_DIR, '2024-epl-points-won-and-lost.png')
ggsave(
  filename = plot_path,
  width = 8,
  height = 8
)

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

logo_path <- file.path(PROJ_DIR, 'epl-logo-white.png')
add_logo(
  plot_path,
  logo_path,
  logo_scale = 0.1,
  idx_x = 0.01,
  idx_y = 0.98,
  adjust_x = TRUE,
  adjust_y = FALSE
)

