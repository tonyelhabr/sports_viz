library(dplyr)
library(forcats)

library(qs)

## plotting
library(ggplot2)
library(sysfonts)
library(showtext)
library(ggtext)
library(htmltools)
library(scales)
library(glue)

PROJ_DIR <- '84-2024_copa_reactions'

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
  plot.title = ggtext::element_markdown(face = 'bold', size = 18, color = WHITISH_FOREGROUND_COLOR),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown(size = 12, color = COMPLEMENTARY_FOREGROUND_COLOR),
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
  plot.caption = ggtext::element_markdown(size = 11, color = WHITISH_FOREGROUND_COLOR, hjust = 0, face = 'plain'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 12, color = WHITISH_FOREGROUND_COLOR, hjust = 1),
  plot.tag.position = c(0.99, 0.01),
  panel.spacing.x = grid::unit(2, 'lines'),
  panel.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = WHITISH_FOREGROUND_COLOR),
  strip.text = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0),
)
ggplot2::update_geom_defaults('text', list(color = WHITISH_FOREGROUND_COLOR, size = 14 / .pt))

PALETTE <- c(
  'Copa America 2024' = '#42bfdd',
  'EURO 2024' = '#907ad6'
)

## main ----
generate_fotmob_team_logo_url <- function(team_id) {
  sprintf('https://images.fotmob.com/image_resources/logo/teamlogo/%s.png', team_id)
}

generate_label <- function(home_team_id, away_team_id, regular_score_line, pen_score_line, competition_name) {
  bar_color <-  PALETTE[competition_name]
  bar_color_span <- glue::glue('<span style="color:{bar_color}">')
  pen_line <- ifelse(!is.na(pen_score_line), paste0(bar_color_span, '..</span><span style="font-size:10px">(', pen_score_line, ')</span>'), '')
  glue::glue("<img src='{generate_fotmob_team_logo_url(home_team_id)}' width='10' height='10'/><span style='font-size:16px;color:#1c1c1c'>{bar_color_span}..</span>{regular_score_line}{pen_line}{bar_color_span}..</span><img src='{generate_fotmob_team_logo_url(away_team_id)}' width='10' height='10'/>")
}

match_passes <- qs::qread(file.path(PROJ_DIR, 'match_passes.qs')) |> 
  dplyr::mutate(
    passes_per_minute_index = dplyr::row_number(dplyr::desc(passes_per_minute)),
    passes_per_minute_index_inv = dplyr::row_number(passes_per_minute)
  )

MAX_RANK <- 20
GROUPS <- c(
  sprintf('Top %s Matches', MAX_RANK),
  sprintf('Bottom %s Matches', MAX_RANK)
)

filt_match_passes <- match_passes |> 
  dplyr::filter(
    passes_per_minute_index <= MAX_RANK | passes_per_minute_index_inv <= MAX_RANK
  ) |> 
  dplyr::mutate(
    ## smart way to order labels such that highest and lowest are at the top of each facet
    order_index = pmin(passes_per_minute_index, passes_per_minute_index_inv) + runif(dplyr::n(), -0.49, 0.49),
    match_label = forcats::fct_reorder(
      generate_label(
        home_team_id, 
        away_team_id, 
        regular_score_line,
        pen_score_line,
        competition_name
      ), 
      -order_index
    ),
    group = factor(
      ifelse(
        passes_per_minute_index <= MAX_RANK,
        GROUPS[1],
        GROUPS[2]
      ),
      levels = GROUPS
    )
  )

filt_match_passes |> 
  ggplot2::ggplot() +
  ggplot2::aes(
    x = passes_per_minute,
    y = match_label
  ) +
  ggplot2::geom_col(
    ggplot2::aes(fill = competition_name)
  ) +
  ggplot2::scale_fill_manual(
    values = PALETTE
  ) +
  ggplot2::guides(
    fill = 'none'
  ) +
  ggtext::geom_richtext(
    hjust = 0,
    vjust = 0.5,
    fill = NA,
    family = FONT,
    label.colour = NA,
    # lineheight = 0.8,
    ggplot2::aes(
      x = 0.1,
      label = match_label
    )
  ) +
  ggplot2::geom_text(
    hjust = 1.25,
    vjust = 0.5,
    family = FONT,
    fontface = 'bold',
    color = 'white',
    size = 14 / .pt,
    ggplot2::aes(
      label = sprintf(paste0('%.', 1, 'f'), passes_per_minute)
    )
  ) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank()
  ) +
  ggplot2::facet_wrap(
    ~group,
    ncol = 2,
    scales = 'free_y'
  ) +
  ggplot2::labs(
    title = 'Combined Passes per Minute',
    subtitle = glue::glue('The pacing of <b><span style="color:{PALETTE["EURO 2024"]}">EURO 2024</span></b> and <b><span style="color:{PALETTE["Copa America 2024"]}">Copa America 2024</span></b> matches has been completely different.'),
    caption = '<i>Total of 76 matches played among the two tournaments through 2024-07-06.<br/>Added and extra time are accounted for.</i>',
    tag = TAG_LABEL,
    y = NULL,
    x = NULL
  )

ggplot2::ggsave(
  filename = file.path(PROJ_DIR, 'copa-euro-2024-passes.png'),
  width = 7.5,
  height = 7.5
)
