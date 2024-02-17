# https://github.com/etmckinley/good-lucky-matrix/blob/main/good-lucky%20matrix.qmd
library(itscalledsoccer)
library(readr)
library(dplyr)
library(ggplot2)
library(ggtext)
library(ggimage)
library(tibble)
library(cowplot)

PROJ_DIR <- '81-2023_mls_g_minus_xg'

FONT <- 'Oswald'
GRAYISH_COLOR <- '#9f9f9f'
BLACKISH_COLOR <- '#15202B'
theme_asa <- function(...) {
  list(
    ...,
    theme_minimal(base_family = FONT),
    theme(
      text = element_text(color = 'white', size = 20),
      axis.text.y = element_text(color = 'white', size = 12),
      axis.text.x = element_text(color = 'white', size = 12),
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = 'top',
      plot.title = element_markdown(size = 20, hjust = 0),
      plot.title.position = 'plot',
      plot.caption.position = 'plot',
      plot.subtitle = element_text(color = 'white', size = 14),
      plot.caption = element_markdown(color = GRAYISH_COLOR, size = 11),
      panel.border = element_blank(),
      panel.grid.major.y = element_line(color = GRAYISH_COLOR, linetype = 'dashed', linewidth = 0.2),
      panel.grid.major.x = element_line(color = GRAYISH_COLOR, linetype = 'dashed', linewidth = 0.2),
      panel.background = element_rect(fill = BLACKISH_COLOR, color = BLACKISH_COLOR),
      plot.background = element_rect(fill = BLACKISH_COLOR, color = BLACKISH_COLOR)
    )
  )
}

client <- AmericanSoccerAnalysis$new()

# https://github.com/etmckinley/good-lucky-matrix
team_info <- read_csv(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vTiZfW7pSUWPttpHSMlAwgMyXwdAeLAW6HuoHwZa69FrNpfzqVkM_0DaeAveTG7hvbCSK-HBh31QxIM/pub?gid=95813594&single=true&output=csv'
)

xg_by_team <- client$get_team_xgoals(
  leagues = 'mls',
  season_name = 2023,
  stage_name = 'Regular Season'
) 

xg_by_team_ranks <- xg_by_team |>
  inner_join(
    team_info |> 
      select(
        team_id = team_hashid,
        team = team_short_name
      ),
    by = join_by(team_id)
  ) |> 
  as_tibble() |> 
  transmute(
    team,
    logo = paste0(
      'https://american-soccer-analysis-headshots.s3.amazonaws.com/club_logos/',
      team_id,
      '.png'
    ),
    g_minus_xg = goals_for - xgoals_for,
    g_minus_xg_conceded = goals_against - xgoals_against,
    offense_overperformance_rank = row_number(desc(g_minus_xg)),
    defense_underperformance_rank = row_number(desc(g_minus_xg_conceded))
  ) |> 
  arrange(offense_overperformance_rank)

# pmax(max(abs(xg_by_team_ranks$g_minus_xg)), max(abs(xg_by_team_ranks$g_minus_xg_conceded)))
max_value <- 19
xlims <- c(-max_value, max_value)
ylims = c(-max_value, max_value)

# create data frame with quadrant labels
quadrant_labels <- tibble(
  label = c(
    'Offense Over-performed &\nDefense Under-performed',
    'Offense Under-performed &\nDefense Under-performed',
    'Offense Over-performed &\nDefense Over-performed',
    'Offense Under-performed &\nDefense Over-performed'
  ),
  x = c(xlims[2], xlims[1], xlims[2], xlims[1]),
  y = c(ylims[2], ylims[2], ylims[1], ylims[1])
)

p <- xg_by_team_ranks |> 
  ggplot() +
  aes(
    x = g_minus_xg,
    y = g_minus_xg_conceded
  ) +
  geom_text(
    data = quadrant_labels,
    aes(
      x = x,
      y = y,
      label = label
    ),
    family = 'Oswald',
    # fontface = 'bold',
    size = 14 / .pt,
    color = GRAYISH_COLOR, #  'white',
    alpha = 1, # 0.7,
    hjust = 'inward',
    vjust = 'inward'
  ) +
  geom_vline(
    data = data.frame(),
    aes(
      xintercept = 0
    ),
    color = 'white',
    linewidth = 1
  ) +
  geom_hline(
    data = data.frame(),
    aes(
      yintercept = 0
    ),
    color = 'white',
    linewidth = 1
  ) +
  theme_asa() +
  labs(
    title = 'Actual vs. Expected Goal Scoring and Conceding Performance',
    subtitle = 'MLS, 2023',
    x = 'Goals - xG',
    y = 'Goals Conceded - xG Conceded',
    caption = 'Data: American Soccer Analysis'
  )

p2 <- p +
  ggimage::geom_image(
    aes(
      image = logo
    ),
    size = 0.08,
    asp = 1
  )

p3 <- p2 +
  ggforce::geom_mark_circle(
    data = xg_by_team_ranks |> 
      filter(team == 'Atlanta United') |> 
      mutate(
        label = 'Offense outperformed expectations in scoring, ranking above all teams in `Goals - xG`. Defense underperformed, allowing more goals than expected, worse than all but five teams.'
      ),
    aes(
      description = label,
      group = label
    ),
    expand = unit(0.01, 'mm'),
    con.type = 'straight',
    con.colour = GRAYISH_COLOR,
    con.cap = unit(8, 'mm'),
    label.buffer = unit(10, 'mm'),
    label.margin = margin(1, 1, 1, 1, 'mm'),
    label.lineheight = 0.9,
    label.family = 'Oswald',
    label.fontsize = 9,
    label.colour = 'white',
    label.fill = 'transparent',
    show.legend = FALSE
  )

# https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/
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

plot_path <- file.path(PROJ_DIR, '2023_mls_g_minus_xg.png')
asa_logo_path <- file.path(PROJ_DIR, 'ASALogo.png')
ggsave(
  p3,
  filename = plot_path,
  width = 7,
  height = 7
)

add_logo(
  plot_path,
  asa_logo_path,
  logo_scale = 0.18,
  idx_x = 0.98,
  idx_y = 0.01,
  adjust_x = FALSE,
  adjust_y = TRUE
)

