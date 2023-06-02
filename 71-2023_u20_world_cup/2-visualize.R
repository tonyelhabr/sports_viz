library(qs)
library(dplyr)
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
# library(cropcircles)

proj_dir <- '71-2023_u20_world_cup'
img_dir <- file.path(proj_dir, 'img')
dir.create(img_dir, showWarnings = FALSE, recursive = FALSE)

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
  plot.title = element_text(face = 'bold', size = 20, color = 'white', hjust = 0),
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

player_stats_p90 <- qs::qread(file.path(data_dir, 'player_stats_p90.qs'))

min_minutes_played <- 90
top_player_stats_p90 <- player_stats_p90 |> 
  filter(time_played >= min_minutes_played) |> 
  slice_max(linebreaks_attempted_completed_p90, n = 10) |> 
  mutate(
    local_player_picture_url = map2(
      player_picture_url,
      player_id,
      ~{
        path <- file.path(img_dir, sprintf('%s.png', ..2))
        if (file.exists(path)) {
          return(path)
        }
        download.file(..1, destfile = path, mode = 'wb')
        path
      }
    ),
    player_lab = sprintf('<span style="font-size:14px;color:white"><b>%s</b></span><br/><span style="font-size:12px;color:#ffffff">%s</span>', player_name, country),
    across(
      player_lab,
      \(x) fct_reorder(x, linebreaks_attempted_completed_p90)
    )
  )

p <- top_player_stats_p90 |> 
  ggplot() +
  aes(
    x = linebreaks_attempted_completed_p90,
    y = player_lab
  ) +
  geom_col(
    fill = '#00a896',
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
      x = 3,
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
      label = sprintf('%.1f', linebreaks_attempted_completed_p90)
    )
  ) +
  theme(
    plot.title = element_text(size = 18),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    y = NULL,
    x = NULL,
    title = 'Most linebreaks attempted per 90 minutes',
    subtitle = 'FIFA U-20 World Cup 2023',
    caption = glue::glue(
      '
      **Data**: FIFA through June 2. Minimum {min_minutes_played} minutes played.
      '
    ),
    tag = tag_lab
  )
p

p_with_logos <- p +
  ## passing in more than 1 row to geom_image isn't working for some reason...
  map(
    top_player_stats_p90 |>
      group_split(row_number()),
    ~{
      ggimage::geom_image(
        data = .x,
        size = 0.06,
        hjust = 0,
        # vjust = 0.5,
        aes(
          x = 0.5,
          image = local_player_picture_url
        )
      )
    }
  )

linebreaks_plot_path <- file.path(proj_dir, 'top_linebreaks_attempted_p90.png')
ggsave(
  p_with_logos,
  filename = linebreaks_plot_path,
  width = 7,
  height = 7
)

add_logo(
  linebreaks_plot_path,
  file.path(proj_dir, 'FPLUS_FU2023_Argentina_Textmark.png'),
  delete = TRUE, 
  logo_scale = 0.12,
  idx_x = 0.02,
  idx_y = 0.98,
  adjust_y = FALSE
)



p <- player_stats_p90 |> 
  filter(time_played > 90) |> 
  arrange(desc(total_distance_p90))
ggplot() +
  aes(
    x = linebreaks_attempted_completed_p90,
    y = total_distance_p90
  ) +
  geom_point(color = 'white')

grid.newpage()
plot <- print(
  p,
  vp = viewport(
    width = unit(0.8, "npc"),
    height = unit(0.8, "npc"), 
    angle = 45
  )
)
vp <- viewport(x = 0.82, y = 0.98, width = 0, height = 0)
pushViewport(vp)
plot <- grid.grab()
plot <- as.ggplot(plot)
ggsave(file.path(proj_dir, 'temp.png'), bg = blackish_background, width = 1500, height = 1500, units = "px")


pak::pak('thebioengineer/camcorder')
player_stats_p90 |>
  filter(time_played > 90, linebreaks_completed_all_lines_p90 > 0) |> 
  ggplot() +
  aes(
    x = linebreaks_attempted_completed_p90,
    y = linebreaks_completed_all_lines
  ) +
  geom_point(color = 'white')
library(camcorder)
gg_record(
  dir = file.path(tempdir(), '71'),
  device = 'png',
  width = 6,
  height = 6,
  units = 'in',
  dpi = 300
)
