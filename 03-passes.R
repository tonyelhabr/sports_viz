
library(tidyverse)
# library(patchwork)
# library(metR)
dir_proj <- '21-202021_epl_heatmap'
path_events <- file.path(dir_proj, 'events.rds')
path_meta <- file.path(dir_proj, 'meta.rds')
path_forms <- file.path(dir_proj, 'forms.rds')
events <- path_events %>% read_rds()
meta <- path_meta %>% read_rds()
forms <- path_forms %>% read_rds()
extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = element_text('Karla', face = 'bold', size = 18, color = 'gray20', hjust = 0.5),
  # plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', face = 'bold', size = 14, color = 'gray20'),
  axis.text = element_text('Karla', size = 14),
  # axis.title = element_text(size = 24, face = 'bold'),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  plot.caption = element_text('Karla', size = 12, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.02, 0.01),
  legend.text = element_text(size = 14),
  strip.text = element_text(size = 14),
  strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

path_logo <- file.path(dir_proj, 'premier-league.png')
add_logo <-
  function(path_viz,
           path_logo,
           idx_x,
           idx_y,
           logo_scale = 0.1,
           adjust_x = TRUE,
           adjust_y = TRUE,
           path_suffix = '_w_logo') {
    plot <- path_viz %>% magick::image_read()
    logo_raw <- path_logo %>% magick::image_read()
    
    plot_height <- magick::image_info(plot)$height
    plot_width <- magick::image_info(plot)$width
    
    logo <- magick::image_scale(logo_raw, as.character(round(plot_width * logo_scale)))
    
    info <- magick::image_info(logo)
    logo_width <- info$width
    logo_height <- info$height
    
    x_pos <- plot_width - idx_x * plot_width
    y_pos <- plot_height - idx_y * plot_height
    
    if(adjust_x) {
      x_pos <- x_pos - logo_width
    }
    
    if(adjust_y) {
      y_pos <- y_pos - logo_height
    }
    
    offset <- paste0('+', x_pos, '+', y_pos)
    
    viz <- plot %>% magick::image_composite(logo, offset = offset)
    ext <- path_viz %>% tools::file_ext()
    rgx_ext <- sprintf('[.]%s$', ext)
    
    res <-
      magick::image_write(
        viz,
        path_viz %>% str_replace(rgx_ext, sprintf('%s.%s', path_suffix, ext))
      )
    res
  }

# main ----
fouls <- events %>% filter(type_name == 'Foul')
fouls
events %>% count(type_name, sort = TRUE)
passes <- events %>% filter(type_name == 'Pass')
passes_filt <- passes %>% filter(player_name == 'Adama Traoré')
passes_filt

passes_filt %>% 
  ggplot() +
  ggsoccer::annotate_pitch(limits = FALSE) +
  ggsoccer::theme_pitch() +
  geom_segment(
    aes(x = x, y = y, xend = end_x, yend = end_y),
    lineend = 'butt', linejoin = 'mitre'
  ) +
  facet_wrap(~outcome_type_name)


convert_to_xyz <- function(data, n = 200) {
  x <- data %>% pull(x)
  y <- data %>% pull(y)
  # x_rng <- range(x)
  # y_rng <- range(y)
  x_rng <- c(0, 100)
  y_rng <- x_rng
  
  bw_x <- MASS::bandwidth.nrd(x)
  bw_y <- MASS::bandwidth.nrd(y)
  bw_xy <- c(bw_x, bw_y)
  dz <- MASS::kde2d(x, y, h = bw_xy, n = n, lims = c(x_rng, y_rng))
  colnames(dz$z) <- dz$y
  res <- 
    dz$z %>%
    as_tibble() %>% 
    mutate(x = dz$x) %>% 
    pivot_longer(-x, names_to = 'y', values_to = 'z') %>% 
    mutate(y = as.double(y)) %>% 
    mutate(across(z, list(norm = ~(.x - min(.x)) / (max(.x) - min(.x)))))
  res
}

z <- passes_filt %>% convert_to_xyz()
z

z %>% 
  ggplot() +
  aes(x = x, y = y, z = z_norm) +
  ggsoccer::annotate_pitch(limits = FALSE) +
  ggsoccer::theme_pitch() +
  geom_contour_filled(bins = 8, alpha = 0.7) +
  metR::geom_contour_tanaka(bins = 8, alpha = 0.7) +
  guides(fill = FALSE) +
  scale_fill_brewer(palette = 'Blues')
