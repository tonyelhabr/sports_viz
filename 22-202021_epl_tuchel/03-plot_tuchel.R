
library(tidyverse)
library(patchwork)
dir_proj <- '22-202021_epl_tuchel'
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

type_names <- c('Pass', 'TakeOn', 'BallTouch')
pal <- c('#ed1c24', '#ffffff', '#034694')
team <- 'Chelsea'

# main ----
plot_player <- 
  function(player,
           type_name,
           n = 200L,
           ...) {
    # player <- 'Mason Mount'
    # https://www.schemecolor.com/chelsea-f-c-logo-colors.php

    # Pre-calculated this.
    # frac_diff <- .get_frac_diff(player, team)
    if(player == 'Mason Mount') {
      path_player_logo <- file.path(dir_proj, 'mason-mount.png')
      frac_diff <- 1530/835
    } else if (player == 'Kai Havertz') {
      path_player_logo <- file.path(dir_proj, 'kai-havertz.png')
      frac_diff <- 884/391
    }
    
    path_events <- file.path(dir_proj, sprintf('events_%s.rds', team))
    path_meta <- file.path(dir_proj, sprintf('meta_%s.rds', team))
    events <- path_events %>% read_rds()
    meta <- path_meta %>% read_rds()
    
    events_player <- 
      events %>% 
      left_join(
        meta %>% 
          select(match_id, time_stamp) %>% 
          mutate(across(time_stamp, lubridate::ymd_hms)),
        by = 'match_id'
      ) %>% 
      filter(player_name == !!player)

    events_player_filt <- 
      events_player %>% 
      filter(type_name == !!type_name & outcome_type_name == 'Successful')
    
    .date_split <- lubridate::ymd('2021-01-25')
    events_player1 <- events_player_filt %>% filter(time_stamp < .date_split)
    events_player2_init <- events_player_filt %>% filter(time_stamp >= .date_split)
    set.seed(42)
    events_player2_extra <- events_player2_init %>% sample_frac(frac_diff - 1, replace = TRUE)
    events_player2 <- bind_rows(events_player2_init, events_player2_extra)
    x1 <- events_player1 %>% pull(x)
    y1 <- events_player1 %>% pull(y)
    x2 <- events_player2 %>% pull(x)
    y2 <- events_player2 %>% pull(y)
    # x_rng <- range(c(x1, x2))
    # y_rng <- range(c(y1, y2))
    x_rng <- c(0, 100)
    y_rng <- x_rng
    bw_x <- MASS::bandwidth.nrd(c(x1, x2))
    bw_y <- MASS::bandwidth.nrd(c(y1, y2))
    bw_xy <- c(bw_x, bw_y)
    d21 <- MASS::kde2d(x1, y1, h = bw_xy, n = n, lims = c(x_rng, y_rng))
    d22 <- MASS::kde2d(x2, y2, h = bw_xy, n = n, lims = c(x_rng, y_rng))
    dz <- d22
    dz$z <- d22$z - d21$z
    colnames(dz$z) <- dz$y
    dzz <- 
      dz$z %>%
      as_tibble() %>% 
      mutate(x = dz$x) %>% 
      pivot_longer(-x, names_to = 'y', values_to = 'z') %>% 
      mutate(y = as.double(y)) %>% 
      mutate(across(z, list(norm = ~(.x - min(.x)) / (max(.x) - min(.x)))))
    
    rng <- range(dzz$z)
    max_abs <- max(abs(rng))
    
    viz <- 
      ggplot() +
      geom_raster(data = dzz, aes(x = x, y = y, fill = z)) +
      scale_fill_gradient2(
        midpoint = 0, 
        limits = c(-abs(max_abs), abs(max_abs)), 
        low = pal[1], 
        high = pal[3]
      ) +
      annotate_pitch(fill = NA) +
      guides(fill = guide_legend('')) +
      ggsoccer::theme_pitch() +
      coord_flip() +
      scale_y_reverse() +
      guides(fill = FALSE)
    viz
  }

plots2_init <- type_names %>% map(~plot_player(player = 'Mason Mount', type_name = .x))
plots1_init <- type_names %>% map(~plot_player(player = 'Kai Havertz', type_name = .x))
plots2 <- patchwork::wrap_plots(plots2_init, ncol = 1)
plots1 <- patchwork::wrap_plots(plots1_init, ncol = 1)
# plots1
# plots2
# plots <- patchwork::wrap_plots(plots1, plots2, nrow = 1)
# plots

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

viz_type_names <-
  type_names %>%
  tibble(type_name = .) %>% 
  mutate(
    type_name = 
      case_when(
        type_name == 'TakeOn' ~ 'Take On',
        type_name == 'BallTouch' ~ 'Touch',
        TRUE ~ type_name
      )
  ) %>% 
  ggplot() +
  aes(y = desc(type_name), x = 0) +
  geom_tile(fill = NA, show.legend = FALSE) +
  geom_text(
    aes(label = type_name),
    hjust = 0.5,
    family = 'Karla',
    size = pts(24),
    color = 'grey50',
    fontface = 'bold'
  ) +
  theme(
    axis.text.y = element_blank(), 
    axis.text.x = element_blank()
  ) +
  labs(
    # title = ''
    x = NULL,
    y = NULL
  )
viz_type_names

# gplots::col2hex('grey50')
plots <- 
  patchwork::wrap_plots(plots1, viz_type_names, plots2, nrow = 1, widths = c(2, 1, 2)) +
  patchwork::plot_annotation(
    title = glue::glue('<b><span style="color:{pal[3]}">More</span></b> vs. <b><span style="color:{pal[1]}">Less</span></b> Non-shot Activity under Tuchel vs. Lampard'),
    # subtitle = player,
    subtitle = 'Kai Havertz <span style="color:#7F7F7F">&</span> Mason Mount',
    caption = '**Viz**: Tony ElHabr | **Data**: 2020-21 Premier League through Matchweek 31',
    theme = theme(
      plot.title = ggtext::element_markdown(face = 'bold', size = 18, hjust = 0.5, color = 'grey50'),
      plot.subtitle = ggtext::element_markdown(face = 'bold', size = 16, hjust = 0.5, color = 'grey20'),
      plot.caption = ggtext::element_markdown(hjust = 0, size = 14)
    )
  )
plots

path_tuchel <- file.path(dir_proj, 'viz_tuchel.png')
ggsave(
  plot = plots,
  filename = path_tuchel,
  height = 8,
  width = 8,
  type = 'cairo'
)

plots_w_logo_init <-
  add_logo(
    path_viz = path_tuchel,
    path_logo = file.path(dir_proj, 'mason-mount.png'),
    idx_x = 0.05,
    logo_scale = 0.1,
    adjust_y = FALSE,
    idx_y = 0.95
  )

plots_w_logo <-
  add_logo(
    path_suffix = '',
    path_viz = plots_w_logo_init,
    path_logo = file.path(dir_proj, 'kai-havertz.png'),
    idx_x = 0.95,
    logo_scale = 0.1,
    adjust_y = FALSE,
    idx_y = 0.95
  )
plots_w_logo
