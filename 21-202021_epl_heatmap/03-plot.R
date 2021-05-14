
library(tidyverse)
library(patchwork)
library(metR)
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
           path_suffix = '_w_logo',
           delete = TRUE) {
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

    if(!delete) {
      return(res)
    }
    fs::file_delete(path_viz)
    res
  }

# main ----
fouls <- events %>% filter(type_name == 'Foul')
fouls

forms_labs <-
  tibble(
    row_max = c(rep(3L, 3), rep(4L, 4), rep(5L, 5), rep(6L, 6), rep(7L, 7), rep(8L, 8)),
    row = c(seq.int(1, 3), seq.int(1, 4), seq.int(1, 5), seq.int(1, 6), seq.int(1, 7), seq.int(1, 8)),
    pos = c(c('G', 'D', 'M'), c('G', 'D', 'M', 'F'), c('G', 'D', 'M', 'M', 'F'), c('G', 'D', 'M', 'M', 'A', 'F'), c('G', 'D', 'M', 'M', 'A', 'A', 'F'), c('G', 'D', 'M', 'M', 'M', 'A', 'A', 'F'))
  )
forms_labs

pos <-
  forms %>% 
  group_by(match_id, team_id, idx) %>% 
  mutate(row = dense_rank(y)) %>% 
  mutate(row_max = max(row)) %>% 
  ungroup() %>% 
  left_join(forms_labs, by = c('row', 'row_max')) %>% 
  count(player_id, player_name, pos) %>% 
  group_by(player_id, player_name) %>%
  mutate(total = sum(n)) %>% 
  mutate(frac = n / total) %>% 
  slice_max(frac, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(player_id, player_name, pos)
pos

fouls_pos <- 
  fouls %>% 
  left_join(pos) %>% 
  filter(pos != 'G') %>% 
  mutate(across(pos, ~ordered(.x, c('D', 'M', 'F')))) %>% 
  mutate(across(pos, ~fct_recode(.x, 'Defenders' = 'D', 'Midfielders' = 'M', 'Forwards' = 'F')))
fouls_pos

fouls_by_player <-
  fouls_pos %>% 
  drop_na(player_id) %>% 
  count(outcome_type_name, pos, player_id, player_name, name = 'n') %>% 
  arrange(outcome_type_name, pos, desc(n))
fouls_by_player

fouls_by_player_top <-
  fouls_by_player %>% 
  group_by(outcome_type_name, pos) %>% 
  slice_max(n, n = 3, with_ties = FALSE) %>% 
  ungroup()
fouls_by_player_top

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

fouls_xyz <-
  fouls_pos %>% 
  group_nest(outcome_type_name, pos) %>% 
  mutate(data = map(data, convert_to_xyz)) %>% 
  unnest(data)
fouls_xyz

pos <- c('Defenders', 'Midfielders', 'Forwards')
lab_tag <- '**Viz**: Tony ElHabr | **Data**: 2020-21 Premier League through Matchweek 30'

.plot_by_outcome_type_name <- 
  function(.outcome_type_name,
           dir = dir_proj,
           ext = 'png',
           file = sprintf('viz_fouls_%s_by_pos', tolower(lab_outcome)),
           path = file.path(dir_proj, sprintf('%s.%s', file, ext))) {
  
  fouls_xyz_filt <- fouls_xyz %>% filter(outcome_type_name == .outcome_type_name)
  lab_outcome <- case_when(
    .outcome_type_name == 'Unsuccessful' ~ 'Made',
    .outcome_type_name == 'Successful' ~ 'Drawn'
  )
  .plot_by_pos <- function(.pos) {
    pal <- case_when(
      .pos == 'Forwards' ~ 'Greens',
      .pos == 'Midfielders' ~ 'Oranges',
      .pos == 'Defenders' ~ 'Purples'
    )
    
    color_title <- case_when(
      .pos == 'Forwards' ~ '#005a32',
      .pos == 'Midfielders' ~ '#8c2d04',
      .pos == 'Defenders' ~ '#4a1486'
    )
    
    lab_title <- case_when(
      .pos == 'Midfielders' ~ sprintf('Where are Fouls <b><span style="color:black">%s</span></b>?', lab_outcome),
      TRUE ~ ' '
    )
    
    viz <-
      fouls_xyz_filt %>%
      filter(pos == .pos) %>% 
      ggplot() +
      aes(x = x, y = y, z = z_norm) +
      ggsoccer::annotate_pitch(limits = FALSE) +
      ggsoccer::theme_pitch() +
      geom_contour_filled(bins = 8, alpha = 0.7) +
      metR::geom_contour_tanaka(bins = 8, alpha = 0.7) +
      guides(fill = FALSE) +
      scale_fill_brewer(palette = pal) +
      # coord_fixed(clip = 'off') +
      theme(
        plot.title = ggtext::element_markdown(size = 24, hjust = 0.5, color = 'grey50'),
        plot.subtitle = ggtext::element_markdown(size = 18, hjust = 0.5)
      ) +
      labs(
        title = lab_title,
        subtitle = glue::glue('<b><span style="color:{color_title}">{.pos}</span></b>')
      )
    viz
  }
  
  plots <- pos %>% map(.plot_by_pos)
  viz_fouls_by_pos <- 
    patchwork::wrap_plots(plots, row = 1) +
    patchwork::plot_annotation(
      caption = lab_tag,
      theme = theme(plot.caption = ggtext::element_markdown(hjust = 0, size = 14))
    )
  viz_fouls_by_pos
  
  ggsave(
    plot = viz_fouls_by_pos,
    filename = path,
    height = 6,
    width =  3 * 6,
    type = 'cairo'
  )
  viz_fouls_by_pos
}

path_fouled_by_pos <- file.path(dir_proj, 'viz_fouled_by_pos.png')
viz_fouled_by_pos <- 
  .plot_by_outcome_type_name('Successful', path = path_fouled_by_pos)
viz_fouled_by_pos

add_logo(
  path_viz = path_fouled_by_pos,
  path_logo = path_logo,
  idx_x = 0.01,
  logo_scale = 0.08,
  adjust_y = FALSE,
  idx_y = 1
)

# player-specific ----
plot_player_vs_pos <-
  function(player,
           outcome_type_name = 'Unsuccessful',
           pos = NULL,
           color = 'red',
           n_bin = 8L,
           n = 200L,
           dir = dir_proj,
           file = player,
           ext = 'png',
           path = file.path(dir, sprintf('%s.%s', file, ext)),
           path_logo = file.path(dir, sprintf('%s.%s', str_replace_all(tolower(player), ' ', '-'), ext))) {
    
    if(is.null(pos)) {
      player_name_filt <- 
        pos <-
        fouls_pos %>% 
        filter(player_name == !!player) %>% 
        distinct(pos) %>% 
        pull(pos) %>% 
        as.character() %>% 
        .[1]
    }
    lab_outcome <- case_when(
      outcome_type_name == 'Unsuccessful' ~ 'Made',
      outcome_type_name == 'Successful' ~ 'Drawn'
    )
    
    fouls_pos_filt <- fouls_pos %>% filter(pos == !!pos)
    fouls_pos1 <- fouls_pos_filt %>% filter(player_name == !!player)
    fouls_pos2 <- fouls_pos_filt %>% filter(player_name != !!player)
    x1 <- fouls_pos1 %>% pull(x)
    y1 <- fouls_pos1 %>% pull(y)
    x2 <- fouls_pos2 %>% pull(x)
    y2 <- fouls_pos2 %>% pull(y)
    x_rng <- range(c(x1, x2))
    y_rng <- range(c(y1, y2))
    bw_x <- MASS::bandwidth.nrd(c(x1, x2))
    bw_y <- MASS::bandwidth.nrd(c(y1, y2))
    bw_xy <- c(bw_x, bw_y)
    d21 <- MASS::kde2d(x1, y1, h = bw_xy, n = n, lims = c(x_rng, y_rng))
    d22 <- MASS::kde2d(x2, y2, h = bw_xy, n = n, lims = c(x_rng, y_rng))
    dz <- d21
    dz$z <- d21$z - d22$z
    colnames(dz$z) <- dz$y
    dzz <- 
      dz$z %>%
      as_tibble() %>% 
      mutate(x = dz$x) %>% 
      pivot_longer(-x, names_to = 'y', values_to = 'z') %>% 
      mutate(y = as.double(y)) %>% 
      mutate(across(z, list(norm = ~(.x - min(.x)) / (max(.x) - min(.x)))))
    
    pal <- colorRampPalette(c('white', color))(n_bin) # %>% scales::show_col()
    viz <- 
      dzz %>% 
      filter(z_norm > mean(z_norm)) %>% 
      ggplot() +
      # Not as smooth without the sqrt
      aes(x = x, y = y, z = sqrt(z_norm)) +
      ggsoccer::annotate_pitch(limits = FALSE) +
      ggsoccer::theme_pitch() +
      metR::geom_contour_fill(bins = n_bin, alpha = 0.7) +
      metR::geom_contour_tanaka(bins = n_bin, alpha = 0.7) +
      guides(fill = FALSE) +
      scale_fill_gradientn(colors = pal) +
      # coord_fixed(clip = 'off') +
      theme(
        plot.title.position = 'plot',
        plot.title = ggtext::element_markdown(size = 24, hjust = 0, color = 'grey50'),
        plot.tag = ggtext::element_markdown(hjust = 0, size = 14)
      ) +
      labs(
        subtitle = ' ',
        tag = lab_tag,
        title = glue::glue('Fouls <b><span style="color:black">{lab_outcome}</span><b> by <b><span style="color:{color}">{player}</span><b> vs. All Other {pos}')
      )
    viz
    
    h <- 8
    # browser()
    ggsave(
      plot = viz,
      filename = path,
      height = h,
      width = h * 1.5,
      type = 'cairo'
    )

    add_logo(
      path_viz = path,
      path_logo = path_logo,
      # adjust_x = FALSE,
      idx_x = 0.1,
      logo_scale = 0.08,
      adjust_y = FALSE,
      idx_y = 1
    )
    viz
  }

viz_fouled <-
  plot_player_vs_pos(
    player = 'Jack Grealish',
    file = 'viz_fouled',
    color = '#670e36', # '#95bfe5',
    outcome_type_name = 'Successful'
  )
viz_fouled

viz_fouler <-
  plot_player_vs_pos(
    player = 'Tomas Soucek',
    file = 'viz_fouler',
    color = '#7a263a',
    outcome_type_name = 'Unsuccessful'
  )
viz_fouler 

# Despite missing several matches, Grealish still leads the EPL in fouls drawn (100) by a wide margin. (Second is Sadio Mané with 67). He owns the left attacking half.