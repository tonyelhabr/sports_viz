
library(tidyverse)
# require(pluralize)
dir_proj <- '22-202021_epl_traore'
source(file.path(dir_proj, 'annotate_pitch.R'))
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

lab_tag <- '**Viz**: Tony ElHabr | **Data**: 2020-21 Premier League through Matchweek 31'
# events_2viz <-
#   sprintf(
#     '%s %s', 
#     rep(sprintf('%successful', c('Uns', 'S')), 3), 
#     c(rep('Pass', 2), rep('TakeOn', 2), rep('BallTouch', 2))
#   )
# events_2viz
events_2viz <- sprintf('Successful %s', c('Pass', 'TakeOn', 'BallTouch'))

# main ----
do_player <- function(player, team, team_understat, path_player) {
  
  path_events <- file.path(dir_proj, sprintf('events_%s.rds', team))
  path_meta <- file.path(dir_proj, sprintf('meta_%s.rds', team))
  events <- path_events %>% read_rds()
  meta <- path_meta %>% read_rds()
  
  events_filt <- 
    events %>% 
    filter(player_name == player) %>% 
    mutate(event = sprintf('%s %s', outcome_type_name, type_name))
  events_filt
  
  meta_filt_init <-
    events_filt %>%
    distinct(season, match_id) %>%
    left_join(meta) %>% 
    group_by(season) %>% 
    mutate(idx = row_number(time_stamp)) %>% 
    ungroup() %>% 
    select(season, match_id, idx) %>% 
    arrange(season, idx)
  meta_filt_init
  
  idx_threshold <- 
    meta_filt_init %>% 
    group_by(season) %>% 
    slice_max(idx) %>% 
    ungroup() %>% 
    slice_min(idx) %>% 
    pull(idx)
  idx_threshold
  
  meta_filt <-
    meta_filt_init %>% 
    filter(idx <= idx_threshold)
  meta_filt
  
  # Not completely necessary at this point, but get minues played.
  # n_match <- events_filt %>% distinct(season, match_id) %>% count(season)
  # n_match
  players <- 
    c(2019, 2020) %>% 
    map_dfr(
      ~understatr::get_team_players_stats(team_understat, .x)
    ) %>% 
    janitor::clean_names()
  # players %>% count(year)
  
  players_filt <- 
    players %>% 
    filter(player_name == player)
  players_filt
  
  mp <-
    players_filt %>% 
    mutate(season = sprintf('%s-%s', year, year + 1)) %>% 
    select(season, player_name, mp = time)
  mp
  
  # mp$mp[1] / mp$mp[2]
  frac_diff <-
    mp %>% 
    pivot_wider(names_from = season, values_from = mp) %>% 
    mutate(frac = `2019-2020` / `2020-2021`) %>% 
    pull(frac)
  frac_diff
  # frac_diff <- 1.221072
  
  # n_type <- events_filt %>% count(type_name, sort = TRUE)
  # n_type
  
  # n_type_season <-
  #   events_filt %>% 
  #   count(season, type_name, outcome_type_name, sort = TRUE) %>% 
  #   left_join(mp) %>% 
  #   mutate(n_p90 = n * 90 / mp)
  # n_type_season
  # 
  # n_type_season %>%
  #   select(season, type_name, outcome_type_name, player_name, n_p90) %>%
  #   pivot_wider(names_from = season, values_from = n_p90)
  # 
  # # NOTE: Doesn't split out by season.
  # events_filt %>% 
  #   count(type_name, outcome_type_name) %>% 
  #   pivot_wider(names_from = outcome_type_name, values_from = n, values_fill = 0L) %>% 
  #   mutate(
  #     total = (Successful + Unsuccessful),
  #     frac = Successful / total
  #   ) %>% 
  #   arrange(desc(total))

  # xengagement::team_accounts_mapping %>% filter(team %>% str_detect('Wolv')) %>% select(color_pri, color_sec) %>% gather() %>% pull(value) %>% scales::show_col()
  .pal <- c('#231f20', '#ffffff', '#fdb913')
  
  # TODO: Do a diff of successful vs unsuccessful first to find relative success intra-season, then do a diff between seasons for inter-seasonal difference.
  .plot_diff <- 
    function(event = 'Successful Pass',
             # # color = 'red',
             pal = .pal,
             n = 200L,
             dir = dir_proj,
             file = str_replace_all(snakecase::to_snake_case(event), '\\s', '-'),
             ext = 'png',
             path = file.path(dir, sprintf('%s.%s', file, ext)),
             ...) {
      # event <- 'Successful Pass'
      # n = 200L
      
      events_filt_filt <- events_filt %>% filter(event == !!event)
      events_filt1 <- events_filt_filt %>% filter(season == '2019-2020')
      events_filt2_init <- events_filt_filt %>% filter(season == '2020-2021')
      events_filt2_extra <- events_filt2_init %>% sample_frac(frac_diff - 1, replace = TRUE)
      events_filt2 <- bind_rows(events_filt2_init, events_filt2_extra)
      x1 <- events_filt1 %>% pull(x)
      y1 <- events_filt1 %>% pull(y)
      x2 <- events_filt2 %>% pull(x)
      y2 <- events_filt2 %>% pull(y)
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
      # dzz %>% ggplot() + aes(x = z_norm) + geom_density()
      
      rng <- range(dzz$z)
      max_abs <- max(abs(rng))
      # pal <- RColorBrewer::brewer.pal(3, 'PRGn')
      
      # p <- ggplot() + annotate_pitch(colour = 'black', fill = NA) + ggsoccer::theme_pitch()
      # generator <- scales::col_numeric(c("#231f20", "#fdb913"), domain = NULL)
      # generator(1:8) %>% scales::show_col()
      lab_title <-
        event %>% 
        str_remove('Successful ') %>% 
        snakecase::to_sentence_case() %>% 
        pluralize::pluralize()
      
      viz <- 
        ggplot() +
        # metR::geom_contour_fill(data = dzz, aes(x = x, y = y, z = z), bins = n_bin, alpha = 0.7) +
        # metR::geom_contour_tanaka(data = dzz, aes(x = x, y = y, z = z), bins = n_bin, alpha = 0.7) +
        geom_raster(data = dzz, aes(x = x, y = y, fill = z), alpha = 1) +
        scale_fill_gradient2(
          midpoint = 0, 
          limits = c(-abs(max_abs), abs(max_abs)), 
          low = pal[1], 
          high = pal[3]
        ) +
        annotate_pitch(fill = NA) +
        guides(fill = guide_legend('')) +
        ggsoccer::theme_pitch() +
        theme(
          plot.title.position = 'plot',
          plot.title = ggtext::element_markdown(size = 18, hjust = 0.5, color = 'grey20')
        ) +
        guides(fill = FALSE) +
        labs(
          title = lab_title
        )
      viz
      
      h <- 6
      ggsave(
        plot = viz,
        filename = path,
        height = h,
        width = h * 1.5,
        type = 'cairo'
      )
      viz
    }

  plots <- 
    events_2viz %>% 
    map(
      ~.plot_diff(.x)
    )
  plots
  
  viz_player <- 
    patchwork::wrap_plots(plots, row = 1) +
    patchwork::plot_annotation(
      # title = sprintf('2020/21 %s Relative to 2019/20', player_filt),
      title = glue::glue('<b><span style="color:{colorspace::darken(pal[3], 0.3)}">More</span></b> vs. <b><span style="color:{colorspace::darken(pal[1], 0.3)}">Less</span></b> Actions, 2020/21 Relative to 2019/20'),
      subtitle = player,
      caption = lab_tag,
      theme = theme(
        plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 18, hjust = 0.5)
        plot.caption = ggtext::element_markdown(hjust = 0, size = 14)
      )
    )
  viz_player
  
  add_logo(
    path_viz = path_player,
    path_logo = path_logo,
    idx_x = 0.01,
    logo_scale = 0.08,
    adjust_y = FALSE,
    idx_y = 1
  )
}

# main ----

tibble(
  player = 'Adama Traoré',
  team = 'Wolverhampton-Wanderers',
  team_understat = 'Wolverhampton Wanderers',
  path_player = file.path(dir_proj, 'adama-traore.png'),
) %>% 
  mutate(
    res = pmap(list(player, team, team_understat, path_player))
  )

# dev ----
pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

viz[[2]] +
  ggtext::geom_richtext(
    data = tibble(),
    fill = NA, label.color = NA,
    size = pts(14),
    family = 'Karla',
    aes(
      x = 5,
      y = 90,
      label = 'Traore has spent more time on the left half<br/> of the field'
    ),
    hjust = 0
  )

ggplot() +
  theme(
    plot.title.position = 'plot',
    plot.title = ggtext::element_markdown(size = 18, hjust = 0.5, color = 'grey20'),
    plot.tag = ggtext::element_markdown(hjust = 0, size = 14)
  ) +
  labs(
    title = glue::glue('<b><span style="color:{colorspace::darken(pal[3], 0.3)}">More</span></b> vs. <b><span style="color:{colorspace::darken(pal[1], 0.3)}">Less</span></b> Actions, 2020/21 Compared to 2019/20')
  )
