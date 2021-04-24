
library(tidyverse)
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

# events_2viz <-
#   sprintf(
#     '%s %s', 
#     rep(sprintf('%successful', c('Uns', 'S')), 3), 
#     c(rep('Pass', 2), rep('TakeOn', 2), rep('BallTouch', 2))
#   )
# events_2viz
events_2viz <- sprintf('Successful %s', c('Pass', 'TakeOn', 'BallTouch'))

# main ----
.get_frac_diff <- function(player, team) {
  players <- 
    c(2019, 2020) %>% 
    map_dfr(
      ~understatr::get_team_players_stats(!!team, .x)
    ) %>% 
    janitor::clean_names()
  # players %>% count(year)
  
  players_filt <- 
    players %>% 
    filter(player_name == !!player)
  players_filt
  
  mp <-
    players_filt %>% 
    mutate(season = sprintf('%s-%s', year, year + 1)) %>% 
    select(season, player_name, mp = time)
  mp
  
  frac_diff <-
    mp %>% 
    pivot_wider(names_from = season, values_from = mp) %>% 
    mutate(frac = `2019-2020` / `2020-2021`) %>% 
    pull(frac)
  frac_diff
}

do_player <- function(player) {
  
  if(player == 'Mohamed Salah') {
    # Use Wolves' black instead of the green.
    pal <- c('#231f20', '#ffffff', '#e0202C')
    team <- 'Liverpool'
    team_understat <- 'Liverpool'
    path_player_logo <- file.path(dir_proj, 'mo-salah.png')
    frac_diff <- 1.14828
    darken_factor <- 0
  } else if (player == 'Adama Traoré') {
    pal <- c('#231f20', '#ffffff', '#fdb913')
    team <- 'Wolverhampton-Wanderers'
    team_understat <- 'Wolverhampton Wanderers'
    path_player_logo <- file.path(dir_proj, 'adama-traore.png')
    frac_diff <- 1.221072
    darken_factor <- 0.2
  }
  
  path_events <- file.path(dir_proj, sprintf('events_%s.rds', team))
  events <- path_events %>% read_rds()
  
  events_player <- 
    events %>% 
    filter(player_name == !!player) %>% 
    mutate(event = sprintf('%s %s', outcome_type_name, type_name))
  events_player
  
  # Pre-calculated this.
  # frac_diff <- .get_frac_diff(player, team_understat)

  # TODO: Do a diff of successful vs unsuccessful first to find relative success intra-season, then do a diff between seasons for inter-seasonal difference.
  .plot_diff <- 
    function(event = 'Successful Pass',
             # # color = 'red',
             n = 200L,
             dir = dir_proj,
             file = str_replace_all(snakecase::to_snake_case(event), '\\s', '-'),
             ext = 'png',
             path = file.path(dir, sprintf('%s.%s', file, ext)),
             ...) {

      lab_title <- event %>% str_remove('Successful ')
      lab_title <-
        case_when(
          lab_title == 'TakeOn' ~ 'Take On',
          lab_title == 'BallTouch' ~ 'Touch',
          TRUE ~ lab_title
        )
      path <- file.path(dir_proj, sprintf('%s.png', sprintf('%s-%s.png', str_replace_all(snakecase::to_snake_case(lab_title), '\\s', '-'), player)))
      events_player_filt <- events_player %>% filter(event == !!event)
      events_player1 <- events_player_filt %>% filter(season == '2019-2020')
      events_player2_init <- events_player_filt %>% filter(season == '2020-2021')
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
      
      # h <- 6
      # ggsave(
      #   plot = viz,
      #   filename = path,
      #   height = h,
      #   width = h * 1.5,
      #   type = 'cairo'
      # )
      viz
    }

  plots <- events_2viz %>% map(.plot_diff)

  viz_player <- 
    patchwork::wrap_plots(plots, row = 1) +
    patchwork::plot_annotation(
      title = glue::glue('<b><span style="color:{colorspace::darken(pal[3], darken_factor)}">More</span></b> vs. <b><span style="color:{colorspace::darken(pal[1], 0.3)}">Less</span></b> Actions 2020/21 Relative to 2019/20, <b><span style="color:black">{player}</span></b>'),
      # subtitle = player,
      caption = '**Viz**: Tony ElHabr | **Data**: 2020-21 Premier League through Matchweek 31',
      theme = theme(
        plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 18, hjust = 0, color = 'grey50'),
        # plot.subtitle = ggtext::element_markdown('Karla', face = 'bold', size = 16, hjust = 0.5, color = 'grey20'),
        plot.caption = ggtext::element_markdown(hjust = 0, size = 14)
      )
    )
  viz_player
  
  path_player <- file.path(dir_proj, sprintf('viz_diffs-%s.png', player))
  h <- 8
  ggsave(
    plot = viz_player,
    filename = path_player,
    height = h,
    width = 1.5 * h,
    type = 'cairo'
  )
  
  add_logo(
    path_viz = path_player,
    path_logo = path_player_logo,
    idx_x = 0.05,
    logo_scale = 0.05,
    adjust_y = FALSE,
    idx_y = 0.73
  )
}

# main ----

c('Adama Traoré', 'Mohamed Salah') %>% walk(do_player)

# dev ----
# pts <- function(x) {
#   as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
# }
# 
# viz[[2]] +
#   ggtext::geom_richtext(
#     data = tibble(),
#     fill = NA, label.color = NA,
#     size = pts(14),
#     family = 'Karla',
#     aes(
#       x = 5,
#       y = 90,
#       label = 'Traore has spent more time on the left half<br/> of the field'
#     ),
#     hjust = 0
#   )
# 
# ggplot() +
#   theme(
#     plot.title.position = 'plot',
#     plot.title = ggtext::element_markdown(size = 18, hjust = 0.5, color = 'grey20'),
#     plot.tag = ggtext::element_markdown(hjust = 0, size = 14)
#   ) +
#   labs(
#     title = glue::glue('<b><span style="color:{colorspace::darken(pal[3], 0.3)}">More</span></b> vs. <b><span style="color:{colorspace::darken(pal[1], 0.3)}">Less</span></b> Actions, 2020/21 Compared to 2019/20')
#   )
