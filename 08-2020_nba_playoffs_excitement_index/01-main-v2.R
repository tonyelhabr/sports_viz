
library(tidyverse)

dir_proj <- fs::path('08-2020_nba_playoffs_excitement_index')
dir_data <- fs::path(dir_proj, 'data')
fs::dir_create(dir_data)
path_export <- fs::path(dir_data, 'nba_playoffs_excitement_index_2.rds')

path_gif <- fs::path(dir_proj, '2020_nba_playoffs_excitement_index_20201010.gif')
n_sec <- 20
fps <- 20
n_sec_end <- 3
height <- 600
width <- 900
n_frame <- (n_sec + n_sec_end) * fps # 150

if(!fs::file_exists(path_export)) {
  host <- 'http://stats.inpredictable.com/'
  sess <- host %>% polite::bow()
  sess
  
  # https://adv-r.hadley.nz/function-operators.html
  delay_by <- function(f, amount = 5) {
    force(f)
    force(amount)
    
    function(...) {
      Sys.sleep(amount)
      f(...)
    }
  }
  
  retrieve_excitement_index <- function(date, id_game, verbose = TRUE) {
    assertthat::is.count(date)
    assertthat::is.number(id_game)
    year <- date %>% lubridate::year()
    month <- date %>% lubridate::month()
    if(verbose) {
      x <- glue::glue('Retrieving excitement index for `date = "{date}"` and `id_game = "{id_game}"`')
      cli::cat_line(x)
    }
    headers <- c(
      `Connection` = 'close',
      `Accept` = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
      `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.83 Safari/537.36',
      `Referer` = 'http://stats.inpredictable.com/nba/wpBox.php', # ?season=2019&month=09&date=2020-09-05&gid=0041900232',
      `Accept-Encoding` = 'gzip, deflate',
      `Accept-Language` = 'en-US,en;q=0.9'
    )
    
    # url <- 'http://stats.inpredictable.com/nba/wpBox.php?season=2019&month=09&date=2020-09-04&gid=0041900203'
    url <- 'http://stats.inpredictable.com/nba/wpBox.php'
    q <- list(season = as.character(year - 1), month = sprintf('%02d', month), date = strftime(date, '%Y-%m-%d'), gid = paste0('00', id_game))
    req <- url %>% httr::GET(httr::add_headers(headers), query = q)
    httr::stop_for_status(req)
    cont <- req %>% httr::content(encoding = 'UTF-8')
    node <- cont %>% rvest::html_nodes(xpath = '//*[@id="score"]/table/tbody/tr[1]/td[5]')
    assertthat::assert_that(length(node) == 1L)
    val <- node %>% rvest::html_text() %>% str_remove_all('\\s.*$') %>% as.double()
    val
  }
  
  # seasons <- 1996:2016
  seasons <- 2020
  f_q <- quietly(nbastatR::game_logs)
  logs <- 
    seasons %>% 
    map_dfr(
      ~f_q(.x, result_types = 'team', season_types = 'Playoffs', assign_to_environment = FALSE) %>% pluck('result')
    ) %>% 
    janitor::clean_names() 
  logs
  
  logs_slim <-
    logs %>% 
    filter(slug_team == slug_team_winner) %>% 
    select(year = year_season, date = date_game, id_game, tm_w = slug_team_winner, tm_l = slug_team_loser, slug_matchup) %>% 
    filter(date >= lubridate::ymd('20200907')) %>% 
    group_by(year) %>% 
    mutate(idx_season = row_number(date)) %>% 
    ungroup()
  logs_slim
  
  f_s <- safely(retrieve_excitement_index, otherwise = NA_real_)
  f_ss <- delay_by(f_s)
  vals <-
    logs_slim %>% 
    mutate(val = map2_dbl(date, id_game, ~f_ss(..1, ..2) %>% purrr::pluck('result')))
  vals
  fs::dir_create(dirname(path_export))
  write_rds(vals, path_export)
} else {
  vals <- read_rds(path_export)
}
vals

vals_1 <- path_export %>% str_remove('_2') %>% read_rds()
vals_2 <- path_export %>% read_rds()
# vals_1 %>% tail()
# vals_2 %>% head()

vals <- 
  bind_rows(vals_1, vals_2) %>% 
  group_by(year) %>% 
  mutate(
    idx_season = row_number(date)
  ) %>% 
  ungroup() 

season_idx_combos <-
  crossing(
    vals %>% distinct(year),
    vals %>% distinct(idx_season)
  )

idx_season_max <- 
  vals %>% 
  filter(year == 2020) %>% 
  filter(idx_season == max(idx_season)) %>% 
  pull(idx_season)
idx_season_max

vals_proc <-
  vals %>% 
  full_join(season_idx_combos) %>% 
  # filter(idx_season <= idx_season_max) %>% 
  replace_na(list(val = 0)) %>% 
  arrange(year, idx_season) %>% 
  group_by(year) %>% 
  mutate(val_cumu = cumsum(val)) %>% 
  ungroup() %>% 
  mutate(grp = sprintf('%04d-%02d', year - 1, year %% 100)) %>% 
  group_by(idx_season) %>% 
  mutate(
    rnk = row_number(desc(val_cumu))
  ) %>% 
  ungroup()
vals_proc

res_best <-
  seq.int(1L, 27L, by = 3) %>% 
  tibble(n_top = .) %>% 
  mutate(
    rnk =
      map_int(
        n_top,
        ~vals_proc %>% 
          group_by(year) %>% 
          # slice_max(n = 5, order_by = desc(val)) %>% 
          # arrange(desc(val), .by_group = TRUE) %>% 
          filter(row_number(desc(val)) <= .x) %>% 
          ungroup() %>% 
          group_by(year) %>% 
          summarize(across(val, sum)) %>% 
          ungroup() %>% 
          mutate(rnk = row_number(desc(val))) %>% 
          filter(year == 2020) %>% 
          pull(rnk)
      )
  )
res_best

vals_proc_filt <- 
  vals_proc %>% 
  # mutate(keep = idx_season %% 5L == 0 & rnk <= 15L) %>% 
  filter(year != 1996) %>% 
  mutate(keep = idx_season %% 4L == 0 | idx_season == max(idx_season)) %>% 
  filter(keep)
vals_proc_filt
vals_proc_filt %>% filter(idx_season == 89L) %>% arrange(desc(val_cumu))
vals_proc_filt %>% filter(idx_season == 80) # idx_season_max)
vals_proc %>% filter(idx_season > 82L) %>% count(year)
# vals_proc %>% filter(year == 2020L) %>% arrange(-rnk)

do_theme_set()
viz <-
  vals_proc_filt %>% 
  ggplot() +
  aes(y = -rnk, group = grp) +
  # geom_col(fill = 'grey20') +
  geom_tile(
    data = vals_proc_filt %>% filter(year != 2020),
    aes(x = val_cumu / 2, width = val_cumu, height = 0.9, fill = year), color = NA # , fill = 'grey20'
  ) +
  scale_fill_gradient(low = 'grey70', high = 'grey20') + # palette = 'Greys') +
  guides(fill = FALSE) +
  geom_tile(
    data = vals_proc_filt %>% filter(year == 2020),
    aes(x = val_cumu / 2, width = val_cumu, height = 0.9), color = NA, fill = 'blue'
  ) +
  geom_text(
    aes(x = val_cumu - 1, label = grp),
    hjust = 1.1,
    family = 'Karla',
    size = 5,
    fontface = 'bold',
    color = 'white'
  ) +
  # gganimate::transition_states(idx_season, transition_length = 4, state_length = 1) +
  theme(
    # axis.text.y = element_markdown(),
    axis.text.y = element_blank(),
    # plot.subtitle = element_markdown(),
    # panel.grid.major.x = element_blank(),
    plot.caption = element_text(size = 12),
    # plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0),
    panel.grid.major.y = element_blank(),
    plot.title = ggtext::element_markdown(size = 16),
    plot.margin = margin(10, 10, 10, 10),
    plot.tag.position = c(.01, 0.01),
  ) +
  # coord_cartesian(clip = 'off', expand = FALSE) +
  labs(
    title = 'Excitement of <b><span style="color:blue">this year\'s<span></b> NBA playoffs compared to playoffs since 1997-98',
    x = 'Total excitement index',
    # subtitle = 'NBA, 2019-20 Restart',
    subtitle = 'After {closest_state} games',
    # subtitle = 'After {idx_season} games',
    tag = 'Viz: @TonyElHabr | Data: https://www.inpredictable.com/',
    caption = 'Excitement index: total in-game win probability change', 
    x = NULL, 
    y = NULL
  )
viz
# ggsave(plot = viz, filename = fs::path(dir_proj, '2020_nba_playoffs_excitement_index_20201007.png'), width = 10.5, height = 10.5, type = 'cairo')
viz_anim <-
  viz +
  gganimate::transition_states(idx_season, transition_length = 4, state_length = 0.1, wrap = FALSE) +
  gganimate::view_follow(fixed_x = FALSE, fixed_y = FALSE)
# viz_anim

gganimate::animate(
  viz_anim,
  nframe = n_frame,
  fps = fps,
  height = height,
  width = width,
  renderer = gganimate::gifski_renderer(path_gif),
  end_pause = n_sec_end * fps
)

