
library(tidyverse)

path_export <- here::here('data', '08_nba_playoffs_excitement_index.rds')
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
  
  seasons <- 1996:2016
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
vals %>% count(year)
idx_season_max <- vals %>% filter(year == 2020) %>% filter(idx_season == max(idx_season)) %>% pull(idx_season)

# vals %>% 
#   group_by(year) %>% 
#   mutate(
#     across(val, list(cumu = cumsum)),
#     grp = year %>% factor()
#   ) %>% 
#   ungroup()
#   ggplot() +
#   aes(x = idx_season, y = val_cumu, group = grp, color = grp) +
#   geom_step()

vals_agg <-
  vals %>% 
  filter(idx_season <= idx_season_max) %>% 
  # filter(year == 2020) %>% 
  group_by(year) %>% 
  summarize(across(val, sum)) %>% 
  ungroup() %>% 
  filter(val != 0) %>% 
  mutate(
    rnk = row_number(desc(val)),
    grp = sprintf('%04d-%02d', year - 1, year %% 100) %>% forcats::fct_reorder(., val)
  ) %>% 
  arrange(desc(val))
vals_agg

do_theme_set()
viz <-
  vals_agg %>% 
  ggplot() +
  aes(x = val, y = grp) +
  geom_col() +
  coord_cartesian(xlim = c(280, 360)) +
  geom_col(fill = 'grey20') +
  geom_text(
    data = vals_agg %>% filter(year == 2020),
    aes(label = grp),
    hjust = -0.1,
    family = 'Karla',
    size = 6,
    fontface = 'bold',
    color = 'blue'
  ) +
  geom_text(
    data = vals_agg %>% filter(year != 2020),
    aes(label = grp),
    hjust = 1.1,
    family = 'Karla',
    size = 5,
    fontface = 'bold',
    color = 'white'
  ) +
  theme(
    # axis.text.y = element_markdown(),
    axis.text.y = element_blank(),
    # plot.subtitle = element_markdown(),
    # panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = ggtext::element_markdown(size = 16),
    plot.caption = element_text(size = 12),
    plot.margin = margin(10, 10, 10, 10),
    plot.tag.position = c(.01, 0.01),
  ) +
  labs(
    title = 'This year\'s NBA playoffs have been the <b><span style="color:blue">3rd</span></b> most "exciting" (up to this point) since 1997-98',
    x = 'Total excitement index',
    # subtitle = 'NBA, 2019-20 Restart',
    tag = 'Viz: @TonyElHabr | Data: https://www.inpredictable.com/',
    caption = 'excitement = total in-game win probability change', 
    x = NULL, 
    y = NULL
  )
viz
ggsave(plot = viz, filename = here::here('plots', '2020_nba_playoffs_excitement_index_20200907.png'), width = 10.5, height = 10.5, type = 'cairo')
