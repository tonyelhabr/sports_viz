

# https://fansided.com/2014/08/29/long-take-three-point-shooting-stabilize/
# https://www.baseballprospectus.com/news/article/17659/baseball-therapy-its-a-small-sample-size-after-all/
extrafont::loadfonts(device = 'win', quiet = TRUE)
library(tidyverse)
# library(fifadb) # remotes::install_github('RobWHickman/fifadb')
library(ggsoccer)
options(readr.num_columns = 0)

# data retrieval

path_stats <- here::here('data', 'stats-tm-understat.rds')
path_shots <- here::here('data', 'shots-player-understat.rds')

stats_exists <- path_stats %>% fs::file_exists()
shots_exists <- path_shots %>% fs::file_exists()

if(!stats_exists | !shots_exists) {
  
  require(understatr)
  
  f_robustly <- function(f, pb, ..., sleep = 1L) {
    assertthat::assert_that(!missing(pb))
    f_safely <- safely(f, otherwise = tibble())
    res <- f_safely(...)$result
    Sys.sleep(sleep)
    pb$tick()
    res
  }
  
  initialize_pb <-
    function(data,
             total = nrow(data),
             format = '[:bar] :percent eta :eta\n',
             width = 80L,
             ...) {
      res <-
        progress::progress_bar$new(
          total = !!total,
          format = format,
          width = width,
          ...
        )
      res
    }
}

if(!stats_exists) {
  
  get_stats_robustly <-
    partial(
      f_robustly,
      understatr::get_team_players_stats,
      pb = tms %>% initialize_pb(),
      ... =
    )
  
  lgs_meta <- understatr::get_leagues_meta()
  # years <- lgs_meta %>% pull(year)
  # years <- 2018L:2019L
  
  get_tm_stats_robustly <-
    partial(
      f_robustly,
      understatr::get_league_teams_stats,
      pb = lgs_meta %>% initialize_pb(),
      ... =
    )
  
  tm_stats <- 
    lgs_meta %>% 
    mutate(res = map2(league_name, year, get_tm_stats_robustly)) %>% 
    select(res) %>% 
    unnest(res)
  
  tms <- tm_stats %>% distinct(league_name, year, team_name)
  
  stats_nested <-
    tms %>% 
    mutate(
      res = map2(team_name, year, get_stats_robustly)
    ) %>% 
    select(-team_name, -year) %>% 
    unnest(res)
  stats
  
  write_rds(stats, path_stats)
  
} else {
  stats <- read_rds(path_stats)
}

if(!shots_exists) {
  
  shots_by_player <-
    stats %>%
    group_by(player_name, player_id) %>% 
    summarize(shots = sum(shots)) %>% 
    ungroup() %>% 
    arrange(desc(shots))
  
  get_shots_robustly <-
    partial(
      f_robustly,
      understatr::get_player_shots,
      pb = shots_by_player %>% initialize_pb(),
      ... =
    )
  
  shots <-
    shots_by_player %>% 
    select(player_id, player_name) %>% 
    mutate(
      res = map(player_id, get_shots_robustly)
    ) %>% 
    select(res) %>% 
    unnest(res)
  
  write_rds(shots, path_shots)
} else {
  shots <- read_rds(path_shots)
}

shots <- shots %>% janitor::clean_names()
shots
shots_players <- shots %>% distinct(player, player_id)
stats_players <- stats %>% distinct(player = player_name, player_id)


shots %>% count(result)

shots_proc <-
  shots %>% 
  filter(result != 'OwnGoal') %>%
  mutate(
    # Opta coords
    x = x * 100,
    y = y * 100
  )
shots_proc


hex_bounds <- function(x, binwidth) {
  c(
    plyr::round_any(min(x), binwidth, floor) - 1e-6,
    plyr::round_any(max(x), binwidth, ceiling) + 1e-6
  )
}

calculate_hex_coords <- function(df, binwidths) {
  df <- shots_proc
  xbnds <- hex_bounds(df$x, binwidths[1])
  xbins <- diff(xbnds) / binwidths[1]
  ybnds <- hex_bounds(df$y, binwidths[2])
  ybins <- diff(ybnds) / binwidths[2]
  
  hb <- hexbin(
    x = df$x,
    y = df$y,
    xbins = xbins,
    xbnds = xbnds,
    ybnds = ybnds,
    shape = ybins / xbins,
    IDs = TRUE
  )
  
  df_hb <- mutate(df, hexbin_id = hb@cID)
  
  hexbin_stats <-
    df_hb %>%
    group_by(hexbin_id) %>%
    summarize(
      hex_attempts = n(),
      hex_pct = mean(shot_made_numeric),
      hex_points_scored = sum(shot_made_numeric * shot_value),
      hex_points_per_shot = mean(shot_made_numeric * shot_value)
    )
  
  hexbin_ids_to_zones <- df_hb %>%
    group_by(hexbin_id, shot_zone_range, shot_zone_area) %>%
    summarize(attempts = n()) %>%
    ungroup() %>%
    arrange(hexbin_id, desc(attempts)) %>%
    group_by(hexbin_id) %>%
    filter(row_number() == 1) %>%
    select(hexbin_id, shot_zone_range, shot_zone_area)
  
  hexbin_stats <- inner_join(hexbin_stats, hexbin_ids_to_zones, by = "hexbin_id")
  
  # from hexbin package, see: https://github.com/edzer/hexbin
  sx <- hb@xbins / diff(hb@xbnds)
  sy <- (hb@xbins * hb@shape) / diff(hb@ybnds)
  dx <- 1 / (2 * sx)
  dy <- 1 / (2 * sqrt(3) * sy)
  origin_coords <- hexcoords(dx, dy)
  
  hex_centers <- hcell2xy(hb)
  
  hexbin_coords <- bind_rows(lapply(1:hb@ncells, function(i) {
    data.frame(
      x = origin_coords$x + hex_centers$x[i],
      y = origin_coords$y + hex_centers$y[i],
      center_x = hex_centers$x[i],
      center_y = hex_centers$y[i],
      hexbin_id = hb@cell[i]
    )
  }))
  
  inner_join(hexbin_coords, hexbin_stats, by = "hexbin_id")
}


binwidths = c(5, 5)
hex_coords <- shots_proc %>% calculate_hex_coords(binwidths = binwidths)
hex_coords

xbnds <- hex_bounds(shots_proc$x, binwidths[1])
xbins <- diff(xbnds) / binwidths[1]
ybnds <- hex_bounds(shots_proc$y, binwidths[2])
ybins <- diff(ybnds) / binwidths[2]

viz <-
  shots %>% 
  filter(player == 'Cristiano Ronaldo') %>% 
  # head(30) %>% 
  filter(result != 'OwnGoal') %>% 
  mutate(
    x = X * 100,
    y = Y * 100
  ) %>% 
  ggplot() + 
  aes(x = x, y = y) +
  ggsoccer::annotate_pitch() +
  # scale_size(range = c(1,6)) + 
  # geom_point(aes(x = x , y = 100 - y), alpha = 0.6) +
  geom_hex() +
  theme_pitch() +
  coord_flip(xlim = c(50, 100), ylim = c(0, 100)) + 
  # facet_wrap(~ h_a, ncol = 2) + 
  # theme_fivethirtyeight() +
  theme(
    axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(), 
    text = element_text(size = 12), 
    plot.title = element_text(size = 15)
  ) + 
  labs(caption = "Data: Understat") +
  theme_pitch()
viz
