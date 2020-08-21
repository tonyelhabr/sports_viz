

# ui ----
library(tidyverse)
library(hexbin)
library(jsonlite)
library(httr)
library(scales)

percent_formatter <- function(x) {
  scales::percent(x, accuracy = 1)
}

players_url <- "http://stats.nba.com/stats/commonallplayers?LeagueID=00&Season=2019-20&IsOnlyCurrentSeason=0"

request_headers <- c(
  `Connection` = "keep-alive",
  `Accept` = "application/json, text/plain, */*",
  `x-nba-stats-token` = "true",
  `X-NewRelic-ID` = "VQECWF5UChAHUlNTBwgBVw==",
  `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36",
  `x-nba-stats-origin` = "stats",
  `Sec-Fetch-Site` = "same-origin",
  `Sec-Fetch-Mode` = "cors",
  `Referer` = "https://stats.nba.com/players/leaguedashplayerbiostats/",
  `Accept-Encoding` = "gzip, deflate, br",
  `Accept-Language` = "en-US,en;q=0.9"
)
request <- GET(players_url, add_headers(request_headers))

players_data <- fromJSON(content(request, as = "text"))
players <- tbl_df(data.frame(players_data$resultSets$rowSet[[1]], stringsAsFactors = FALSE))
names(players) <- tolower(players_data$resultSets$headers[[1]])

players <- mutate(
  players,
  person_id = as.numeric(person_id),
  rosterstatus = as.logical(as.numeric(rosterstatus)),
  from_year = as.numeric(from_year),
  to_year = as.numeric(to_year),
  team_id = as.numeric(team_id)
)

if (Sys.Date() <= as.Date("2017-10-20")) {
  players <- mutate(players, to_year = pmin(to_year, 2016))
}

players$name <- sapply(players$display_last_comma_first, function(s) {
  paste(rev(strsplit(s, ", ")[[1]]), collapse = " ")
})

first_year_of_data <- 1996
last_year_of_data <- max(players$to_year)
season_strings <- paste(
  first_year_of_data:last_year_of_data,
  substr(first_year_of_data:last_year_of_data + 1, 3, 4),
  sep = "-"
)
names(season_strings) <- first_year_of_data:last_year_of_data

available_players <- filter(players, to_year >= first_year_of_data)

names_table <- table(available_players$name)
dupe_names <- names(names_table[which(names_table > 1)])

available_players$name[available_players$name %in% dupe_names] <- paste(
  available_players$name[available_players$name %in% dupe_names],
  available_players$person_id[available_players$name %in% dupe_names]
)

available_players$lower_name <- tolower(available_players$name)
available_players <- arrange(available_players, lower_name)

find_player_by_name <- function(n) {
  filter(available_players, lower_name == tolower(n))
}

find_player_id_by_name <- function(n) {
  find_player_by_name(n)$person_id
}

circle_points <- function(center = c(0, 0), radius = 1, npoints = 360) {
  angles <- seq(0, 2 * pi, length.out = npoints)
  return(data_frame(
    x = center[1] + radius * cos(angles),
    y = center[2] + radius * sin(angles)
  ))
}

width <- 50
height <- 94 / 2
key_height <- 19
inner_key_width <- 12
outer_key_width <- 16
backboard_width <- 6
backboard_offset <- 4
neck_length <- 0.5
hoop_radius <- 0.75
hoop_center_y <- backboard_offset + neck_length + hoop_radius
three_point_radius <- 23.75
three_point_side_radius <- 22
three_point_side_height <- 14

court_themes <- list(
  light = list(
    court = "floralwhite",
    lines = "#999999",
    text = "#222222",
    made = "#00bfc4",
    missed = "#f8766d",
    hex_border_size = 1,
    hex_border_color = "#000000"
  ),
  dark = list(
    court = "#000004",
    lines = "#999999",
    text = "#f0f0f0",
    made = "#00bfc4",
    missed = "#f8766d",
    hex_border_size = 0,
    hex_border_color = "#000000"
  )
)


plot_court <- function(court_theme = court_themes$light, use_short_three = FALSE) {
  if (use_short_three) {
    three_point_radius <- 22
    three_point_side_height <- 0
  }

  court_points <- data_frame(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
    y = c(height, 0, 0, height, height),
    desc = "perimeter"
  )

  court_points <- bind_rows(
    court_points,
    data_frame(
      x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
      y = c(0, key_height, key_height, 0),
      desc = "outer_key"
    )
  )

  court_points <- bind_rows(
    court_points,
    data_frame(
      x = c(-backboard_width / 2, backboard_width / 2),
      y = c(backboard_offset, backboard_offset),
      desc = "backboard"
    )
  )

  court_points <- bind_rows(
    court_points,
    data_frame(
      x = c(0, 0),
      y = c(backboard_offset, backboard_offset + neck_length),
      desc = "neck"
    )
  )

  foul_circle <- circle_points(center = c(0, key_height), radius = inner_key_width / 2)

  foul_circle_top <- filter(foul_circle, y > key_height) %>%
    mutate(desc = "foul_circle_top")

  foul_circle_bottom <- filter(foul_circle, y < key_height) %>%
    mutate(
      angle = atan((y - key_height) / x) * 180 / pi,
      angle_group = floor((angle - 5.625) / 11.25),
      desc = paste0("foul_circle_bottom_", angle_group)
    ) %>%
    filter(angle_group %% 2 == 0) %>%
    select(x, y, desc)

  hoop <- circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    mutate(desc = "hoop")

  restricted <- circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")

  three_point_circle <- circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    filter(y >= three_point_side_height, y >= hoop_center_y)

  three_point_line <- data_frame(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )

  court_points <- bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )


  court_points <- court_points

  ggplot() +
    geom_path(
      data = court_points,
      aes(x = x, y = y, group = desc),
      color = court_theme$lines
    ) +
    coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
    theme_minimal(base_size = 22) +
    theme(
      text = element_text(color = court_theme$text),
      plot.background = element_rect(fill = "floralwhite", color = "floralwhite"),
      panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
      legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}

plot_court(court_themes$light)

fetch_shots_by_player_id_and_season = function(player_id, season, season_type) {
  
  request = GET(
    "http://stats.nba.com/stats/shotchartdetail",
    query = list(
      PlayerID = player_id,
      Season = season,
      SeasonType = season_type,
      PlayerPosition = "",
      ContextMeasure = "FGA",
      DateFrom = "",
      DateTo = "",
      GameID = "",
      GameSegment = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      Period = 0,
      Position = "",
      RookieYear = "",
      SeasonSegment = "",
      TeamID = 0,
      VsConference = "",
      VsDivision = ""
    ),
    add_headers(request_headers)
  )
  
  stop_for_status(request)
  
  data = content(request)
  
  raw_shots_data = data$resultSets[[1]]$rowSet
  col_names = tolower(as.character(data$resultSets[[1]]$headers))
  
  if (length(raw_shots_data) == 0) {
    shots = data.frame(
      matrix(nrow = 0, ncol = length(col_names))
    )
  } else {
    shots = data.frame(
      matrix(
        unlist(raw_shots_data),
        ncol = length(col_names),
        byrow = TRUE
      )
    )
  }
  
  shots = tbl_df(shots)
  names(shots) = col_names
  
  shots = mutate(shots,
                 loc_x = as.numeric(as.character(loc_x)) / 10,
                 loc_y = as.numeric(as.character(loc_y)) / 10 + hoop_center_y,
                 shot_distance = as.numeric(as.character(shot_distance)),
                 shot_made_numeric = as.numeric(as.character(shot_made_flag)),
                 shot_made_flag = factor(shot_made_flag, levels = c("1", "0"), labels = c("made", "missed")),
                 shot_attempted_flag = as.numeric(as.character(shot_attempted_flag)),
                 shot_value = ifelse(tolower(shot_type) == "3pt field goal", 3, 2),
                 game_date = as.Date(game_date, format = "%Y%m%d")
  )
  
  raw_league_avg_data = data$resultSets[[2]]$rowSet
  league_avg_names = tolower(as.character(data$resultSets[[2]]$headers))
  league_averages = tbl_df(data.frame(
    matrix(unlist(raw_league_avg_data), ncol = length(league_avg_names), byrow = TRUE)
  ))
  names(league_averages) = league_avg_names
  league_averages = mutate(league_averages,
                           fga = as.numeric(as.character(fga)),
                           fgm = as.numeric(as.character(fgm)),
                           fg_pct = as.numeric(as.character(fg_pct)),
                           shot_value = ifelse(shot_zone_basic %in% c("Above the Break 3", "Backcourt", "Left Corner 3", "Right Corner 3"), 3, 2)
  )
  
  return(list(player = shots, league_averages = league_averages))
}

hex_bounds <- function(x, binwidth) {
  c(
    plyr::round_any(min(x), binwidth, floor) - 1e-6,
    plyr::round_any(max(x), binwidth, ceiling) + 1e-6
  )
}

calculate_hex_coords = function(shots, binwidths) {
  xbnds = hex_bounds(shots$loc_x, binwidths[1])
  xbins = diff(xbnds) / binwidths[1]
  ybnds = hex_bounds(shots$loc_y, binwidths[2])
  ybins = diff(ybnds) / binwidths[2]
  
  hb = hexbin(
    x = shots$loc_x,
    y = shots$loc_y,
    xbins = xbins,
    xbnds = xbnds,
    ybnds = ybnds,
    shape = ybins / xbins,
    IDs = TRUE
  )
  
  shots = mutate(shots, hexbin_id = hb@cID)
  
  hexbin_stats = shots %>%
    group_by(hexbin_id) %>%
    summarize(
      hex_attempts = n(),
      hex_pct = mean(shot_made_numeric),
      hex_points_scored = sum(shot_made_numeric * shot_value),
      hex_points_per_shot = mean(shot_made_numeric * shot_value)
    ) 
  
  hexbin_ids_to_zones = shots %>%
    group_by(hexbin_id, shot_zone_range, shot_zone_area) %>%
    summarize(attempts = n()) %>%
    ungroup() %>%
    arrange(hexbin_id, desc(attempts)) %>%
    group_by(hexbin_id) %>%
    filter(row_number() == 1) %>%
    select(hexbin_id, shot_zone_range, shot_zone_area)
  
  hexbin_stats = inner_join(hexbin_stats, hexbin_ids_to_zones, by = "hexbin_id")
  
  # from hexbin package, see: https://github.com/edzer/hexbin
  sx = hb@xbins / diff(hb@xbnds)
  sy = (hb@xbins * hb@shape) / diff(hb@ybnds)
  dx = 1 / (2 * sx)
  dy = 1 / (2 * sqrt(3) * sy)
  origin_coords = hexcoords(dx, dy)
  
  hex_centers = hcell2xy(hb)
  
  hexbin_coords = bind_rows(lapply(1:hb@ncells, function(i) {
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


calculate_hexbins_from_shots = function(shots, league_averages, binwidths, min_radius_factor, fg_diff_limits, fg_pct_limits, pps_limits) {
  if (nrow(shots) == 0) {
    return(list())
  }
  
  grouped_shots = group_by(shots, shot_zone_range, shot_zone_area)
  
  zone_stats = grouped_shots %>%
    summarize(
      zone_attempts = n(),
      zone_pct = mean(shot_made_numeric),
      zone_points_scored = sum(shot_made_numeric * shot_value),
      zone_points_per_shot = mean(shot_made_numeric * shot_value)
    )
  
  league_zone_stats = league_averages %>%
    group_by(shot_zone_range, shot_zone_area) %>%
    summarize(league_pct = sum(fgm) / sum(fga))
  
  hex_data = calculate_hex_coords(shots, binwidths = binwidths)
  
  join_keys = c("shot_zone_area", "shot_zone_range")
  
  hex_data = hex_data %>%
    inner_join(zone_stats, by = join_keys) %>%
    inner_join(league_zone_stats, by = join_keys)
  
  max_hex_attempts = max(hex_data$hex_attempts)
  
  
  hex_data = mutate(hex_data,
                    radius_factor = min_radius_factor + (1 - min_radius_factor) * log(hex_attempts + 1) / log(max_hex_attempts + 1),
                    adj_x = center_x + radius_factor * (x - center_x),
                    adj_y = center_y + radius_factor * (y - center_y),
                    bounded_fg_diff = pmin(pmax(zone_pct - league_pct, fg_diff_limits[1]), fg_diff_limits[2]),
                    bounded_fg_pct = pmin(pmax(zone_pct, fg_pct_limits[1]), fg_pct_limits[2]),
                    bounded_points_per_shot = pmin(pmax(zone_points_per_shot, pps_limits[1]), pps_limits[2]))
  
  list(hex_data = hex_data, fg_diff_limits = fg_diff_limits, fg_pct_limits = fg_pct_limits, pps_limits = pps_limits)
  
}

get_data <- function(id, seasons, season_type) {
  
  df <- fetch_shots_by_player_id_and_season(id, seasons, season_type)
  
  shots <- as.data.frame(df[1])
  league_averages <- as.data.frame(df[2])
  
  
  names(shots) <- sub(".*\\.", "", names(shots))
  names(league_averages) <- sub(".*\\.", "", names(league_averages))
  
  
  hex_data <- calculate_hexbins_from_shots(shots, league_averages,binwidths = c(1.5,1.5), min_radius_factor = .25, fg_diff_limits = c(-0.15, 0.15), fg_pct_limits = c(0.2, 0.7), pps_limits = c(0.5, 1.5))  
  
  df <- hex_data
  
  df <- as.data.frame(df[1])
  
  df$season <- seasons
  df$person_id <- id
  
  
  names(df) <- sub(".*\\.", "", names(df))
  
  return(df)
  
}

