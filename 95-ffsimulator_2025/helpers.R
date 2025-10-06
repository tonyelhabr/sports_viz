.tony_espn_stat_map <- function() {
  c(
    # Passing Stats
    "0" = "Each Pass Attempted",
    "1" = "Each Pass Completed",
    "2" = "Each Incomplete Pass",
    "3" = "Passing Yards",
    "4" = "TD Pass",
    "5" = "Every 5 passing yards",
    "6" = "Every 10 passing yards",
    "7" = "Every 20 passing yards",
    "8" = "Every 25 passing yards",
    "9" = "Every 50 passing yards",
    "10" = "Every 100 passing yards",
    "11" = "Every 5 pass completions",
    "12" = "Every 10 pass completions",
    "13" = "Every 5 pass incompletions",
    "14" = "Every 10 pass incompletions",
    "15" = "40+ yard TD pass bonus",
    "16" = "50+ yard TD pass bonus",
    "17" = "300-399 yard passing game",
    "18" = "400+ yard passing game",
    "19" = "2pt Passing Conversion",
    "20" = "Interceptions Thrown",
    "21" = "Passing Completion Pct",
    "22" = "Passing Yards Per Game",
    
    # Rushing Stats
    "23" = "Rushing Attempts",
    "24" = "Rushing Yards",
    "25" = "TD Rush",
    "26" = "2pt Rushing Conversion",
    "27" = "Every 5 rushing yards",
    "28" = "Every 10 rushing yards",
    "29" = "Every 20 rushing yards",
    "30" = "Every 25 rushing yards",
    "31" = "Every 50 rushing yards",
    "32" = "Every 100 rushing yards",
    "33" = "Every 5 rush attempts",
    "34" = "Every 10 rush attempts",
    "35" = "40+ yard TD rush bonus",
    "36" = "50+ yard TD rush bonus",
    "37" = "100-199 yard rushing game",
    "38" = "200+ yard rushing game",
    "39" = "Rushing Yards Per Attempt",
    "40" = "Rushing Yards Per Game",
    
    # Receiving Stats
    "41" = "Receptions",
    "42" = "Receiving Yards",
    "43" = "TD Reception",
    "44" = "2pt Receiving Conversion",
    "45" = "40+ yard TD rec bonus",
    "46" = "50+ yard TD rec bonus",
    "47" = "Every 5 receiving yards",
    "48" = "Every 10 receiving yards",
    "49" = "Every 20 receiving yards",
    "50" = "Every 25 receiving yards",
    "51" = "Every 50 receiving yards",
    "52" = "Every 100 receiving yards",
    "53" = "Each reception",
    "54" = "Every 5 receptions",
    "55" = "Every 10 receptions",
    "56" = "100-199 yard receiving game",
    "57" = "200+ yard receiving game",
    "58" = "Receiving Target",
    "59" = "Receiving Yards After Catch",
    "60" = "Receiving Yards Per Catch",
    "61" = "Receiving Yards Per Game",
    "62" = "Total 2pt Conversions",
    "63" = "Fumble Recovered for TD",
    "64" = "Sacked",
    "65" = "Passing Fumbles",
    "66" = "Rushing Fumbles",
    "67" = "Receiving Fumbles",
    "68" = "Total Fumbles",
    "69" = "Passing Fumbles Lost",
    "70" = "Rushing Fumbles Lost",
    "71" = "Receiving Fumbles Lost",
    "72" = "Total Fumbles Lost",
    "73" = "Total Turnovers",
    
    # Kicking Stats
    "74" = "FG Made (50+ yards)",
    "75" = "FG Attempted (50+ yards)",
    "76" = "FG Missed (50+ yards)",
    "77" = "FG Made (40-49 yards)",
    "78" = "FG Attempted (40-49 yards)",
    "79" = "FG Missed (40-49 yards)",
    "80" = "FG Made (0-39 yards)",
    "81" = "FG Attempted (0-39 yards)",
    "82" = "FG Missed (0-39 yards)",
    "83" = "Total FG Made",
    "84" = "Total FG Attempted",
    "85" = "Total FG Missed",
    "86" = "Each PAT Made",
    "87" = "Each PAT Attempted",
    "88" = "Each PAT Missed",
    
    # Defensive Stats
    "89" = "0 points allowed",
    "90" = "1-6 points allowed",
    "91" = "7-13 points allowed",
    "92" = "14-17 points allowed",
    "93" = "Blocked Punt or FG return for TD",
    "94" = "Fumble or INT Return for TD",
    "95" = "Each Interception",
    "96" = "Each Fumble Recovered",
    "97" = "Blocked Punt, PAT or FG",
    "98" = "Each Safety",
    "99" = "Each Sack",
    "100" = "1/2 Sack",
    "101" = "Kickoff Return TD",
    "102" = "Punt Return TD",
    "103" = "Interception Return TD",
    "104" = "Fumble Return TD",
    "105" = "Total Return TD",
    "106" = "Each Fumble Forced",
    "107" = "Assisted Tackles",
    "108" = "Solo Tackles",
    "109" = "Total Tackles",
    "110" = "Every 3 Total Tackles",
    "111" = "Every 5 Total Tackles",
    "112" = "Stuffs",
    "113" = "Passes Defensed",
    "114" = "Kickoff Return Yards",
    "115" = "Punt Return Yards",
    "116" = "Every 10 kickoff return yards",
    "117" = "Every 25 kickoff return yards",
    "118" = "Every 10 punt return yards",
    "119" = "Every 25 punt return yards",
    "120" = "Points Allowed",
    "121" = "18-21 points allowed",
    "122" = "22-27 points allowed",
    "123" = "28-34 points allowed",
    "124" = "35-45 points allowed",
    "125" = "46+ points allowed",
    "126" = "Points Allowed Per Game",
    "127" = "Yards Allowed",
    "128" = "Less than 100 total yards allowed",
    "129" = "100-199 total yards allowed",
    "130" = "200-299 total yards allowed",
    "131" = "300-349 total yards allowed",
    "132" = "350-399 total yards allowed",
    "133" = "400-449 total yards allowed",
    "134" = "450-499 total yards allowed",
    "135" = "500-549 total yards allowed",
    "136" = "550+ total yards allowed",
    "137" = "Yards Allowed Per Game",
    
    # Punter Stats
    "138" = "Net Punts",
    "139" = "Punt Yards",
    "140" = "Punts Inside the 10",
    "141" = "Punts Inside the 20",
    "142" = "Blocked Punts",
    "143" = "Punts Returned",
    "144" = "Punt Return Yards",
    "145" = "Touchbacks",
    "146" = "Fair Catches",
    "147" = "Punt Average",
    "148" = "Punt Average 44.0+",
    "149" = "Punt Average 42.0-43.9",
    "150" = "Punt Average 40.0-41.9",
    "151" = "Punt Average 38.0-39.9",
    "152" = "Punt Average 36.0-37.9",
    "153" = "Punt Average 34.0-35.9",
    "154" = "Punt Average 33.9 or less",
    
    # Head Coach Stats
    "155" = "Team Win",
    "156" = "Team Loss",
    "157" = "Team Tie",
    "158" = "Points Scored",
    "159" = "Points Scored Per Game",
    "160" = "Margin of Victory",
    "161" = "25+ point Win Margin",
    "162" = "20-24 point Win Margin",
    "163" = "15-19 point Win Margin",
    "164" = "10-14 point Win Margin",
    "165" = "5-9 point Win Margin",
    "166" = "1-4 point Win Margin",
    "167" = "1-4 point Loss Margin",
    "168" = "5-9 point Loss Margin",
    "169" = "10-14 point Loss Margin",
    "170" = "15-19 point Loss Margin",
    "171" = "20-24 point Loss Margin",
    "172" = "25+ point Loss Margin",
    "173" = "Margin of Victory Per Game",
    "174" = "Winning Pct",
    
    # Additional TD Bonuses
    "175" = "0-9 yd TD pass bonus",
    "176" = "10-19 yd TD pass bonus",
    "177" = "20-29 yd TD pass bonus",
    "178" = "30-39 yd TD pass bonus",
    "179" = "0-9 yd TD rush bonus",
    "180" = "10-19 yd TD rush bonus",
    "181" = "20-29 yd TD rush bonus",
    "182" = "30-39 yd TD rush bonus",
    "183" = "0-9 yd TD rec bonus",
    "184" = "10-19 yd TD rec bonus",
    "185" = "20-29 yd TD rec bonus",
    "186" = "30-39 yd TD rec bonus",
    "187" = "D/ST Points Allowed",
    "188" = "D/ST 0 points allowed",
    "189" = "D/ST 1-6 points allowed",
    "190" = "D/ST 7-13 points allowed",
    "191" = "D/ST 14-17 points allowed",
    "192" = "D/ST 18-21 points allowed",
    "193" = "D/ST 22-27 points allowed",
    "194" = "D/ST 28-34 points allowed",
    "195" = "D/ST 35-45 points allowed",
    "196" = "D/ST 46+ points allowed",
    "197" = "D/ST Points Allowed Per Game",
    
    # Additional Field Goal Stats
    "198" = "FG Made (50-59 yards)",
    "199" = "FG Attempted (50-59 yards)",
    "200" = "FG Missed (50-59 yards)",
    "201" = "FG Made (60+ yards)",
    "202" = "FG Attempted (60+ yards)",
    "203" = "FG Missed (60+ yards)",
    
    # Special Conversions and Safeties
    "204" = "Offensive 2pt Return",
    "205" = "Defensive 2pt Return",
    "206" = "2pt Return",
    "207" = "Offensive 1pt Safety",
    "208" = "Defensive 1pt Safety",
    "209" = "1pt Safety",
    
    # Additional Stats
    "210" = "Games Played",
    "211" = "Passing First Down",
    "212" = "Rushing First Down",
    "213" = "Receiving First Down",
    "214" = "FG Made Yards",
    "215" = "FG Missed Yards",
    "216" = "FG Attempt Yards",
    "217" = "Every 5 FG Made yards",
    "218" = "Every 10 FG Made yards",
    "219" = "Every 20 FG Made yards",
    "220" = "Every 25 FG Made yards",
    "221" = "Every 50 FG Made yards",
    "222" = "Every 100 FG Made yards",
    "223" = "Every 5 FG Missed yards",
    "224" = "Every 10 FG Missed yards",
    "225" = "Every 20 FG Missed yards",
    "226" = "Every 25 FG Missed yards",
    "227" = "Every 50 FG Missed yards",
    "228" = "Every 100 FG Missed yards",
    "229" = "Every 5 FG Attempt yards",
    "230" = "Every 10 FG Attempt yards",
    "231" = "Every 20 FG Attempt yards",
    "232" = "Every 25 FG Attempt yards",
    "233" = "Every 50 FG Attempt yards",
    "234" = "Every 100 FG Attempt yards"
  )
}

RAW_STAT_TYPE_MAPPING <- jsonlite::read_json(file.path(PROJ_DIR, 'main.json')) |>
  tibble::enframe() |> 
  tidyr::unnest_wider(value)
STAT_TYPE_MAPPING <- RAW_STAT_TYPE_MAPPING |> 
  select(id, description) |> 
  tibble::deframe()

# replacement for https://github.com/ffverse/ffscrapr/blob/main/R/espn_scoring.R
ff_scoring.tony_espn_conn <- function(conn) {
  scoring_rules_init <-
    espn_getendpoint(conn, view = "mSettings") %>%
    purrr::pluck("content", "settings", "scoringSettings", "scoringItems") %>%
    tibble::tibble() %>%
    tidyr::unnest_wider(1)
  
  scoring_rules <- scoring_rules_init %>%
    dplyr::mutate(stat = .tony_espn_stat_map()[as.character(.data$statId)] %>% unname())

  overrides <-
    scoring_rules %>%
    dplyr::mutate(
      override_pos = purrr::map(.data$pointsOverrides, names)
    ) |> 
    tidyr::unnest_longer(override_pos) |> 
    dplyr::mutate(
      pos = ffscrapr:::.espn_pos_map()[as.character(.data$override_pos)], # %>% unname(),
      points = purrr::map_dbl(.data$pointsOverrides, purrr::pluck, 1, .default = NA_real_)
    ) %>%
    tidyr::unnest("override_pos") %>%
    dplyr::filter(!is.na(.data$points)) %>%
    dplyr::select(
      "pos",
      "points",
      "stat_id" = "statId",
      "stat_name" = "stat"
    )
  
  main_stats <-
    scoring_rules %>%
    tidyr::expand_grid(pos = c("QB", "RB", "WR", "TE", "K", "P", "DT", "DE", "LB", "CB", "S", "HC", "DST")) %>%
    dplyr::select(
      "pos",
      "points",
      "stat_id" = "statId",
      "stat_name" = "stat"
    ) %>%
    dplyr::anti_join(overrides, by = c("pos", "stat_id")) %>%
    dplyr::bind_rows(overrides)
  
  return(main_stats)
}


NFLFASTR_STAT_MAPPING_ESPN <- tibble::tibble(
  nflfastr_event = c(
    # Passing stats
    "interceptions",
    "passing_2pt_conversions", 
    "passing_tds",
    "passing_yards",
    "passing_yards_25",
    "passing_first_downs",
    "attempts",
    "completions",
    "sacks",
    "sack_yards",
    
    # Receiving stats
    "receiving_2pt_conversions",
    "receiving_fumbles_lost",
    "receiving_tds", 
    "receiving_yards",
    "receiving_yards_10",
    "receptions",
    "receiving_first_downs",
    "receiving_fumbles",
    "targets",
    
    # Rushing stats
    "rushing_2pt_conversions",
    "rushing_fumbles_lost", 
    "rushing_tds",
    "rushing_yards",
    "rushing_yards_10",
    "rushing_first_downs",
    "carries",
    "rushing_fumbles",
    
    # Sack fumbles
    "sack_fumbles_lost",
    "sack_fumbles",
    
    # Special teams
    "special_teams_tds",
    
    # Field goals - made
    "fg_made",
    "fg_made_0_19",
    "fg_made_20_29", 
    "fg_made_30_39",
    "fg_made_40_49",
    "fg_made_50_59",
    "fg_made_60_",
    "fg_made_distance",
    
    # Field goals - missed
    "fg_missed",
    "fg_missed_0_19",
    "fg_missed_20_29",
    "fg_missed_30_39", 
    "fg_missed_40_49",
    "fg_missed_50_59",
    "fg_missed_60_",
    "fg_missed_distance",
    
    # Field goals - other
    "fg_att",
    "fg_pct",
    "fg_blocked",
    
    # Extra points
    "pat_made",
    "pat_missed", 
    "pat_att",
    "pat_pct",
    "pat_blocked"
  ),
  
  ff_event = c(
    # Passing stats
    "Interceptions Thrown",
    "2pt Passing Conversion",
    "TD Pass", 
    "Passing Yards",
    "Every 25 passing yards",
    "Passing First Down",
    "Each Pass Attempted",
    "Each Pass Completed",
    "Sacked",
    NA, # No direct mapping for sack yards
    
    # Receiving stats
    "2pt Receiving Conversion",
    "Total Fumbles Lost", # Receiving fumbles lost maps to general fumbles lost
    "TD Reception",
    "Receiving Yards", 
    "Every 10 receiving yards",
    "Receptions",
    "Receiving First Down",
    "Receiving Fumbles",
    "Receiving Target",
    
    # Rushing stats  
    "2pt Rushing Conversion",
    "Total Fumbles Lost", # Rushing fumbles lost maps to general fumbles lost
    "TD Rush",
    "Rushing Yards",
    "Every 10 rushing yards", 
    "Rushing First Down",
    "Rushing Attempts",
    "Rushing Fumbles",
    
    # Sack fumbles
    "Total Fumbles Lost", # Sack fumbles lost maps to general fumbles lost  
    NA, # No direct mapping for sack fumbles
    
    # Special teams
    "Total Return TD", # Special teams TDs map to return TDs
    
    # Field goals - made
    "Total FG Made",
    "FG Made (0-39 yards)", # 0-19 maps to under 40
    "FG Made (0-39 yards)", # 20-29 maps to under 40
    "FG Made (0-39 yards)", # 30-39 maps to under 40
    "FG Made (40-49 yards)",
    "FG Made (50-59 yards)",
    "FG Made (60+ yards)",
    "FG Made Yards",
    
    # Field goals - missed
    "Total FG Missed", 
    "FG Missed (0-39 yards)", # 0-19 maps to under 40
    "FG Missed (0-39 yards)", # 20-29 maps to under 40
    "FG Missed (0-39 yards)", # 30-39 maps to under 40
    "FG Missed (40-49 yards)",
    "FG Missed (50-59 yards)", 
    "FG Missed (60+ yards)",
    "FG Missed Yards",
    
    # Field goals - other
    "Total FG Attempted",
    NA, # No direct mapping for FG percentage
    NA, # No direct mapping for blocked FGs in scoring
    
    # Extra points
    "Each PAT Made",
    "Each PAT Missed",
    "Each PAT Attempted", 
    NA, # No direct mapping for PAT percentage
    NA  # No direct mapping for blocked PATs in scoring
  )
) |> 
  dplyr::filter(!is.na(ff_event)) |> 
  dplyr::transmute(
    nflfastr_event,
    platform = 'espn',
    ff_event
  )

ff_scoringhistory.tony_espn_conn <- function(conn, season = 1999:nflreadr::most_recent_season(), ...) {
  checkmate::assert_numeric(season, lower = 1999, upper = as.integer(format(Sys.Date(), "%Y")))

  league_rules <-
    ff_scoring(conn) %>%
    # ff_scoring.tony_espn_conn(conn) %>%
    dplyr::left_join(
      # ffscrapr::nflfastr_stat_mapping %>% 
      #   dplyr::filter(.data$platform == "espn") %>%
      NFLFASTR_STAT_MAPPING_ESPN %>%
        dplyr::filter(!is.na(.data$ff_event)) %>%
        dplyr::as_tibble() %>%
        dplyr::select(
          "nflfastr_event",
          "platform",
          "ff_event"
        ),
      by = c("stat_name" = "ff_event"),
      relationship = "many-to-many"
    ) |> 
    dplyr::filter(!is.na(.data$nflfastr_event))
  
  ros <- ffscrapr:::.nflverse_roster(season)

  ps <- ffscrapr:::.nflverse_player_stats_long(season)
  
  if("K" %in% league_rules$pos){
    ps <- dplyr::bind_rows(
      ps,
      ffscrapr:::.nflverse_kicking_long(season))
  }
  
  if(any(c("Every 25 passing yards","Every 10 rushing yards","Every 10 receiving yards") %in% league_rules$stat_name)){
    ps <- ffscrapr:::.espn_threshold_scoring(ps)
  }
  
  res <- ros %>%
    dplyr::inner_join(
      ps,
      by = c("gsis_id"="player_id","season"),
      relationship = "many-to-many"
    ) %>%
    dplyr::inner_join(
      league_rules,
      by = c("metric"="nflfastr_event","pos"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(points = .data$value * .data$points) %>%
    dplyr::group_by(.data$season, .data$week, .data$gsis_id, .data$sportradar_id) %>%
    dplyr::mutate(points = round(sum(.data$points, na.rm = TRUE), 2)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      id_cols = c("season", "week", "gsis_id", "sportradar_id", "espn_id", "player_name", "pos", "team", "points"),
      names_from = "metric",
      values_from = "value",
      values_fill = 0,
      values_fn = max
    )

  return(res)
}
