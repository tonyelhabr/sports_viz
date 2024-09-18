

PROJ_DIR <- '86-xgot_heatmap'
raw_match_details <- qs::qread( file.path(PROJ_DIR, 'match_details.qs'))

convert_ogs_y_to_z_g <- function(y) {
  y * 2.32 / 0.613
}

match_details <- raw_match_details |> 
  dplyr::transmute(
    match_id,
    season = parent_league_season,
    time = strptime(match_time_utc, format = '%a, %b %d, %Y, %H:%M UTC', tz = 'UTC') %>% lubridate::ymd_hms(),
    date = lubridate::date(time),
    
    home_id = home_team_id,
    away_id = away_team_id,
    home_team = home_team,
    away_team = away_team,
    id,
    # team_id,
    # team,
    player_id,
    player_name,
    period,
    min,
    min_added,
    situation,
    event_type,
    shot_type,
    
    x,
    y,
    x_b = blocked_x,
    y_b = blocked_y,
    y_g = goal_crossed_y,
    z_g = goal_crossed_z,
    
    xg = expected_goals,
    xgot = expected_goals_on_target,
    
    on_goal_shot_x,
    on_goal_shot_y,
    on_goal_shot_zoom_ratio,
    
    is_blocked,
    is_on_target,
    is_own_goal,
    is_saved_off_line,
    is_from_inside_box

  ) |> 
  dplyr::mutate(
    is_g = event_type == 'Goal',
    is_penalty = (xg == 0.7884 & round(x) == 94 & round(y) == 34),
    ## ASA tiers
    xg_tier = case_when(
      is_penalty ~ NA_character_,
      is.na(xg) ~ NA_character_,
      xg >= 0.33 ~ 'great',
      xg >= 0.15 ~ 'good',
      xg >= 0.05 ~ 'average',
      TRUE ~ 'poor'
    ) %>% 
      ordered(c('great', 'good', 'average', 'poor', NA_character_)),
    xg_diff = xgot - xg,
    z_g = dplyr::case_when(
        !is.na(xgot) ~ dplyr::coalesce(z_g, convert_ogs_y_to_z_g(on_goal_shot_y)),
        TRUE ~ z_g
      )
    )
  )
