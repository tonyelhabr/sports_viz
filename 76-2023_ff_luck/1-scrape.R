library(ffscrapr)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(qs)

PROJ_DIR <- '76-2023_ff_luck'
DATA_DIR <- file.path(PROJ_DIR, 'data')
SEASONS <- 2018:2023
ESPN_LEAGUE_ID <- 899513
ESPN_S2 <- Sys.getenv('FF_ESPN_S2')
SWID <- Sys.getenv('FF_SWID')

## data scrape ----
manage_io_operations <- function(conn, f, data_type, overwrite = FALSE, ...) {
  season <- conn$season
  message(sprintf('Scraping %s scores for season = %s.', data_type, conn$season))
  path <- file.path(DATA_DIR, paste0(data_type, '-scores-', conn$season, '.qs'))
  
  if (file.exists(path) & isFALSE(overwrite)) {
    return(qs::qread(path, show_col_types = FALSE))
  }
  Sys.sleep(runif(1, 1, 3))
  
  res <- f(conn, ...)
  qs::qsave(res, path)
  res
}

scrape_weekly_player_scores <- function(conn) {
  max_week <- ifelse(conn$season == lubridate::year(Sys.Date()), 14, 18)
  ffscrapr::ff_starters(conn, weeks = 1:max_week) |> 
    dplyr::mutate(
      season = as.integer(conn$season),
      .before = 1
    )
}

scrape_weekly_team_scores <- function(conn, player_scores) {
  
  sched <- ffscrapr::ff_schedule(conn)
  franchises <- ffscrapr::ff_franchises(conn)
  
  projected_scores <- player_scores |> 
    dplyr::filter(lineup_slot != 'BE') |> 
    dplyr::group_by(
      week,
      franchise_id
    ) |> 
    dplyr::summarize(
      projected_score = sum(projected_score)
    ) |> 
    dplyr::ungroup()
  
  sched |> 
    dplyr::select(
      week,
      franchise_id,
      user_score = franchise_score,
      opponent_id,
      opponent_score,
      result
    ) |> 
    dplyr::inner_join(
      franchises |> 
        dplyr::select(
          franchise_id,
          user_name
        ),
      by = dplyr::join_by(franchise_id)
    ) |> 
    dplyr::inner_join(
      franchises |> 
        dplyr::select(
          opponent_id = franchise_id,
          opponent_user_name = user_name
        ),
      by = dplyr::join_by(opponent_id)
    ) |> 
    dplyr::left_join(
      projected_scores |> 
        dplyr::select(
          week,
          franchise_id,
          user_projected_score = projected_score
        ),
      by = dplyr::join_by(week, franchise_id)
    ) |> 
    dplyr::left_join(
      projected_scores |> 
        dplyr::select(
          week,
          opponent_id = franchise_id,
          opponent_projected_score = projected_score
        ),
      by = dplyr::join_by(week, opponent_id)
    ) |> 
    dplyr::transmute(
      season = as.integer(conn$season),
      week,
      franchise_id,
      user_name,
      opponent_user_name,
      user_score,
      opponent_id,
      opponent_score,
      user_projected_score,
      opponent_projected_score,
      result
    )
}

weekly_scores <- purrr::map(
  SEASONS,
  \(season) {
    overwrite <- ifelse(season == max(SEASONS), TRUE, FALSE)

    conn <- ffscrapr::espn_connect(
      season = season,
      league_id = ESPN_LEAGUE_ID,
      espn_s2 = ESPN_S2,
      swid = SWID
    )
    
    player_scores <- manage_io_operations(
      conn, 
      data_type = 'player',
      f = scrape_weekly_player_scores,
      overwrite = overwrite
    )
    
    team_scores <- manage_io_operations(
      conn, 
      player_scores = player_scores, 
      data_type = 'team',
      f = scrape_weekly_team_scores,
      overwrite = overwrite
    )
    
    list(
      'player' = player_scores,
      'team' = team_scores
    )
  }
)

clean_team_name <- function(x) {
  dplyr::case_when(
    x == 'Andrew ElHabr' ~ 'Andrew E.',
    x == 'Andrew Lara' ~ 'Andrew L.',
    x == 'Manuel Espinosa' ~ 'Manny',
    x == 'Juan Pineda' ~ 'JP',
    TRUE ~ gsub('\\s.*$', '', x)
  )
}

weekly_scores |> 
  purrr::map_dfr(\(.x) .x[['team']]) |>
  dplyr::mutate(
    dplyr::across(
      c(
        user_name,
        opponent_user_name
      ),
      \(.x) clean_team_name(.x)
    )
  ) |> 
  readr::write_csv(
    file.path(DATA_DIR, 'team-scores-all.csv')
  )

weekly_scores |> 
  purrr::map_dfr(\(.x) .x[['player']]) |> 
  dplyr::mutate(
    franchise_name  = clean_team_name(franchise_name)
  ) |> 
  qs::qsave(
    file.path(DATA_DIR, 'player-scores-all.qs')
  )
