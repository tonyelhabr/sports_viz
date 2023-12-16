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
    return(qs::qread(path))
  }
  Sys.sleep(runif(1, 1, 3))
  
  res <- f(conn, ...)
  qs::qsave(res, path)
  res
}

.add_season_col <- function(df, conn) {
  df |> 
    dplyr::mutate(
      season = as.integer(conn$season),
      .before = 1
    )
}

.clean_user_name <- function(x) {
  dplyr::case_when(
    x == 'Andrew ElHabr' ~ 'Andrew E.',
    x == 'Andrew Lara' ~ 'Andrew L.',
    x == 'Manuel Espinosa' ~ 'Manny',
    x == 'Juan Pineda' ~ 'JP',
    TRUE ~ gsub('\\s.*$', '', x)
  )
}

scrape_franchises <- function(conn) {
  ffscrapr::ff_franchises(conn) |> 
    .add_season_col(conn)
}

scrape_schedules <- function(conn) {
  ffscrapr::ff_schedule(conn) |> 
    .add_season_col(conn)
}

scrape_weekly_player_scores <- function(conn) {
  max_week <- ifelse(conn$season == lubridate::year(Sys.Date()), 14, 18)
  ffscrapr::ff_starters(conn, weeks = 1:max_week) |> 
    .add_season_col(conn)
}

ff_data <- purrr::map(
  SEASONS,
  \(season) {
    overwrite <- ifelse(season == max(SEASONS), TRUE, FALSE)
    
    conn <- ffscrapr::espn_connect(
      season = season,
      league_id = ESPN_LEAGUE_ID,
      espn_s2 = ESPN_S2,
      swid = SWID
    )
    
    franchises <- manage_io_operations(
      conn, 
      data_type = 'franchises',
      f = scrape_franchises,
      overwrite = overwrite
    )
    
    franchises <- franchises |> 
      dplyr::mutate(
        user_name = .clean_user_name(user_name)
      )
    
    schedules <- manage_io_operations(
      conn, 
      data_type = 'schedule',
      f = scrape_schedules,
      overwrite = overwrite
    )
    
    player_scores <- manage_io_operations(
      conn, 
      data_type = 'player',
      f = scrape_weekly_player_scores,
      overwrite = overwrite
    )

    list(
      'franchises' = franchises,
      'schedules' = schedules,
      'player_scores' = player_scores
    )
  }
)

map_dfr_ff_data <- function(ff_data, name) {
  purrr::map_dfr(
    ff_data,
    \(.x) .x[[name]]
  ) 
}

franchises <- map_dfr_ff_data(ff_data, 'franchises')
schedules <- map_dfr_ff_data(ff_data, 'schedules')
weekly_player_scores <- map_dfr_ff_data(ff_data, 'player_scores')

weekly_projected_scores <- weekly_player_scores |> 
  dplyr::filter(lineup_slot != 'BE') |> 
  dplyr::group_by(
    season,
    week,
    franchise_id
  ) |> 
  dplyr::summarize(
    projected_score = sum(projected_score)
  ) |> 
  dplyr::ungroup()

weekly_team_scores <- schedules |> 
  dplyr::select(
    season,
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
        season,
        franchise_id,
        user_name
      ),
    by = dplyr::join_by(season, franchise_id)
  ) |> 
  dplyr::inner_join(
    franchises |> 
      dplyr::select(
        season,
        opponent_id = franchise_id,
        opponent_user_name = user_name
      ),
    by = dplyr::join_by(season, opponent_id)
  ) |> 
  dplyr::left_join(
    weekly_projected_scores |> 
      dplyr::select(
        season,
        week,
        franchise_id,
        user_projected_score = projected_score
      ),
    by = dplyr::join_by(season, week, franchise_id)
  ) |> 
  dplyr::left_join(
    weekly_projected_scores |> 
      dplyr::select(
        season,
        week,
        opponent_id = franchise_id,
        opponent_projected_score = projected_score
      ),
    by = dplyr::join_by(season, week, opponent_id)
  ) |> 
  dplyr::select(
    season,
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

readr::write_csv(
  franchises,
  file.path(DATA_DIR, 'franchises-all.csv')
)

qs::qsave(
  weekly_player_scores,
  file.path(DATA_DIR, 'player-scores-all.qs')
)

readr::write_csv(
  weekly_team_scores,
  file.path(DATA_DIR, 'team-scores-all.csv')
)
