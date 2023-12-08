library(ffscrapr)
library(dplyr)
library(purrr)
library(readr)

PROJ_DIR <- '76-2023_ff_luck'
DATA_DIR <- file.path(PROJ_DIR, 'data')
SEASONS <- 2018:2023
ESPN_LEAGUE_ID <- 899513
ESPN_S2 <- Sys.getenv('FF_ESPN_S2')
SWID <- Sys.getenv('FF_SWID')

## data scrape ----
scrape_espn_season <- function(season, overwrite = FALSE) {
  
  message(sprintf('Scraping scores for season = %s.', season))
  path <- file.path(DATA_DIR, paste0('team-scores-', season, '.csv'))
  if (file.exists(path) & isFALSE(overwrite)) {
    return(readr::read_csv(path))
  }
  conn <- ffscrapr::espn_connect(
    season = season,
    league_id = ESPN_LEAGUE_ID,
    espn_s2 = ESPN_S2,
    swid = SWID
  )
  
  starters <- ffscrapr::ff_starters(conn, weeks = 1:18)
  sched <- ffscrapr::ff_schedule(conn)
  franchises <- ffscrapr::ff_franchises(conn)
  
  weekly_projected_scores <- starters |> 
    dplyr::filter(lineup_slot != 'BE') |> 
    dplyr::group_by(
      week,
      franchise_id
    ) |> 
    dplyr::summarize(
      projected_score = sum(projected_score)
    )
  
  res <- sched |> 
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
      weekly_projected_scores |> 
        dplyr::select(
          week,
          franchise_id,
          user_projected_score = projected_score
        ),
      by = dplyr::join_by(week, franchise_id)
    ) |> 
    dplyr::left_join(
      weekly_projected_scores |> 
        dplyr::select(
          week,
          opponent_id = franchise_id,
          opponent_projected_score = projected_score
        ),
      by = dplyr::join_by(week, opponent_id)
    ) |> 
    dplyr::transmute(
      season = .env$season,
      week,
      user_name,
      opponent_user_name,
      user_score,
      opponent_score,
      user_projected_score,
      opponent_projected_score,
      result
    )
  readr::write_csv(res, path)
  res
}

slowly_scrape_espn_season <- purrr::slowly(
  scrape_espn_season
)

scores <- purrr::map_dfr(
  SEASONS,
  \(season) {
    overwrite <- ifelse(season == max(SEASON), TRUE, FALSE)
    slowly_scrape_espn_season(season, overwrite = overwrite)
  }
)

readr::write_csv(
  scores,
  file.path(DATA_DIR, 'team-scores-all.csv')
)
