library(ffscrapr)
library(dplyr)
library(purrr)
library(cli)
library(qs)

ESPN_LEAGUE_ID <- 899513
ESPN_S2 <- Sys.getenv('FF_ESPN_S2')
SWID <- Sys.getenv('FF_SWID')
PROJ_DIR <- '76-2023_ff_luck'

## data scrape ----
scrape_espn_season <- function(season) {
  cli::cli_inform('Scraping scores for season = {season}.')
  conn <- ffscrapr::espn_connect(
    season = season,
    league_id = ESPN_LEAGUE_ID,
    espn_s2 = ESPN_S2,
    swid = SWID
  )
  
  sched <- ffscrapr::ff_schedule(conn)
  franchises <- ffscrapr::ff_franchises(conn)
  
  sched |> 
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
    dplyr::transmute(
      season = .env$season,
      week,
      user_name,
      opponent_user_name,
      user_score = franchise_score,
      opponent_score,
      result
    )
}

slowly_scrape_espn_season <- purrr::slowly(
  scrape_espn_season
)

SEASONS <- 2019:2023
scores <- purrr::map_dfr(
  SEASONS,
  slowly_scrape_espn_season
)

qs::qsave(
  scores,
  file.path(PROJ_DIR, 'seasons.qs')
)
