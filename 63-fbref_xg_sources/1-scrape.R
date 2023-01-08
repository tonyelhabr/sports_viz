library(purrr)
library(worldfootballR)

dir_proj <- '63-fbref_xg_sources'
dir_data <- file.path(dir_proj, 'data')
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)
path_opta <- file.path(dir_data, 'big5_team_shooting_opta.rds')

opta <- 2018:2022 |>
  map_dfr(
    ~{
      path <- file.path(dir_data, sprintf('big5_team_shooting_opta_%s.rds', .x))
      if (file.exists(path)) {
        return(readRDS(path))
      }
      res <- worldfootballR::fb_big5_advanced_season_stats(
        season_end_year = .x,
        stat_type = 'shooting',
        team_or_player = 'team',
        time_pause = 5
      )
      saveRDS(res, path)
      res
    }
  )
saveRDS(opta, path_opta)
