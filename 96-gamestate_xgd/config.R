COUNTRY <- 'USA'
GENDER <- 'M'
TIER <- '1st'
SEASON_END_YEAR <- 2025
ORDERED_GAMESTATE_LABELS <- c('Trailing', 'Tied', 'Leading')

TODAY <- Sys.Date()
DATA_DIR <- file.path(
  PROJ_DIR, 
  'data'
)
FBREF_DATA_DIR <- file.path(
  DATA_DIR, 
  sprintf('country=%s-gender=%s-tier=%s-season_end_year=%s', COUNTRY, GENDER, TIER, SEASON_END_YEAR),
  sprintf('date=%s', TODAY)
)

dir.create(FBREF_DATA_DIR, showWarnings = FALSE, recursive = TRUE)