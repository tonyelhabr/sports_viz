COUNTRY <- 'USA'
GENDER <- 'M'
TIER <- '1st'
SEASON_END_YEAR <- 2024

TODAY <- Sys.Date()
DATA_DIR <- file.path(
  PROJ_DIR, 
  'data', 
  sprintf('country=%s-gender=%s-tier=%s-season_end_year=%s', COUNTRY, GENDER, TIER, SEASON_END_YEAR),
  sprintf('date=%s', TODAY)
)
dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)