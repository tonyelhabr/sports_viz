library(tibble)
library(tidyr)
library(dplyr)
library(purrr)
library(qs)

PROJ_DIR <- '86-xgot_heatmap'

league_id_values <- list(
  'big5' = list(
    country = c(
      'ENG' = 47,
      'ESP' = 87, 
      'FRA' = 53, 
      'GER' = 54, 
      'ITA' = 55
    ),
    tier = '1st',
    gender = 'M'
  ),
  'other_1st_M' = list(
    country = c(
      'USA' = 130
    ),
    tier = '1st',
    gender = 'M'
  ),
  '2nd_M' = list(
    country = c(
      'ENG' = 50
    ),
    tier = '2nd',
    gender = 'M'
  )
)

league_ids <- league_id_values |> 
  tibble::enframe(name = 'group', value = 'value') |> 
  tidyr::unnest_wider(value) |> 
  tidyr::unnest_longer(country, values_to = 'league_id', indices_to = 'country')


load_fotmob_match_details <- function(league_id) {
  url <- sprintf(
    'https://github.com/JaseZiv/worldfootballR_data/releases/download/fotmob_match_details/%s_match_details.rds',
    league_id
  )
  readRDS(url(url))
}

match_details <- league_ids |> 
  dplyr::left_join(
    league_ids$league_id |>
      unname() |> 
      purrr::map(
        \(.x) {
          message(sprintf('Downloading match details for `league_id = "%s"`.', .x))
          load_fotmob_match_details(.x)
        }
      ) |> 
      purrr::list_rbind(), 
    by = dplyr::join_by(league_id)
  )

qs::qsave(match_details, file.path(PROJ_DIR, 'match_details.qs'))
