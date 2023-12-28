library(dplyr)
library(readr)

compared_rankings <- read_csv(
  'https://github.com/tonyelhabr/club-rankings/releases/download/club-rankings/compared-rankings.csv',
  show_col_types = FALSE
)

filt_compared_rankings <- compared_rankings |> 
  dplyr::filter(
    league_clubelo %in% c('ENG-1', 'ENG-2'),
    date >= lubridate::ymd('2023-09-04')
  ) |> 
  dplyr::select(
    date,
    league_clubelo,
    id_opta,
    team_clubelo,
    team_opta,
    rating_clubelo,
    rating_opta,
    rank_clubelo,
    rank_opta
  )

generate_logo_url <- function(id) {
  sprintf('https://omo.akamai.opta.net/image.php?secure=true&h=omo.akamai.opta.net&sport=football&entity=team&description=badges&dimensions=150&id=%s', id)
}

latest_compared_rankings <- filt_compared_rankings |> 
  dplyr::slice_max(date, n = 1)

latest_compared_rankings |>
  filter(!is.na(rank_clubelo)) |> 
  mutate(
    drank = rank_opta - rank_clubelo
  ) |> 
  arrange(desc(abs(drank)))
