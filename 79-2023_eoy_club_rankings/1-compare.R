library(dplyr)
library(readr)

COUNTRIES <- c('ENG', 'ESP', 'GER', 'FRA', 'ITA')
compared_rankings <- read_csv(
  'https://github.com/tonyelhabr/club-rankings/releases/download/club-rankings/compared-rankings.csv',
  show_col_types = FALSE
)

filt_compared_rankings <- compared_rankings |> 
  filter(
    # country == 'ENG',
    country %in% COUNTRIES,
    date >= lubridate::ymd('2023-09-04')
  ) |> 
  select(
    date,
    country,
    id_opta,
    team_clubelo,
    team_opta,
    rating_clubelo,
    rating_opta,
    rank_clubelo,
    rank_opta
  )

latest_compared_rankings <- filt_compared_rankings |> 
  dplyr::slice_max(date, n = 1)

mapping <- read_csv('https://raw.githubusercontent.com/tonyelhabr/club-rankings/main/team-mapping.csv') |> 
  filter(n_opta == 1, country %in% COUNTRIES) |>
  select(
    country,
    id_opta,
    team_opta,
    team_clubelo
  )
c(
  'Arsenal',
  'Atletico',
  'Barcelona',
  'Dortmund',
  ## 'Porto'
  'Inter'
)

clubelo_rankings <- read_csv(
  'https://github.com/tonyelhabr/club-rankings/releases/download/club-rankings/clubelo-club-rankings.csv',
  show_col_types = FALSE
) |> 
  filter(
    Country %in% COUNTRIES
  ) |> 
  select(
    team_clubelo = Club,
    rating_clubelo = Elo,
    country = Country,
    level = Level,
    date,
    updated_at
  ) |> 
  group_by(date, updated_at) |> 
  mutate(
    rank_clubelo = row_number(desc(rating_clubelo)),
    .before = 1
  ) |> 
  ungroup() |> 
  group_by(date) |> 
  slice_max(updated_at, n = 1, with_ties = TRUE) |> 
  ungroup() |> 
  select(-updated_at)

latest_clubelo_rankings <- clubelo_rankings |> 
  slice_max(date) |> 
  filter(level == 1) |> 
  distinct(country, team_clubelo)
latest_clubelo_rankings |> count(country, level)

generate_logo_url <- function(id) {
  sprintf('https://omo.akamai.opta.net/image.php?secure=true&h=omo.akamai.opta.net&sport=football&entity=team&description=badges&dimensions=150&id=%s', id)
}

## Verona is missing??
compared_rankings |> filter(team_clubelo == 'Verona')

latest_compared_rankings |>
  semi_join(
    latest_clubelo_rankings,
    by = join_by(country, team_clubelo)
  ) |> 
  mutate(
    rerank_opta = row_number(rank_opta),
    rerank_clubelo = row_number(rank_clubelo),
    drank = rank_opta - rank_clubelo,
    redrank = rerank_opta - rerank_clubelo
  ) |> 
  arrange(desc(abs(redrank)))

## TODO: Do this for UCL teams
