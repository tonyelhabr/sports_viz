library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(ggplot2)

CLUBELO_TEAMS <- c(
  'Arsenal',
  'Atletico',
  'Barcelona',
  'Bayern',
  'FC Kobenhavn',
  'Dortmund',
  'Porto',
  'Inter',
  'Lazio',
  'RB Leipzig',
  'Man City',
  'Napoli',
  'Paris SG',
  'PSV',
  'Real Madrid',
  'Sociedad'
)

generate_logo_url <- function(id) {
  sprintf('https://omo.akamai.opta.net/image.php?secure=true&h=omo.akamai.opta.net&sport=football&entity=team&description=badges&dimensions=150&id=%s', id)
}

compared_rankings <- read_csv(
  'https://github.com/tonyelhabr/club-rankings/releases/download/club-rankings/compared-rankings.csv',
  show_col_types = FALSE
)

daily_compared_rankings <- compared_rankings |> 
  filter(
    team_clubelo %in% CLUBELO_TEAMS,
    date >= ymd('2023-09-04')
  ) |> 
  transmute(
    date,
    # logo_url = generate_logo_url(id_opta),
    id_opta,
    team = ifelse(team_clubelo == 'Inter', team_clubelo, team_opta),
    rating_clubelo,
    rating_opta,
    rank_clubelo,
    rank_opta
  )

daily_compared_rankings |> 
  ggplot() +
  aes(
    x = date,
    y = rating_opta,
    color = team
  ) +
  geom_point() +
  geom_step()

filt_compared_rankings <- daily_compared_rankings |> 
  group_by(team) |> 
  arrange(date, .by_group = TRUE) |> 
  mutate(
    changed = round((rating_clubelo - dplyr::lag(rating_clubelo, n = 1, default = -1)), 2) != 0
  ) |> 
  ungroup() |> 
  filter(changed) |> 
  arrange(date, team) |> 
  transmute(
    date,
    id_opta,
    team,
    `Club Elo` = rank_clubelo,
    `Opta` = rank_opta
  )

filt_compared_rankings |> 
  pivot_longer(
    c(`Club Elo`, `Opta`),
    names_to = 'source',
    values_to = 'rank'
  ) |> 
  ggplot() +
  aes(
    x = date,
    y = rank,
    color = source
  ) +
  geom_point() +
  geom_step() +
  facet_wrap(~team, scales = 'free_y')

latest_compared_rankings <- filt_compared_rankings |> 
  dplyr::slice_max(date, n = 1)


mapping <- read_csv('https://raw.githubusercontent.com/tonyelhabr/club-rankings/main/team-mapping.csv') |> 
  filter(n_opta == 1) |>
  select(
    country,
    id_opta,
    team_opta,
    team_clubelo
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
