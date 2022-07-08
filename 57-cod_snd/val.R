library(tidyverse)
library(DBI)
library(janitor)
library(jsonlite)

## https://www.kaggle.com/datasets/visualize25/valorant-pro-matches-full-data?resource=download
con <- dbConnect(RSQLite::SQLite(), 'c:/users/antho/downloads/valorant.sqlite')
query_db <- function(conn, statement) {
  DBI::dbGetQuery(conn = conn, statement = statement) |> 
    tibble() |> 
    janitor::clean_names()
}

games <- query_db(con, 'select * from Games')
matches <- query_db(con, 'select * from Matches')
scoreboard <- query_db(con, 'select * from Game_Scoreboard')
raw_rounds <- query_db(con, 'select * from Game_Rounds')
clean_json <- function(x) {
  x |> 
    str_replace_all("\\'", '\\"') |> 
    str_replace_all('([A-z0-9]+)[:]', '"\\1":') |> 
    fromJSON()
}
possibly_clean_json <- possibly(clean_json, otherwise = NULL)

rounds <- raw_rounds |> 
  mutate(
    round_history = map(round_history, possibly_clean_json)
  ) |> 
  unnest_longer(round_history) |> 
  unnest_wider(round_history) |> 
  clean_names() |> 
  mutate(
    round = as.integer(round_history_id),
    .before = 'round_winner'
  ) |> 
  select(-round_history_id) |> 
  filter(!is.na(round))

teams <- bind_rows(
  games |> 
    distinct(team_id = team1id, team = team1), 
  games |> 
    distinct(team_id = team2id, team = team2)
) |> 
  distinct(team_id, team) |> 
  arrange(team_id)
teams
rounds |> 
  mutate(
    across(
      score_after_round,
      list(
        pre_cumu_w = ~str_remove(.x, '-.*$') |> as.integer(),
        pre_cumu_l = ~str_remove(.x, '^.*-') |> as.integer()
      ),
      .names = '{fn}'
    )
  ) |> 
  left_join(
    teams |> select(team1id = team_id, team_1 = team)
  ) |> 
  left_join(
    teams |> select(team2id = team_id, team_2 = team)
  ) |> 
  select(
    game_id,
    team_1,
    team_2,
    pre_cumu_w,
    pre_cumu_l
  )

rounds |> 
  count(score_after_round, sort = TRUE)
