library(httr)
library(jsonlite)
library(dplyr)
library(tibble)
library(tidyr)
library(janitor)

## There is a way to get team logos from FBref, but they have a white background
##   by default, and making the background transparent for a plot with a dark
##   background is kind of a pain in the ass. So let's pull images from fotmob.
## This function is basically a minified version of what used to exist as
##   worldfootballR::fotmob_get_league_tables(). I rely on FBref and fotmob listing
##   teams in the same order alphabetically, which works fine for the MLS. A
##   better, scalable strategy for binding team names between sources is to
##   order teams by points / placement in the standings.

get_fotmob_standings <- function(leauge_id = 130) {
  url <- paste0('https://www.fotmob.com/api/leagues?id=', league_id)
  resp <- httr::GET(url)
  cont <- httr::content(resp, as = 'text')
  result <- jsonlite::fromJSON(cont)
  table_init <- result$table$data
  tables <- dplyr::bind_rows(table_init$tables)
  tables$table$all[[3]] |>
    dplyr::transmute(
      team = name,
      team_id = id,
      pts,
      logo_url = sprintf('https://images.fotmob.com/image_resources/logo/teamlogo/%s.png', team_id)
    ) |>
    tibble::as_tibble()
}

fotmob_standings <- get_fotmob_standings()
qs::qsave(fotmob_standings, file.path(DATA_DIR, 'fotmob_standings.qs'))

get_fotmob_playoff_status <- function() {
  url <- paste0('https://www.fotmob.com/api/leagues?id=', league_id)
  resp <- httr::GET(url)
  cont <- httr::content(resp, as = 'text')
  result <- jsonlite::fromJSON(cont)
  playoffs_init <- dplyr::bind_rows(result$playoff$rounds) |> tibble::as_tibble()
  playoffs_filt <- playoffs_init |> dplyr::filter(started)
  playoffs_unnested <- playoffs_filt |>
    tidyr::unnest(matchups, names_sep = '_') |> 
    janitor::clean_names()
  playoff_winners_and_losers <- playoffs_unnested |> 
    dplyr::filter(!is.na(matchups_winner)) |> 
    dplyr::transmute(
      winning_team = dplyr::case_when(
        matchups_home_team_id == matchups_winner ~ matchups_home_team,
        matchups_away_team_id == matchups_winner ~ matchups_away_team
      ),
      losing_team = dplyr::case_when(
        matchups_home_team_id == matchups_winner ~ matchups_away_team,
        matchups_away_team_id == matchups_winner ~ matchups_home_team
      )
    )
  
  losing_teams <- playoff_winners_and_losers$losing_team
  teams_still_in <- playoff_winners_and_losers |> 
    dplyr::distinct(team = winning_team) |> 
    dplyr::filter(!(team %in% losing_teams))
  
  dplyr::bind_rows(
    tibble::tibble(team = losing_teams) |> dplyr::mutate(playoff_status = 'out'),
    teams_still_in |> dplyr::mutate(playoff_status = 'in')
  ) |> 
    dplyr::arrange(team)
}


team_playoff_status <- get_fotmob_playoff_status()
qs::qsave(team_playoff_status, file.path(DATA_DIR, 'team_playoff_status.qs'))