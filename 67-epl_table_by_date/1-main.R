library(readr)
library(dplyr)
library(tidyr)
library(worldfootballR)
library(stringr)
library(lubridate)

proj_dir <- '66-compare_club_rankings'

raw_epl_matches <- worldfootballR::fotmob_get_league_matches(
  league_id = 47,
  season = '2022/2023'
)

# team_logo_url = sprintf('https://images.fotmob.com/image_resources/logo/teamlogo/%s.png', team_id)
clean_epl_matches <- raw_epl_matches |> 
  rename(match_id = id) |> 
  unnest_wider(c(home, away, status), names_sep = '_') |> 
  filter(status_finished, !status_cancelled) |> 
  transmute(
    matchweek = round,
    # across(matchweek, ~na_if(.x, 7L)), ## the queen
    date = date(status_utcTime),
    rn = row_number(status_utcTime),
    home_id = home_id,
    home_name = home_shortName, 
    home_g = as.integer(str_remove(status_scoreStr, '\\s[-].*$')), 
    away_id,
    away_name = away_shortName, 
    away_g = as.integer(str_remove(status_scoreStr, '^.*[-]\\s'))
  ) |> 
  mutate(
    home_pts = case_when(
      home_g > away_g ~ 3L,
      home_g < away_g ~ 0L,
      home_g == away_g ~ 1L
    ),
    .after = home_g
  ) |> 
  mutate(
    away_pts = case_when(
      home_g > away_g ~ 0L,
      home_g < away_g ~ 3L,
      home_g == away_g ~ 1L
    ),
    .after = away_g
  )
clean_epl_matches |> filter(is.na(home_pts))

long_epl_matches <- bind_rows(
  clean_epl_matches |>
    select(matchweek, date, rn, starts_with('home'), g_conceded = away_g) |> 
    rename_with(~str_remove(.x, 'home_'), starts_with('home')) |> 
    mutate(side = 'home'),
  clean_epl_matches |>
    select(matchweek, date, rn, starts_with('away'), g_conceded = home_g) |> 
    rename_with(~str_remove(.x, 'away_'), starts_with('away')) |> 
    mutate(side = 'away')
) |> 
  mutate(
    gd = g - g_conceded
  )
long_epl_matches |> filter(is.na(pts))
long_epl_matches |> 
  arrange(rn)|> 
  group_by(name) |> 
  mutate(
    across(
      c(matchweek, date, rn),
      list(
        lag1 = lag,
        lead1 = lead
      )
    )
  ) |> 
  filter(matchweek < matchweek_lag1) |> 
  ungroup()

cumu_epl_matches <- long_epl_matches |> 
  arrange(rn) |> 
  group_by(name) |> 
  mutate(
    across(
      matchweek,
      ## e.g. postponed matches due to the queen's passing
      ~ifelse(
        matchweek < lag(matchweek, default = 0),
        lag(matchweek),
        matchweek
      )
    ),
    across(
      c(g, g_conceded, gd, pts),
      list(cumu = cumsum),
      .names = '{.fn}_{.col}'
    )
  ) |> 
  ungroup()

get_table_by <- function(df, ...) {
  df |> 
    group_by(...) |> 
    mutate(
      rank1 = min_rank(desc(cumu_pts))
    ) |> 
    ungroup() |> 
    group_by(..., rank1) |> 
    mutate(
      rank2 = min_rank(desc(cumu_gd)) - 1L
    ) |> 
    ungroup() |> 
    group_by(..., rank1, rank2) |>
    mutate(
      rank3 = min_rank(desc(cumu_g)) - 1L
    ) |> 
    ungroup() |> 
    group_by(..., rank1, rank2, rank3) |>
    mutate(
      rank4 = row_number(name) - 1L
    ) |> 
    ungroup() |> 
    mutate(
      rank = rank1 + rank2 + rank3 + rank4,
      .keep = 'unused'
    ) |> 
    arrange(..., rank)
}

cumu_epl_table_by_matchweek <- cumu_epl_matches |> get_table_by(matchweek)
# cumu_epl_table_by_matchweek |> count(matchweek, rank) |> filter(n > 1)
# cumu_epl_table_by_matchweek |> filter(matchweek == 1L)
cumu_epl_table_by_date <- cumu_epl_matches |> get_table_by(date)
# cumu_epl_table_by_date |> arrange(date, rank)

summarize_props <- function(df) {
  df |> 
    filter(matchweek > 3) |> 
    group_by(name) |> 
    summarize(
      n = n(),
      prop_top4 = sum(rank <= 4) / n(),
      prop_top6 = sum(rank <= 6) / n(),
      prop_bot3 = sum(rank >= 18) / n()
    ) |> 
    ungroup()
}

cumu_epl_prop_by_matchweek <- cumu_epl_table_by_matchweek |> summarize_props()
cumu_epl_prop_by_date <- cumu_epl_table_by_date |> summarize_props()

inner_join(
  cumu_epl_prop_by_matchweek |> select()
)
