library(worldfootballR)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(qs)

proj_dir <- '67-table_by_date'

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
    # across(gp, ~na_if(.x, 7L)), ## the queen
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
    select(date, rn, starts_with('home'), g_conceded = away_g) |> 
    rename_with(~str_remove(.x, 'home_'), starts_with('home')) |> 
    mutate(side = 'home'),
  clean_epl_matches |>
    select(date, rn, starts_with('away'), g_conceded = home_g) |> 
    rename_with(~str_remove(.x, 'away_'), starts_with('away')) |> 
    mutate(side = 'away')
) |> 
  group_by(name) |> 
  mutate(
    gp = row_number(date),
    .before = 1
  ) |> 
  ungroup() |> 
  mutate(
    gd = g - g_conceded
  )

cumu_epl_matches <- long_epl_matches |> 
  arrange(rn) |> 
  group_by(name) |> 
  mutate(
    across(
      gp,
      ## e.g. postponed matches due to the queen's passing
      ~ifelse(
        gp < lag(gp, default = 0),
        lag(gp),
        gp
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

cumu_epl_table_by_gp <- cumu_epl_matches |> get_table_by(gp)
range(cumu_epl_matches$date)
cumu_epl_table_by_date <- cumu_epl_matches |> 
  complete(
    date = seq.Date(min(cumu_epl_matches$date), max(cumu_epl_matches$date), by = 'day'),
    name = unique(cumu_epl_matches$name)
  ) |> 
  group_by(name) |> 
  fill(gp, g, g_conceded, gd, pts, starts_with('cumu'), .direction = 'down') |> 
  ungroup() |> 
  mutate(
    across(c(gp, g, g_conceded, gd, pts, starts_with('cumu')), \(x) replace_na(x, 0))
  ) |> 
  get_table_by(date)

qs::qsave(cumu_epl_table_by_gp, file.path(proj_dir, 'cumu_epl_table_by_gp.qs'))
qs::qsave(cumu_epl_table_by_date, file.path(proj_dir, 'cumu_epl_table_by_date.qs'))
