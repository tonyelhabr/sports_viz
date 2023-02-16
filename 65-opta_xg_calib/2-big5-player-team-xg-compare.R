library(dplyr)
library(readr)
library(worldfootballR)

proj_dir <- '65-opta_xg_calib'
data_dir <- file.path(proj_dir, 'data')
match_shooting_path <- file.path(data_dir, 'match_shooting.rds')

match_shooting <- read_rds(match_shooting_path)

comp_country_mapping <- c(
  'Bundesliga' = 'GER',
  'La Liga' = 'ESP',
  'Ligue 1' = 'FRA',
  'Premier League' = 'ENG',
  'Seria A' = 'ITA'
)

raw_big5_shooting <- load_fb_big5_advanced_season_stats(
  season_end_year = 2018:2022,
  stat_type = 'shooting',
  team_or_player = 'team'
)

big5_shooting <- raw_big5_shooting |> 
  as_tibble() |> 
  filter(Team_or_Opponent == 'team') |> 
  transmute(
    season_end_year = Season_End_Year,
    country = unname(comp_country_mapping[Comp]),
    team = Squad, 
    shots = Sh_Standard,
    shots_ot = SoT_Standard,
    g = Gls_Standard,
    xg = xG_Expected,
    npxg = npxG_Expected,
    npg = Gls_Standard - PK_Standard,
    g_xg_d = g - xg,
    npg_npxg_d = npg - npxg
  ) |> 
  arrange(season_end_year, country, team)

## this should be slightly higher than the team aggregates (above) because of the way shots after deflections are handled (https://fbref.com/en/expected-goals-model-explained/)
big5_shooting_agg <- match_shooting |> 
  filter(group == 'big5') |> 
  transmute(
    country = Country,
    team = Squad,
    season_end_year = Season_End_Year,
    xg = as.numeric(xG),
    psxg = as.numeric(PSxG),
    g = Outcome == 'Goal',
    is_penalty = str_detect(Player, '\\(pen\\)')
  ) |> 
  group_by(season_end_year, country, team) |> 
  summarize(
    shots = n(),
    shots_ot = sum(!is.na(psxg)),
    npg = sum(ifelse(is_penalty, 0, 1) * g, na.rm = TRUE),
    npxg = sum(ifelse(is_penalty, 0, 1) * xg, na.rm = TRUE),
    across(
      c(
        xg,
        psxg,
        g
      ),
      ~sum(.x, na.rm = TRUE)
    )
  ) |> 
  ungroup() |> 
  transmute(
    season_end_year,
    country,
    team,
    shots,
    shots_ot,
    g,
    xg,
    npxg,
    npg,
    g_xg_d = g - xg,
    npg_npxg_d = npg - npxg
  ) |> 
  arrange(season_end_year, country, team)

pivot_shots_longer <- function(df) {
  df |>
    pivot_longer(
      -c(season_end_year, country, team),
      names_to = 'stat',
      values_to = 'value'
    )
}

combined <- bind_rows(
  big5_shooting |> mutate(source = 'raw'),
  big5_shooting_agg |> mutate(source = 'agg')
) |>
  pivot_longer(
    -c(season_end_year, country, team, source),
    names_to = 'stat',
    values_to = 'value'
  ) |> 
  pivot_wider(
    names_from = source,
    values_from = value
  ) |> 
  group_by(stat) |> 
  mutate(
    raw_rescaled = scales::rescale(raw, to = c(0, 1)),
    agg_rescaled = scales::rescale(agg, to = c(0, 1))
  )
combined |> arrange(desc(raw_rescaled))
