

big5_suffix <- 'releases/download/fb_big5_advanced_season_stats/big5_team_shooting.rds'

df <- sprintf('https://github.com/JaseZiv/worldfootballR_data/%s', big5_suffix) |>
  url() |> 
  readRDS() |>
  filter(
    Team_or_Opponent == 'team',
    Season_End_Year >= 2018,
    Season_End_Year < 2023
  )

df |>
  group_by(season = Season_End_Year) |>
  summarize(
    g = sum(Gls_Standard),
    xg = sum(xG_Expected),
    npxg = sum(npxG_Expected),
    pg = sum(PK_Standard)
  ) |>
  ungroup() |>
  mutate(
    g_xg_d = g - xg,
    npg_npxg_d = g - pg - npxg
  )