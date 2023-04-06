library(readr)
library(dplyr)
library(gt)
library(gtExtras)
library(glue)
library(worldfootballR)
library(purrr)

proj_dir <- '66-compare_club_rankings'
source(file.path(proj_dir, 'helpers.R'))
source(file.path(proj_dir, 'gt.R'))

league_id_mapping <- c(
  '47' = 'ENG',
  '53' = 'FRA',
  '54' = 'GER',
  '55' = 'ITA',
  '87' = 'ESP'
)
big5_tables <- worldfootballR::fotmob_get_league_tables(
  league_id = names(league_id_mapping),
  season = '2022/2023'
)

name_fixes <- c(
  'Monaco' = 'AS Monaco',
  '1. FC Köln' = 'FC Cologne',
  'Union Berlin' = '1. FC Union Berlin', 
  'Wolfsburg' = 'VfL Wolfsburg',
  'Milan' = 'AC Milan',
  'Roma' = 'AS Roma',
  'Hellas Verona' = 'Verona'
)

overall_big5_tables <- big5_tables |> 
  filter(table_type == 'all') |> 
  mutate(
    table_name = coalesce(name_fixes[table_name], table_name)
  )

league_name_mapping <- c(
  'Barclays Premier League' = '47',
  'French Ligue 1' = '53',
  'German Bundesliga' = '54',
  'Italy Serie A' = '55',
  'Spanish Primera Division' = '87'
)

compared_big5_rankings <- compared_latest_rankings |> 
  filter(
    league_538 %in% names(league_name_mapping)
  ) |>
  mutate(
    league_id = league_name_mapping[league_538]
  ) |> 
  arrange(league_id, team_538)

big5_mapping <- bind_cols(
  compared_big5_rankings |> 
    select(league_id, team_538) |> 
    arrange(league_id, team_538),
  overall_big5_tables |>
    arrange(league_id, table_name) |> 
    select(table_name)
)

mapped_big5_rankings <- compared_big5_rankings |> 
  group_by(league_538) |> 
  mutate(
    rel_rank_538 = row_number(rank_538),
    rel_rank_opta = row_number(rank_opta)
  ) |> 
  inner_join(
    big5_mapping,
    by = join_by(league_id, team_538)
  ) |> 
  inner_join(
    overall_big5_tables |> 
      transmute(
        across(league_id, as.character), 
        table_name, 
        table_idx, 
        table_pts
      ),
    by = join_by(league_id, table_name)
  ) |> 
  arrange(table_idx)

agg_big5_rankings <- mapped_big5_rankings |> 
  select(
    league_id,
    league_538,
    rel_rank_538,
    rel_rank_opta,
    table_idx
  ) |> 
  group_by(league_id, league_538) |> 
  summarize(
    cor_538 = cor(rel_rank_538, table_idx, method = 'spearman'),
    cor_opta = cor(rel_rank_opta, table_idx, method = 'spearman'),
    mad_538 = mean(abs(rel_rank_538 - table_idx)),
    mad_opta = mean(abs(rel_rank_opta - table_idx))
  )

logo_urls <- c(
  'Barclays Premier League' = 'https://projects.fivethirtyeight.com/soccer-predictions/images/premier-league-logo.png?v=d8d3413d',
  'German Bundesliga' = 'https://projects.fivethirtyeight.com/soccer-predictions/images/bundesliga-logo.png?v=3cc82c94',
  'Spanish Primera Division' = 'https://projects.fivethirtyeight.com/soccer-predictions/images/la-liga-logo.png?v=16d33bfb',
  'Italy Serie A' = 'https://projects.fivethirtyeight.com/soccer-predictions/images/serie-a-logo.png?v=6f85ceee',
  'French Ligue 1' = 'https://projects.fivethirtyeight.com/soccer-predictions/images/ligue-1-logo.png?v=4c8d7f80'
)
