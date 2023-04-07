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

logo_urls <- c(
  'Barclays Premier League' = 'https://projects.fivethirtyeight.com/soccer-predictions/images/premier-league-logo.png?v=d8d3413d',
  'German Bundesliga' = 'https://projects.fivethirtyeight.com/soccer-predictions/images/bundesliga-logo.png?v=3cc82c94',
  'Spanish Primera Division' = 'https://projects.fivethirtyeight.com/soccer-predictions/images/la-liga-logo.png?v=16d33bfb',
  'Italy Serie A' = 'https://projects.fivethirtyeight.com/soccer-predictions/images/serie-a-logo.png?v=6f85ceee',
  'French Ligue 1' = 'https://projects.fivethirtyeight.com/soccer-predictions/images/ligue-1-logo.png?v=4c8d7f80'
)

agg_big5_rankings <- mapped_big5_rankings |> 
  select(
    league_538,
    rel_rank_538,
    rel_rank_opta,
    table_idx
  ) |> 
  group_by(league_538) |> 
  summarize(
    cor_538 = cor(rel_rank_538, table_idx, method = 'spearman'),
    cor_opta = cor(rel_rank_opta, table_idx, method = 'spearman'),
    mad_538 = mean(abs(rel_rank_538 - table_idx)),
    mad_opta = mean(abs(rel_rank_opta - table_idx))
  ) |> 
  ungroup() |> 
  mutate(
    logo_url = logo_urls[league_538],
    .before = league_538
  )


tb_big5 <- agg_big5_rankings |> 
  transmute(
    # rank = row_number(),
    logo_url,
    league_538,
    league_538_dummy = '',
    cor_538,
    cor_opta,
    mad_538,
    mad_opta
  ) |> 
  gt::gt() |> 
  .gt_theme_538() |> 
  gt::cols_label(
    logo_url = '',
    league_538 = '',
    cor_538 = gt::html(label_538),
    cor_opta = gt::html(label_opta),
    mad_538 = gt::html(label_538),
    mad_opta = gt::html(label_opta)
  ) |> 
  gtExtras::gt_merge_stack(
    league_538, 
    league_538_dummy,
    palette = c('black', 'grey50'),
    font_weight = c('bold', 'normal')
  ) |> 
  gt::text_transform(
    locations = gt::cells_body(columns = logo_url),
    fn = function(x) {
      gt::web_image(
        url = x,
        height = 25
      )
    }
  )  |>
  gt::fmt_percent(
    columns = c(cor_538, cor_opta),
    decimals = 0
  ) |> 
  gt::fmt_number(
    columns = c(mad_538, mad_opta),
    decimals = 1
  ) |> 
  gt::tab_spanner(
    label = 'Cor.',
    columns = c(cor_538, cor_opta),
    id = 'cor'
  ) |> 
  gt::tab_spanner(
    label = 'MAD',
    columns = c(mad_538, mad_opta),
    id = 'mad'
  ) |> 
  gt::tab_footnote(
    footnote = 'Spearman correlation of rankings with actual standings',
    location = gt::cells_column_spanners('cor')
  ) |> 
  gt::tab_footnote(
    footnote = 'Mean absolute deviation (MAD) with actual standings',
    location = gt::cells_column_spanners('mad')
  ) |> 
  gt::tab_header(
    title = gt::md('**How similar are the current standings and<br/>club rankings for the Big 5 leagues?**'),
    subtitle = gt::md(glue::glue("{label_opta}'s ranks tend to be closer to the standings than {label_538}'s"))
  ) |> 
  gt::tab_source_note(
    source_note = gt::md(paste0('*Source ranks normalized to the league.*<br/>*', baseline_caption))
  )
tb_big5

path_big5 <- file.path(proj_dir, 'big5.png')
gt::gtsave(
  tb_big5,
  filename = path_big5,
  vheight = 1100,
  vwidth = 550,
  zoom = 2
)
widen_image(path_big5, ratio = 0.5)
