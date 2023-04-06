library(readr)
library(dplyr)
library(gt)
library(gtExtras)
library(glue)
library(worldfootballR)

proj_dir <- '66-compare_club_rankings'
source(file.path(proj_dir, 'helpers.R'))
source(file.path(proj_dir, 'gt.R'))

bdl_table <- worldfootballR::fotmob_get_league_tables(
  league_id = 54, 
  season = '2022/2023'
)

name_fixes <- c(
  '1. FC Köln' = 'FC Cologne',
  'Union Berlin' = '1. FC Union Berlin', 
  'Wolfsburg' = 'VfL Wolfsburg'
)

overall_bdl_table <- bdl_table |> 
  filter(table_type == 'all') |> 
  mutate(
    table_name = coalesce(name_fixes[table_name], table_name)
  )

compared_bdl_rankings <- compared_latest_rankings |> 
  filter(
    league_538 == 'German Bundesliga'
  ) |>
  arrange(rank_538)

bdl_mapping <- bind_cols(
  compared_bdl_rankings |> select(team_538) |> arrange(team_538),
  overall_bdl_table |> select(table_name) |> arrange(table_name)
)

tb_bdl <- compared_bdl_rankings |> 
  inner_join(
    bdl_mapping,
    by = join_by(team_538)
  ) |> 
  inner_join(
    overall_bdl_table |> 
      select(table_name, table_idx, table_pts),
    by = join_by(table_name)
  ) |> 
  arrange(table_idx) |> 
  transmute(
    rank = row_number(),
    league_538 = '',
    logo_url = generate_logo_url(id_opta),
    team_538,
    table_pts,
    rank_538 = row_number(rank_538),
    drank_538 = rank - rank_538,
    rank_opta = row_number(rank_opta),
    drank_opta = rank - rank_opta
  ) |> 
  gt::gt() |> 
  .gt_theme_538() |> 
  gt::cols_label(
    rank = '',
    logo_url = '',
    team_538 = '',
    table_pts = 'Pts',
    rank_538 = gt::md('Rank'),
    drank_538 = gt::html('<b>&#916</b>'),
    rank_opta = gt::md('Rank'),
    drank_opta = gt::html('<b>&#916</b>')
  ) |> 
  gt::tab_spanner(
    label = gt::md(label_538),
    columns = c(rank_538, drank_538)
  ) |> 
  gt::tab_spanner(
    label = gt::md(label_opta),
    columns = c(rank_opta, drank_opta)
  ) |> 
  gtExtras::gt_merge_stack(
    team_538,
    league_538, 
    palette = c('black', 'grey50'),
    font_weight = c('bold', 'normal')
  ) |> 
  gtExtras::gt_fa_rank_change(
    drank_538, 
    # palette = c("#1b7837", "lightgrey", "#762a83"),
    palette = c("#1b7837", "lightgrey", "#ca0020"),
    fa_type = 'angles',
    font_color = 'match'
  ) |> 
  gtExtras::gt_fa_rank_change(
    drank_opta, 
    palette = c("#1b7837", "lightgrey", "#ca0020"),
    fa_type = 'angles',
    font_color = 'match'
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
  gt::tab_header(
    # title = gt::md('**Opta is not as bullish on the EPL as 538**'),
    # subtitle = gt::md(glue::glue("English Premier League club rankings, according to {label_538} and {label_opta}"))
    title = gt::md('**German Bundesliga**'),
    subtitle = gt::md(glue::glue('Bundesliga club rankings, according to {label_538} and {label_opta}'))
  ) |> 
  gt::tab_source_note(
    source_note = gt::md(baseline_caption)
  )
tb_bdl

path_bdl <- file.path(proj_dir, 'bdl.png')
gt::gtsave(
  tb_bdl,
  filename = path_bdl,
  vheight = 1100,
  vwidth = 550,
  zoom = 2
)
widen_image(path_bdl, ratio = 1)
