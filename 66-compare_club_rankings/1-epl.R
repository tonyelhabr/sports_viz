library(readr)
library(dplyr)
library(gt)
library(gtExtras)
library(glue)
library(worldfootballR)

proj_dir <- '66-compare_club_rankings'
source(file.path(proj_dir, 'helpers.R'))
source(file.path(proj_dir, 'gt.R'))


epl_table <- fotmob_get_league_tables(league_id = 47, season = '2022/2023')

overall_epl_table <- epl_table |> 
  filter(table_type == 'all')

compared_epl_rankings <- compared_latest_rankings |> 
  filter(
    league_538 == 'Barclays Premier League'
  ) |>
  arrange(rank_538)

mapping <- bind_cols(
  compared_epl_rankings |> select(team_538) |> arrange(team_538),
  overall_epl_table |> select(table_name) |> arrange(table_name)
)

tb_epl <- compared_epl_rankings |> 
  inner_join(
    mapping,
    by = join_by(team_538)
  ) |> 
  inner_join(
    overall_epl_table |> 
      select(table_name, table_idx, table_pts),
    by = join_by(table_name)
  ) |> 
  arrange(table_idx) |> 
  transmute(
    league_538 = '',
    logo_url = generate_logo_url(id_opta),
    team_538,
    table_pts,
    rank_538,
    rank_opta,
    drank
  ) |> 
  gt::gt() |> 
  .gt_theme_538() |> 
  gt::cols_label(
    logo_url = '',
    team_538 = '',
    table_pts = 'Pts',
    rank_538 = gt::md(label_538),
    rank_opta = gt::md(label_opta),
    drank = gt::html('<b>&#916</b>')
  ) |> 
  gt::tab_spanner(
    label = gt::md('**Rank**'),
    columns = c(rank_538, rank_opta)
  ) |> 
  gtExtras::gt_merge_stack(
    team_538,
    league_538, 
    palette = c('black', 'grey50'),
    font_weight = c('bold', 'normal')
  ) |> 
  gtExtras::gt_fa_rank_change(
    drank, 
    # palette = c("#1b7837", "lightgrey", "#762a83"),
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
    title = gt::md('**Opta is not as bullish on the EPL as 538**'),
    subtitle = gt::md(glue::glue("English Premier League club rankings, according to {label_538} and {label_opta}"))
  ) |> 
  gt::tab_source_note(
    source_note = gt::md(baseline_caption)
  )
tb_epl

gt::gtsave(
  tb_epl,
  filename = file.path(proj_dir, 'epl.png'),
  vheight = 1100,
  vwidth = 550,
  zoom = 2
)
