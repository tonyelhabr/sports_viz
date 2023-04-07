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
    # rank = row_number(),
    league_538 = '',
    logo_url = generate_logo_url(id_opta),
    team_538,
    table_pts,
    table_idx,
    rank_538 = row_number(rank_538),
    rank_opta = row_number(rank_opta), # 
    # rank_538_dummy = rank_538,
    # rank_opta_dummy = rank_opta
  ) |> 
  gt::gt() |> 
  .gt_theme_538() |> 
  gt::cols_label(
    logo_url = '',
    team_538 = '',
    table_pts = gt::md('**Pts**'),
    rank_538 = gt::md(glue::glue('**Standing** vs. {label_opta} and {label_538} Ranks'))
  ) |> 
  gtExtras::gt_merge_stack(
    team_538,
    league_538, 
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
  gt_plt_dumbbell2_custom(
    rank_538,
    rank_opta,
    table_idx,
    palette = c(unname(club_rankings_palette), 'black', '#D3D3D3', 'grey50'),
    # club_rankings_palette = c("#ED713B", "#7B1582", '#000', '#D3D3D3', '#7f7f7f'),
    text_font = 'Titillium Web',
    text_size = 3,
    width = 70,
    rng_val = c(0, 19),
  ) |> 
  gt::tab_header(
    title = gt::md('**Bundesliga standings and club rankings**'),
    subtitle = gt::md(glue::glue("{label_opta}'s ranks tend to be closer to the standings than {label_538}'s"))
  ) |> 
  gt::tab_source_note(
    source_note = gt::md(paste0('*Source ranks normalized to the league.*<br/>', baseline_caption))
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
