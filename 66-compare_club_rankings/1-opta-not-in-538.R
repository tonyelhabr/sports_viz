library(readr)
library(dplyr)
library(gt)
library(gtExtras)
library(glue)

proj_dir <- '66-compare_club_rankings'
source(file.path(proj_dir, 'helpers.R'))]
source(file.path(proj_dir, 'gt.R'))

opta_rankings <- generate_club_rankings_url('opta-club') |> 
  read_csv() |> 
  filter(date == .env$club_rankings_date) |> 
  slice_max(updated_at, n = 1, with_ties = TRUE) |> 
  filter(!is.na(rank))

top_opta_not_in_538 <- opta_rankings |> 
  anti_join(
    compared_latest_rankings,
    by = join_by(date, id == id_opta)
  ) |> 
  arrange(rank) |> 
  slice_min(rank, n = 10)

nations <- c(
  '1sxaf8l7fknmucd72fdretogm' = 'Poland',
  '5ocdn3a6s75u0d0dy0rbou0xc' = 'Czech Republic',
  'c9swyor08g9pedxpe3n321svu' = 'Saudi Arabia',
  '6ouaoxrghzeqpzqk02xn3t4r9' = 'Croatia',
  '39r69xdnw8usnndtetv3un9z3' = 'Bosnia and Herzegovina',
  'ealjallt13942jb2q2hk4buxa' = 'Israel',
  '2kzl64gp8x37bon8tyxojqrr1' = 'Saudi Arabia',
  'eu0or974evp0gojv63d3imktn' = 'Cyprus',
  '57jcqe38hakh2hfit2zsogsb' = 'Saudi Arabia',
  '3yt473wfo7f64lhmyfsf0zxcg' = 'Cyprus'
)

tb_top_opta_not_in_538 <- top_opta_not_in_538 |> 
  transmute(
    nation = nations[id],
    logo_url = generate_logo_url(id),
    team,
    rank
  ) |> 
  gt::gt() |> 
  .gt_theme_538() |> 
  gt::cols_label(
    logo_url = '',
    team = '',
    rank = 'Rank'
  ) |> 
  gtExtras::gt_merge_stack(
    team,
    nation, 
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
  ) |>
  gt::tab_header(
    title = gt::md('**538 hates small nations?**'),
    subtitle = gt::md(glue::glue("Highest ranked teams in {label_opta}'s club rankings<br/>that are not in {label_538}'s rankings"))
  ) |> 
  gt::tab_source_note(
    source_note = gt::md(baseline_caption)
  )

gt::gtsave(
  tb_top_opta_not_in_538,
  filename = file.path(proj_dir, 'top_opta_not_in_538.png'),
  vheight = 1100,
  vwidth = 550,
  zoom = 2
)

