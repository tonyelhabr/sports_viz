library(readr)
library(dplyr)
library(gt)
library(gtExtras)
library(glue)

proj_dir <- '66-compare_club_rankings'
source(file.path(proj_dir, 'helpers.R'))
source(file.path(proj_dir, 'gt.R'))

biggest_positive_top100_538_diffs <- compared_latest_rankings |> 
  filter(
    rank_538 <= 100
  ) |> 
  arrange(desc(drank)) |> 
  slice_max(drank, n = 10, with_ties = FALSE)

biggest_negative_top100_538_diffs <- compared_latest_rankings |>
  filter(
    rank_538 <= 100
  ) |>
  arrange(drank) |>
  slice_min(drank, n = 10, with_ties = FALSE)

biggest_positive_top100_opta_diffs <- compared_latest_rankings |>
  filter(
    rank_opta <= 100
  ) |>
  arrange(desc(drank)) |>
  slice_max(drank, n = 10, with_ties = FALSE)

biggest_negative_top100_opta_diffs <- compared_latest_rankings |>
  filter(
    rank_opta <= 100
  ) |>
  arrange(drank) |>
  slice_min(drank, n = 10, with_ties = FALSE)

make_table <- function(
    df, 
    title, 
    subtitle, 
    caption = NULL,
    filename = deparse(substitute(df)),
    include_league = TRUE,
    vheight = 1100,
    vwidth = vheight / 2,
    ...
) {
  
  df <- df |> 
    transmute(
      league_538,
      logo_url = generate_logo_url(id_opta),
      team_538,
      rank_538,
      rank_opta
    )
  
  if (isFALSE(include_league)) {
    df$league_538 <- ''
  }
  
  tb <- df |> 
    gt::gt() |> 
    .gt_theme_538() |> 
    gt::cols_label(
      logo_url = '',
      team_538 = '',
      rank_538 = ''
    )
  
  if (isTRUE(include_league)) {
    tb <- gtExtras::gt_merge_stack(
      tb, 
      team_538, 
      league_538,
      palette = c('black', 'grey50'),
      font_weight = c('bold', 'normal')
    )
  } else {
    tb <- gtExtras::gt_merge_stack(
      tb,
      team_538,
      league_538, 
      font_size = c('14px', '0px')
    )
  }
  
  init_caption <- baseline_caption
  caption <- if (!is.null(caption)) {
    paste0(caption, '<br/>', init_caption)
  } else {
    init_caption
  }
  
  tb <- tb |> 
    gt::text_transform(
      locations = gt::cells_body(columns = logo_url),
      fn = function(x) {
        gt::web_image(
          url = x,
          height = 25
        )
      }
    ) |>
    gt_plt_dumbbell_custom(
      rank_538,
      rank_opta,
      palette = c(unname(club_rankings_palette), '#D3D3D3', 'grey50'),
      text_font = 'Titillium Web',
      text_size = 3,
      ...
    ) |> 
    gt::tab_header(
      title = gt::md(glue::glue('**{title}**')),
      subtitle = gt::md(subtitle)
    ) |> 
    gt::tab_source_note(
      source_note = gt::md(caption)
    )
  
  path <- file.path(proj_dir, sprintf('%s.png', filename))
  gt::gtsave(
    tb,
    filename = path,
    vheight = vheight,
    vwidth = vwidth,
    zoom = 2
  )
  widen_image(path, ratio = 1)
}

league_brazil <- 'Brasileiro Série A'
label_brazil <- sprintf('<b><span style="color:green">%s</span></b>', league_brazil)

biggest_negative_top100_538_diffs |> 
  mutate(
    across(
      league_538,
      ~ifelse(
        .x == league_brazil,
        label_brazil,
        .x
      )
    )
  ) |> 
  make_table(
    filename = 'biggest_negative_top100_538_diffs',
    title = sprintf('538 loves %s', label_brazil),
    subtitle = glue::glue('Biggest club ranking differences where {label_538} is more bullish than {label_opta}'),
    caption = '***Criteria**: Limited to teams that 538 has ranked in its top 100.*',
    width = 50,
    rng_val = c(1, 200),
    vheight = 1100,
    vwidth = 550
  )

make_table(
  biggest_positive_top100_opta_diffs,
  filename = 'biggest_positive_top100_opta_diffs',
  title = sprintf('Opta is higher on teams in less known leagues'),
  subtitle = glue::glue('Biggest club ranking differences where {label_opta} is more bullish than {label_538}'),
  caption = '***Criteria**: Limited to teams that Opta has ranked in its top 100.*',
  width = 50,
  rng_val = c(1, 200),
  vheight = 1100,
  vwidth = 550
)
