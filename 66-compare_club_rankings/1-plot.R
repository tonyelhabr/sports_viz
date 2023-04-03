library(readr)
library(dplyr)
library(gt)
library(gtExtras)
library(glue)

proj_dir <- '66-compare_club_rankings'
source(file.path(proj_dir, 'gt.R'))

compared_rankings <- read_csv('https://github.com/tonyelhabr/club-rankings/releases/download/club-rankings/compared-rankings.csv') |> 
  mutate(
    drank = rank_538 - rank_opta
  )

compared_latest_rankings <- compared_rankings |> 
  slice_max(date, n = 1, with_ties = TRUE)

biggest_positive_top100_538_diffs <- compared_latest_rankings |> 
  filter(
    rank_538 <= 100
  ) |> 
  arrange(desc(drank)) |> 
  slice_max(drank, n = 10, with_ties = FALSE)

# biggest_negative_top100_538_diffs <- compared_latest_rankings |> 
#   filter(
#     rank_538 <= 100
#   ) |> 
#   arrange(drank) |> 
#   slice_min(drank, n = 10, with_ties = FALSE)
# 
# biggest_positive_top100_opta_diffs <- compared_latest_rankings |> 
#   filter(
#     rank_opta <= 100
#   ) |> 
#   arrange(desc(drank)) |> 
#   slice_max(drank, n = 10, with_ties = FALSE)
# 
# biggest_negative_top100_opta_diffs <- compared_latest_rankings |> 
#   filter(
#     rank_opta <= 100
#   ) |> 
#   arrange(drank) |> 
#   slice_min(drank, n = 10, with_ties = FALSE)
palette <- c(
  '538' = '#ED713B',
  'Opta' = '#7B1582' # '#00ADEF'
)

label_538 <- glue::glue('<b><span style="color:{palette[["538"]]}">538</span></b>')
label_opta <- glue::glue('<b><span style="color:{palette[["Opta"]]}">Opta</span></b>')

make_table <- function(
    df, 
    title, 
    subtitle, 
    filename = deparse(substitute(df)),
    include_league = TRUE,
    vheight = 1100,
    vwidth = vheight / 2,
    ...
) {
  
  df <- df |> 
    transmute(
      # league = gsub('^[A-z]+\\s', '', league_538),
      league_538,
      url_logo = sprintf('https://omo.akamai.opta.net/image.php?secure=true&h=omo.akamai.opta.net&sport=football&entity=team&description=badges&dimensions=150&id=%s', id_opta),
      team_538,
      # rank_538,
      # rank_opta,
      rank_538_dummy = rank_538,
      rank_opta_dummy = rank_opta
    )
  
  if (isFALSE(include_league)) {
    # df <- df |> select(-league_538)
    df$league_538 <- ''
  }
  
  tb <- df |> 
    gt() |> 
    .gt_theme_538() |> 
    gt::cols_label(
      url_logo = '',
      team_538 = '',
      # rank_538 = '', # gt::html(glue::glue('<b><span style="color:{palette[["538"]]}">538</span></b>')),
      # rank_opta = '', # gt::html(glue::glue('<b><span style="color:{palette[["Opta"]]}">Opta</span></b>')),
      rank_538_dummy = ''
    )
  
  if (isTRUE(include_league)) {
    tb <- gtExtras::gt_merge_stack(
      tb, 
      team_538, 
      league_538,
      palette = c('black', 'grey30'),
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
  
  tb <- tb |> 
    gt::text_transform(
      locations = gt::cells_body(columns = url_logo),
      fn = function(x) {
        gt::web_image(
          url = x,
          height = 25
        )
      }
    ) |>
    # gt::tab_spanner(
    #   columns = c(rank_538, rank_opta),
    #   label = 'Rank'
    # ) |> 
    gt_plt_dumbbell_custom(
      rank_538_dummy,
      rank_opta_dummy,
      palette = c(unname(palette), '#D3D3D3', 'grey50'),
      text_font = 'Titillium Web',
      text_size = 3,
      ...
    ) |> 
    gt::tab_header(
      title = gt::md(glue::glue('**{title}**')),
      subtitle = gt::md(subtitle)
    ) |> 
    gt::tab_source_note(
      source_note = gt::md('***Updated**: 2023-04-01.*')
    )
  
  gt::gtsave(
    tb,
    filename = file.path(proj_dir, sprintf('%s.png', filename)),
    vheight = vheight,
    vwidth = vwidth,
    zoom = 2
  )
}

make_table(
  biggest_positive_top100_538_diffs,
  filename = 'biggest_positive_top100_538_diffs',
  title = 'Club ranking differences',
  subtitle = glue::glue('Biggest deviations where {label_opta} rank is **lower** than {label_538}'),
  width = 50,
  rng_val = c(1, 110),
  vheight = 1100,
  vwidth = 550
)

compared_latest_rankings |> 
  filter(
    league_538 == 'Barclays Premier League'
  ) |>
  arrange(rank_538) |> 
  make_table(
    filename = 'epl',
    include_league = FALSE,
    title = 'English Premier League club rankings',
    subtitle = glue::glue('Comparison of {label_538} and {label_opta} club rankings'),
    width = 70,
    rng_val = c(1, 150),
    vheight = 1100
  )
