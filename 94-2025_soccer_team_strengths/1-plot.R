# download from https://www.pitchrank.fyi/assets/index-Dp7bmWOn.js
library(tidyverse)
library(janitor)
PROJ_DIR <- '94-2025_soccer_team_strengths'
raw <- readr::read_csv(
  file.path(PROJ_DIR, 'index-Dp7bmWOn.js'), 
  skip = 132, 
  n_max = 139089 - 132,
  col_names = c(
    'Date', 'Team', 'League', 'NumMatches','Strength','Strength_smoothed','Strength_adjusted','is_active_2526'
  )
) |> 
  janitor::clean_names() |> 
  rename(country = league)

diff_values <- raw |> 
  filter(is_active_2526) |> 
  select(
    date,
    team,
    country,
    strength,
    strength_smoothed,
    strength_adjusted
  ) |> 
  pivot_longer(
    starts_with('strength')
  ) |> 
  arrange(team, country, name, date) |> 
  group_by(team, country, name) |> 
  mutate(
    lag_value = coalesce(lag(value), value),
    diff_value = value - lag_value
  ) |> 
  ungroup() |> 
  arrange(desc(abs(diff_value)))

diff_values |> 
  filter(
    country %in% c('England', 'Spain', 'France', 'Germany', 'Italy'),
    name == 'strength_adjusted'
  )

top_bot <- raw |> 
  filter(is_active_2526, country %in% c('England', 'Spain', 'France', 'Germany', 'Italy')) |> 
  filter(
    date %in% c('2025-06-04', '2025-08-13')
  ) |>
  arrange(desc(strength_adjusted)) |> 
  transmute(
    # date = case_when(
    #   date == '2025-05-28' ~ 'a',
    #   date == '2025-08-13' ~ 'b'
    # ) |> 
    #   ordered(),
    date,
    team,
    country,
    strength_adjusted
  ) |> 
  pivot_wider(
    names_from = date,
    values_from = strength_adjusted,
    names_sort = TRUE
  ) |> 
  mutate(
    diff = `2025-08-13` - `2025-06-04`
  ) 

top <- top_bot |> arrange(desc(diff)) |> head(10)
bot <- top_bot |> arrange(diff) |> head(10)
bot

top_ids <- tibble::tribble(
  ~team, ~fotmob_team_id,
  "Elche CF", 9857,
  "Tottenham Hotspur", 8586,
  "Manchester United", 10260,
  "Sunderland", 8472,
  "Paris Saint Germain", 9847,
  "FC Koln", 8722,
  "Real Madrid", 8633,
  "Arsenal", 9825,
  "Napoli", 9875,
  "RB Leipzig", 178475
) |>
  dplyr::transmute(
    team,
    logo_url = sprintf('https://images.fotmob.com/image_resources/logo/teamlogo/%s.png', fotmob_team_id)
  ) |>
  tibble::as_tibble()

bot_ids <- tibble::tribble(
  ~team, ~fotmob_team_id,
  "Bologna", 9857,
  "Fulham", 9879,
  "Brentford", 9937,
  "Bournemouth", 8678,
  "Atalanta BC", 8524,
  "Union Berlin", 8149,
  "Augsburg", 8406,
  "St Pauli", 8152,
  "Crystal Palace", 9826,
  "Brighton and Hove Albion", 10204
) |>
  dplyr::transmute(
    team,
    logo_url = sprintf('https://images.fotmob.com/image_resources/logo/teamlogo/%s.png', fotmob_team_id)
  ) |>
  tibble::as_tibble()


top_with_ids <- top |> 
  left_join(top_ids)

bot_with_ids <- bot |> 
  left_join(bot_ids)


DATE_PALETTE <- c(
  '2025-06-04' = '#ED713B',
  '2025-08-13' = '#7B1582'
)

label_before <- glue::glue('<b><span style="color:{DATE_PALETTE[["2025-06-04"]]}">Jun. 4</span></b>')
label_after <- glue::glue('<b><span style="color:{DATE_PALETTE[["2025-08-13"]]}">Aug. 13</span></b>')

library(rlang)
library(magrittr)
library(gtExtras)
library(dplyr)
library(ggplot2)
library(scales)
library(gt)

## https://github.com/jthomasmock/gtExtras/blob/master/R/gt_plt_dumbbell.R
gt_plt_dumbbell_custom <- function(
    gt_object,
    col1 = NULL,
    col2 = NULL,
    label = NULL,
    palette = c("#378E38", "#A926B6", "#D3D3D3", "grey50"),
    width = 70,
    text_args = list(accuracy = 1),
    text_size = 2.5,
    text_font = 'mono', ## addition
    rng_val = NULL ## addition
) {
  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))
  stopifnot("'palette' must be 4 colors in order of col1, col2, bar color, tie color" = length(palette) == 4)
  
  if (rlang::quo_is_null(rlang::enquo(col1)) | rlang::quo_is_null(rlang::enquo(col2))) {
    stop("'col1' and 'col2' must be specified")
  }
  
  # extract the values from specified columns
  df_in <- gtExtras::gt_index(gt_object, {{ col1 }}, as_vector = FALSE) %>%
    dplyr::select(x1 = {{ col1 }}, x2 = {{ col2 }})
  
  if (length(df) == 0) {
    return(gt_object)
  }
  
  all_vals <- df_in %>%
    unlist() %>%
    as.vector()
  
  if ((length(rng_val) != 2) | any(is.null(rng_val))) {
    rng_val <- range(all_vals, na.rm = TRUE)
  }
  
  tab_out <- gt_object %>%
    text_transform(
      locations = cells_body({{ col1 }}),
      fn = function(x) {
        dumbbell_fx <- function(col1_vals, col2_vals, text_args, text_size) {
          all_df_in_vals <- c(col1_vals, col2_vals)
          
          if (any(is.na(all_df_in_vals)) | any(is.null(all_df_in_vals))) {
            return("<div></div>")
          }
          
          df_vals <- dplyr::tibble(x1 = col1_vals, x2 = col2_vals)
          
          # TODO: revisit horizontal adjustment
          hjust_val <- if(col1_vals >= col2_vals) {
            list(-0.1,1.1)
          } else {
            list(1.1,-0.1)
          }
          
          df_vals1 <- dplyr::filter(df_vals, x1 != x2)
          df_vals2 <- dplyr::filter(df_vals, x1 == x2)
          plot_obj <- df_vals |> 
            ggplot(aes(y = "y1")) +
            geom_segment(
              data = df_vals1,
              aes(x = x1, xend = x2, yend = "y1"),
              linewidth = 1.5,
              color = palette[3]
            ) +
            geom_point(
              data = df_vals1,
              aes(x = x1),
              color = "white",
              pch = 21,
              fill = palette[1],
              size = 4, # 3,
              stroke = 1.25
            ) +
            geom_point(
              data = df_vals1,
              aes(x = x2),
              color = "white",
              pch = 21,
              fill = palette[2],
              size = 4, # 3,
              stroke = 1.25
            ) +
            geom_point(
              data = df_vals2,
              aes(x = x2),
              color = "white",
              pch = 21,
              fill = palette[4],
              size = 4, # 3,
              stroke = 1.25
            ) +
            geom_text(
              data = df_vals1,
              aes(
                x = x1, y = 1.05,
                label = do.call(scales::label_number, text_args)(x1),
              ),
              # TODO: revisit horizontal adjustment
              hjust = hjust_val[[1]],
              family = text_font, # "mono"
              color = palette[1],
              size = text_size,
              
            ) +
            geom_text(
              data = df_vals1,
              aes(
                x = x2, y = 1.05,
                label = do.call(scales::label_number, text_args)(x2),
              ),
              # TODO: revisit horizontal adjustment
              hjust = hjust_val[[2]],
              family = text_font, # "mono"
              color = palette[2],
              size = text_size
            ) +
            geom_text(
              data = df_vals2,
              aes(
                x = x1, y = 1.05,
                label = do.call(scales::label_number, text_args)(x1),
              ),
              # TODO: revisit horizontal adjustment
              hjust = 0.5,
              family = text_font, # "mono"
              color = palette[4],
              size = text_size,
            ) +
            coord_cartesian(xlim = rng_val) +
            # scale_x_continuous(expand = expansion(mult = c(0.1, 0.1))) +
            scale_y_discrete(expand = expansion(mult = c(0.03, 0.095))) +
            theme(
              legend.position = "none",
              plot.margin = margin(0, 0, 0, 0, "pt"),
              plot.background = element_blank(),
              panel.background = element_blank()
            ) +
            theme_void()
          
          gtExtras:::save_svg(plot_obj, height = 7, width = width, units = "mm")
        }
        
        tab_built <- mapply(
          dumbbell_fx,
          df_in[[1]],
          df_in[[2]],
          list(text_args),
          text_size,
          SIMPLIFY = FALSE
        )
        tab_built
      }
    ) %>%
    gt::cols_align(align = "left", columns = {{ col1 }}) %>%
    gt::cols_hide({{ col2 }})
  
  if(!is.null(label)){
    
    return(
      tab_out %>%
        cols_label({{ col1 }} := label)
    )
  }
  
  tab_out
  
}

.gt_theme_538 <- function(data, ...) {
  data |>
    gt::opt_table_font(
      font = list(
        gt::google_font('Titillium Web'),
        gt::default_fonts()
      )
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "top", color = "black", weight = px(0)
      ),
      locations = gt::cells_column_labels(
        columns = gt::everything()
      )
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom", color = "black", weight = px(1)
      ),
      locations = gt::cells_row_groups()
    ) |>
    gt::tab_options(
      column_labels.background.color = "white",
      heading.border.bottom.style = "none",
      table.border.top.width = px(3),
      table.border.top.style = "none", # transparent
      table.border.bottom.style = "none",
      column_labels.font.weight = "normal",
      column_labels.border.top.style = "none",
      column_labels.border.bottom.width = gt::px(2),
      column_labels.border.bottom.color = "black",
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = gt::px(1),
      row_group.border.bottom.color = "white",
      stub.border.color = "white",
      stub.border.width = gt::px(0),
      data_row.padding = gt::px(1),
      source_notes.font.size = 12,
      source_notes.border.lr.style = "none",
      table.font.size = 14,
      heading.title.font.size = 18,
      heading.subtitle.font.size = 14,
      heading.align = "left",
      heading.padding = gt::px(0),
      footnotes.padding = gt::px(0),
      footnotes.font.size = 12,
      table_body.border.bottom.color = 'black',
      ...
    ) |>
    opt_css(
      ".gt_title {
        line-height: 1em;
      }
    ",
      add = TRUE
    )
}


widen_image <- function(path, ratio = NULL, ratio_resolution = 0.25, height_resolution = 100) {
  img <- magick::image_read(path)
  info <- magick::image_info(img)
  w <- info$width
  h <- info$height
  r <- h / w
  new_ratio <- if (is.null(ratio)) {
    ratio_resolution * ceiling(ratio / ratio_resolution)
  } else {
    if (ratio > r) {
      stop(
        sprintf('`ratio` must be smaller than %s', round(r, 2))
      )
    }
    ratio
  }
  
  new_h <- height_resolution * ceiling(h / height_resolution)
  new_w <- new_h / new_ratio
  dw <- round((new_w - w) / 2)
  if (dw < 0) {
    stop(
      'New width smaller than original width'
    )
  }
  dh <- round((new_h - h) / 2)
  new_img <- magick::image_border(img, 'white', sprintf('%sx%s', dw, dh))
  magick::image_write(new_img, path)
}

make_table <- function(
    df, 
    title, 
    subtitle = '', 
    caption = NULL,
    filename = deparse(substitute(df)),
    vheight = 1100,
    vwidth = vheight / 2,
    ...
) {

  df <- df |> 
    transmute(
      country,
      logo_url,
      team,
      `2025-06-04`,
      `2025-08-13`
    )

  tb <- df |> 
    gt::gt() |> 
    .gt_theme_538() |> 
    gt::cols_label(
      logo_url = '',
      team = '',
      `2025-06-04` = ''
    )
  
  tb <- gtExtras::gt_merge_stack(
    tb, 
    team, 
    country,
    palette = c('black', 'grey50'),
    font_weight = c('bold', 'normal')
  )

  init_caption <- ''
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
      `2025-06-04`,
      `2025-08-13`,
      palette = c(unname(DATE_PALETTE), '#D3D3D3', 'grey50'),
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
  
  path <- file.path(PROJ_DIR, sprintf('%s.png', filename))
  gt::gtsave(
    tb,
    filename = path,
    vheight = vheight,
    vwidth = vwidth,
    zoom = 2
  )
  widen_image(path, ratio = .75)
}


top_with_ids |> 
  make_table(
    filename = 'biggest_positive_diffs_big5',
    title = 'Who got *better* over the summer?',
    subtitle = glue::glue('Big 5 teams with largest positive rating changes between {label_before} and {label_after}'),
    caption = '**Source*: PitchRank*',
    text_args = list(accuracy = 0.01),
    width = 50,
    rng_val = c(1, 3.5),
    vheight = 1100,
    vwidth = 550
  )

bot_with_ids |> 
  make_table(
    filename = 'biggest_negative_diffs_big5',
    title = 'Who got *worse* over the summer?',
    subtitle = glue::glue('Big 5 teams with largest negative rating changes between {label_before} and {label_after}'),
    caption = '**Source*: PitchRank*',
    text_args = list(accuracy = 0.01),
    width = 50,
    rng_val = c(1, 3.5),
    vheight = 1100,
    vwidth = 550
  )

