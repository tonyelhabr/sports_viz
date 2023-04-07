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

gt_plt_dumbbell2_custom <- function(
    gt_object,
    col1 = NULL,
    col2 = NULL,
    ref = NULL,
    label = NULL,
    palette = c("#378E38", "#A926B6", "black", "#D3D3D3", "grey50"),
    width = 70,
    text_args = list(accuracy = 1),
    text_size = 2.5,
    text_font = 'mono', ## addition
    rng_val = NULL ## addition
) {
  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))
  stopifnot("'palette' must be 5 colors in order of col1, col2, ref, bar color, tie color" = length(palette) ==5)
  
  if (rlang::quo_is_null(rlang::enquo(col1)) | rlang::quo_is_null(rlang::enquo(col2)) | rlang::quo_is_null(rlang::enquo(ref))) {
    stop("'col1', 'col2', 'ref' must be specified")
  }
  
  # extract the values from specified columns
  df_in <- gtExtras::gt_index(gt_object, {{ col1 }}, as_vector = FALSE) %>%
    dplyr::select(x1 = {{ col1 }}, x2 = {{ col2 }}, r = {{ ref }})
  
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
        dumbbell_fx <- function(col1_vals, col2_vals, ref_vals, text_args, text_size) {
          all_df_in_vals <- c(col1_vals, col2_vals, ref_vals)

          if (any(is.na(all_df_in_vals)) | any(is.null(all_df_in_vals))) {
            return("<div></div>")
          }
          
          df_vals <- dplyr::tibble(x1 = col1_vals, x2 = col2_vals, r = ref_vals)
          
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
              aes(x = pmin(x1, x2, r), xend = pmax(x1, x2, r), yend = "y1"),
              linewidth = 1.5,
              color = palette[4]
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
              fill = palette[5],
              size = 4, # 3,
              stroke = 1.25
            ) +
            geom_point(
              data = df_vals,
              aes(x = r),
              color = "white",
              pch = 21,
              fill = palette[3],
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
              # hjust = hjust_val[[1]],
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
              # hjust = hjust_val[[2]],
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
              # hjust = 0.5,
              family = text_font, # "mono"
              color = palette[5],
              size = text_size,
            ) +
            geom_text(
              data = df_vals,
              aes(
                x = r, y = 1.05,
                label = do.call(scales::label_number, text_args)(r),
              ),
              # TODO: revisit horizontal adjustment
              hjust = 0.5,
              family = text_font, # "mono"
              color = palette[3],
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
          df_in[[3]],
          list(text_args),
          text_size,
          SIMPLIFY = FALSE
        )
        tab_built
      }
    ) %>%
    gt::cols_align(align = "left", columns = {{ col1 }}) %>%
    gt::cols_hide({{ col2 }}) %>%
    gt::cols_hide({{ ref }})
  
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

