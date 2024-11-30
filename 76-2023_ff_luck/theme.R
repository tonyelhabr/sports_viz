library(ggplot2)
library(sysfonts)
library(ggtext)
library(grid)

WHITISH_FOREGROUND_COLOR <- 'white'
COMPLEMENTARY_FOREGROUND_COLOR <- '#cbcbcb' # '#f1f1f1'
BLACKISH_BACKGROUND_COLOR <- '#1c1c1c'
COMPLEMENTARY_BACKGROUND_COLOR <- '#4d4d4d'
FONT <- 'Lato'
sysfonts::font_add_google(FONT, FONT)

ggplot2::theme_set(ggplot2::theme_minimal())
ggplot2::theme_update(
  text = ggplot2::element_text(family = FONT),
  title = ggplot2::element_text(size = 26, color = WHITISH_FOREGROUND_COLOR),
  plot.title = ggplot2::element_text(face = 'bold', size = 20, color = WHITISH_FOREGROUND_COLOR),
  plot.title.position = 'plot',
  plot.subtitle = ggplot2::element_text(size = 16, color = COMPLEMENTARY_FOREGROUND_COLOR),
  axis.text = ggplot2::element_text(color = WHITISH_FOREGROUND_COLOR, size = 14),
  # axis.title = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.title.x = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.title.y = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.line = ggplot2::element_blank(),
  strip.text = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0),
  panel.grid.major = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor.x = ggplot2::element_blank(),
  panel.grid.minor.y = ggplot2::element_blank(),
  plot.margin = ggplot2::margin(10, 20, 10, 20),
  plot.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR),
  plot.caption = ggplot2::element_text(color = WHITISH_FOREGROUND_COLOR, hjust = 0, size = 10, face = 'plain'),
  plot.caption.position = 'plot',
  plot.tag = ggplot2::element_text(size = 10, color = WHITISH_FOREGROUND_COLOR, hjust = 1),
  plot.tag.position = c(0.99, 0.99),
  panel.spacing.x = grid::unit(2, 'lines'),
  panel.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR)
)

.gt_theme_538 <- function(gt_object, ..., quiet = TRUE) {
  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))
  
  table_id <- subset(gt_object[['_options']], parameter == 'table_id')[["value"]][[1]]
  
  if(is.na(table_id)){
    table_id <- gt::random_id()
    if(isFALSE(quiet)){
      message(glue::glue(
        "Table has no assigned ID, using random ID '{table_id}' to apply `gt::opt_css()`",
        "\nAvoid this message by assigning an ID: `gt(id = '')` or `gt_theme_538(quiet = TRUE)`"
      ))
    }
    
    opt_position <- which("table_id" %in% gt_object[["_options"]][["parameter"]])[[1]]
    gt_object[["_options"]][["value"]][[opt_position]] <- table_id
  }
  
  gt_object %>%
    opt_table_font(
      font = list(
        google_font("Cairo"),
        default_fonts()
      ),
      weight = 400
    ) %>%
    tab_style(
      locations = cells_title("title"),
      style = cell_text(
        font = google_font("Chivo"),
        weight = 700
      )
    ) %>%
    tab_style(
      locations = cells_title("subtitle"),
      style = cell_text(
        font = google_font("Chivo"),
        weight = 300
      )
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = "top", color = "black", weight = px(0)
        ),
        cell_text(
          font = google_font("Chivo"),
          # transform = "uppercase",
          v_align = "bottom",
          size = px(14),
          weight = 200
        )
      ),
      locations = list(
        gt::cells_column_labels(),
        gt::cells_stubhead()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "black", weight = px(1)
      ),
      locations = cells_row_groups()
    ) %>%
    tab_options(
      column_labels.background.color = "white",
      data_row.padding = px(3),
      heading.border.bottom.style = "none",
      table.border.top.width = px(3),
      table.border.top.style = "none", # transparent
      table.border.bottom.style = "none",
      column_labels.font.weight = "normal",
      column_labels.border.top.style = "none",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "black",
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "white",
      stub.border.color = "white",
      stub.border.width = px(0),
      source_notes.font.size = 12,
      source_notes.border.lr.style = "none",
      table.font.size = 16,
      heading.align = "left",
      ...
    ) %>%
    opt_css(
      paste0("#", table_id, " tbody tr:last-child {border-bottom: 2px solid #ffffff00;}"),
      add = TRUE
    )
}
