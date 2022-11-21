library(tidyverse)
library(qs)
library(extrafont)
library(ggtext)
library(patchwork)
library(gt)
library(gtExtras)
library(magick)
library(patchwork)
library(googlesheets4)

dir_proj <- '56-attendance'
path_data <- file.path(dir_proj, 'clean_data.qs')
path_venue_capacities <- file.path(dir_proj, 'venue_capacities.qs')

gray_wv <- rgb(24, 24, 24, maxColorValue = 255)
gray_grid_wv <- rgb(64, 64, 64, maxColorValue = 255)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla', color = gray_wv),
  title = element_text(size = 20, color = gray_wv),
  plot.title = ggtext::element_markdown(face = 'bold', size = 20),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown(size = 16),
  axis.text = element_text(size = 14),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = gray_grid_wv),
  panel.grid.minor = element_line(color = gray_grid_wv),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = 'white', color = 'white'),
  plot.caption = ggtext::element_markdown(hjust = 1, size = 10, face = 'italic'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 12, hjust = 0),
  plot.tag.position = c(0.01, 0.02),
  panel.background = element_rect(fill = 'white', color = 'white')
)

.gt_theme_538 <- function(data, ...) {
  data %>%
    gt::opt_all_caps() |> 
    gt::opt_table_font(
      font = list(
        gt::google_font('Karla'),
        gt::default_fonts()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = 'bottom', color = 'transparent', weight = gt::px(3)
      ),
      locations = gt::cells_body(
        columns = everything(),
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    gt::tab_options(
      column_labels.background.color = 'white',
      heading.border.bottom.style = 'none',
      table.border.top.width = gt::px(3),
      table.border.top.style = 'none',
      table.border.bottom.style = 'none',
      column_labels.font.weight = 'bold', # 'normal',
      column_labels.border.top.style = 'none',
      column_labels.border.bottom.width = gt::px(2),
      column_labels.border.bottom.color = 'black',
      row_group.border.top.style = 'none',
      row_group.border.top.color = 'black',
      row_group.border.bottom.width = gt::px(1),
      row_group.border.bottom.color = 'white',
      stub.border.color = 'white',
      stub.border.width = gt::px(0),
      data_row.padding = gt::px(3),
      source_notes.font.size = 12,
      source_notes.border.lr.style = 'none',
      table.font.size = 10, # 15
      heading.align = 'left',
      
      column_labels.font.size = 11,
      ...
    )
}

team_mapping <-  read_sheet(ss = '1nIw1PgozYsb11W-QSzjgPlW-qrsJWLiPB8x6fTTMqiI') |> 
  transmute(
    rn = row_number(),
    across(is_alternative_fbref, ~replace_na(.x, FALSE)),
    team_538,
    team_fbref_stats
  )

venue_capacities <- path_venue_capacities |> qs::qread()
us_map <- map_data('state') |> as_tibble()
us_border <- borders('usa', colour = gray_grid_wv)

df <- path_data |> qs::qread()

usl_teams <- df |> 
  filter(league == 'USL') |> 
  group_by(
    season, 
    team,
    venue, lat, long, capacity
  ) |> 
  summarize(
    n = n(),
    n_non_na = sum(!is.na(attendance)),
    across(
      c(attendance, attendance_prop, trunc_attendance_prop),
      median,
      na.rm = TRUE
    )
  ) |> 
  ungroup() |> 
  # filter(n > 1) |> 
  arrange(
    desc(attendance_prop)
  )

usl_venues <- usl_teams |> 
  filter(season == 2019) |> 
  group_by(team) |> 
  slice_max(n, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  mutate(
    rn = row_number((sqrt(lat^2+long^2))),
    .before = 1
  ) |> 
  arrange(rn)

base <- ggplot() +
  borders('usa') +
  coord_map('albers',  lat0 = 45.5, lat1 = 29.5) +
  theme(
    axis.text = element_blank(), 
    panel.grid.major = element_blank()
  ) +
  labs(x = NULL, y = NULL)

a <- 0.8
pal <- viridisLite::plasma(11, direction = -1, alpha = a)
p_map <- base +
  geom_point(
    data = usl_venues,
    show.legend = FALSE,
    shape = 21,
    color = gray_grid_wv,
    aes(
      fill = trunc_attendance_prop,
      size = trunc_attendance_prop,
      x = long,
      y = lat
    )
  ) +
  scale_size(
    range = c(2, 5)
  ) +
  ggrepel::geom_text_repel(
    data = usl_venues,
    family = 'Karla',
    fontface = 'bold',
    min.segment.length = 0,
    force = 5,
    aes(
      x = long,
      y = lat,
      label = rn
    )
  ) +
  scale_fill_stepsn(
    breaks = seq(0, 1, by = 0.1),
    colours = pal
  ) +
  ggtext::geom_richtext(
    data = tibble(),
    fill = NA_character_,
    label.color = NA_character_,
    family = 'Karla',
    hjust = 0,
    vjust = 0,
    size = 14 / .pt,
    aes(
      label = '<b>USL Median Team Attendance %</b><br/>2019',
      x = -130,
      y = 49
    )
  ) +
  ylim(NA, 55)
p_map

generate_gt <- function(df) {
  df |> 
    gt::gt() |> 
    gt::cols_label(
      rn = '#',
      path = 'Team',
      team = ' ',
      venue = 'Venue',
      capacity = 'Cap.',
      trunc_attendance_prop = '%'
    ) |> 
    .gt_theme_538() |> 
    gt::text_transform(
      locations = gt::cells_body('path'),
      fn = function(x) {
        gt::local_image(
          filename = x,
          height = 25
        )
      }
    ) |>
    gt::fmt_percent(
      decimals = 0,
      columns = 'trunc_attendance_prop'
    ) |> 
    gt::data_color(
      columns = 'trunc_attendance_prop',
      alpha = a,
      colors = scales::col_bin(pal, bins = seq(0, 1, by = 0.1))
    ) |> 
    gt::cols_align(
      align = 'right',
      columns = c('capacity', 'trunc_attendance_prop')
    )
}

## Try this https://stackoverflow.com/questions/65835639/arrange-gt-tables-side-by-side-or-in-a-grid-or-table-of-tables, since it doesn't save with some shiny tag
tbs <- usl_venues |> 
  inner_join(
    team_mapping |> 
      transmute(
        team = team_538,
        path = sprintf('%s/img/%s.png', dir_proj, team_fbref_stats)
      ),
    by = 'team'
  ) |> 
  transmute(
    rn,
    path,
    across(team, ~str_trunc(.x, 26)),
    across(venue, ~str_trunc(.x, 30)),
    # n_non_na,
    across(
      capacity,
      ~paste0(
        ifelse(
          .x %% 1000 == 0,
          .x / 1000,
          round(.x / 1000, 1)
        ),
        'K'
      )
    ),
    trunc_attendance_prop
  ) |> 
  gtExtras::gt_double_table(generate_gt, noisy = FALSE)
tbs[[1]]

tbs_html <- tbs |> 
  map(gt::as_raw_html)

tb <- data.frame(
  tb1 = tbs_html[[1]],
  tb2 = tbs_html[[2]]
) |> 
  gt::gt() |> 
  gt::fmt_markdown(columns = everything()) |> 
  gt::tab_options(
    column_labels.hidden = TRUE
  )

## https://github.com/ddsjoberg/bstfun/blob/master/R/as_ggplot.R
as_ggplot <- function(x, ...) {
  # save gt as image
  path <- tempfile(fileext = '.png')
  gt::gtsave(x, filename = path, vwidth = 1200, vheight = 800, zoom = 3, ...)
  path |> 
    magick::image_read() |> 
    magick::image_ggplot(interpolate = TRUE)
}
p_tb <- as_ggplot(tb)

l <- c(
  area(t = 1, l = 1, b = 8, r = 8),
  area(t = 1, l = 8, b = 8, r = 16)
)
ps <- p_map + p_tb + plot_layout(design = l)
w <- 16
ggsave(
  ps,
  filename = file.path(dir_proj, 'ex.png'),
  width = w,
  height = .5 * w
)

