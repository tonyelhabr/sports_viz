
library(tidyverse)
library(qs)
library(lubridate)

dir_proj <- '56-attendance'
dir_data <- file.path(dir_proj, 'data')
path_attendance <- file.path(dir_proj, 'attendance.qs')
path_importance <- file.path(dir_proj, 'importance.qs')
path_team_logos <- file.path(dir_proj, 'team_logos.qs')
path_venue_capacities <- file.path(dir_proj, 'venue_capacities.csv')
path_team_mapping <- file.path(dir_proj, 'fbref_538_team_mapping.csv')

## outputs
path_data <- file.path(dir_proj, 'data.qs')
path_logos <- file.path(dir_proj, 'logos.qs')

filter_dates <- function(df) {
  df |> 
    filter(!is.na(date), date < lubridate::ymd('2022-06-02')) ## scrape date
}
attendance <- path_attendance |> 
  qs::qread() |> 
  filter_dates()
importance <- path_importance |> 
  qs::qread() |> 
  filter_dates()
team_logos <- path_team_logos |> qs::qread()

venue_capacities <- path_venue_capacities |> 
  read_csv(
    col_types = cols(
      venue = 'c', 
      season = 'i',
      max_attendance = 'i',
      capacity = 'i'
    )
  )

team_mapping <- path_team_mapping |> 
  read_csv(show_col_types = FALSE)

logos <- team_mapping |>
  left_join(
    team_logos |>
      select(team_fbref_stats = name, home_logo_path = path),
    by = 'team_fbref_stats'
  ) |> 
  select(team = team_538, path = home_logo_path)

league_mapping <- tibble(
  league_fbref = c('Premier League', 'EFL Championship', 'Major League Soccer', 'USL Championship'),
  league_538 = c('Barclays Premier League', 'English League Championship', 'Major League Soccer', 'United Soccer League'),
  league_abbrv = c('EPL', 'EFL', 'MLS', 'USL')
)

make_season_label <- function(season, is_pre_pandemic) {
  case_when(
    season == 2020 & is_pre_pandemic ~ '2020, Pre-Pandemic',
    season == 2020 & !is_pre_pandemic ~ '2020, Post-Pandemic',
    TRUE ~ as.character(season)
  ) |> 
    factor(levels = c('2018', '2019', '2020, Pre-Pandemic', '2020, Post-Pandemic', '2021', '2022'))
}

df <- attendance |> 
  transmute(
    season,
    date,
    across(wk, as.integer),
    is_weekend = day %in% c('Sat', 'Sun'),
    league,
    home_team,
    away_team,
    venue,
    attendance
  ) |> 
  left_join(
    team_mapping |>
      select(home_team = team_fbref, home_team_stats = team_fbref_stats),
    by = 'home_team'
  ) |>
  select(-home_team_stats) |> 
  left_join(
    venue_capacities |> 
      select(venue, season, lat, long, capacity),
    by = c('venue', 'season')
  ) |> 
  inner_join(
    team_mapping |> 
      select(home_team = team_fbref, team_538),
    by = 'home_team'
  ) |> 
  select(-home_team) |> 
  rename(home_team = team_538) |> 
  inner_join(
    team_mapping |> 
      select(away_team = team_fbref, team_538),
    by = 'away_team'
  ) |> 
  select(-away_team) |> 
  rename(away_team = team_538) |> 
  inner_join(
    league_mapping |> 
      select(league = league_fbref, league_538, league_abbrv),
    by = 'league'
  ) |> 
  select(-league) |> 
  rename(league = league_538) |> 
  left_join(
    importance |> 
      select(
        date,
        league,
        home_team,
        home_importance,
        away_importance
      ),
    by = c('date', 'league', 'home_team')
  ) |> 
  select(-league) |> 
  rename(league = league_abbrv) |> 
  ## TODO:
  ## 1. Factor month?
  ## 2. Add a is_second_half column (harder to calculate)
  mutate(
    is_pre_pandemic = date <= lubridate::ymd('2020-03-15'),
    is_post_pandemic = date <= lubridate::ymd('2021-07-01'),
    season_label = make_season_label(season, is_pre_pandemic),
    across(matches('importance$'), ~.x / 100),
    importance = (home_importance + away_importance) / 2,
    attendance_prop = attendance / capacity,
    trunc_attendance_prop = ifelse(attendance_prop > 1, 1, attendance_prop)
  )
qs::qsave(df, path_data)
qs::qsave(logos, path_logos)

## extra ----

usl <- df |> filter(league == 'USL') 
safc <- df |> filter(home_team == 'San Antonio FC')

## debugging ----
usl |>
  select(
    season,
    date,
    home_team,
    away_team,
    venue,
    attendance,
    capacity,
    attendance_prop,
    trunc_attendance_prop
  ) |>
  slice_max(attendance_prop, n = 20, with_ties = F)

usl |> 
  ggplot() +
  aes(x = capacity, y = attendance_prop) +
  geom_point()

usl_teams <- df |> 
  filter(league == 'USL') |> 
  group_by(
    season, 
    team = home_team,
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
us_map <- map_data('state') |> as_tibble()
us_border <- borders('usa', colour = gray_grid_wv)

base <- ggplot() +
  # geom_polygon(
  #   data = us_border,
  #   aes(x = long, y = lat, group = group),
  #   color = 'grey50',
  #   size = 0.25,
  #   fill = NA
  # ) +
  borders('usa') +
  coord_map('albers',  lat0 = 45.5, lat1 = 29.5) +
  theme(
    axis.text = element_blank(), 
    panel.grid.major = element_blank()
  ) +
  labs(x = NULL, y = NULL)
base

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
    min.segment.length = 0,
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
  labs(
    title = 'USL Median Team Attendance %',
    subtitle = '2019'
  )
p_map
# gb <- ggplot_build(p_map)
# gb$data[[2]]$colour |> table()

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

library(gt)
library(gtExtras)
library(patchwork)

generate_gt <- function(df) {
  df |> 
    gt::gt() |> 
    gt::cols_label(
      rn = '#',
      team = 'Team',
      venue = 'Venue',
      capacity = 'Cap.',
      trunc_attendance_prop = '%'
    ) |> 
    .gt_theme_538() |> 
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

# usl_venue_tables <- usl_venues |> 
#   select(
#     rn,
#     team,
#     venue,
#     # n_non_na,
#     capacity,
#     trunc_attendance_prop
#   ) |> 
#   gt_double_table(generate_gt) |> 
#   gt_two_column_layout(zoom = 1, output = 'html')

## Try this https://stackoverflow.com/questions/65835639/arrange-gt-tables-side-by-side-or-in-a-grid-or-table-of-tables, since it doesn't save with some shiny tag
tbs <- usl_venues |> 
  transmute(
    rn,
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
  gt_double_table(generate_gt, noisy = FALSE)

tb <- data.frame(
  tb1 = tbs[[1]] |> 
    gt::tab_footnote(
      footnote = '% of capacity',
      locations = gt::cells_column_labels(columns = 'trunc_attendance_prop')
    ) |> 
    gt::as_raw_html(),
  tb2 = tbs[[2]] |> 
    gt::tab_footnote(
      footnote = gt::html('<br/>'),
      locations = gt::cells_column_labels(columns = 'trunc_attendance_prop')
    ) |> 
    gt::as_raw_html()
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
# gt::gtsave(tb, filename = file.path(dir_proj, 'tbs.html'))

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

# usl_teams_by_season <- df |> 
#   filter(
#     league == 'USL'
#   ) |>
#   group_by(
#     season,
#     season_label = make_season_label(season, is_pre_pandemic),
#     team = home_team, logo_path = home_logo_path,
#     venue, lat, long, capacity, max_capacity
#   ) |> 
#   summarize(
#     n = n(),
#     n_non_na = sum(!is.na(attendance)),
#     across(
#       c(attendance, attendance_prop, trunc_attendance_prop),
#       median,
#       na.rm = TRUE
#     )
#   ) |> 
#   ungroup() |> 
#   arrange(team, season_label)
# 
# diff_props <- usl_teams_by_season |> 
#   filter(
#     season != 2020
#   ) |> 
#   select(team, season, season_label, n, prop = trunc_attendance_prop) |> 
#   group_by(team) |> 
#   mutate(
#     prion_n = lag(n),
#     prior_prop = lag(prop)
#   ) |> 
#   ungroup() |> 
#   mutate(
#     diff_prop = prop - prior_prop,
#     pct_prop = diff_prop / prior_prop
#   ) |> 
#   ungroup() |> 
#   arrange(desc(diff_prop))
# diff_props |> filter(team == 'Birmingham Legion FC')
# 
# diff_props |> 
#   ggplot() +
#   aes(x = season_label, y = prop, color = team, group = team) +
#   geom_point(aes(size = n)) +
#   geom_line() +
#   guides(color = 'none', size = 'none')
