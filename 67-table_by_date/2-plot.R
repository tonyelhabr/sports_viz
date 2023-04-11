library(dplyr)
library(qs)
library(ggplot2)
library(scales)
library(forcats)

proj_dir <- '67-table_by_date'
cumu_epl_table_by_gp <- qs::qread(file.path(proj_dir, 'cumu_epl_table_by_gp.qs'))
cumu_epl_table_by_date <- qs::qread(file.path(proj_dir, 'cumu_epl_table_by_date.qs'))

summarize_props <- function(df) {
  df |> 
    group_by(name) |> 
    summarize(
      prop_top4 = sum(rank <= 4) / n(),
      prop_top6 = sum(rank <= 6) / n(),
      prop_bot3 = sum(rank >= 18) / n()
    ) |> 
    ungroup()
}

cumu_epl_prop_by_gp <- cumu_epl_table_by_gp |> 
  filter(gp > 3) |>
  summarize_props()

cumu_epl_prop_by_date <- cumu_epl_table_by_date |> 
  filter(date >= '2022-08-22') |> 
  summarize_props()

cumu_epl_props <- inner_join(
  cumu_epl_prop_by_gp |> 
    rename_with(~paste0(.x, '_gps'), -name),
  cumu_epl_prop_by_date  |> 
    rename_with(~paste0(.x, '_days'), -name),
  by = join_by(name)
)

long_cumu_epl_props <- cumu_epl_props |> 
  pivot_longer(
    -name,
    names_pattern = '(^.*)_(gps|days)',
    names_to = c('prop_type', 'type'),
    values_to = 'prop'
  ) |> 
  mutate(
    across(prop_type, ~str_remove(.x, '^prop_'))
  )

pivoted_cumu_epl_props <- long_cumu_epl_props |> 
  pivot_wider(
    names_from = type,
    values_from = prop
  ) |> 
  mutate(
    d = gps - days
  )

pivoted_cumu_epl_props |> 
  group_by(prop_type, with_ties = FALSE) |> 
  slice_max(d, n = 3) |> 
  ungroup()

pivoted_cumu_epl_props |> 
  group_by(prop_type) |> 
  slice_min(d, n = 3, with_ties = FALSE) |> 
  ungroup()

epl_props_top4 <- pivoted_cumu_epl_props |> 
  filter(prop_type == 'top4') |> 
  filter(days > 0, gps > 0) |> 
  transmute(
    across(name, ~fct_reorder(.x, days)),
    gps, 
    days
  ) |> 
  pivot_longer(
    -name,
    names_to = 'by',
    values_to = 'value'
  )

epl_props_top4_manu <- epl_props_top4 |> filter(name == 'Man United')
epl_props_top4 |> 
  ggplot() +
  aes(
    x = value,
    y = name,
    group = by,
    fill = by
  ) +
  geom_col(
    position = position_dodge2(padding = 0)
  ) +
  scale_x_continuous(
    labels = percent
  ) +
  ggtext::geom_richtext(
    data = tibble(lab = glue::glue('<b><span style="color:white">In terms of actual days, Spurs have been in the top top 4 for much longer than they have actually been in the top 4 in terms of matches</b>'), name = 'Tottenham'),
    aes(x = 0.79, y = 2.1, label = lab),
    family = 'Titillium Web',
    size = pts(11.5),
    fill = NA_character_,
    label.color = NA_character_,
    hjust = 0,
    vjust = 0
  )
labs(
  title = 'Proportion of season spent in top 4 of table',
  subtitle = "Which teams feels like they've been in the top 4 longer than they actually have?",
  y = NULL,
  x = '% of season in Top 4'
)



pivoted_cumu_epl_props |> 
  ggplot() +
  aes(x = gps, y = (gps - days)) +
  geom_point(aes(color = prop_type))

cumu_epl_table_by_gp |> filter(name == 'Tottenham', gp >= 19)

complete_cumu_epl_table_by_date <- cumu_epl_table_by_date |> 
  group_by(date) |> 
  filter(all(gp > 3)) |> 
  ungroup()

latest_epl_table <- complete_cumu_epl_table_by_date |> 
  slice_max(date, n = 1, with_ties = TRUE)
first_epl_table <- complete_cumu_epl_table_by_date |> 
  slice_min(date, n = 1, with_ties = TRUE)


d <- complete_cumu_epl_table_by_date |> 
  left_join(
    latest_epl_table |> select(name, latest_rank = rank),
    by = join_by(name)
  ) |> 
  mutate(
    name = fct_reorder(name, -latest_rank)
  )

big6_names <- c('Arsenal', 'Man City', 'Man United', 'Tottenham', 'Liverpool', 'Chelsea')
d_highlight <- d |> filter(name %in% big6_names)
d_bckgrd <- d |> filter(!(name %in% big6_names))

d |> 
  ggplot() +
  aes(
    x = date,
    y = -rank,
    group = name,
    color = name
  ) +
  geom_line(
    data = d_bckgrd,
    size = 1.5,
    color = '#DBDBDB'
  ) +
  geom_line(
    data = d_highlight,
    size = 1.5,
    # width = 1,
    show.legend = FALSE,
    aes(color = name),
  ) +
  scale_x_date(
    date_breaks = '1 month',
    date_labels = '%b'
  ) +
  scale_y_continuous(
    breaks = -20:-1,
    labels = rep('', 20),
    sec.axis = sec_axis(
      ~.,
      breaks = -20:-1,
      labels = rev(latest_epl_table$name)
    )
  )
