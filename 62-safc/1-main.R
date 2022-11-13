library(worldfootballR)
library(dplyr)
library(tidyr)
library(purrr)
library(qs)
library(lubridate)
library(ggplot2)
library(ggbeeswarm)
library(ggpath)
library(extrafont)
library(ggtext)
library(scales)

dir_proj <- '62-safc'

blackish_background <- '#1c1c1c'
# gray_points <- '#4d4d4d'
# gray_text <- '#999999'

font <- 'Titillium Web'
extrafont::loadfonts(quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = font),
  title = element_text(size = 20, color = 'white'),
  plot.title = element_text(face = 'bold', size = 20, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = element_text(size = 16, color = '#f1f1f1'),
  axis.text = element_text(color = 'white', size = 14),
  axis.title = element_text(size = 16, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = gray_points),
  panel.grid.minor = element_line(color = gray_points),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  strip.text = element_text(size = 14, color = 'white', face = 'bold', hjust = 0.5),
  plot.background = element_rect(fill = blackish_background, color = blackish_background),
  plot.caption = element_text(color = 'white', hjust = 1, size = 10, face = 'italic'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 14, color = 'white', hjust = 0),
  plot.tag.position = c(0.01, 0.02),
  panel.background = element_rect(fill = blackish_background, color = blackish_background)
)

usl_matches <- load_fotmob_matches_by_date(league_id = 8972) |> 
  mutate(
    across(date, lubridate::date)
  )

usl_matches_filt <- usl_matches |> 
  filter(!match_status_cancelled) |> 
  filter(!(date == ymd('2021-10-16') & match_id == 3566032L)) |> ## duplicate
  mutate(season = year(date)) |> 
  filter(season %in% c(2021L, 2022L)) 

usl_match_winners <- usl_matches_filt |>
  distinct(
    match_id,
    season,
    date,
    home_id,
    away_id,
    home_name,
    away_name,
    home_score,
    away_score,
    home_pen_score,
    away_pen_score
  ) |>
  mutate(
    winning_side = case_when(
      !is.na(home_pen_score) & home_pen_score > away_pen_score ~ 'home',
      !is.na(away_pen_score) & home_pen_score < away_pen_score ~ 'away',
      home_score == away_score ~ 'draw',
      home_score > away_score ~ 'home',
      home_score < away_score ~ 'away'
    ),
    winning_team_id = case_when(
      winning_side == 'home' ~ home_id,
      winning_side == 'away' ~ away_id,
      winning_side == 'draw' ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  )
usl_match_winners

## most recently used team names for each id
team_mapping <- bind_rows(
  usl_match_winners |> 
    group_by(team_id = home_id, team_name = home_name) |> 
    slice_max(match_id, n = 1) |> 
    ungroup(),
  usl_match_winners |>
    group_by(team_id = away_id, team_name = away_name) |> 
    slice_max(match_id, n = 1) |> 
    ungroup()
) |> 
  select(team_id, team_name, match_id) |> 
  group_by(team_id) |> 
  slice_max(match_id, n = 1) |> 
  ungroup() |>
  select(team_id, team_name) |> 
  arrange(team_name) |> 
  mutate(
    team_logo_url = sprintf('https://images.fotmob.com/image_resources/logo/teamlogo/%s.png', team_id)
  )

get_fotmob_match_team_stats <- function(match_id, overwrite = FALSE) {
  path <- file.path(dir_proj, 'data', sprintf('%s.qs', match_id))
  if (file.exists(path) & isFALSE(overwrite)) {
    return(qs::qread(path))
  }
  Sys.sleep(runif(1))
  res <- worldfootballR::fotmob_get_match_team_stats(match_id)
  qs::qsave(res, path)
  res
}

usl_match_team_stats <- usl_matches_filt$match_id |> 
  map_dfr(get_fotmob_match_team_stats) |> 
  inner_join(
    usl_matches_filt |> select(match_id, date, season),
    by = 'match_id'
  ) |> 
  relocate(season, date, .after = match_id)

poss <- usl_match_team_stats |> 
  filter(stats_title == 'Ball possession') |> 
  mutate(
    across(ends_with('value'), as.integer)
  )

pass <- usl_match_team_stats |> 
  filter(stats_title == 'Accurate passes') |> 
  distinct(match_id, season, home_team_id, away_team_id, home_value, away_value) |> 
  mutate(
    across(
      c(home_value, away_value),
      ~as.integer(str_replace(.x, '(.*\\()([0-9]+)([%]\\))', '\\2')) / 100
    )
  )

pivot_stat_longer <- function(df) {
  df |> 
    select(match_id, season, home_id = home_team_id, away_id = away_team_id, home_value, away_value) |> 
    pivot_longer(
      -c(match_id, season),
      names_to = c('side', 'stat'),
      names_sep = '_'
    ) |> 
    pivot_wider(
      names_from = stat,
      values_from = value
    ) |> 
    rename(team_id = id)
}

long_poss <- poss |> pivot_stat_longer()
long_pass <- pass |> pivot_stat_longer()

long_match_winners <- usl_match_winners |> 
  select(match_id, home_id, away_id, winning_team_id) |> 
  pivot_longer(
    -c(match_id, winning_team_id),
    names_to = 'side',
    values_to = 'team_id',
    names_pattern = '(^.*)_id'
  ) |> 
  mutate(
    match_result = case_when(
      is.na(winning_team_id) ~ 'd',
      team_id == winning_team_id ~ 'w',
      team_id != winning_team_id ~ 'l',
    )
  )

poss_result <- long_poss |> 
  inner_join(
    long_match_winners |> select(match_id, team_id, match_result),
    by = c('match_id', 'team_id')
  )

lt50_poss <- poss_result |> 
  group_by(team_id, season) |> 
  mutate(n_matches = n()) |> 
  ungroup() |> 
  filter(value < 50) |> 
  count(team_id, season, n_matches, sort = TRUE) |> 
  transmute(
    team_id,
    season,
    stat = 'lt50_poss',
    value = n / n_matches
  )

lt50_poss_wds <- poss_result |> 
  filter(match_result %in% c('w', 'd')) |> 
  group_by(team_id, season) |> 
  mutate(n_matches = n()) |> 
  ungroup() |> 
  filter(value < 50) |> 
  count(team_id, season, n_matches, sort = TRUE) |> 
  transmute(
    team_id,
    season,
    stat = 'lt50_poss_given_wds',
    value = n / n_matches
  ) |> 
  arrange(desc(value))

pass_acc_means <- long_pass |> 
  group_by(season, team_id) |> 
  summarize(
    across(value, mean)
  ) |> 
  ungroup() |> 
  transmute(
    team_id,
    season,
    stat = 'pass_accuracy',
    value
  )

stat_mapping <- c(
  'lt50_poss' = '% matches with\n<50% possession',
  'lt50_poss_given_wds' = '% of won or draw matches\nhaving <50% possession',
  'pass_accuracy' = 'Average match\npass accuracy'
)

df <- bind_rows(
  lt50_poss,
  lt50_poss_wds,
  pass_acc_means
) |> 
  inner_join(
    stat_mapping |> enframe('stat', 'stat_lab'),
    by = 'stat'
  ) |> 
  mutate(
    across(stat_lab, ~factor(.x, labels = unname(stat_mapping)))
  )

p_init <- df |> 
  ggplot() +
  geom_quasirandom(
    aes(
      x = stat_lab,
      y = value
    ),
    groupOnX = TRUE
  )

gb <- p_init |> ggplot_build()
df_jittered <- gb$data[[1]] |> 
  bind_cols(df) |> 
  inner_join(
    team_mapping,
    by = 'team_id'
  )

safc_team_id <- 722269L
lcfc_team_id <- 614316L
finals_team_ids <- c(safc_team_id, lcfc_team_id)

top_df_jittered <- df_jittered |> 
  filter(season == 2022L, team_id %in% finals_team_ids)

p <- ggplot() +
  aes(
    y = y,
    x = x
  ) +
  geom_from_path(
    width = 0.1,
    alpha = 0.5,
    data = df_jittered |>
      anti_join(
        top_df_jittered |>
          select(team_id, season),
        by = c('team_id', 'season')
      ),
    aes(
      path = team_logo_url
    )
  ) +
  # geom_point(
  #   data = df_jittered |> 
  #     anti_join(
  #       top_df_jittered |> 
  #         select(team_id, season),
  #       by = c('team_id', 'season')
  #     ),
  #   size = 2,
  #   color = gray_points
  # ) +
  geom_from_path(
    width = 0.25,
    alpha = 1,
    data = top_df_jittered,
    aes(
      path = team_logo_url
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~stat_lab, nrow = 1, scales = 'free') +
  labs(
    title = '2022 USL Championship Final Preview',
    subtitle = 'Finals teams compared to all 2021 and 2022 USL teams',
    x = NULL,
    y = NULL
  ) +
  theme(
    panel.background = element_rect(color = 'white'),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank()
  )
p

path <- file.path(dir_proj, 'usl_2022_finals_preview.png')
ggsave(
  p,
  filename = path,
  width = 10,
  height = 8
)

path_res <- add_logo(
  path_viz = path,
  path_logo = file.path(dir_proj, 'usl-bw.png'),
  delete = TRUE,
  logo_scale = 0.1,
  adjust_y = FALSE,
  adjust_x = TRUE,
  idx_x = 0.01,
  idx_y = 0.98
)
