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

dir_proj <- '62-safc'

blackish_background <- '#1c1c1c'
gray_points <- '#4d4d4d'
gray_text <- '#999999'

font <- 'Titillium Web'
extrafont::loadfonts(quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = font),
  title = element_text(size = 20, color = 'white'),
  plot.title = element_text(face = 'bold', size = 16, color = 'white'),
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
  # filter(home_name == 'San Antonio FC' | away_name == 'San Antonio FC') |> 
  # filter(date >= ymd('2022-03-12')) |>
  filter(!(date == ymd('2021-10-16') & match_id == 3566032L)) |> ## duplicate
  mutate(season = year(date)) |> 
  filter(season %in% c(2021L, 2022L)) 

usl_match_winners <- usl_matches_filt |>
  distinct(match_id, season, date, home_id, away_id, home_name, away_name, home_score, away_score, home_pen_score, away_pen_score) |> 
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
    rename(team_id = id) |> 
    inner_join(
      team_mapping,
      by = 'team_id'
    )
  
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

wide_poss_result_n <- poss_result |> 
  count(season, team_id, match_result = ifelse(match_result == 'w', 'w', 'ld'), lt50_poss = value < 50) |> 
  mutate(
    across(lt50_poss, ~ifelse(.x, 'lt50_poss', 'gte50_poss'))
  ) |> 
  pivot_wider(
    names_from = c(lt50_poss, match_result),
    values_from = n,
    values_fill = 0L
  ) |> 
  inner_join(
    team_mapping |> select(team_id, team_name),
    by = 'team_id'
  ) |> 
  relocate(team_name, .after = team_id)
wide_poss_result_n |> arrange(desc(lt50_poss_w))

pass_medians <- long_pass |> 
  inner_join(
    long_match_winners |> select(match_id, team_id, match_result),
    by = c('match_id', 'team_id')
  ) |> 
  group_by(season, team_id) |> 
  summarize(
    across(value, median)
  ) |> 
  ungroup() |> 
  inner_join(
    team_mapping |> select(team_id, team_name),
    by = 'team_id'
  ) |> 
  relocate(team_name, .after = team_id)
pass_medians |> arrange(value)

poss_result |> 
  count(team_id, match_result, lt50_poss = value < 50) |> 
  group_by(team_id, lt50_poss) |> 
  summarize(across(n, sum)) |> 
  ungroup() |> 
  group_by(team_id) |> 
  mutate(
    total = sum(n)
  ) |> 
  ungroup() |> 
  filter(lt50_poss) |> 
  mutate(
    prop = n / total
  ) |> 
  arrange(desc(prop))

n_matches_lt50_poss <- poss_result |> 
  filter(value < 50) |> 
  count(team_id, sort = TRUE) |> 
  inner_join(team_mapping, by = 'team_id')


p_init <- n_matches_lt50_poss |> 
  ggplot() +
  geom_quasirandom(
    aes(
      y = 1,
      x = value
    ),
    groupOnX = TRUE
  ) +
  facet_wrap(~stat, ncol = 1, scales = 'free')
gb <- p_init |> ggplot_build()
n_matches_lt50_poss_jittered <- gb$data[[1]] |> 
  bind_cols(n_matches_lt50_poss)

safc_team_id <- 722269L
lcfc_team_id <- 614316L
finals_team_ids <- c(safc_team_id, lcfc_team_id)
p <- ggplot() +
  aes(
    y = y,
    x = x
  ) +
  # geom_from_path(
  #   width = 0.1,
  #   alpha = 0.5,
  #   data = n_matches_lt50_poss_jittered |> filter(team_id != safc_team_id),
  #   aes(
  #     path = team_logo_url
  #   )
  # ) +
  geom_point(
    data = n_matches_lt50_poss_jittered,
    size = 2,
    color = gray_points
  ) +
  geom_from_path(
    width = 0.1,
    alpha = 1,
    data = n_matches_lt50_poss_jittered |> filter(season == 2022L, team_id %in% finals_team_ids),
    aes(
      path = team_logo_url
    )
  ) +
  labs(
    title = '# of matches with <50% possession',
    subtitle = '2022 USL season',
    y = '# of matches',
    x = NULL
  )
p
ggsave(
  p,
  filename = file.path(dir_proj, 'n_matches_lt50_poss.png'),
  width = 3.5,
  height = 7
)
