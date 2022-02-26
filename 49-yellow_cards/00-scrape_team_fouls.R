
library(tidyverse)
library(worldfootballR)
library(glue)
library(janitor)
library(ggridges)
library(extrafont)
library(ggtext)
library(cowplot)

gray_wv <- rgb(24, 24, 24, maxColorValue = 255)
gray_grid_wv <- rgb(64, 64, 64, maxColorValue = 255)

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla', color = 'white'),
  title = element_text('Karla', size = 24, color = 'white'),
  plot.title = element_text('Karla', face = 'bold', size = 24, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', size = 18, color = '#f1f1f1'),
  axis.text = element_text('Karla', color = 'white', size = 14),
  axis.title = element_text('Karla', size = 14, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = gray_grid_wv),
  panel.grid.minor = element_line(color = gray_grid_wv),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = gray_wv, color = gray_wv),
  plot.caption = element_text('Karla', color = 'white', hjust = 1, size = 12, face = 'italic'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'white', hjust = 0),
  plot.tag.position = c(0.01, 0.01),
  strip.text = element_text('Karla', color = 'white', face = 'bold', size = 12, hjust = 0),
  panel.background = element_rect(fill = gray_wv, color = gray_wv),
  legend.text = element_text('Karla', color = 'white', hjust = 1, size = 12)
)


dir_proj <- '49-yellow_cards'
dir_data <- file.path(dir_proj, 'data')
dir.create(dir_data, showWarnings = FALSE)

params <- crossing(
  country = c('ENG', 'ITA', 'GER', 'FRA', 'ESP'),
  season_end_year = c(2013:2022)
)
params

time <- worldfootballR::get_season_team_stats(
  country = 'ENG', 
  gender = 'M', 
  season_end_year = 2022, 
  tier = '1st', 
  stat_type = 'country_table'
) %>% 
  as_tibble()

scrape_season_team_stats <- function(country, season_end_year, stat_type = 'misc', overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('season-%s-%s-%s.rds', stat_type, country, season_end_year))
  suffix <- glue::glue('for `country = "{country}"`, `season_end_year = "{season_end_year}"`, `stat_type = "{stat_type}"`.')
  if(file.exists(path) & !overwrite) {
    cat(glue::glue('{Sys.time()}: Returning early {suffix}'), sep = '\n')
    return(read_rds(path))
  }
  res <-  worldfootballR::get_season_team_stats(
    country = country, 
    gender = 'M', 
    season_end_year = season_end_year, 
    tier = '1st', 
    stat_type = stat_type
  )
  cat(glue::glue('{Sys.time()}: Retrieved data {suffix}'), sep = '\n')
  write_rds(res, path)
  res
}

f <- possibly(scrape_season_team_stats, otherwise = tibble(), quiet = FALSE)

do_scrape <- function(params, .stat_type, ...) {
  params %>% 
    mutate(
      data = map2(country, season_end_year, ~f(country = ..1, season_end_year = ..2, stat_type = .stat_type))
    ) %>% 
    select(data) %>% 
    unnest(data) %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    transmute(
      country, 
      season = sprintf('%s-%s', season_end_year - 1, str_sub(season_end_year, 3, 4)), 
      team = squad, 
      ...
    )
}

fouls <- params %>% 
  do_scrape(
    'misc',
    side = team_or_opponent,
    fouls = fls
  ) %>% 
  filter(side == 'team')

mp <- params %>% 
  filter(
    season_end_year == 2022
  ) %>% 
  do_scrape(
    'league_table',
    mp
  )

fouls_extrapolated <- fouls %>% 
  left_join(
    mp
  ) %>% 
  mutate(
    across(mp, replace_na, 38),
    across(
      fouls,
      list(
        extrapolated = ~.x * (38 / mp)
      )
    ),
    fouls_per_match = fouls_extrapolated / 38
  )


## in season ----
params <- crossing(
  country = 'ENG',
  gender = 'M',
  season_end_year = c(2015:2022),
  tier = '1st'
)
params

urls <- params %>% 
  mutate(
    url = 
      pmap(
        list(country, gender, season_end_year, tier),
        worldfootballR::get_match_urls
      )
  ) %>% 
  unnest(url)
urls

scrape_misc_stats <- function(url, stat_type = 'misc', team_or_player = 'player', overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('stats-%s-%s-%s.rds', basename(url), stat_type, team_or_player))
  suffix <- glue::glue('for `url = "{url}"`, `stat_type = "{stat_type}"`, `team_or_player = "{team_or_player}"`.')
  
  if(file.exists(path) & !overwrite) {
    cat(glue::glue('{Sys.time()}: Returning early {suffix}'), sep = '\n')
    return(read_rds(path))
  }
  res <- worldfootballR::get_advanced_match_stats(url, stat_type = stat_type, team_or_player = team_or_player)
  cat(glue::glue('{Sys.time()}: Retrieved data {suffix}'), sep = '\n')
  write_rds(res, path)
  res
}


older_epl_urls <- urls %>% filter(country == 'ENG', season_end_year >= 2015, season_end_year <= 2017)
newer_epl_urls <- urls %>% filter(country == 'ENG', season_end_year >= 2018)
h <- possibly(scrape_misc_stats, otherwise = tibble())

do_scrape_misc_stats <- function(df, .stat_type) {
  df %>% 
    mutate(
      data = map(url, h, stat_type = .stat_type)
    ) %>% 
    select(data) %>% 
    unnest(data) %>% 
    janitor::clean_names() %>% 
    transmute(
      # across(season, str_replace, '-20', '-'), 
      across(season, str_remove_all, '-.*$'),
      date = match_date %>% lubridate::mdy(), 
      # link = game_url %>% basename(), 
      team, 
      fouls = fls
    )
}

older_misc <- older_epl_urls %>% 
  do_scrape_misc_stats('summary')
newer_misc <- newer_epl_urls %>% 
  do_scrape_misc_stats('misc')

misc <- bind_rows(
  older_misc,
  newer_misc
)
misc

misc_agg <- misc %>% 
  group_by(season, date, team) %>% 
  summarize(
    across(fouls, sum)
  ) %>% 
  ungroup()

mp_epl_2022 <- mp %>% 
  filter(
    season == '2021-22', country == 'ENG'
  ) %>% 
  pull(mp) %>% 
  sum()

min_mp_epl_2022 <- mp %>% 
  filter(
    season == '2021-22', country == 'ENG'
  ) %>% 
  slice_min(mp, n = 1, with_ties = FALSE) %>% 
  pull(mp)

mp_epl_20222 <- mp %>% 
  filter(
    season == '2021-22', country == 'ENG'
  ) %>% 
  pull(mp) %>% 
  sum()

fouls_agg_trunc <- misc_agg %>% 
  group_by(season, team) %>% 
  slice_min(date, n = min_mp_epl_2022, with_ties = FALSE) %>% 
  summarize(
    mp = n(),
    across(
      fouls,
      sum
    )
  ) %>% 
  ungroup() %>% 
  mutate(
    fouls_per_match = fouls / mp
  )
fouls_agg_trunc

fouls_agg_trunc2 <- misc_agg %>% 
  group_by(season) %>% 
  slice_min(date, n = mp_epl_20222, with_ties = FALSE) %>% 
  ungroup() %>% 
  group_by(season, team) %>% 
  summarize(
    mp = n(),
    across(
      fouls,
      sum
    )
  ) %>% 
  ungroup() %>% 
  mutate(
    fouls_per_match = fouls / mp
  )

p <- fouls_agg_trunc2 %>% 
  ggplot() +
  aes(
    alpha = fouls_per_match,
    fill = stat(x),
    y = season,
    x = fouls_per_match
  ) +
  geom_abline(
    data = tibble(),
    color = 'white',
    size = 2,
    linetype = 2,
    aes(
      intercept = 5.5,
      slope = 0
    )
  ) +
  geom_density_ridges_gradient(
    scale = 1.5,
    rel_min_height = 0.01,
    quantiles = 4,
    quantile_lines = TRUE
  ) +
  scale_fill_viridis_c(option = 'B', end = 0.1, begin = 1) +
  guides(
    fill = FALSE
  ) +
  scale_alpha_continuous(
    range = c(0.1, 0.2)
  ) +
  coord_flip() +
  labs(
    x = 'Fouls per match',
    y = NULL
  )


p <- fouls_extrapolated %>% 
  filter(country == 'ENG') %>% 
  ggplot() +
  aes(
    alpha = fouls_per_match,
    fill = stat(x),
    y = season,
    x = fouls_per_match
  ) +
  geom_abline(
    data = tibble(),
    size = 2,
    linetype = 2,
    color = gray_grid_wv,
    aes(
      intercept = 5.5,
      slope = 0
    )
  ) +
  geom_density_ridges_gradient(
    scale = 1.5,
    rel_min_height = 0.01,
    quantiles = 4,
    quantile_lines = TRUE
  ) +
  scale_fill_viridis_c(option = 'B', end = 0, begin = 1) +
  guides(
    fill = 'none'
  ) +
  scale_alpha_continuous(
    range = c(0.1, 0.2)
  ) +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = 'Has the Premier League been more aggressive\nsince the 2017-18 season?',
    x = 'Fouls per match',
    y = NULL,
    caption = 'Ridge lines mark 25th, 50th and 75th quantiles across the 20 teams.',
    tag = '**Viz**: Tony ElHabr | **Data**: fbref via {worldfootballR}'
  )
p

path_fouls <- file.path(dir_proj, 'fouls_per_match.png')
ggsave(
  plot = p,
  filename = path_fouls,
  width = 10,
  height = 7.5
)

tonythemes:::add_logo(
  path_viz = path_fouls,
  path_logo = file.path(dir_proj, 'epl-logo-white.png'),
  delete = FALSE,
  logo_scale = 0.1,
  idx_x = 0.01,
  idx_y = 0.98,
  adjust_y = FALSE
)

