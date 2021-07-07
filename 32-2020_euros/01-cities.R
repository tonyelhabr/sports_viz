
library(tidyverse)
library(rvest)

dir_proj <- '32-2020_euros'

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 24, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown('Karla', face = 'bold', size = 18, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  # plot.caption = ggtext::element_markdown('Karla', size = 16, color = 'gray20', hjust = 1),
  plot.caption = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.016),
  # legend.text = element_text(color = 'gray20', size = 14),
  # strip.text = element_text(color = 'gray20', size = 14),
  # strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))


cities_db <- 
  maps::world.cities %>%
  as_tibble() %>% 
  set_names(c('city', 'country', 'pop', 'lat', 'lon', 'is_capital')) %>% 
  mutate(across(city, ~str_remove(.x, "^'")))
cities_db

cities_venues <-
  file.path(dir_proj, 'cities.csv') %>%
  read_csv() %>% 
  separate(city, into = c('city', 'country'), sep = '\\,\\s') %>% 
  # select(-country) %>% 
  inner_join(cities_db)
cities_venues
cities_db %>% filter(city == 'London')

urls <-
  worldfootballR::get_match_urls(
    country = '', 
    gender = 'M', 
    season_end_year = 2021, 
    tier = '', 
    non_dom_league_url = 'https://fbref.com/en/comps/676/history/European-Championship-Seasons'
  )
urls

lineups <-
  urls %>% 
  map_dfr(~worldfootballR::get_match_lineups(.x), .id = 'idx') %>% 
  as_tibble() %>%
  janitor::clean_names() %>% 
  mutate(across(idx, as.integer))
lineups

lineups_agg <-
  lineups %>% 
  mutate(across(idx, as.integer)) %>% 
  rename(mp = min) %>% 
  group_by(idx) %>% 
  summarize(across(mp, max, na.rm = TRUE)) %>% 
  ungroup()
lineups_agg

df <-
  worldfootballR::get_match_results(
    country = '', 
    gender = 'M', 
    season_end_year = 2021, 
    tier = '', 
    non_dom_league_url = 'https://fbref.com/en/comps/676/history/European-Championship-Seasons'
  ) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>%
  mutate(idx = row_number())

df_aug <-
  df %>% 
  select(idx, round, day, date, time, home, away, g_h = home_goals, g_a = away_goals, venue) %>% 
  drop_na(g_h) %>% 
  left_join(lineups_agg) %>% 
  mutate(
    across(venue, ~str_remove(.x, '\\s\\(.*$') %>% str_trim())
  )
df_aug

df_agg <-
  df_aug %>% 
  group_by(venue) %>% 
  summarize(
    n = n(),
    mp = sum(mp),
    across(matches('^g_[ha]$'), sum, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(g_tot = g_h + g_a) %>% 
  mutate(g_avg = g_tot / n, g_avg_wt = 90 * g_tot / mp) %>% 
  inner_join(cities_venues) %>% 
  arrange(desc(g_avg))
df_agg

# https://github.com/elidom/my_TidyTues/blob/master/TidyTues_32_Energy.Rmd
world <- rnaturalearth::ne_countries(scale = 'medium', returnclass = 'sf') #  %>% as_tibble()
world %>% relocate(last_col())

wt_agg <-
  df_agg %>% 
  summarize(
    across(
      g_avg_wt,
      list(
        min = min,
        max = max
      ),
      .names = '{fn}'
    )
  )
wt_agg

df_agg_viz <-
  df_agg %>% 
  mutate(
    s = scales::rescale(g_avg_wt, from = c(wt_agg$min, wt_agg$max), to = c(11, 22)),
    lab = sprintf('<b><span style="font-size:%.1fpt">%.2f</span></b><span style="font-size:11pt"> %s</span>', s, g_avg_wt, venue)
  )
df_agg_viz

f_text <- partial(
  ggtext::geom_richtext,
  label.color = NA,
  family = 'Karla',
  aes(label = lab),
  ... = 
)

cities_left <- c('London', 'Munich', 'Bucharest', 'Saint Petersburg')
g_avg_wt_caption <- df_agg_viz %>% filter(city == 'Baku') %>% pull(g_avg_wt)
viz <-
  df_agg_viz %>% 
  ggplot() +
  aes(x = lon, y = lat, size = g_avg_wt) +
  # scale_size_area() %>% 
  geom_sf(data = world, inherit.aes = FALSE, fill = NA, colour = 'grey60') +
  geom_point(
    data = df_agg_viz %>% filter(city != 'London'),
    color = 'black'
  ) +
  geom_point(
    data = df_agg_viz %>% filter(city == 'London'),
    # size = 2,
    color = 'red'
  ) +
  # scale_size(range = c(2, 5)) +
  f_text(
    data = 
      df_agg_viz %>% 
      filter(!(city %in% cities_left)) %>% 
      mutate(lab = sprintf('<b><span style="font-size:%.1fpt">%.2f</span></b> <span style="font-size:11pt">%s</span>', s, g_avg_wt, venue)),
    hjust = 0,
    nudge_x = 0.7
  ) +
  f_text(
    data = 
      df_agg_viz %>%
      filter((city %in% cities_left)) %>%
      filter(city != 'London') %>% 
      mutate(lab = sprintf('<span style="font-size:11pt">%s</span> <b><span style="font-size:%.1fpt">%.2f</span></b>', venue, s, g_avg_wt)),
    hjust = 1,
    nudge_x = -0.7
  ) +
  f_text(
    data = 
      df_agg_viz %>%
      filter((city == 'London')) %>% 
      mutate(lab = sprintf('<span style="font-size:11pt;color:red">%s</span> <b><span style="font-size:%.1fpt;color:red">%.2f</span></b>', venue, s, g_avg_wt)),
    hjust = 1,
    nudge_x = -0.7
  ) +
  coord_sf(xlim = c(-10, 30), ylim = c(37, 62)) +
  theme(
    legend.position = 'none',
    panel.grid.major = element_blank(),
    axis.text = element_blank()
  ) +
  labs(
    title = '<span style="font-size:22pt;color:#333333">Match Goals Per 90 By Venue</span>',
    subtitle = '<span style="color:#7F7F7F">The trophy may be coming </span><span style="color:red">home</span><span style="color:#7F7F7F">, but the goals sure aren\'t.</span>',
    # subtitle = '<span style="color:#7F7F7F">Is it really coming </span><span style="color:red">home</span><span style="color:#7F7F7F">?</span>',
    caption = sprintf('Updated through 2021-07-06<br/>Not shown: Baki Olimpiya Stadionu (%.2f)', g_avg_wt_caption),
    tag = '**Viz**: Tony ElHabr | **Data** fbref',
    x = NULL,
    y = NULL
  )
viz

path_viz <- file.path(dir_proj, 'cities.png')
ggsave(
  plot = viz,
  filename = path_viz,
  width = 8,
  height = 8,
  type = 'cairo'
)

add_logo(
  path_viz = path_viz,
  path_logo = file.path(dir_proj, 'euros-2020.png'),
  logo_scale = 0.1,
  # delete = FALSE,
  idx_x = 0.06,
  # adjust_y = FALSE,
  idx_y = 0.07
)
