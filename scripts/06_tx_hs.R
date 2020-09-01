
extrafont::loadfonts(device = 'win', quiet = TRUE)
library(tidyverse)
library(here)
library(ggtext)

theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = element_text('Karla', face = 'bold', size = 18, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', face = 'bold', size = 14, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  # axis.title = element_text(size = 24, face = 'bold'),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  # axis.text = element_text('Karla', size = 12, face = 'bold', color = 'gray20'),
  # axis.title.x = element_text(hjust = 0.95),
  # axis.title.y = element_text(hjust = 0.95),
  # axis.line = element_line(color = 'gray80'),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  # plot.margin = margin(25, 25, 25, 25),
  plot.margin = margin(10, 10, 10, 10),
  # plot.background = element_rect(fill = '#F3F4F6', color = '#F3F4F6'),
  plot.background = element_rect(fill = '#fffaf0', color = NA),
  # plot.caption = element_text(size = 15, face = 'italic'),
  plot.caption = element_text('Karla', size = 14, color = 'gray50', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = element_text('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.02),
  # legend.text = element_text(size = 14),
  legend.text = element_text(size = 14),
  # legend.background = element_rect(fill = '#F3F4F6', color = '#F3F4F6'),
  # legend.position = c(.85, .85))
  panel.background = element_rect(fill = '#fffaf0', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

# funcs ----
import_bands <- memoise::memoise({
  function(path_export = here::here('data-raw', '06', 'tx-hs-band.rds')) {
    if(fs::file_exists(path_export)) {
      return(read_rds(path_export))
    }
    
    require(rvest)
    url <- 'https://smbc.uiltexas.org/archives.htm'
    page <- url %>% xml2::read_html()
    # children <- page %>% rvest::html_children()
    main <- page %>% rvest::html_nodes('.main')
    df <- 
      main %>% 
      rvest::html_node('table') %>% 
      rvest::html_table(fill = TRUE) %>% 
      .[[1]] %>% 
      as_tibble() %>% 
      janitor::clean_names()
    res <-
      df %>%
      mutate(idx = row_number()) %>% 
      relocate(idx) %>% 
      rename(conf = conference) %>% 
      filter(conf != 'B') %>% # Drop the 1979 year, where `conf == 'B'`
      filter(round != 'Round') %>% # Extra heading row at the end
      separate(school_isd, into = c('school', 'isd'), sep = '\\s\\(') %>% 
      mutate(
        across(c(place, year), as.integer),
        across(isd, ~str_remove(.x, '\\)$'))
      ) %>% 
      mutate(tier = conf %>% str_sub(1, 1) %>% as.integer()) %>% 
      select(idx, year, conf, tier, round, place, school, isd, notes)
    write_rds(res, path_export)
    res
  }
})

# instructions:
# 1. Make a copy of https://drive.google.com/open?id=1BRTPCLzc09oW6NpqPoHabXwKwjzwxOVO to your local google drive (link originally found here: https://www.6atexasfootball.com/forum/main-forum/199726-complete-game-by-game-football-histories/page10).
# 2. Download a copy of this workbook.
# 3. Show formulas on the Data sheet for column A.
# 4. Copy-paste formulas to a text file.
.get_fb_score_links_df <- memoise::memoise({function() {
  path_links <- here::here('data-raw', '06', 'tx-hs-fb-links.txt')
  lines <- path_links %>% read_lines()
  df <- lines %>% tibble(line = .)
  df
  res <-
    df %>% 
    mutate(has_link = line %>% str_detect('^[=]')) %>% 
    mutate(
      link = line %>% str_remove_all('^[=]HYPERLINK\\(|\\,.*\\)$|["]') %>% if_else(has_link, ., NA_character_),
      # This `school` extraction actaully also works fine for those without links
      school = line %>% str_remove_all('^.*\\,.|\\)$|["]'),
    ) %>% 
    select(school, has_link, link)
  res
}})

.download_fb_scores <-
  function(link,
           dir = here::here('data-raw', '06', 'schools'),
           file = '',
           path = fs::path(dir, sprintf('%s.xlsx', file))) {
    if (fs::file_exists(path)) {
      return(path)
    }
    id <- link %>% googledrive::as_id()
    dr <- id %>% googledrive::as_dribble()
    res <- dr %>% googledrive::drive_download(path = path)
    path
  }

.import_fb_scores <- function(path) {
  res_init <- path %>% readxl::read_excel(col_types = 'text')
  res <-
    res_init %>% 
    select(
      coach = Coach,
      season = Season,
      district = District,
      conf = Classification,
      date = Date,
      week = Week,
      opp = Opponent,
      pf = PF,
      pa = PA,
      mov = `Margin of Victory`, 
      w = Win,
      l = Loss,
      t = Tie,
      gp = `Games Played`, 
      w_cumu = `All Time Wins`,
      l_cumu = `All Time Losses`,
      t_cumu = `All Time Ties`,
      g_cumu = `All Time Games`,
      w_frac_cumu = `Win Percentage`
    ) %>% 
    mutate(
      across(date, lubridate::mdy),
      across(c(season, week, pf, pa, mov, w, l, t, gp, w_cumu, l_cumu, t_cumu, g_cumu), as.integer),
      across(w_frac_cumu, as.double)
    )
  res
}

retrieve_fb_scores <- function(links = NULL, path = here::here('data-raw', '06', 'fb_scores.rds')) {
  
  if(fs::file_exists(path)) {
    res <- path %>% read_rds()
    return(res)
  }
  
  if(is.null(links)) {
    links <-
      .get_fb_score_links_df() %>% 
      filter(has_link) %>%
      select(-has_link)
  }
  
  links_info <-
    links %>% 
    mutate(path = map2_chr(link, school, ~.download_fb_scores(link = ..1, file = ..2))) %>% 
    select(-link)
  
  .import_fb_scores_q <- quietly(.import_fb_scores)
  res <-
    links_info %>% 
    mutate(data = map(path, ~.import_fb_scores_q(.x) %>% pluck('result'))) %>% 
    select(-path) %>% 
    unnest(data)
  
  write_rds(res, path)
  res
}

# Have to filter for these cuz the scores data only has 5A and 6A of present day.
filter_conf <- function(data) {
  res <- data %>% filter(conf %in% sprintf('%dA', 5:6))
}

# bands ----
bands_raw <- import_bands()

bands <- 
  bands_raw %>% 
  select(-notes) %>% 
  mutate(
    across(school, ~str_remove_all(.x, '\\sHS|\\,')), 
    across(isd, ~str_remove(.x, '\\sC?ISD$')),
    across(round, ~ordered(.x, c('Prelims', 'Finals')))
  ) %>% 
  mutate(
    across(
      where(is.character),
      ~iconv(.x, from = 'ASCII', to = 'UTF-8') %>% str_replace_all('B\\s', ' ')
    )
  ) %>%
  # This is the only weird case that needs a fix.
  mutate(across(school, ~case_when(str_detect(.x, 'Bowie') ~ 'Austin Bowie', TRUE ~ .x))) %>% 
  arrange(year, round, tier, school) %>% 
  mutate(idx = row_number()) %>% 
  group_by(school) %>% 
  arrange(year, round, .by_group = TRUE) %>% 
  # Index by school (to idenitfy last appearance later)
  mutate(idx_grp = row_number()) %>% 
  ungroup()
bands

# Adding percent ranks. Preparing for ranking aggregation.
bands_aug <-
  bands %>% 
  # filter(!is.na(place)) %>% 
  mutate(
    # rnk_conf = 1L,  # (tier - min(tier) + 1L),
    prnk_conf = (tier - min(tier) + 1L) / (max(tier) - min(tier) + 1L),
    is_imputed = if_else(is.na(place), TRUE, FALSE),
    w = if_else(!is.na(place) & place == 1L, 1L, 0L)
  ) %>% 
  mutate(
    w_final = if_else(w == 1L & round == 'Finals', 1L, 0L)
  ) %>% 
  group_by(year, conf, round) %>% 
  mutate(n = n()) %>% 
  mutate(
    across(place, ~coalesce(.x, n))
  ) %>% 
  mutate(
    # Adjust for weird cases where not all bands are listed for a comp, so a band can have a lower place than that which would be possible if looking only at the number of bands appearing in these records.
    across(n, ~if_else(.x < place, place, .x))
  ) %>% 
  mutate(
    rnk_place = (n - place + 1L),
    prnk_place = ((n - place + 1L) / (n - min(place) + 1L))
  ) %>% 
  ungroup() %>% 
  mutate(
    score_raw = rnk_place,
    # score_rnk = ifelse(round == 'Prelims', 1, 2) * rnk_conf * rnk_place,
    score_prnk = ifelse(round == 'Prelims', 0.5, 1) * prnk_conf * prnk_place
  )

bands_schools <-
  bands_aug %>% 
  group_by(school) %>% 
  filter(idx_grp == max(idx_grp)) %>% 
  ungroup() %>% 
  select(school, isd, year, conf, idx, idx_grp) %>% 
  # `idx_grp` == `n_app` for last `idx_grp`
  arrange(desc(idx))
bands_schools

# Ranking bands.
bands_agg <-
  bands_aug %>% 
  # mutate(across(place, ~coalesce(.x, round(n / 2)))) %>% 
  # filter(!is.na(place)) %>% 
  group_by(school) %>% 
  summarize(
    # across(c(score_rnk, score_prnk, w, w_final), sum), 
    across(c(score_raw, score_prnk, w, w_final), sum), 
    across(c(place, n, prnk_place), mean), 
    n_app = n()
  ) %>% 
  ungroup() %>% 
  inner_join(bands_schools %>% select(school, conf)) %>% # Add the conference.
  mutate(
    rnk_raw = row_number(desc(score_raw)),
    rnk_prnk = row_number(desc(score_prnk)),
    rnk = row_number(desc(score_prnk))
  ) %>% 
  # relocate(rnk_rnk, rnk_prnk) %>% 
  relocate(matches('^rnk')) %>% 
  # select(-rnk_rnk) %>% 
  arrange(rnk)
bands_agg

# Didn't filter by conference earlier since bands could have been in lower conferences before.
bands_agg_filt <- bands_agg %>% filter_conf()
bands_agg_filt

bands_agg_filt_adj <-
  bands_agg_filt %>% 
  # select(-matches('^rnk')) %>% 
  mutate(
    rnk_raw = row_number(rnk_raw),
    rnk_prnk = row_number(rnk_prnk), 
    rnk = row_number(rnk)
  )
bands_agg_filt_adj

# fb ----
fb <- retrieve_fb_scores() %>% select(-coach, -opp, -mov, -date, -matches('cumu$'))
fb

# Identify current day schools.
fb_last <- 
  fb %>% 
  filter(season == 2019L) %>% 
  group_by(school) %>% 
  # arrange(week) %>% 
  filter(week == max(week)) %>% 
  ungroup()
fb_last

fb_proc <-
  fb %>% 
  semi_join(fb_last %>% select(school)) %>% 
  # Band competition data starts in 1979.
  filter(season >= 1979) %>% 
  # Some records have `gp == 0` indicating that it was a bye week. Leaving these weeks is probably harmless (as long as you adjust for them), but I like getting rid of them.
  filter(gp == 1L) %>% 
  group_by(school) %>% 
  arrange(season, week, .by_group = TRUE) %>% 
  mutate(
    idx_school = row_number(),
    g_cumu = cumsum(gp),
    across(c(w, l, t), list(cumu = cumsum))
  ) %>% 
  ungroup() %>% 
  mutate(w_frac_cumu = w_cumu / g_cumu)
fb_proc

fb_proc_last <- 
  fb_proc %>% 
  group_by(school) %>% 
  filter(g_cumu == max(g_cumu)) %>% 
  ungroup() %>% 
  mutate(
    rnk = row_number(desc(w_cumu))
  ) %>% 
  arrange(rnk)
# fb_proc_last %>% arrange(desc(w_frac_cumu))
fb_proc_last %>% arrange(desc(w_cumu))

schools_dict <- 
  here::here('data-raw', '06', 'schools_dict.csv') %>% 
  read_csv(locale = locale(encoding = 'ASCII')) %>%
  mutate(across(c(not_downloaded, is_ambiguous), ~coalesce(.x, FALSE))) %>% 
  filter(!not_downloaded & !is_ambiguous) %>% 
  select(matches('school'))
schools_dict

df <-
  bind_rows(
    bands_agg_filt_adj%>%
      select(school, rnk, score_raw, score_prnk, w, w_final, n_app) %>% 
      pivot_longer(
        -c(school)
      ) %>% 
      mutate(src = 'band') %>% 
      rename(school_band = school) %>% 
      inner_join(schools_dict),
    fb_proc_last %>% 
      select(-w) %>% 
      # filter(school == 'Cedar Park') %>% 
      select(school, rnk, g = g_cumu, w = w_cumu, w_frac = w_frac_cumu) %>% 
      pivot_longer(
        -c(school)
      ) %>% 
      mutate(src = 'fb') %>% 
      rename(school_fb = school) %>% 
      left_join(schools_dict)
  ) %>% 
  relocate(src) %>%
  mutate(school = school_fb) %>% 
  arrange(school)
df

df_wide <-
  df %>% 
  select(school, src, name, value) %>% 
  pivot_wider(
    names_from = c('name', 'src'),
    values_from = c('value')
  ) %>% 
  mutate(across(rnk_band, ~coalesce(.x, max(rnk_band, na.rm = TRUE))), across(score_prnk_band, ~coalesce(.x, 0)))
df_wide

df_agg <-
  df %>% 
  group_by(src, name) %>% 
  summarize(
    across(c(value), list(mean = mean))
  ) %>% 
  ungroup()
df_agg

n_top <- 30L
df_filt <- df_wide %>% filter(rnk_band <= n_top & rnk_fb <= n_top)
df_filt %>% 
  inner_join(schools_dict %>% rename(school = school_fb)) %>% 
  inner_join(bands_schools %>% select(school_band = school, isd))
df_filt
bands_schools
fit <-
  df_wide %>%
  lm(formula(rnk_fb ~ rnk_band), data = .) %>%
  broom::tidy()
fit

df_wide %>%
  select(matches('^rnk'), score_prnk_band, w_fb, w_frac_fb) %>%
  corrr::correlate()

df_filt_other <- 
  df_wide %>% 
  filter(rnk_band == 1L | rnk_fb == 1L) %>% 
  mutate(lab = case_when(rnk_band == 1L ~ sprintf('%s (%s)', school, 'Best Band'), TRUE ~ sprintf('%s (%s)', school, 'Best Football Team')))
df_filt_other
df_filt_anti <- df_wide %>% anti_join(df_filt) %>% anti_join(df_filt_other)
df_filt_anti

arw_annotate <- arrow(length = unit(3, 'pt'), type = 'closed')
viz <-
  df_wide %>% 
  ggplot() +
  aes(x = score_prnk_band, y = w_fb) +
  geom_point(
    data = df_filt,
    aes(size = w_frac_fb),
    color = 'red'
  ) +
  geom_point(
    data = df_filt_other,
    color = 'blue',
    aes(size = w_frac_fb)
  ) +
  geom_point(
    data = df_filt_anti %>% filter(score_prnk_band == 0),
    aes(size = w_frac_fb),
    alpha = 0.1
  ) +
  geom_point(
    data = df_filt_anti %>% filter(score_prnk_band > 0),
    aes(size = w_frac_fb)
  ) +
  # ggforce::geom_mark_circle(
  #   data = df_filt,
  #   label.fill = NA,
  #   # n = 0,
  #   expand = unit(3, 'mm'),
  #   aes(label = school, group = school)
  # ) +
  ggrepel::geom_text_repel(
    data = df_filt_other,
    box.padding = 1,
    family = 'Karla',
    # arrow = arw_annotate,
    aes(label = lab, group = school)
  ) +
  ggrepel::geom_text_repel(
    data = df_filt,
    box.padding = 1,
    lineheight = -2,
    family = 'Karla',
    # arrow = arw_annotate,
    aes(label = school, group = school)
  ) +
  # geom_smooth(method = 'lm') +
  scale_radius(name = 'Win %', range = c(0.1, 5), labels = scales::percent) +
  annotate(
    geom = 'curve',
    x = 9.8,
    y = 160,
    xend = 9.3,
    yend = 176,
    size = 1,
    # angle = -75,
    curvature = -0.1,
    arrow = arw_annotate
  ) +
  geom_text(
    inherit.aes = FALSE,
    aes(x = 9.85, y = 160),
    size = 4,
    vjust = 1,
    hjust = 0,
    lineheight = 1,
    # family = 'Arial',
    # fontface = 'bold',
    family = 'Karla',
    color = 'grey20',
    label = glue::glue('Size of circle indicates football team win %.
                       Some schools have high win %\'s but are newer (started after 1980), 
                       so they haven\'t accumulated as many wins as older schools.')
  ) +
  theme(
    # legend.spacing.x = unit(0, 'cm'),
    # legend.title = element_text(size = 12),
    # legend.text = element_text(size = rel(0.6)),
    # legend.margin = margin(-10, 0, -1, 0),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.subtitle = element_markdown(size = 10),
    legend.position = 'none'
  ) +
  labs(
    title = 'Do the Best Texas High School Football Teams also have the Best Marching Bands?',
    subtitle = glue::glue('The correlation between a school\'s football team wins and a weighted sum of the school\'s band competition placings is only <b><span style="color:black">18.5%</span></b> (177 large schools).<p><span style="color:red">4 schools</span> rank among the top 30 in both football and band.'),
    # caption = 'The scale for band competition placings is not intuitive, so the numbers are not shown. The methodology employs percent ranks.',
    tag = 'Viz: @TonyElHabr\nData (band): https://www.uiltexas.org/music/archives\nData (football): https://www.6atexasfootball.com',
    y = 'Football Team Wins Since 1980', 
    x = 'Weighted Sum of\nBand Competition Placings Since 1980'
  )
viz
ggsave(plot = viz, filename = here::here('plots', 'tx_hs_fb_band.png'), width = 12, height = 8, type = 'cairo')

