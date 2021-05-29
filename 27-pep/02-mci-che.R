# https://en.wikipedia.org/wiki/List_of_European_Cup_and_UEFA_Champions_League_finals
library(tidyverse)
dir_proj <- '27-pep'

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla', color = 'gray20'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 18, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown('Karla', face = 'bold', size = 18, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  axis.title = element_text(size = 18, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  plot.caption = ggtext::element_markdown('Karla', size = 11, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 11, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.01),
  legend.text = element_text(color = 'gray20', size = 14),
  # strip.text = element_text(color = 'gray20', size = 14),
  # strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

supplement <-
  file.path(dir_proj, 'supplement.csv') %>% 
  read_csv()
supplement

df <- 
  file.path(dir_proj, 'ucl_matches.csv') %>% 
  read_csv() %>% 
  mutate(
    across(date, ~lubridate::mdy(.x)),
    cnd_h = !is.na(g_h) & (g_h > g_a | (!is.na(penalty_h) & penalty_h > penalty_a)),
    cnd_a = !is.na(g_a) & (g_h < g_a | (!is.na(penalty_a) & penalty_h < penalty_a))
  ) %>% 
  left_join(supplement %>% rename_all(~sprintf('%s_h', .x))) %>% 
  left_join(supplement %>% rename_all(~sprintf('%s_a', .x)))
df

lubridate::year(df[['date']]) <- ifelse(lubridate::month(df[['date']]) <= 6L, 2031, 2030)

seq_date_first <- seq.Date(lubridate::ymd('2030-09-01'), lubridate::ymd('2031-08-01'), by = 'month')
seq_date_mid <- seq.Date(lubridate::ymd('2030-09-15'), lubridate::ymd('2031-08-15'), by = 'month')
seq_date_mid
seq_date <- sort(c(seq_date_first, seq_date_mid))
seq_date
labs_date <- rep(c(month.abb[c(9:12)], month.abb[c(1:8)]), each = 2) # %>% sort()
labs_date
idx <- (seq_along(labs_date) %% 2) == 1L
idx
labs_date[idx] <- ''
labs_date
length(seq_date)
length(labs_date)

f_text <- partial(
  ggtext::geom_richtext,
  label.color = NA,
  fill = NA,
  family = 'Karla',
  fontface = 'bold',
  # hjust = 0,
  aes(label = lab),
  ... = 
)

w <- 10
buffer <- 0
date_1_split <- lubridate::ymd('2030-10-15')

# fmt_w <- '<span style="color:black;font-size:14pt">%s</span>'
fmt_w <- '<span style="color:%s;font-size:12pt">%s</span>'
# fmt_pen <- '<span style="font-size:8pt"> (%s)</span>'
df_aug <-
  df %>%
  mutate(
    lab = 
      # gplots::col2hex('grey80')
      sprintf(
        '<span style="color:#333333;font-size:10pt">%s %s%s - %s%s %s%s</span>',
        ifelse(
          cnd_h,
          sprintf(fmt_w, color_pri_h, abbrv_h),
          abbrv_h
        ),
        ifelse(is.na(g_h), '?', g_h),
        ifelse(!is.na(penalty_h), sprintf('<span style="font-size:8pt"> (%s)</span>', penalty_h), ''),
        ifelse(!is.na(penalty_a), sprintf('<span style="font-size:8pt">(%s) </span>', penalty_a), ''),
        ifelse(is.na(g_a), '?', g_a),
        ifelse(
          # !is.na(g_a) & (g_h < g_a | (!is.na(penalty_a) & penalty_h < penalty_a)), 
          cnd_a,
          sprintf(fmt_w, color_pri_a, abbrv_a),
          abbrv_a
        ),
        # sprintf('%s%s', date, ifelse(type == 'ucl', ' (UCL Final)', ''))
        ifelse(type == 'ucl', ' <span style="font-size:8pt">(UCL Final)</span>', '')
      )
  )
df_aug

df_final <-
  df_aug %>% 
  select(season, date, type, lab, matches('cnd'), matches('url')) %>% 
  filter(type %in% c('1', '2', 'ucl'))
df_final

df_logo <-
  df_final %>% 
  filter(cnd_h | cnd_a) %>% 
  mutate(url = ifelse(cnd_h, url_logo_538_h, url_logo_538_a)) %>% 
  # select(-matches('url_')) %>% 
  select(season, date, type, url)
df_logo

grid <-
  seq.Date(lubridate::ymd('2030-09-01'), lubridate::ymd('2031-07-01'), by = 'day') %>% 
  tibble(date = .) %>% 
  crossing(df %>% select(season)) %>% 
  # crossing(
  #   tibble(season1 = 2007:2020) %>% 
  #     mutate(season2 = sprintf('%02d', season1 %% 2000 + 1)) %>% 
  #     unite(matches('season'), col = 'season', sep = '-')
  # ) %>% 
  left_join(df_final %>% select(season, date, type, lab, matches('cnd'))) %>% 
  mutate(
    across(season, ordered)
  ) %>% 
  mutate(
    has_game = ifelse(!is.na(lab), TRUE, NA)
  )
grid

date_ucl_current <- lubridate::ymd('2031-05-29')
# gr <- 1.618
ratio <- 16 / 9
h_img <- 500
w_img <- round(h_img / ratio, 0)
p <- 
  grid %>% 
  # drop_na(lab) %>% 
  # filter(type %in% c('1', '2', 'ucl')) %>% 
  ggplot() +
  aes(x = date, y = season) +
  # geom_tile(
  #   data =
  #     grid %>%
  #     drop_na(lab) %>%
  #     filter(!(cnd_h | cnd_a)) %>% filter(!(date == !!date_ucl_current)),
  #   fill = 'grey80',
  #   height = 0.4, width = (2 * w)
  # ) +
  geom_point(
  # ggforce::geom_circle(
    data =
      grid %>%
      drop_na(lab) %>%
      filter(!(cnd_h | cnd_a)) %>% filter(!(date == !!date_ucl_current)),
    fill = 'grey80',
    shape = 1,
    size = 10
  ) +
  # geom_tile(
  #   data = 
  #     grid %>% 
  #     drop_na(lab) %>% 
  #     filter(date == !!date_ucl_current),
  #   # color = 'grey20',
  #   size = 2,
  #   color = 'grey80',
  #   fill = 'white',
  #   height = 0.4, width = (2 * w), show.legend = FALSE
  # ) +
  # ggimage::geom_image(
  #   data =
  #     grid %>%
  #     drop_na(lab) %>%
  #     filter(date == !!date_ucl_current),
  #   size = 0.06,
  #   aes(image = file.path(dir_proj, 'pep-headshot.png'))
  # ) +
  ggimage::geom_image(
    # 1.5 ratio to match how this gets saved
    data = df_logo %>% mutate(across(url, ~sprintf('%s&w=%s&h=500', .x, !!w_img))),
    size = 0.05,
    aes(image = url)
  ) +
  f_text(
    data =
      grid %>%
      filter(type == '1') %>%
      filter(!(type == '1' & date > date_1_split)) %>%
      mutate(date = date + lubridate::days(w + buffer)),
    hjust = 0
  ) +
  f_text(
    data =
      grid %>%
      filter(type == '2' | (type == '1' & date > !!date_1_split)) %>%
      mutate(date = date + lubridate::days(-(w + buffer))),
    hjust = 1
  ) +
  f_text(
    data = 
      grid %>%
      filter(type == 'ucl') %>% 
      mutate(date = date + lubridate::days(w + buffer)),
    hjust = 0
  ) +
  # scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  # coord_cartesian(clip = 'off') +
  scale_x_date(labels = labs_date, breaks = seq_date, limits = range(seq_date)) +
  # coord_equal(ratio = 1 / gr) +
  theme(
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    panel.grid.major.y = element_blank(),
    axis.ticks.x = element_line(color = c('grey80', rep(c(NA, 'grey80'), t = length(seq_date)))),
    panel.grid.major.x = element_line(color = c('grey80', rep(c(NA, 'grey80'), t = length(seq_date)))),
    axis.ticks.y = element_blank()
  ) +
  labs(
    title = 'Champions League (Re-)Matches between Teams from the Same League',
    subtitle = 'Since 2007-08 Season',
    tag = '**Viz**: Tony ElHabr',
    caption = 'Only domestic league matches and Champions League matches are shown.',
    x = NULL,
    y = NULL
  )
p

h <- 7
ggsave(
  plot = p,
  filename = file.path(dir_proj, 'viz_ucl_rematches.png'),
  # width = 2 * h / gr,
  width = h * ratio,
  height = h,
  # units = 'in',
  type = 'cairo'
)

