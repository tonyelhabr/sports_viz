
library(tidyverse)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = element_text('Karla', face = 'bold', size = 18, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', face = 'bold', size = 14, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  plot.caption = element_text('Karla', size = 14, color = 'gray50', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = element_text('Karla', size = 12, color = 'gray50', hjust = 0), 
  plot.tag.position = c(.01, 0.02),
  legend.text = element_text(size = 14),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

auth <- twitchr::twitch_auth()
user <- twitchr::get_users(login = 'nickwan_datasci')
id <- user %>% pull(id)
followers <- 
  twitchr::get_all_follows(to_id = id) %>% 
  mutate(
    across(followed_at, ~lubridate::with_tz(.x, 'America/New_York'))
  )
followers

ts_by_day_init <-
  followers %>% 
  mutate(
    date = followed_at %>% lubridate::date() 
  ) %>% 
  group_by(date) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(n_cumu = cumsum(n))
ts_by_day_init

seq_date <-
  seq.Date(
    ts_by_day_init$date %>% min(),
    ts_by_day_init$date %>% max(),
    by = 'day'
  )
seq_date

ts_by_day <-
  ts_by_day_init %>% 
  right_join(seq_date %>% tibble(date = ., n_fill = 0L)) %>% 
  arrange(date) %>% 
  fill(n_cumu) %>% 
  mutate(n = coalesce(n, n_fill))
ts_by_day

dates_sliced <- 
  seq.Date(
    lubridate::ymd('2021-03-02'), 
    lubridate::ymd('2021-03-23'), 
    by = 'week'
  )

dates_sliced_lead1 <- dates_sliced + lubridate::days(1)

ts_by_day_2021 <-
  ts_by_day %>% 
  mutate(year = date %>% lubridate::year()) %>% 
  filter(year == 2021L) 

.f_mark <- partial(
  ggforce::geom_mark_circle,
  label.family = 'Karla',
  color = 'black',
  ... = 
)

viz_by_day_2021 <-
  ts_by_day_2021 %>% 
  ggplot() +
  aes(x = date, y = n) +
  geom_segment(
    data = 
      . %>% 
      filter(!(date %in% c(dates_sliced, dates_sliced_lead1))),
    aes(x = date, xend = date, y = 0, yend = n),
    size = 0.5
  ) +
  geom_segment(
    data = . %>% filter(date %in% dates_sliced),
    aes(x = date, xend = date, y = 0, yend = n),
    size = 1.25,
    color = 'blue'
  ) +
  geom_segment(
    data = . %>% filter(date %in% dates_sliced_lead1),
    aes(x = date, xend = date, y = 0, yend = n),
    size = 1.25,
    color = 'red'
  ) +
  geom_point(
    data = . %>% 
      filter(!(date %in% c(dates_sliced, dates_sliced_lead1)))
  ) +
  geom_point(
    data = . %>% filter(date %in% dates_sliced),
    size = 3,
    color = 'blue'
  ) +
  geom_point(
    data = . %>% filter(date %in% dates_sliced_lead1),
    size = 3,
    color = 'red'
  ) +
  scale_x_date(
    limits = c(lubridate::ymd('2021-01-01'), max(seq_date)),
    date_breaks = '1 week',
    date_labels = '%m/%d'
  ) +
  ggtext::geom_richtext(
    data = . %>% inner_join(tibble(date = min(dates_sliced) - lubridate::days(1), lab = "<b><span style='font-size:14pt; color:blue'>\"Sliced\" Show</span></b>")),
    fill = NA, label.color = NA,
    family = 'Karla',
    hjust = 1,
    aes(y = 45, label = lab)
  ) +
  ggtext::geom_richtext(
    data = . %>% inner_join(tibble(date = min(dates_sliced_lead1) + lubridate::days(1), lab = "<b><span style='font-size:14pt; color:red'>Day After \"Sliced\"</span></b>")),
    fill = NA, label.color = NA,
    family = 'Karla',
    hjust = 0,
    aes(y = 35, label = lab)
  ) +
  theme(
    panel.grid.major.x = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = 'Nick\'s Daily # of New Twitch Followers'
  )
viz_by_day_2021

ggsave(
  plot = viz_by_day_2021,
  filename = file.path('19-nickwan', 'nick_wan_twitch_followers_w_sliced.png'),
  width = 12,
  height = 4,
  type = 'cairo'
)
