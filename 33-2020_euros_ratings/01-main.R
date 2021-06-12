
library(tidyverse)

dir_proj <- '33-2020_euros_ratings'
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
  plot.caption = ggtext::element_markdown('Karla', size = 16, color = 'gray20', hjust = 1),
  # plot.caption = element_text('Karla', size = 12, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.01),
  legend.text = element_text(color = 'gray20', size = 14),
  # strip.text = element_text(color = 'gray20', size = 14),
  # strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))


f_read <- function(x) {
  file.path(dir_proj, sprintf('%s.csv', x)) %>% 
    read_csv()
}
leagues_init <-
  f_read('leagues') %>% 
  rename(league_name = name) # %>% 
  # mutate(
  #   across(
  #     league_name,
  #     # Specifically for MLS playoffs
  #     ~case_when(
  #       country == 'USA' & league_name %>% str_detect('Major League Soccer') ~ 'Major League Soccer',
  #       country == 'Spain' & league_name == 'Primera Division' ~ 'LaLiga',
  #       TRUE ~ .x
  #     )
  #   )
  # )
leagues_init
leagues_init %>% count(league_id)
league_names <-
  leagues_init %>%
  arrange(league_id, desc(season)) %>% 
  group_by(league_id) %>% 
  filter(row_number() == 1L) %>% 
  ungroup() %>% 
  select(-season)
league_names %>% filter(country == 'Spain')

leagues <- leagues_init %>% select(-league_name) %>% inner_join(league_names)
leagues
leagues %>% filter(country != 'International')

leagues %>% filter(country == 'USA') %>% count(league_id)
teams <- f_read('teams')
players <- f_read('players') %>% mutate(across(dob, lubridate::date))

# TODO: Empirical bayes?
player_ratings <-
  f_read('player_ratings') %>% 
  mutate(
    value = offensive_value + defensive_value
  )
player_ratings

leagues_n <- 
  leagues %>% 
  count(country, league_name, sort = TRUE) %>% 
  filter(league_name %>% str_detect('Europa|Champions|Playoff', negate = TRUE)) %>% 
  filter(league_name != 'Major League Soccer') %>% 
  filter(n > 2L)
leagues_n

age_grps <-
  tibble(
    from = c(18L, 24L, 27L, 30L),
    to = c(24L, 27L, 30L, 36L)
  )
age_grps

position_mapping <-
  tibble(
    position_old = c('AM', 'FW', 'M', 'DM', 'D'),
    position = c('A', 'A', 'M', 'M', 'D')
  )
position_mapping

df_init <-
  list(
    leagues %>% semi_join(leagues_n %>% select(-n)),
    teams,
    player_ratings,
    players %>% 
      drop_na(position, dob) %>% 
      select(player_id = id, player_name = name, position, dob) %>% 
      mutate(across(position, ~str_remove_all(.x, '\\(.*\\)') %>% str_remove_all('\\,.*$'))) %>% 
      filter(position != 'GK') %>% 
      rename(position_old = position) %>% 
      inner_join(position_mapping) %>% 
      select(-position_old)
  ) %>% 
  reduce(inner_join) %>%  
  group_by_at(vars(-c(games_played:value))) %>% 
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    value_pm = value / minutes,
    v = 90 * value_pm
  ) %>% 
  filter(minutes > (5 * 90)) %>% 
  # filter(minutes != 0) %>% 
  mutate(age = lubridate::time_length(lubridate::ymd(sprintf('%s-08-01', season)) - dob, 'year') %>% floor() %>% as.integer()) %>% 
  filter(age >= 18 & age <= 35) %>% 
  distinct()
df_init

df_init <-
  data.table::as.data.table(df_init %>% mutate(age2 = age, age3 = age))[
    data.table::as.data.table(age_grps), 
    on=.(age2 >= from, age3 < to)
  ] %>% 
  as_tibble() %>% 
  unite('age_grp', age2, age3, sep = '<=x<')
df_init

# df_init %>% count(age_grp, sort = TRUE)
# df %>% filter(is.na(v))

df_filt <- df_init %>% filter(minutes >= (20 * 90))
df_filt

estimate_beta <- function(x) {
  mu <- mean(x)
  var <- var(x)
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  list(alpha = alpha, beta = beta)
}

# estimate_beta(df_filt$value_pm)
# estimate_beta(df_filt$v)
lst <- estimate_beta(df_filt$value_pm)
lst

# df_init %>% 
#   relocate(minutes, v, value, value_pm) %>% 
#   mutate(v2 = 90 * (value + lst$alpha) / (minutes + lst$alpha + lst$beta)) %>% 
#   relocate(v2) %>% 
#   sample_frac(0.1) %>% 
#   ggplot() +
#   aes(x = v, y = v2) +
#   geom_point(alpha = 0.1)

df <-
  df_init %>% 
  mutate(v = 90 * (value + lst$alpha) / (minutes + lst$alpha + lst$beta)) 

v_min <- 
  df %>% 
  drop_na(v) %>% 
  filter(!is.infinite(v)) %>% 
  summarize(`_` = min(v)) %>% 
  pull(`_`)
v_min

df <- df %>% mutate(across(v, ~log(.x + abs(!!v_min) + 0.001)))
df

agg <-
  df %>% 
  group_by(league_id, league_name, country, season, position, age_grp) %>% 
  summarize(
    n = n(),
    across(
      v,
      list(mean = mean, sd = sd), na.rm = TRUE, .names = '{.fn}'
    )
  ) %>% 
  ungroup() %>% 
  filter(n > 1L)
agg

df <-
  df %>% 
  inner_join(agg) %>%
  mutate(z = (v - mean) / sd)
df

df %>% count(age_grp, sort = TRUE)
df %>% count(position, sort = TRUE)
df %>% 
  mutate(across(age_grp, factor)) %>% 
  ggplot() +
  aes(x = z, color = age_grp) +
  geom_density() +
  coord_cartesian(xlim = c(-3, 3))
df

df %>% 
  ggplot() +
  aes(x = z) +
  geom_histogram(binwidth = 0.1) +
  coord_cartesian(xlim = c(-3, 3))

ids <-
  df %>% 
  distinct(player_id, league_id)
ids

ids_gt1 <-
  ids %>% 
  count(player_id, sort = TRUE) %>% 
  filter(n > 1L) %>% 
  select(-n) %>% 
  inner_join(ids)
ids_gt1

ids_gt1_meta <-
  ids_gt1 %>% 
  left_join(
    df %>% 
      select(player_id, league_id, country, league_name, season, z) %>% 
      unite('league', country, league_name, sep = '_') %>% 
      mutate(across(league, ~str_replace_all(.x, '\\s|[.]', '_') %>% str_replace_all('[_]+', '_')))
  )
ids_gt1_meta %>% count(league, sort = TRUE)

f_rename <- function(suffix) {
  ids_gt1_meta %>% 
    mutate(dummy = 0) %>% 
    select(player_id, dummy, league, matches('^(season|z)')) %>% 
    rename_with(~sprintf('%s_%s', .x, suffix), c(league, matches('^(season|z)')))
}

res_init <-
  full_join(
    f_rename(1),
    f_rename(2)
  ) %>% 
  select(-dummy) %>% 
  filter(league_1 != league_2) %>% 
  mutate(z_diff = z_1 - z_2)
res_init

res_init_filt <-
  res_init %>% 
  filter((season_2 == (season_1 + 1)) | (season_2 == season_1))
res_init_filt

dummies <-
  res_init_filt %>% 
  select(season = season_1, matches('league'), z_diff) %>% 
  mutate(idx = row_number()) %>% 
  pivot_longer(-c(idx, season, z_diff)) %>% 
  mutate(across(name, ~str_remove(.x, 'league_') %>% as.integer())) %>% 
  mutate(across(name, ~if_else(.x == 1L, -1L, 1L))) %>% 
  pivot_wider(names_from = value, values_from = name, values_fill = 0L) %>% 
  select(-idx) %>% 
  # Make this the NA coefficient
  relocate(Netherlands_Eredivisie, .after = last_col())
dummies

fit_dummy <- dummies %>% lm(formula(z_diff ~ . - season), data = .)
fit_dummy

extract_coefs <- function(fit) {
  fit %>% 
    broom::tidy() %>% 
    select(league = term, estimate) %>% 
    arrange(desc(estimate))
}

coefs <-
  fit_dummy %>% 
  extract_coefs()
coefs

p_coefs <-
  coefs %>% 
  drop_na(estimate) %>% 
  mutate(across(league, ~forcats::fct_reorder(.x, estimate))) %>% 
  ggplot() +
  aes(y = league, x = estimate) +
  geom_col(fill = 'grey30', color = NA_character_) +
  theme(
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = 'Overall League Strength Estimates',
    y = NULL,
    x = 'Estimate'
  )
p_coefs

ggsave(
  plot = p_coefs,
  filename = file.path(dir_proj, 'coefs.png'),
  width = 8,
  height = 8,
  type = 'cairo'
)

fits_by_season <-
  dummies %>% 
  filter(season <= 2020) %>% 
  group_nest(season) %>% 
  mutate(
    fit = map(data, ~lm(formula(z_diff ~ .), data = .x)),
    coefs = map(fit, extract_coefs)
  )

coefs_by_season <-
  fits_by_season %>% 
  select(season, coefs) %>% 
  unnest(coefs) %>% 
  # mutate(across(season, factor))
  mutate(across(season, ~sprintf('%04d-07-01', .x) %>% lubridate::ymd()))
coefs_by_season

leagues_filt <- c('England_Premier_League', 'Spain_LaLiga', 'Italy_Serie_A', 'Germany_1_Bundesliga', 'France_Ligue_1')
coefs_by_season_filt <-
  coefs_by_season %>% 
  filter(league %in% leagues_filt)

coefs_by_season_nofilt <-
  coefs_by_season %>% 
  filter(!(league %in% leagues_filt))

seq_date_first <- seq.Date(lubridate::ymd('2010-01-01'), lubridate::ymd('2021-01-01'), by = 'year')
seq_date_mid <- seq.Date(lubridate::ymd('2010-07-01'), lubridate::ymd('2020-07-01'), by = 'year')
seq_date_mid
seq_date <- sort(c(seq_date_first, seq_date_mid))
seq_date
labs_date <- c(rep(2010:2020, each = 2), 2021)
labs_date
idx <- (seq_along(labs_date) %% 2) == 1L
idx
labs_date[idx] <- ''
labs_date

pal <- c('#003f5c', '#58508d', '#bc5090', '#ff6361', '#ffa600')
p_coefs_by_season <-
  coefs_by_season_nofilt %>% 
  ggplot() +
  aes(x = season, y = estimate, group = league) +
  geom_point(color = 'grey80') +
  geom_line(color = 'grey80') +
  geom_point(
    data = coefs_by_season_filt,
    size = 3,
    aes(color = league)
  ) +
  geom_line(
    data = coefs_by_season_filt,
    size = 1.25,
    aes(color = league)
  ) +
  scale_color_manual(values = pal) +
  scale_x_date(labels = labs_date, breaks = seq_date, limits = range(seq_date)) +
  guides(color = guide_legend(title = '', override.aes = list(size = 3))) +
  theme(
    axis.ticks.x = element_line(color = c('grey80', rep(c(NA, 'grey80'), t = length(seq_date)))),
    panel.grid.major.x = element_line(color = c('grey80', rep(c(NA, 'grey80'), t = length(seq_date)))),
    legend.position = 'top'
  ) +
  labs(
    title = 'League Strength Coefficients',
    y = 'Power Coefficient',
    x = NULL
  )
p_coefs_by_season

ggsave(
  plot = p_coefs_by_season,
  filename = file.path(dir_proj, 'coefs_by_year.png'),
  width = 12,
  height = 8,
  type = 'cairo'
)

coefs_dummy <-
  fit_dummy %>% 
  broom::tidy() %>% 
  select(league = term, estimate) %>% 
  arrange(desc(estimate)) %>% 
  drop_na(estimate)
coefs_dummy

preds_dummy <-
  fit_dummy %>% 
  # broom::augment(dummies)
  predict(dummies) %>% 
  tibble(.pred = .) %>% 
  bind_cols(dummies %>% select(z_diff))
preds_dummy

# other models ----
res <-
  bind_rows(
    res_init %>% select(league = league_1, z_diff),
    res_init %>% mutate(z_diff = -z_diff) %>% select(league = league_2, z_diff)
  )
res

fit_bt <-
  BradleyTerry2::BTm(
    outcome = result, 
    player1 = league_1, 
    player2 = league_2, 
    id = 'league', 
    data = res_init %>% mutate(result = if_else(z_diff < 0, 1L, 0L)) %>% mutate(across(matches('^league'), factor))
  )
fit_bt

coefs_bt <-
  fit_bt %>% 
  broom::tidy() %>% 
  mutate(across(term, ~str_remove(.x, 'league'))) %>% 
  select(league = term, estimate) %>% 
  arrange(-estimate)
coefs_bt

