
library(tidyverse)
source(here::here('scripts', 'dev', '0x_tx_hs_helpers.R'))

# Have to filter for these cuz the scores data only has 5A and 6A of present day.
.filter_conf <- function(data) {
  res <- data %>% filter(conf %in% sprintf('%dA', 5:6))
}

# fb ----
# Not sure I will actually use this. Probably just to check that the top football schools found via the scores are "reasonable".
fb_rnks <- import_fb_rnks()
fb_rnks_pts <-
  fb_rnks %>% 
  filter(key %in% c('TOTAL POINTS', 'Current Class')) %>% 
  pivot_wider(names_from = key, values_from = value) %>% 
  janitor::clean_names() %>% 
  # Intentionally will have warnings here.
  separate(current_class, into = c('conf', 'div'), sep = '-') %>% 
  rename(pts = total_points)
fb_rnks_pts_filt <- fb_rnks_pts %>% .filter_conf()
fb_rnks_pts_filt

# bands ----
bands <- 
  import_bands() %>% 
  select(-notes) %>% 
  mutate(
    across(school, ~str_remove_all(.x, '\\sHS|\\,')), 
    across(isd, ~str_remove(.x, '\\sC?ISD$')),
    across(round, ~ordered(.x, c('Prelims', 'Finals')))
  ) %>% 
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
# bands_aug %>% filter(is.na(w))
# bands_aug %>% count(rnk_conf)
# bands_aug %>% arrange(desc(score_rnk))

# # How frequently did each school appear in each conference?
# bands_schools_conf <-
#   bands_aug %>% 
#   group_by(school) %>% 
#   mutate(n_app = n()) %>% 
#   ungroup() %>% 
#   group_by(school, conf, n_app) %>% 
#   summarize(n_conf = n()) %>% 
#   mutate(frac_app = n_conf / n_app) %>% 
#   ungroup()
# 
# # Identify the most common conference for each school. (This is purely informational.)
# bands_schools <-
#   bands_schools_conf %>% 
#   group_by(school) %>% 
#   filter(row_number(desc(frac_app)) == 1L) %>% 
#   ungroup()
# bands_schools

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
    # rnk_rnk = row_number(desc(score_rnk)),
    rnk = row_number(desc(score_prnk))
  ) %>% 
  # relocate(rnk_rnk, rnk_prnk) %>% 
  relocate(matches('^rnk')) %>% 
  # select(-rnk_rnk) %>% 
  arrange(rnk)
bands_agg

# Didn't filter by conference earlier since bands could have been in lower conferences before.
bands_agg_filt <- bands_agg %>% .filter_conf()
bands_agg_filt

bands_agg_filt_adj <-
  bands_agg_filt %>% 
  # select(-matches('^rnk')) %>% 
  mutate(
    rnk_raw = row_number(rnk_raw),
    rnk_prnk = row_number(rnk_prnk), 
    rnk = row_number(rnk)
  )
bands_agg_filt_adj %>% clipr::write_clip()
bands_agg_filt_adj %>% 
  head(30) %>% 
  select(school) %>% clipr::write_clip()

bands_agg_filt_adj %>% 
  ggplot() +
  aes(x = score_raw, y = score_prnk) +
  geom_point()

bands_agg_filt_adj %>% 
  mutate(rnk_diff = (rnk_raw - rnk_prnk)) %>% 
  filter(rnk <= 30) %>% 
  arrange(desc(abs(rnk_diff)))

bands_agg_filt_adj %>% 
  ggplot() +
  aes(x = rnk_raw, y = rnk_prnk) +
  geom_point()

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
    # rnk_w_cumu = row_number(desc(w_cumu)),
    # rnk_w_frac_cumu = row_number(desc(w_frac_cumu))
    rnk = row_number(desc(w_cumu))
  ) %>% 
  arrange(rnk)
# fb_proc_last %>% arrange(desc(w_frac_cumu))
fb_proc_last %>% arrange(desc(w_cumu))

# w_cumu <- fb_proc_last %>% pull(w_cumu)
# g_cumu <- fb_proc_last %>% pull(g_cumu)
# ll <- function(alpha, beta) {
#   x <- w_cumu
#   size <- g_cumu
#   -sum(VGAM::dbetabinom.ab(x, size, alpha, beta, log = TRUE))
# }
# 
# m <- stats4::mle(ll, start = list(alpha = 1, beta = 1))
# m_coef <- m %>% coef() %>% round()
# m_coef
# alpha <- 12 # m_coef[['alpha']]
# beta <- 12 # m_coef[['beta']]
# 
# fb_proc_last_adj <-
#   fb_proc_last %>% 
#   mutate(
#     w_frac_cumu_adj = (w_cumu + alpha) / (g_cumu + alpha + beta),
#     rnk_w_frac_cumu_adj = row_number(desc(w_frac_cumu_adj))
#   ) %>% 
#   arrange(desc(w_frac_cumu_adj))
# fb_proc_last_adj %>% relocate(matches('^rnk_'))
# fb_last_adj %>% 
#   arrange(desc(w_cumu)) %>% 
#   select(school, w_cumu, w_frac_cumu_adj)

# fb_proc_last %>% 
#   arrange(rnk_w_cumu) %>% 
#   select(school_fb = school) %>% 
#   head(30)

schools_dict <- 
  here::here('data-raw', '0x', 'schools_dict2.xlsx') %>% 
  readxl::read_excel() %>% 
  mutate(across(c(not_downloaded, is_ambiguous), ~coalesce(.x, FALSE))) %>% 
  filter(!not_downloaded & !is_ambiguous) %>% 
  select(matches('school'))
schools_dict
# schools_dict_n <- schools_dict %>% count(school_fb, sort = T)

df <-
  # `conf` isn't really useful, but oh well.
  bind_rows(
    bands_agg_filt_adj %>%
      select(school, rnk, score_raw, score_prnk, w, w_final, n_app)
      pivot_longer(
        -c(school, rnk)
      ) %>% 
      select(rnk, school_band = school, name, value  %>% 
      mutate(src = 'band') %>% 
      left_join(schools_dict),
    fb_proc_last %>% 
      select(rnk, school_fb = school, value = w_cumu) %>% 
      mutate(src = 'fb') %>% 
      left_join(schools_dict)
  ) %>% 
  relocate(src) %>%
  mutate(school = school_fb) %>% 
  # group_by(school) %>% 
  # mutate(n = n()) %>% 
  # ungroup()
  arrange(school)
df

df_wide <-
  df %>% 
  pivot_wider(
    names_from = 'src',
    values_from = c('rnk', 'value')
  )
df_wide    

df %>% 
  ggplot() +
  aes(x = 

schools_band <- df %>% filter(!is.na(school_fb)) %>% filter(src == 'band') %>% arrange(rnk)
schools_fb <- df %>% filter(is_missing_band | !is.na(school_band)) %>% filter(src == 'fb') %>% arrange(rnk)
schools_fb

df %>% filter(n > 1L)
df %>% filter(n == 1L)
df %>% filter(n == 1L) %>% filter(src == 'fb') %>% filter(is_missing_band | !is.na(school)) -> z
df %>% filter(n == 1L) %>% filter(src == 'band') %>% filter(!is.na(school)) -> z

viz_init <-
  df %>%
  mutate(z = 'dummy') %>% 
  ggplot() +
  aes(y = z, x = value) +
  ggbeeswarm::geom_quasirandom(alpha = 0.5, groupOnX = FALSE) +
  facet_wrap(~src, scales = 'free', ncol = 1L)
viz_init

b <- viz_init %>% ggplot_build() #  %>% as_tibble()
df_viz <- b$data[[1]] %>% as_tibble() %>% arrange(desc(x))
idx_filt <- 1L:30L
schools_dict_n_aug <- schools_dict %>% left_join(schools_dict_n)
schools_dict_n_aug
df_viz_band <- df_viz %>% filter(PANEL == '1') %>% slice(idx_filt) %>% bind_cols(schools_band)
df_viz_band %>% anti_join(df %>% filter(src == 'fb') %>% select(school, value_fb = value))
df_viz_fb <- df_viz %>% filter(PANEL == '2') %>% slice(idx_filt) %>% bind_cols(schools_fb)
df_viz_fb

viz <-
  viz_init +
  geom_point(
    data = df_viz_band %>% mutate(src == 'band'),
    color = 'orange',
  ) +
  geom_point(
    data = df_viz_fb %>% mutate(src == 'fb'),
    color = 'orange',
  ) +
  ggrepel::geom_text_repel(
    data = df %>% filter(n == 2L),
    aes(label = school),
    color = 'blue'
  ) +
  facet_wrap(~src, scales = 'free', ncol = 1L) +
  labs(
    x = NULL,
    y = NULL
  )