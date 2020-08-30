
library(tidyverse)
source(here::here('scripts', 'dev', '0x_tx_hs_helpers.R'))

# Have to filter for these cuz the scores data only has 5A and 6A of present day.
.filter_conf <- function(data) {
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
  here::here('data-raw', '0x', 'schools_dict.csv') %>% 
  read_csv(locale = locale(encoding = 'ASCII')) %>%
  # read_csv() %>% 
  # here::here('data-raw', '0x', 'schools_dict2.xlsx') %>% 
  # readxl::read_excel() %>% 
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
      inner_join(schools_dict)
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
  )
df_wide

df_wide %>% 
  ggplot() +
  aes(y = score_prnk_band, x = w_fb) +
  geom_point(aes(size = w_frac_fb)) +
  # geom_smooth(method = 'lm') +
  scale_radius(range = c(0.1, 4))

df_wide %>% 
  replace_na(list(score_prnk_band = 0)) %>% 
  ggplot() +
  aes(x = score_prnk_band, y = w_frac_fb) +
  geom_point(aes(size = w_fb)) +
  geom_smooth(method = 'lm') +
  scale_radius(range = c(0.1, 6))
