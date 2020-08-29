
library(tidyverse)
source(here::here('scripts', 'dev', '0x_tx_hs_helpers.R'))

# fb ----
# Not sure I will actually use this.
fb <- import_fb()
fb_pts <-
  fb %>% 
  filter(key %in% c('TOTAL POINTS', 'Current Class')) %>% 
  pivot_wider(names_from = key, values_from = value) %>% 
  janitor::clean_names() %>% 
  separate(current_class, into = c('conf', 'div'), sep = '-') %>% 
  mutate(tier = conf %>% str_remove('A$') %>% as.integer()) %>% 
  rename(pts = total_points)
fb_pts_filt <- fb_pts %>% filter(conf %in% sprintf('%dA', 5:6))
fb_pts_filt
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
  mutate(idx = row_number())

# Adding percent ranks. Preparing for ranking aggregation.
bands_aug <-
  bands %>% 
  # filter(!is.na(place)) %>% 
  mutate(
    rnk_conf = (tier - min(tier) + 1L),
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
    across(n, ~if_else(.x < place, place, .x))
  ) %>% 
  mutate(
    # Need to coalesce for when `place` is NA.
    rnk_place = (n - place + 1L), #  %>% coalesce(1),
    prnk_place = ((n - place + 1L) / (n - min(place) + 1L)) #  %>% coalesce(1 / n)
  ) %>% 
  ungroup() %>% 
  mutate(
    score_rnk = ifelse(round == 'Prelims', 1, 2) * rnk_conf * rnk_place,
    score_prnk = ifelse(round == 'Prelims', 0.5, 1) * prnk_conf * prnk_place
  )
bands_aug %>% filter(is.na(w))
bands_aug %>% count(rnk_conf)
bands_aug %>% arrange(desc(score_rnk))

# How frequently did each school appear in each confereence?
bands_schools_conf <-
  bands_aug %>% 
  group_by(school) %>% 
  mutate(n_app = n()) %>% 
  ungroup() %>% 
  group_by(school, conf, n_app) %>% 
  summarize(n_conf = n()) %>% 
  mutate(frac_app = n_conf / n_app) %>% 
  ungroup()

# Identify the most common conference for each school. (This is purely informational.)
bands_schools <-
  bands_schools_conf %>% 
  group_by(school) %>% 
  filter(row_number(desc(frac_app)) == 1L) %>% 
  ungroup()
bands_schools

# Ranking bands.
bands_agg <-
  bands_aug %>% 
  # mutate(across(place, ~coalesce(.x, round(n / 2)))) %>% 
  filter(!is.na(place)) %>% 
  group_by(school) %>% 
  summarize(across(c(score_rnk, score_prnk, w, w_final), sum), across(c(place, n, prnk_place), mean), n_app = n()) %>% 
  ungroup() %>% 
  inner_join(bands_schools %>% select(school, conf_pri = conf)) %>% # Add the conference.
  mutate(
    rnk_rnk = row_number(desc(score_rnk)),
    rnk_prnk = row_number(desc(score_prnk))
  ) %>% 
  relocate(rnk_rnk, rnk_prnk) %>% 
  select(-rnk_rnk) %>% 
  arrange(rnk_prnk)
bands_agg

options(tibble.print_min = 30)
bands_agg %>% 
  filter(conf_pri %in% sprintf('%dA', 5:6)) %>% 
  # filter(school != 'Fredericksburg') %>% 
  mutate(rernk_prnk = row_number()) %>% 
  relocate(rernk_prnk) %>% 
  # select(rnk = rernk_prnk, school, conf = conf_pri, score = score_prnk, n_app, w, w_final) %>% 
  select(school_band = school) %>% 
  head(30) 
scores <- retrieve_fb_scores() %>% select(-coach, -opp, -mov, -date)
scores

# Identify current day schools.
scores_last <- 
  scores %>% 
  filter(season == 2019L) %>% 
  group_by(school) %>% 
  filter(g_cumu == max(g_cumu)) %>% 
  ungroup()
scores_last
scores_last %>% filter(school %>% str_detect('Duncan'))

# schools_scores_last <- scores_last %>% pull(school)
# schools_scores_last

scores_proc <-
  scores %>% 
  semi_join(scores_last %>% select(school)) %>% 
  filter(season >= 1979) %>% 
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
scores_proc

scores_proc_last <- 
  scores_proc %>% 
  group_by(school) %>% 
  filter(g_cumu == max(g_cumu)) %>% 
  ungroup() %>% 
  mutate(
    rnk_w_cumu = row_number(desc(w_cumu)),
    rnk_w_frac_cumu = row_number(desc(w_frac_cumu))
  ) %>% 
  arrange(rnk_w_cumu)
scores_proc_last %>% arrange(desc(w_frac_cumu))
scores_proc_last %>% arrange(desc(w_cumu))

# w_cumu <- scores_proc_last %>% pull(w_cumu)
# g_cumu <- scores_proc_last %>% pull(g_cumu)
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
# scores_proc_last_adj <-
#   scores_proc_last %>% 
#   mutate(
#     w_frac_cumu_adj = (w_cumu + alpha) / (g_cumu + alpha + beta),
#     rnk_w_frac_cumu_adj = row_number(desc(w_frac_cumu_adj))
#   ) %>% 
#   arrange(desc(w_frac_cumu_adj))
# scores_proc_last_adj %>% relocate(matches('^rnk_'))
# scores_last_adj %>% 
#   arrange(desc(w_cumu)) %>% 
#   select(school, w_cumu, w_frac_cumu_adj)

scores_proc_last %>% 
  arrange(rnk_w_cumu) %>% 
  select(school_scores = school) %>% 
  head(30)

# bands_last <- bands %>% group_by(school) %>% filter(idx == max(idx)) %>% ungroup()
# bands_last
# bands_last_filt <- bands_last %>% arrange(desc(year)) %>% filter(tier %in% c(5L, 6L))
# bands_last_filt
# bands_last_filt %>% filter(year <= 1981L)
# bands_last_filt
# scores_last %>% inner_join(bands_last_filt)

