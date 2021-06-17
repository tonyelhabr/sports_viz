
# Inspiration: https://hockey-graphs.com/2020/03/02/which-league-is-best/
# Empirical bayes refrence: http://varianceexplained.org/r/empirical_bayes_baseball/
library(tidyverse)
dir_proj <- '33-2020_euros_ratings'
source(file.path(dir_proj, 'helpers.R'))

df <- do_import()
df %>% 
  relocate(league_id) %>% 
  distinct(league_id, country, league_name) %>% 
  left_join(league_mapping %>% mutate(idx = row_number())) %>% 
  arrange(idx)

set.seed(42)
df %>% 
  sample_frac(0.1) %>% 
  ggplot() +
  aes(x = v_orig, y = v) +
  geom_point(aes(size = minutes), alpha = 0.1, show.legend = FALSE) +
  scale_size_area(c(0.1, 2)) +
  theme(
    plot.caption = ggtext::element_markdown(size = 9)
  ) +
  labs(
    title = 'How the Empirical Bayes "normalizes" some of the lower minutes rates',
    caption = 'Adjustement: Infer alpha and beta parameters for beta distribution for player having played 20 * 90 minutes in a season.<br/>Apply parameters to any players having played 5 * 90 minutes in a season (minimum criteria for inclusion).',
    x = 'Original VAEP/90',
    y = 'Adjusted VAEP/90'
  )

df %>% 
  skimr::skim(minutes)

# function time ----
res_trans <- df %>% do_modify_v_col(direct = FALSE) # this is more along the lines of the hockey graphs article
res_direct <- df %>% do_modify_v_col(direct = TRUE) # i end up using this
c(df_trans, agg, v_min) %<-% res_trans
c(df_direct, agg_direct, v_min_direct) %<-% res_direct

df_trans %>% do_plots(direct = FALSE)
df_direct %>% do_plots(direct = TRUE)
df_direct %>% skimr::skim(z)
# arrow::write_parquet(df_direct, file.path(dir_proj, 'df.parquet'))

# map doesnt' return the right result?
# c(df_trans_paired, df_direct_paired) %<-% list(df_trans, df_direct) %>% map(do_get_data)
res_trans_paired <- df_trans %>% do_get_data()
res_direct2z_paired <- df_direct %>% do_get_data(normalize = TRUE)
res_direct_paired <- df_direct %>% do_get_data(normalize = FALSE)
c(df_trans_paired, agg_trans_paired) %<-% res_trans_paired
c(df_direct2z_paired, agg_direct2z_paired) %<-% res_direct2z_paired
c(df_direct_paired, agg_direct_paired) %<-% res_direct_paired

df_direct_paired %>% 
  ggplot() +
  aes(x = z_diff) +
  geom_histogram(binwidth = 0.01) +
  coord_cartesian(xlim = c(-0.5, 0.5))

# Z-normalized difference in adjusted VAEP/90 for players transitioning between leagues/tourneys
df_direct2z_paired %>% 
  ggplot() +
  aes(x = z_diff) +
  geom_histogram(binwidth = 0.1) +
  coord_cartesian(xlim = c(-3, 3))

df_direct2z_paired %>% filter(idx_1 == 26509)

# map+zeallot not working?
# c(dummies_trans_full, fit_trans_full, coefs_trans_full) %<-% df_trans_paired %>% do_fit_dummy(strict = FALSE)
# res_trans_full <- df_trans_paired %>% do_fit_dummy(strict = FALSE)
# res_trans_strict <- df_trans_paired %>% do_fit_dummy(strict = TRUE)
# res_direct2z_full <- df_direct2z_paired %>% do_fit_dummy(strict = FALSE)
# c(dummies_trans_full, fit_trans_full, coefs_trans_full, vps_trans_full) %<-% res_trans_full
# c(dummies_trans_strict, fit_trans_strict, coefs_trans_strict, vps_trans_strict) %<-% res_trans_strict
# c(dummies_direct2z_full, fit_direct2z_full, coefs_direct2z_full, vps_direct2z_full) %<-% res_direct2z_full
res_direct2z_paired <-
  do_fit_dummy(
    df_direct2z_paired,
    agg_direct2z_paired,
    strict = TRUE
  )
res_direct2z_paired$coefs

res_direct_paired <-
  do_fit_dummy(
    df_direct_paired,
    agg_direct_paired,
    strict = FALSE
  )
res_direct_paired$coefs %>% filter(league != '(Intercept)')
c(dummies, fit, coefs, vps) %<-% res_direct2z_paired

dummies_filt <-
  dummies %>% 
  filter(`Champions League (Europe)` == 0L & `Europa League (Europe)` == 0L)
dummies_filt %>% arrange(desc(abs(z_diff)))

# Memphis Depay
dummies_tst <-
  dummies %>% 
  filter(idx_1 == 9241, idx_2 == 506)
dummies_tst

df_direct2z_tst <- dummies_tst %>% select(idx_1, idx_2) %>% inner_join(df_direct2z_paired)
df_direct2z_tst

preds <-
  fit %>% 
  broom::augment(dummies) %>% 
  select(.fitted, idx_1, idx_2) %>%  
  left_join(df_direct_paired) %>% 
  left_join(df_direct %>% rename(idx_1 = idx))
preds

set.seed(42)
preds %>% 
  # slice_sample(n = 1000) %>% 
  ggplot() +
  aes(x = z_diff, y = .fitted, group = league_name) +
  geom_point(aes(color = league_name), alpha = 0.2) +
  # geom_smooth(method = 'lm', color = 'blue')
  geom_smooth(aes(color = league_name), method = 'lm', se = FALSE)

# dummies %>% 
#   ggplot() +
#   aes(x = z_diff) +
#   geom_histogram(binwidth = 0.1) +
#   coord_cartesian(xlim = c(-3, 3))
# 
# f_rename <- function(data, nm = deparse(substitute(data))) {
#   suffix <- nm %>% str_remove('coefs')
#   data %>% 
#     mutate(!!sym(sprintf('rnk%s',  suffix)) := row_number(desc(estimate))) %>% 
#     rename(!!sym(sprintf('estimate%s', suffix)) := estimate)
# }
# 
# coefs_compare <-
#   list(
#     f_rename(coefs_trans_full),
#     f_rename(coefs_trans_strict),
#     f_rename(coefs_direct2z_full),
#     f_rename(coefs)
#   ) %>% 
#   reduce(full_join)
# coefs_compare

vps_by_season <-
  dummies %>% 
  do_plot_vps_by_season(
    # agg,
    .mean = agg_direct2z_paired$mean,
    .sd = agg_direct2z_paired$sd,
    suffix = 'trans_full'
  )
vps_by_season

league_mapping

vps_export <-
  vps %>% 
  filter(league_1 != '(Intercept)') %>% 
  filter(league_2 != '(Intercept)') %>% 
  select(-c(diff, p)) %>% 
  left_join(league_mapping %>% select(league_id_1 = league_id, league_1 = league_lab)) %>% 
  left_join(league_mapping %>% select(league_id_2 = league_id, league_2 = league_lab)) %>% 
  drop_na() %>% 
  relocate(league_id_1) %>% 
  relocate(league_id_2, .before = 'league_2') %>% 
  bind_cols(agg_direct2z_paired %>% select(mean, sd)) %>% 
  relocate(vp, .after = last_col()) %>% 
  arrange(-vp)
vps_export
vps_export %>% count(league_1)
(0.910 - .140) * 0.135 + 0
write_csv(vps_export, file.path(dir_proj, 'vps.csv'))

# stan ----
# go to stan script

# plots ---
vps_filt <-
  vps %>% 
  filter(rnk_1 <= rnk_2) %>% 
  filter(league_2 != '(Intercept)')
vps_filt

viz_diff_v <- 
  vps_filt %>% 
  plot_heatmap('vp')
viz_diff_v



viz_diff_rel <-
  vps_filt %>% 
  plot_heatmap('p')
viz_diff_rel

vps_filt1 <-
  vps_filt %>% 
  filter(league_1 == 'Champions League (Europe)') %>% 
  mutate(p_inv = 1 - p) %>% 
  rename(rnk = rnk_2, league = league_2) %>% 
  mutate(across(league, ~fct_reorder(.x, p_inv)))
vps_filt1

viz_vps_filt1 <-
  vps_filt1 %>% 
  ggplot() +
  aes(y = league, x = p_inv, group = league) +
  geom_col(color = 'grey20', fill = 'grey20') +
  geom_text(
    data = vps_filt1,
    aes(label = league),
    family = 'Karla',
    size = pts(14),
    fontface = 'bold',
    hjust = 1,
    color = 'white'
  ) +
  scale_x_continuous(labels = scales::percent) +
  theme(
    plot.tag.position = c(0.01, 0.016),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = 'How strong is the competition?',
    tag = lab_tag,
    x = 'Competition level relative to Champions League', 
    y = NULL
  )
viz_vps_filt1
league_mapping <- import_league_mapping()
league_mapping


ggsave(
  plot = viz_vps_filt1,
  filename = file.path(dir_proj, 'viz_vps_filt1.png'),
  width = 10,
  height = 7.5,
  type = 'cairo'
)

# maybe #EURO2020 will tell us soemething about national soccer strength
# With nations clashing in #EURO2020, i collabed with @canzhiye to try to quantify relative league/tourney strength. we came up with sensible top 7 (big 5 + UCL + Europa) 
# no team identifiers or match results were used in the making of these rankings
# how much can PV metrics tell us about league... well, it seems to come up with a sensible notion of relative league strength, even if we
# what if i told you we that we can leverage in-game event data to quantify relative league/tourney strength, without any knowledge of match results?
# these are based on before-and-after difference in VAEP/90 (adjusting for low minute totals) for players transitioning between leagues/tourneys
# the outputs are relative VAEP/90 per leauge/tourney
# heatmap uses a baseline VAEP/90 = 0.33 (roughly the median) and the VAEP/90 coefficient for the Champions League to determine relative league strength

