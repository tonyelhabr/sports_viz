
# Inspiration: https://hockey-graphs.com/2020/03/02/which-league-is-best/
# Empirical bayes refrence: http://varianceexplained.org/r/empirical_bayes_baseball/
library(tidyverse)
dir_proj <- '33-2020_euros_ratings'
source(file.path(dir_proj, 'helpers.R'))

c(df_vaep, .) %<-% do_import(col = 'vaep')
c(df_xg, lst_xg) %<-% do_import(col = 'xg')
df_xg %>% arrange(desc(xg_p90)) %>% relocate(xg_p90, v, v_orig)
df_xg %>% arrange(desc(v))

df_vaep %>% 
  group_by(position) %>% 
  summarize(
    across(c(vaep_p90, xg_p90), list(mean = mean, median = median))
  ) %>% 
  pivot_longer(
    -position,
    names_to = c('col', 'stat'),
    names_pattern = '(^.*_p90)_(.*$)'
  ) %>% 
  pivot_wider(names_from = 'position', values_from = 'value')
  glimpse()

# set.seed(42)
# df_vaep_vaep %>% 
#   sample_frac(0.1) %>% 
#   ggplot() +
#   aes(x = v_orig, y = v) +
#   geom_point(aes(size = minutes), alpha = 0.1, show.legend = FALSE) +
#   scale_size_area(c(0.1, 2)) +
#   theme(
#     plot.caption = ggtext::element_markdown(size = 9)
#   ) +
#   labs(
#     title = 'How the Empirical Bayes "normalizes" some of the lower minutes rates',
#     caption = 'Adjustement: Infer alpha and beta parameters for beta distribution for player having played 20 * 90 minutes in a season.<br/>Apply parameters to any players having played 5 * 90 minutes in a season (minimum criteria for inclusion).',
#     x = 'Original VAEP/90',
#     y = 'Adjusted VAEP/90'
#   )
# df %>% skimr::skim(minutes)

# stuff ----
res_vaep_trans <- df_vaep %>% do_modify_v_col(direct = FALSE) # this is more along the lines of the hockey graphs article
res_vaep_direct <- df_vaep %>% do_modify_v_col(direct = TRUE) # i end up using this
res_xg_direct <- df_xg %>% do_modify_v_col(direct = TRUE) 
c(df_vaep_trans, agg_vaep_trans, v_min_vaep_trans) %<-% res_vaep_trans
c(df_vaep_direct, ., .) %<-% res_vaep_direct
c(df_xg_direct, ., .) %<-% res_xg_direct
df_xg_direct %>% arrange(desc(xg_p90)) %>% relocate(xg_p90)
df_vaep_trans %>% do_plots(col = 'vaep', direct = FALSE)
df_vaep_direct %>% do_plots(col = 'vaep', direct = TRUE)
df_xg_direct %>% do_plots(col = 'xg', direct = TRUE)
# arrow::write_parquet(df_vaep_direct, file.path(dir_proj, 'df.parquet'))

res_vaep_trans_paired <- df_vaep_trans %>% do_get_data()
res_vaep_direct2z_paired <- df_vaep_direct %>% do_get_data(normalize = TRUE)
res_vaep_direct_paired <- df_vaep_direct %>% do_get_data(normalize = FALSE)
c(df_vaep_trans_paired, agg_vaep_trans_paired) %<-% res_vaep_trans_paired
c(df_vaep_direct2z_paired, agg_vaep_direct2z_paired) %<-% res_vaep_direct2z_paired
c(df_vaep_direct_paired, agg_vaep_direct_paired) %<-% res_vaep_direct_paired

res_xg_direct2z_paired <- df_xg_direct %>% do_get_data(normalize = TRUE)
res_xg_direct_paired <- df_xg_direct %>% do_get_data(normalize = FALSE)
c(df_xg_direct2z_paired, agg_xg_direct2z_paired) %<-% res_xg_direct2z_paired
c(df_xg_direct_paired, agg_xg_direct_paired) %<-% res_xg_direct_paired

df_vaep %>% 
  select(season, player_name, vaep, xg, minutes, minutes_xg, games_played, games_played_xg, xg_p90, vaep_p90) %>% 
  arrange(desc(xg_p90))

df_vaep_direct_paired %>% 
  ggplot() +
  aes(x = z_diff) +
  geom_histogram(binwidth = 0.01) +
  coord_cartesian(xlim = c(-0.5, 0.5))
df_vaep_direct_paired %>% arrange(desc(abs(z_diff)))

df_xg_direct_paired %>% 
  ggplot() +
  aes(x = z_diff) +
  geom_histogram(binwidth = 0.01) +
  coord_cartesian(xlim = c(-1, 1))

df_xg_direct2z_paired %>% 
  ggplot() +
  aes(x = z_diff) +
  geom_histogram(binwidth = 0.01) +
  coord_cartesian(xlim = c(-1, 1))

# Z-normalized difference in adjusted VAEP/90 for players transitioning between leagues/tourneys
df_vaep_direct2z_paired %>% 
  ggplot() +
  aes(x = z_diff) +
  geom_histogram(binwidth = 0.1) +
  coord_cartesian(xlim = c(-3, 3))

df_vaep_direct2z_paired %>% filter(idx_1 == 26509)

res_vaep_direct2z_paired <-
  do_fit_dummy(
    df_vaep_direct2z_paired,
    agg_vaep_direct2z_paired,
    strict = TRUE
  )
res_vaep_direct2z_paired$coefs

res_vaep_direct_paired <-
  do_fit_dummy(
    df_vaep_direct_paired,
    agg_vaep_direct_paired,
    strict = TRUE
  )
res_vaep_direct_paired$coefs %>% filter(league != '(Intercept)')
c(dummies_vaep, fit_vaep, coefs_vaep, vps_vaep, ...extra) %<-% res_vaep_direct2z_paired
df_vaep_direct_paired %>% filter(league_1 == 'Premier League (England)') %>% filter(season_1 == 2020)

res_xg_direct2z_paired <-
  do_fit_dummy(
    df_xg_direct2z_paired,
    agg_xg_direct2z_paired,
    strict = TRUE
  )
res_xg_direct2z_paired$coefs
res_xg_direct2z_paired$vps
# random pred stuff ----
dummies_vaep_filt <-
  dummies_vaep %>% 
  filter(`Champions League (Europe)` == 0L & `Europa League (Europe)` == 0L)
dummies_vaep_filt %>% arrange(desc(abs(z_diff)))

dummies_vaep_tst <-
  dummies_vaep %>% 
  filter(player_name == 'Memphis Depay', season_1 == 2015)
dummies_vaep_tst %>% glimpse()
dummies_vaep_tst

df_vaep_direct2z_tst <- dummies_vaep_tst %>% select(idx_1, idx_2) %>% inner_join(df_vaep_direct2z_paired)
df_vaep_direct2z_tst

preds_vaep <-
  fit_vaep %>% 
  broom::augment(dummies_vaep) %>% 
  select(.fitted, idx_1, idx_2) %>%  
  left_join(df_vaep_direct_paired) %>% 
  left_join(df_vaep_direct %>% rename(idx_1 = idx)) %>% 
  relocate(v)
preds

set.seed(42)
preds %>% 
  # slice_sample(n = 1000) %>% 
  ggplot() +
  aes(x = z_diff, y = .fitted, group = league_name) +
  geom_point(aes(color = league_name), alpha = 0.2) +
  # geom_smooth(method = 'lm', color = 'blue')
  geom_smooth(aes(color = league_name), method = 'lm', se = FALSE)

# vps_by_season <-
#   dummies %>% 
#   do_plot_vps_by_season(
#     agg,
#     suffix = 'trans_full'
#   )
# vps_by_season
# 
# vps_export <-
#   vps %>% 
#   filter(league_1 != '(Intercept)') %>% 
#   filter(league_2 != '(Intercept)') %>% 
#   select(-c(diff, p)) %>% 
#   left_join(league_mapping %>% select(league_id_1 = league_id, league_1 = league_lab)) %>% 
#   left_join(league_mapping %>% select(league_id_2 = league_id, league_2 = league_lab)) %>% 
#   drop_na() %>% 
#   relocate(league_id_1) %>% 
#   relocate(league_id_2, .before = 'league_2') %>% 
#   bind_cols(agg_direct2z_paired %>% select(mean, sd)) %>% 
#   relocate(vp, .after = last_col()) %>% 
#   arrange(-vp)
# vps_export
# vps_export %>% count(league_1)
# write_csv(vps_export, file.path(dir_proj, 'vps.csv'))

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

