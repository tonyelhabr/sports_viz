
# Inspiration: https://hockey-graphs.com/2020/03/02/which-league-is-best/
# Empirical bayes refrence: http://varianceexplained.org/r/empirical_bayes_baseball/
library(tidyverse)
dir_proj <- '33-2020_euros_ratings'
source(file.path(dir_proj, 'helpers.R'))

res_vaep <- do_import(col = 'vaep')
c(df_vaep, ., baseline_vaep, baseline_vaep_by_grp) %<-% res_vaep
res_xg <- do_import(col = 'xg')
c(df_xg, ., baseline_xg, baseline_xg_by_grp) %<-% res_xg

agg_by_grp <-
  df_vaep %>%
  group_by(position, age_grp) %>%
  summarize(
    across(c(vaep_p90, xg_p90), list(mean = mean, median = median))
  ) %>%
  ungroup() %>%
  pivot_longer(
    -c(position, age_grp),
    names_to = c('col', 'stat'),
    names_pattern = '(^.*_p90)_(.*$)'
  ) %>%
  pivot_wider(names_from = 'position', values_from = 'value')
agg_by_grp
# 
# agg <-
#   df_vaep %>% 
#   summarize(
#     across(c(vaep_p90, xg_p90), list(mean = mean, median = median))
#   ) %>% 
#   pivot_longer(
#     matches('.*'),
#     names_to = c('col', 'stat'),
#     names_pattern = '(^.*_p90)_(.*$)'
#   )
# agg

# stuff ----
res_vaep_direct <- df_vaep %>% do_modify_v_col(direct = TRUE) # i end up using this
res_xg_direct <- df_xg %>% do_modify_v_col(direct = TRUE) 

c(df_vaep_direct, ., .) %<-% res_vaep_direct
c(df_xg_direct, ., .) %<-% res_xg_direct

# df_vaep_direct %>% do_plots(col = 'vaep', direct = TRUE)
# df_xg_direct %>% do_plots(col = 'xg', direct = TRUE)

res_vaep_direct2z_paired <- df_vaep_direct %>% do_get_data(normalize = TRUE)
c(df_vaep_direct2z_paired, agg_vaep_direct2z_paired) %<-% res_vaep_direct2z_paired

res_xg_direct2z_paired <- df_xg_direct %>% do_get_data(normalize = TRUE)
c(df_xg_direct2z_paired, agg_xg_direct2z_paired) %<-% res_xg_direct2z_paired

bund2epl <-
  df_xg_direct2z_paired %>% 
  # filter(player_name == 'Timo Werner')
  # filter(position == 'FW') %>% 
  filter(position %in% c('FW', 'AM')) %>% 
  # filter(age_grp == '18<=x<24') %>% 
  filter(league_2 == 'Premier League (England)') %>%  # %>% count(league_1)
  filter(league_1 == 'Bundesliga 1 (Germany)') %>% 
  relocate(z_1, z_2, z_diff_orig) %>% 
  arrange(-season_1)

bund2epl %>% 
  summarize(z_diff_orig = mean(z_diff_orig))

l2l_xg <-
  df_xg_direct2z_paired %>% 
  group_by(league_1, league_2) %>% 
  summarize(z_1 = mean(z_1), z_2 = mean(z_2), z_diff_orig = mean(z_diff_orig), n = n()) %>% 
  ungroup() %>% 
  arrange(-z_diff_orig)
l2l_xg

viz_xg_emperical <-
  l2l_xg %>% 
  group_by(league_1) %>% 
  mutate(z_diff_orig_mean_1 = mean(z_diff_orig)) %>% 
  ungroup() %>% 
  mutate(rnk_1 = dense_rank(z_diff_orig_mean_1)) %>% 
  mutate(
    across(league_1, ~fct_reorder(.x, rnk_1)),
    across(league_2, ~fct_reorder(.x, -rnk_1))
  ) %>% 
  ggplot() +
  aes(x = rnk_1, y = -rnk_1) +
  geom_tile(aes(fill = z_diff_orig), alpha = 0.7,  height = 0.95, width = 0.95, show.legend = FALSE) +
  geom_text(aes(label = scales::number(z_diff_orig, accuracy = 0.01)), size = pts(14), fontface = 'bold') +
  scale_fill_viridis_c(option = 'B', begin = 0.1, end = 1) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  theme(
    plot.title = ggtext::element_markdown(size = 18),
    plot.subtitle = ggtext::element_markdown(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.title.y = element_text(size = 16),
  ) +
  labs(
    # title = title,
    subtitle = 'Emperical xG/90 change when moving from league A to league B',
    tag = lab_tag,
    y = 'League A',
    x = 'League B'
  )
viz_xg_emperical

l2l %>% filter(position_grp == 'A', league_2 == 'Premier League (England)')
l2l %>% 
  filter(n > 5) -> x


# Z-normalized difference in adjusted VAEP/90 for players transitioning between leagues/tourneys
# df_vaep_direct_paired %>% 
# df_vaep_direct2z_paired %>% 
df_xg_direct2z_paired %>% 
  ggplot() +
  aes(x = z_diff) +
  geom_histogram(binwidth = 0.01) +
  coord_cartesian(xlim = c(-1, 1))

res_vaep_direct2z_paired <- 
  do_fit_dummy(
    df_vaep_direct2z_paired,
    agg_vaep_direct2z_paired,
    baseline_vaep,
    baseline_vaep_by_grp,
    col = 'vaep',
    suffix = 'vaep_direct2z'
  )
res_vaep_direct2z_paired$coefs
c(dummies_vaep, fit_vaep, coefs_vaep, vps_vaep, ...extra) %<-% res_vaep_direct2z_paired
df_xg_direct2z_paired %>% filter(league_1 == 'Premier League (England)') %>% filter(season_1 == 2020)

res_xg_direct2z_paired <-
  do_fit_dummy(
    df_xg_direct2z_paired,
    agg_xg_direct2z_paired,
    baseline_xg,
    baseline_xg_by_grp,
    col = 'xg',
    suffix = 'xg_direct2z'
  )
res_xg_direct2z_paired$coefs
c(dummies_xg, fit_xg, coefs_xg, vps_xg, ...extra) %<-% res_xg_direct2z_paired

# random pred stuff ----
dummies_vaep_filt <-
  dummies_vaep # %>% 
  # filter(`Champions League (Europe)` == 0L & `Europa League (Europe)` == 0L)
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
  left_join(df_vaep_direct2z_paired) %>% 
  left_join(df_vaep_direct %>% rename(idx_1 = idx)) %>% 
  relocate(v)
preds_vaep

set.seed(42)
preds_vaep %>% 
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

