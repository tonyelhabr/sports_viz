
# Inspiration: https://hockey-graphs.com/2020/03/02/which-league-is-best/
library(tidyverse)
dir_proj <- '33-2020_euros_ratings'
source(file.path(dir_proj, 'helpers.R'))

res_vaep <- do_import(col = 'vaep')
c(df_vaep, ., baseline_vaep, baseline_vaep_by_grp) %<-% res_vaep
res_xg <- do_import(col = 'xg')
c(df_xg, ., baseline_xg, baseline_xg_by_grp) %<-% res_xg

# res_vaep_trans <- do_import(col = 'vaep', adjust = TRUE)
# c(df_vaep_trans, adjust_trans, baseline_vaep_trans, baseline_vaep_by_grp_trans) %<-% res_vaep_trans
# 
# res_xg_trans <- do_import(col = 'xg', adjust = TRUE)
# c(df_xg_trans, adjust_trans, baseline_xg_trans, baseline_xg_by_grp_trans) %<-% res_xg_trans

alpha <- 5
beta <- 1000
viz_vaep_adj <-
  df_vaep %>%
  mutate(v_orig = v) %>% 
  mutate(v = 90 * (vaep + !!alpha) / (minutes + !!alpha + !!beta)) %>% 
  # sample_frac(0.1) %>% 
  ggplot() +
  aes(x = v_orig, y = v) +
  geom_point(
    data = . %>% filter(minutes < 1500),
    color = 'red',
    show.legend = FALSE,
    aes(size = minutes),
    alpha = 0.2
  ) +
  geom_point(
    data = . %>% filter(minutes >= 1500),
    alpha = 0.25, 
    show.legend = FALSE
  ) +
  # scale_size_area(c(0.1, 1)) +
  theme(
    plot.caption = ggtext::element_markdown(size = 9),
  ) +
  labs(
    title = 'Empirical Bayes adjustment to VAEP/90',
    subtitle = '2012 - 2020, All Leagues',
    tag = '**Viz***: Tony ElHabr | **Data**: @canzhiye',
    caption = 'Adjustment (beta distribution): alpha = 5, beta = 1000<br/>Players with less than 1500 minutes played annotated in red (stronger adjustment).',
    x = 'Original VAEP/90',
    y = 'Adjusted VAEP/90'
  )
viz_vaep_adj

ggsave(
  plot = viz_vaep_adj,
  filename = file.path(dir_proj, 'viz_vaep_adj.png'),
  height = 6,
  width = 9,
  type = 'cairo'
)

df_vaep %>% 
  arrange(desc(xg)) %>% 
  select(
    `Season` = season,
    `Player` = player_name,
    # `Position` = position,
    # `Age Group` = age_grp,
    `Minutes` = minutes,
    # `League` = league,
    `xG` = xg,
    `VAEP` = vaep
  )

viz_scatter <-
  df_vaep %>% 
  arrange(desc(xg)) %>% 
  ggplot() +
  aes(x = xg, y = vaep) +
  geom_point(color = 'grey30') +
  geom_smooth(se = FALSE) +
  labs(
    title = 'xG vs. VAEP',
    subtitle = '2012 - 2020, All Leagues',
    tag = '**Viz**: Tony ElHabr | **Data**: @canzhiye',
    x = 'xG',
    y = 'VAEP',
    caption = 'Point size represents minutes played.'
  )
viz_scatter

viz_scatter_margin <- 
  ggExtra::ggMarginal(
    viz_scatter,
    type = 'histogram', 
    xparams = list(color = NA), 
    yparams = list(color = NA)
  )
viz_scatter_margin

ggsave(
  plot = viz_scatter_margin,
  filename = file.path(dir_proj, 'viz_xg_v_vaep.png'),
  height = 8,
  width = 8,
  type = 'cairo'
)

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

agg <-
  df_vaep %>%
  summarize(
    across(c(vaep_p90, xg_p90), list(mean = mean, median = median))
  ) %>%
  pivot_longer(
    matches('.*'),
    names_to = c('col', 'stat'),
    names_pattern = '(^.*_p90)_(.*$)'
  )
agg

# stuff ----
res_vaep_direct <- df_vaep %>% do_modify_v_col(direct = TRUE) # i end up using this
res_xg_direct <- df_xg %>% do_modify_v_col(direct = TRUE) 

c(df_vaep_direct, ., .) %<-% res_vaep_direct
c(df_xg_direct, ., .) %<-% res_xg_direct

# df_vaep_direct %>% do_plots(col = 'vaep', direct = TRUE)
# df_xg_direct %>% do_plots(col = 'xg', direct = TRUE)

res_vaep_direct2z_paired <- df_vaep_direct %>% do_get_data(normalize = TRUE)
c(df_vaep_direct2z_paired, agg_vaep_direct2z_paired) %<-% res_vaep_direct2z_paired

options(width = 90)
df_vaep_direct2z_paired %>% 
  filter(season_1 <= 2020) %>% 
  arrange(desc(season_1), desc(z_1)) %>% 
  select(
    `Season` = season_1,
    `Player` = player_name,
    # `Position` = position,
    # `Age Group` = age_grp,
    `League A` = league_1,
    `League B` = league_2,
    `VAEP/90 A` = z_1,
    `VAEP/90 B` = z_2,
    `Diff.` = z_diff_orig,
    # `VAEP/90 Diff. Z` = z_diff
  )

res_xg_direct2z_paired <- df_xg_direct %>% do_get_data(normalize = TRUE)
c(df_xg_direct2z_paired, agg_xg_direct2z_paired) %<-% res_xg_direct2z_paired

res_vaep_direct2z_paired <- 
  do_fit_dummy(
    df_vaep_direct2z_paired,
    agg_vaep_direct2z_paired,
    baseline_vaep,
    baseline_vaep_by_grp,
    col = 'vaep',
    suffix = 'vaep_direct2z'
  )

res_xg_direct2z_paired <-
  do_fit_dummy(
    df_xg_direct2z_paired,
    agg_xg_direct2z_paired,
    baseline_xg,
    baseline_xg_by_grp,
    col = 'xg',
    suffix = 'xg_direct2z'
  )

c(dummies_vaep, fit_vaep, coefs_vaep, vps_vaep, ...extra) %<-% res_vaep_direct2z_paired
c(dummies_xg, fit_xg, coefs_xg, vps_xg, ...extra) %<-% res_xg_direct2z_paired

dummies_vaep %>% 
  select(
    `VAEP/90 Diff Z-Trans` = z_diff,
    matches('\\(')
  ) %>% 
  arrange(`Serie A (Italy)`) %>% 
  str()

coefs_vaep %>% 
  select(
    `League` = league,
    `Estimate` = estimate
  )

vps_vaep %>% 
   mutate(across(p, scales::percent, accuracy = 1)) %>% 
  mutate(diff = estimate_1 - estimate_2) %>% 
  select(
    `League A` = league_1,
    `League B` = league_2,
    `Estimate A` = estimate_1,
    `Estimate B` = estimate_2,
    `Diff.` = diff,
    # `Diff. (VAEP/90)` = vp,
    # `% Diff.` = p
  )

lvls_lg_xg <-
  vps_xg %>% 
  distinct(league = league_1, rnk = rnk_1) %>% 
  drop_na() %>% 
  pull(league)
lvls_lg_xg

l2l_xg <-
  df_xg_direct2z_paired %>% 
  # filter(position == 'FW') %>% 
  # filter(age_grp == '18<=x<24') %>% 
  filter(position_grp == 'A') %>% 
  group_by(league_1, league_2) %>% 
  summarize(n = n(), across(c(z_1, z_2, z_diff_orig), ~mean(.x))) %>% 
  ungroup() %>% 
  filter(league_1 %in% lvls_lg_xg) %>% 
  filter(league_2 %in% lvls_lg_xg)
l2l_xg %>% drop_na()
l2l_xg$league_1 <- l2l_xg$league_1 %>% ordered(lvls_lg_xg)
l2l_xg$league_2 <- l2l_xg$league_2 %>% ordered(rev(lvls_lg_xg))
l2l_xg$league_1 %>% levels()
l2l_xg$league_2 %>% levels()

viz_xg_unadjusted <-
  l2l_xg %>% 
  ggplot() +
  aes(x = league_1, y = league_2) +
  geom_tile(aes(fill = z_diff_orig), alpha = 0.7,  height = 0.95, width = 0.95, show.legend = FALSE) +
  geom_text(aes(label = sprintf('%+.2f', z_diff_orig)), size = pts(14), fontface = 'bold') +
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
    title = 'Un-adjusted xG/90 change when moving from league A to B',
    tag = lab_tag,
    subtitle = 'Forwards and Attacking Midfielders, Ages 18-35, 2012-2020',
    caption = '<span></span>',
    y = 'League A',
    x = 'League B'
  )
viz_xg_unadjusted

ggsave(
  plot = viz_xg_unadjusted,
  filename = file.path(dir_proj, 'viz_xg_unadjusted.png'),
  width = 16,
  height = 8,
  type = 'cairo'
)
