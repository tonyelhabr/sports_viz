
# Inspiration: https://hockey-graphs.com/2020/03/02/which-league-is-best/
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
df_xg_direct2z_paired %>% 
  filter(league_1 == 'Premier League (England)') %>%
  filter(season_1 == 2020)

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
