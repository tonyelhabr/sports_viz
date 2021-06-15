
library(tidyverse)
dir_proj <- '33-2020_euros_ratings'
source(file.path(dir_proj, 'helpers.R'))

league_mapping <- import_league_mapping()
df <- do_import(dir = dir_proj, league_mapping = league_mapping)
set.seed(42)
df %>% 
  sample_frac(0.1) %>% 
  ggplot() +
  aes(x = v_orig, y = v) +
  geom_point(aes(size = minutes), alpha = 0.1) +
  scale_size_area(c(0.1, 2)) +
  labs(
    title = 'How the Empirical Bayes "normalizes" some of the lower minutes rates'
  )

# function time ----
res_trans <- df %>% do_modify_v_col(direct = FALSE)
res_direct <- df %>% do_modify_v_col(direct = TRUE)
c(df_trans, agg, v_min) %<-% res_trans
c(df_direct, agg_direct, v_min_direct) %<-% res_direct
# c(p1_trans, p2_trans, p3_trans) %<-% df_trans %>% do_plots(direct = FALSE)
# c(p1_direct, p2_direct, p3_direct) %<-% df_trans %>% do_plots(direct = TRUE)
df_trans %>% do_plots(direct = FALSE)
df_direct %>% do_plots(direct = TRUE)
df_direct %>% skimr::skim(z)
arrow::write_parquet(df_direct, file.path(dir_proj, 'df.parquet'))

# map doesnt' return the right result?
# c(df_trans_paired, df_direct_paired) %<-% list(df_trans, df_direct) %>% map(do_get_data)
res_trans_paired <- df_trans %>% do_get_data()
res_direct_paired <- df_direct %>% do_get_data(normalize = TRUE)
c(df_trans_paired, agg_trans_paired) %<-% res_trans_paired
c(df_direct_paired, agg_direct_paired) %<-% res_direct_paired

df_direct %>% 
  filter(league_name == 'Champions League') %>% 
  # drop_na(z)
  skimr::skim(z)

df_direct_paired %>% 
  ggplot() +
  aes(x = z_diff) +
  geom_histogram(binwidth = 0.1) +
  coord_cartesian(xlim = c(-3, 3))

df_trans %>% 
  filter(player_name == 'Hakim Ziyech') %>% 
  arrange(desc(season))
df_trans_paired %>% filter(idx_1 == 26509)

# map+zeallot not working?
# c(dummies_trans_full, fit_trans_full, coefs_trans_full) %<-% df_trans_paired %>% do_fit_dummy(strict = FALSE)
res_trans_full <- df_trans_paired %>% do_fit_dummy(strict = FALSE)
res_trans_strict <- df_trans_paired %>% do_fit_dummy(strict = TRUE)
res_direct_full <- df_direct_paired %>% do_fit_dummy(strict = FALSE)
res_direct_strict <- df_direct_paired %>% do_fit_dummy(strict = TRUE) # what i'm ultimately using
c(dummies_trans_full, fit_trans_full, coefs_trans_full) %<-% res_trans_full
c(dummies_trans_strict, fit_trans_strict, coefs_trans_strict) %<-% res_trans_strict
c(dummies_direct_full, fit_direct_full, coefs_direct_full) %<-% res_direct_full
c(dummies_direct_strict, fit_direct_strict, coefs_direct_strict) %<-% res_trans_strict

dummies_direct_strict %>% 
  ggplot() +
  aes(x = z_diff) +
  geom_histogram(binwidth = 0.1) +
  coord_cartesian(xlim = c(-3, 3))

coefs_trans_compare <-
  do_compare_coefs(
    coefs_trans_full,
    coefs_trans_strict,
    suffix = sprintf('trans_%s', c('full', 'strict'))
  )
coefs_trans_compare

coefs_direct_compare <-
  do_compare_coefs(
    coefs_direct_full,
    coefs_direct_strict,
    suffix = sprintf('direct_%s', c('full', 'strict'))
  )
coefs_direct_compare

f_rename <- function(data, nm = deparse(substitute(data))) {
  suffix <- nm %>% str_remove('coefs')
  data %>% 
    mutate(!!sym(sprintf('rnk%s',  suffix)) := row_number(desc(estimate))) %>% 
    rename(!!sym(sprintf('estimate%s', suffix)) := estimate)
}

coefs_compare <-
  list(
    f_rename(coefs_trans_full),
    f_rename(coefs_trans_strict),
    f_rename(coefs_direct_full),
    f_rename(coefs_direct_strict)
  ) %>% 
  reduce(full_join)
coefs_compare

f_plot_coefs_overall <- partial(do_plot_coefs_overall, dir = dir_proj, ... = )
f_plot_coefs_overall(coefs_direct_strict, suffix = 'trans_full')
f_plot_coefs_by_season <- partial(do_plot_coefs_by_season, dir = dir_proj, ... = )
f_plot_coefs_by_season(coefs_direct_strict, suffix = 'trans_full')

# other models ----
res_trans_full_bt <- df_trans_paired %>% do_fit_bt(strict = FALSE)
res_trans_strict_bt <- df_trans_paired %>% do_fit_bt(strict = TRUE)
res_direct_full_bt <- df_direct_paired %>% do_fit_bt(strict = FALSE)
res_direct_strict_bt <- df_direct_paired %>% do_fit_bt(strict = TRUE)
c(dummies_trans_full_bt, fit_trans_full_bt, coefs_trans_full_bt, vps_trans_full_bt) %<-% res_trans_full_bt
c(dummies_trans_strict_bt, fit_trans_strict_bt, coefs_trans_strict_bt, vps_trans_strict_bt) %<-% res_trans_strict_bt
c(dummies_direct_full_bt, fit_direct_full_bt, coefs_direct_full_bt, vps_direct_full_bt) %<-% res_direct_full_bt
c(dummies_direct_strict_bt, fit_direct_strict_bt, coefs_direct_strict_bt, vps_direct_strictl_bt) %<-% res_trans_strict_bt

coefs_compare_bt <-
  list(
    f_rename(coefs_trans_full_bt),
    f_rename(coefs_trans_strict_bt),
    f_rename(coefs_direct_full_bt),
    f_rename(coefs_direct_strict_bt)
  ) %>% 
  reduce(full_join)
coefs_compare_bt

# stan ----
# go to stan script

# rnks ---
rnks <- 
  coefs_direct_strict %>% 
  mutate(rnk = row_number(desc(estimate))) %>% 
  arrange(rnk)
rnks

f_select <- function(suffix, op = 1) {
  rnks %>% 
    mutate(across(league, ~forcats::fct_reorder(.x, op * rnk))) %>% 
    rename_all(~sprintf('%s_%s', .x, suffix)) %>% 
    mutate(dummy = 0L)
}

baseline <- 0.33 # emperical choice based on df_direct %>% skimr::skim(z)
baseline
vps <-
  full_join(
    f_select(1, -1),
    f_select(2, -1)
  ) %>%
  select(-dummy) %>% 
  # filter(player_1 != player_2) %>% 
  mutate(
    diff = estimate_1 - estimate_2,
    vp =  agg_direct_paired$z_diff_sd * (diff + agg_direct_paired$z_diff_mean),
    p = vp / !!baseline
  )
vps
vps$league_1 %>% levels()
vps$league_2 %>% levels()

# r <- 45
# seq_first <- c(rep(league_ids$league, each = 2), '')
# seq_mid <- rep(league_ids$league, each = 2)
# seq <- sort(c(seq_first, seq_mid))
# labs <- c(rep(league_ids$league, each = 4), '')
# idx <- (seq_along(labs) %% 2) == 1L
# # idx[length(idx) - 1] <- TRUE
# labs[idx] <- ''
# labs

vps_filt <-
  vps %>% 
  filter(rnk_1 <= rnk_2) %>% 
  filter(league_2 != '(Intercept)')
vps_filt

viz_diff_v <-
  vps_filt %>% 
  ggplot() +
  aes(x = league_2, y = league_1) +
  geom_tile(aes(fill = vp), alpha = 0.7,  height = 0.95, width = 0.95, show.legend = FALSE) +
  geom_text(aes(label = scales::number(vp, accuracy = 0.01)), size = pts(14), fontface = 'bold') +
  scale_fill_viridis_c(option = 'D', begin = 0.1, end = 1) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  theme(
    plot.title = ggtext::element_markdown(size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.title.y = element_text(angle = 90, size = 16),
  ) +
  labs(
    title = 'Expected change in VAEP/90 when transitioning from League A to B',
    y = 'League A',
    x = 'League B'
  )
viz_diff_v

ggsave(
  plot = viz_diff_v,
  filename = file.path(dir_proj, 'viz_relative_vaep_p90.png'),
  width = 16,
  height = 8,
  type = 'cairo'
)

viz_diff_rel <-
  vps_filt %>% 
  ggplot() +
  aes(x = league_2, y = league_1) +
  geom_tile(aes(fill = p), alpha = 0.7,  height = 0.95, width = 0.95, show.legend = FALSE) +
  geom_text(aes(label = scales::percent(p, accuracy = 1)), size = pts(14), fontface = 'bold') +
  scale_fill_viridis_c(option = 'H', begin = 0.1, end = 0.9) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  # coord_flip(clip = 'off') +
  # coord_cartesian(clip = 'off') +
  theme(
    plot.title = ggtext::element_markdown(size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.title.y = element_text(angle = 90, size = 16),
    plot.caption = ggtext::element_markdown(size = 12, face = 'italic')
    # axis.labels.x = element_text(angle = 45),
    # axis.ticks.x = element_line(color = c('grey80', rep(c('grey80', NA), t = length(labs)))),
  ) +
  labs(
    title = 'Relative increase in competition in League A compared to League B',
    caption = 'Using VAEP/90 baseline of 0.33',
    y = 'League A',
    x = 'League B'
  )
viz_diff_rel

ggsave(
  plot = viz_diff_rel,
  filename = file.path(dir_proj, 'viz_relative_difficulty.png'),
  width = 16,
  height = 8,
  type = 'cairo'
)

# diamond plot? ----
r <- -45
vps_filt <-
  vps %>% 
  filter(rnk_1 >= rnk_2) %>% 
  filter(league_2 != '(Intercept)')
vps_filt

p <-
  vps_filt %>% 
  ggplot() +
  aes(x = league_2, y = league_1) +
  geom_tile(aes(fill = p), alpha = 0.7,  height = 0.95, width = 0.95, show.legend = FALSE) +
  geom_text(aes(label = scales::percent(p, accuracy = 1)), size = pts(14), angle = -1 * r, fontface = 'bold') +
  scale_fill_viridis_c(option = 'H', begin = 0.1, end = 0.9) +
  coord_equal() +
  theme(
    plot.title = ggtext::element_markdown(angle = -1 * r, size = 18, hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(angle = - 1 * r, hjust = 1),
    # axis.text.y = element_text(angle = 360-1 * r, hjust = 1),
    axis.title.y = element_text(angle = 90, size = 16),
    plot.subtitle = ggtext::element_markdown(angle = -1 * r, size = 18, hjust = 0.5, face = 'italic')
  ) +
  labs(
    # title = 'How much "harder" is League A compared to League B?',
    # subtitle = 'Using VAEP/90 baseline of 0.33',
    y = 'League A',
    x = 'League B'
  )
# p

library(grid)
grid::grid.newpage()
grid::pushViewport(grid::viewport(angle = r))
g <-
  ggplotGrob(
    p
  )
# g
grid::grid.draw(g)
# res <- gridExtra::arrangeGrob(g)
# ggsave(plot = g, filename = file.path(dir_proj, 'test.png'), width = 10, height = 10, type = 'cairo')
# 
# grid::grid.newpage()
# grid::pushViewport(grid::viewport(angle = r))
# print(
#   p,
#   vp = grid::viewport(
#     angle = r,
#     width = unit(.75, 'npc'),
#     height = unit(.75, 'npc')
#   )
# )
