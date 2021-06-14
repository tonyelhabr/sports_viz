
library(tidyverse)
dir_proj <- '33-2020_euros_ratings'
source(file.path(dir_proj, 'helpers.R'))

# made this
league_mapping <- import_csv('league_mapping')

leagues_init <-
  import_csv('leagues') %>% 
  rename(league_name = name) %>% 
  mutate(
    # Some manual adjustments prior to using some "auto" name correction (by descending season)
    across(
      league_name,
      ~case_when(
        country == 'USA' & league_name %>% str_detect('Major League Soccer') ~ 'Major League Soccer',
        country == 'China' & league_name %>% str_detect('Super League') ~ 'Super League',
        country == 'Spain' & league_name == 'Primera Division' ~ 'LaLiga',
        TRUE ~ .x
      )
    )
  )
leagues_init
leagues_init %>% count(league_id, sort = TRUE)
league_names <-
  leagues_init %>%
  mutate(
    across(league_name, ~str_remove_all(.x, '\\s+[Gg]rp.*'))
  ) %>% 
  mutate(n_char = nchar(league_name)) %>% 
  arrange(league_id, n_char, desc(season)) %>% 
  group_by(league_id) %>% 
  filter(row_number() == 1L) %>% 
  ungroup() %>% 
  select(-season)
league_names

league_names %>%
  unite('league', country, league_name, sep = '_') %>%
  mutate(across(league, ~str_replace_all(.x, '\\s|[.]', '_') %>% str_replace_all('[_]+', '_'))) %>%
  left_join(league_mapping) %>%
  # select(-league) %>%
  # rename(league = league_lab)
  filter(is.na(league_lab))

leagues <- leagues_init %>% select(-league_name) %>% inner_join(league_names)
leagues

teams <- import_csv('teams')
players <- 
  import_csv('players') %>% 
  mutate(across(dob, lubridate::date)) %>% 
  rename(player_id = id, player_name = name)

player_ratings <-
  import_csv('player_ratings') %>% 
  mutate(
    value = offensive_value + defensive_value
  )
player_ratings

# These have multiple "sub-leagues" (e.g. playoffs, group stages).
# This is corrected later with a summarize.
leagues %>% count(country, league_name, season, sort = TRUE)

leagues_n <- 
  leagues %>% 
  # filter(!(country %in% c('International', 'Europe'))) %>% 
  count(country, league_id, league_name, sort = TRUE) %>% 
  filter(n > 1L)
leagues_n

leagues_n <-
  leagues %>% 
  filter(season >= 2012) %>% 
  distinct(country, league_id, league_name, season) %>% 
  # arrange(country, league_id, league_name, season) %>% 
  count(country, league_id, league_name, sort = TRUE) %>% 
  filter(n >= 5L)
leagues_n

age_grps <-
  tibble(
    from = c(18L, 24L, 27L, 30L),
    to = c(24L, 27L, 30L, 36L)
  )
age_grps

# player_ratings %>% filter(player_id == 11119) %>% arrange(desc(value))

position_mapping <-
  tibble(
    position_old = c('AM', 'FW', 'M', 'DM', 'D'),
    position = c('A', 'A', 'M', 'M', 'D')
  )
position_mapping

df_init <-
  list(
    leagues_n %>% select(-n),
    teams %>% distinct(),
    player_ratings,
    players %>%
      drop_na(position, dob) %>%
      select(player_id, player_name, position, dob) %>%
      mutate(across(position, ~str_remove_all(.x, '\\(.*\\)') %>% str_remove_all('\\,.*$'))) %>%
      filter(position != 'GK') %>%
      rename(position_old = position) %>%
      inner_join(position_mapping) %>%
      select(-position_old)
  ) %>% 
  reduce(inner_join) %>%
  ungroup() %>% 
  mutate(
    value_pm = value / minutes,
    v = 90 * value_pm
  ) %>% 
  filter(minutes > (5 * 90)) %>% 
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

df <-
  df_init %>% 
  rename(v_orig = v) %>% 
  mutate(v = 90 * (value + lst$alpha) / (minutes + lst$alpha + lst$beta)) 

set.seed(42)
df %>% 
  sample_frac(0.1) %>% 
  ggplot() +
  aes(x = v_orig, y = v) +
  geom_point(aes(size = minutes), alpha = 0.1) +
  scale_size_area(c(0.1, 2))

# function time ----
df_trans <- df %>% do_modify_v_col(direct = FALSE)
df_direct <- df %>% do_modify_v_col(direct = TRUE)
# c(p1_trans, p2_trans, p3_trans) %<-% df_trans %>% do_plots(direct = FALSE)
# c(p1_direct, p2_direct, p3_direct) %<-% df_trans %>% do_plots(direct = TRUE)
df_trans %>% do_plots(direct = FALSE)
df_direct %>% do_plots(direct = TRUE)
# df_trans_tst <- df_trans %>% do_filter_tst()
# df_trans_tst

# map doesnt' return the right result?
# c(df_trans_paired, df_direct_paired) %<-% list(df_trans, df_direct) %>% map(do_get_data)
df_trans_paired <- df_trans %>% do_get_data()
df_direct_paired <- df_direct %>% do_get_data()

if(FALSE){
  # map+zeallot not working?
  # c(dummies_trans_full, fit_trans_full, coefs_trans_full) %<-% df_trans_paired %>% do_fit_dummy(strict = FALSE)
  # c(dummies_trans_strict, fit_trans_strict, coefs_trans_strict) %<-% df_trans_paired %>% do_fit_dummy(strict = TRUE)
  # c(dummies_direct_full, fit_direct_full, coefs_direct_full) %<-% df_direct_paired %>% do_fit_dummy(strict = FALSE)
  # c(dummies_direct_strict, fit_direct_strict, coefs_direct_strict) %<-% df_direct_paired %>% do_fit_dummy(strict = TRUE)
  res_trans_full <- df_trans_paired %>% do_fit_dummy(strict = FALSE)
  res_trans_strict <- df_trans_paired %>% do_fit_dummy(strict = TRUE)
  res_direct_full <- df_direct_paired %>% do_fit_dummy(strict = FALSE)
  res_direct_strict <- df_direct_paired %>% do_fit_dummy(strict = TRUE)
  c(dummies_trans_full, fit_trans_full, coefs_trans_full) %<-% res_trans_full
  c(dummies_trans_strict, fit_trans_strict, coefs_trans_strict) %<-% res_trans_strict
  c(dummies_direct_full, fit_direct_full, coefs_direct_full) %<-% res_direct_full
  c(dummies_direct_strict, fit_direct_strict, coefs_direct_strict) %<-% res_trans_strict
  
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
  f_plot_coefs_overall(coefs_trans_full, suffix = 'trans_full')
  f_plot_coefs_by_season <- partial(do_plot_coefs_by_season, dir = dir_proj, ... = )
  f_plot_coefs_by_season(dummies_trans_full, suffix = 'trans_full')
  
  # other models ----
  res_trans_full_bt <- df_trans_paired %>% do_fit_bt(strict = FALSE)
  res_trans_strict_bt <- df_trans_paired %>% do_fit_bt(strict = TRUE)
  res_direct_full_bt <- df_direct_paired %>% do_fit_bt(strict = FALSE)
  res_direct_strict_bt <- df_direct_paired %>% do_fit_bt(strict = TRUE)
  c(dummies_trans_full_bt, fit_trans_full_bt, coefs_trans_full_bt, probs_trans_full_bt) %<-% res_trans_full_bt
  c(dummies_trans_strict_bt, fit_trans_strict_bt, coefs_trans_strict_bt, probs_trans_strict_bt) %<-% res_trans_strict_bt
  c(dummies_direct_full_bt, fit_direct_full_bt, coefs_direct_full_bt, probs_direct_full_bt) %<-% res_direct_full_bt
  c(dummies_direct_strict_bt, fit_direct_strict_bt, coefs_direct_strict_bt, probs_direct_strictl_bt) %<-% res_trans_strict_bt
  
  coefs_compare_bt <-
    list(
      f_rename(coefs_trans_full_bt),
      f_rename(coefs_trans_strict_bt),
      f_rename(coefs_direct_full_bt),
      f_rename(coefs_direct_strict_bt)
    ) %>% 
    reduce(full_join)
  coefs_compare_bt
}
# stan ----
league_ids <-
  df_trans_paired %>% 
  count(league = league_1, sort = TRUE) %>% 
  mutate(player = row_number(desc(n)))
league_ids

df_stan <-
  df_trans_paired %>% 
  left_join(league_ids %>% select(league_1 = league, player_1 = player)) %>% 
  left_join(league_ids %>% select(league_2 = league, player_2 = player)) %>% 
  mutate(y = if_else(z_diff < 0, 1L, 0L)) %>% 
  select(player_1, player_2, y)
df_stan

k <- league_ids %>% nrow()
n <- df_stan %>% nrow()

path_fit_stan <- file.path(dir_proj, 'fit_stan.rds')
path_posterior <- file.path(dir_proj, 'posterior.rds')
path_posterior_exists <- file.exists(path_posterior)
if(!path_posterior_exists) {
  library(rstan)
  rstan::rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores(logical = FALSE))
  lst_stan <-
    list(
      K = k,
      N = n,
      player_1 = df_stan$player_1,
      player_2 = df_stan$player_2,
      y = df_stan$y
    )
  # fit_stan <- rstan::stan_model(file.path(dir_proj, 'model.stan'))
  # write_rds(fit_stan, path_fit_stan)
  # individual_posterior <- rstan::sampling(fit_stan, data = lst_stan)
  # print(individual_posterior, 'alpha', probs=c(0.05, 0.5, 0.95))
  # posterior <- rstan::extract(individual_posterior)
  # posterior
  fit_stan <-
    rstan::stan(
      file = file.path(dir_proj, 'model.stan'), 
      data = lst_stan,
      seed = 42
    )
  write_rds(fit_stan, path_fit_stan)
  posterior <- rstan::extract(fit_stan)
  posterior
  write_rds(posterior, path_posterior)
} else {
  fit_stan <- path_fit_stan %>% read_rds()
  posterior <- path_posterior %>% read_rds()
}
# pars <- fit_stan@sim$pars_oi
# s <- summary(fit_stan, pars, probs = c(0.05, 0.95))
# s_tbl <- 
#   s$summary %>% 
#   # tibble(rownames = 'alpha') %>%
#   as_tibble() %>% 
#   set_names(c('mean', 'se', 'sd', 'p_05', 'p_95', 'n_eff', 'r_hat')) %>% 
#   bind_cols(tibble(term = s$summary %>% rownames()), .) %>% 
#   # mutate(across(c(mean:p_95), ~-.x)) %>% 
#   # mutate(across(term, ~str_remove_all(.x, c('\\[' = '_', '\\]$' = '')))) %>% 
#   filter(term %>% str_detect('alpha')) %>% 
#   select(-term) %>% 
#   bind_cols()
# s_tbl

rnks <- 
  tibble(
    'estimate' = apply(posterior$alpha, 2, mean)
  ) %>% 
  bind_cols(league_ids %>% select(-n)) %>% 
  mutate(rnk = row_number(desc(estimate)))
rnks

pars_mapping <-
  rnks %>% 
  mutate(across(league, ~fct_reorder(.x, estimate))) %>% 
  select(parameter = player, league) %>% 
  mutate(across(parameter, ~sprintf('alpha[%d]', .x)))
pars_mapping

library(bayesplot)
posterior_v <- as.array(fit_stan)
nms <- dimnames(posterior_v)
nms_alpha <- nms$parameters %>% str_subset('alpha')
# bayesplot::mcmc_intervals(
#   posterior_v, 
#   pars = nms_alpha
# )
bayesplot::mcmc_areas(
  posterior_v, 
  par = nms_alpha,
  prob = 0.8,
  prob_outer = 0.99,
  point_est = 'mean'
) +
  tonythemes::theme_tony()
# bayesplot::mcmc_areas_ridges(
#   posterior_v, 
#   par = nms_alpha,
#   prob = 0.8,
#   prob_outer = 0.99
# )
# 
# bayesplot::mcmc_hist(
#   posterior_v, 
#   par = nms_alpha
# )
# bayesplot::mcmc_hist(
#   posterior_v, 
#   par = nms_alpha
# )
# mcmc_areas(posterior, 'alpha')

x = posterior_v
pars = nms_alpha
regex_pars = character()
transformations = list()
area_method = c('equal area') # , 'equal height', 'scaled height')
prob = 0.5
prob_outer = 1
point_est = c('median', 'mean', 'none')
rhat = numeric()
bw = NULL
adjust = NULL
kernel = NULL
n_dens = NULL
# area_method <- match.arg(area_method)

data <- bayesplot::mcmc_areas_data(
  x, pars, regex_pars, transformations,
  prob = prob, prob_outer = prob_outer,
  point_est = point_est, rhat = rhat,
  bw = bw, adjust = adjust, kernel = kernel, n_dens = n_dens
)
data <-
  data %>% 
  left_join(pars_mapping) %>%
  select(-parameter) %>% 
  rename(parameter = league) %>% 
  mutate(across())
data
datas <- split(data, data$interval)
datas
no_point_est <- !rlang::has_name(datas, "point")
datas$point <- if (no_point_est) {
  dplyr::filter(datas$inner, FALSE)
} else {
  datas$point
}
color_by_rhat <- rlang::has_name(data, "rhat_rating")

# faint vertical line at zero if zero is within x_lim
x_lim <- range(datas$outer$x)
x_range <- diff(x_lim)
x_lim[1] <- x_lim[1] - 0.05 * x_range
x_lim[2] <- x_lim[2] + 0.05 * x_range

layer_vertical_line <- if (0 > x_lim[1] && 0 < x_lim[2]) {
  vline_0(color = "gray90", size = 0.5)
} else {
  geom_ignore()
}
groups <- if (color_by_rhat) {
  rlang::syms(c("parameter", "rhat_rating"))
} else {
  rlang::syms(c("parameter"))
}

if (area_method == "equal height") {
  dens_col = ~ scaled_density
} else if (area_method == "scaled height") {
  dens_col = ~ scaled_density * sqrt(scaled_density)
} else {
  dens_col = ~ plotting_density
}

datas$bottom <- datas$outer %>%
  group_by(!!! groups) %>%
  summarise(
    ll = min(.data$x),
    hh = max(.data$x),
    .groups = "drop_last"
  ) %>%
  ungroup()

args_bottom <- list(
  mapping = aes_(x = ~ ll, xend = ~ hh, yend = ~ parameter),
  data = datas$bottom
)
args_inner <- list(
  mapping = aes_(height = dens_col, scale = ~ .9),
  data = datas$inner
)
args_point <- list(
  mapping = aes_(height = dens_col, scale = ~ .9),
  data = datas$point,
  color = NA
)
args_outer <- list(
  mapping = aes_(height = dens_col, scale = ~ .9),
  fill = NA
)

if (color_by_rhat) {
  args_bottom$mapping <- args_bottom$mapping %>%
    modify_aes_(color = ~ rhat_rating)
  args_inner$mapping <- args_inner$mapping %>%
    modify_aes_(color = ~ rhat_rating,
                fill = ~ rhat_rating)
  args_outer$mapping <- args_outer$mapping %>%
    modify_aes_(color = ~ rhat_rating)
  # rhat fill color scale uses light/mid/dark colors. The point estimate needs
  # to be drawn with highlighted color scale, so we manually set the color for
  # the rhat fills.
  dc <- diagnostic_colors("rhat", "color")[["values"]]
  args_point$fill <- dc[datas$point$rhat_rating]
} else {
  args_bottom$color <- 'grey20'# bayesplot:::get_color("dark")
  args_inner$color <- 'grey20' # bayesplot:::get_color("dark")
  args_inner$fill <- 'grey80' # bayesplot:::get_color("light")
  args_point$fill <- 'grey50' # bayesplot:::get_color("mid_highlight")
  args_outer$color <- 'grey20' #  bayesplot:::get_color("dark")
}
# An invisible layer that is 2.5% taller than the plotted one
args_outer2 <- args_outer
args_outer2$mapping <- args_outer2$mapping %>%
  bayesplot:::modify_aes_(scale = .925)
args_outer2$color <- NA

layer_bottom <- do.call(geom_segment, args_bottom)
layer_inner <- do.call(ggridges::geom_ridgeline, args_inner)
layer_outer <- do.call(ggridges::geom_ridgeline, args_outer)
layer_outer2 <- do.call(ggridges::geom_ridgeline, args_outer2)

point_geom <- if (no_point_est) {
  geom_ignore
} else {
  ggridges::geom_ridgeline
}
layer_point <- do.call(point_geom, args_point)

# Do something or add an invisible layer
if (color_by_rhat) {
  scale_color <- bayesplot:::scale_color_diagnostic("rhat")
  scale_fill <- bayesplot:::scale_fill_diagnostic("rhat")
} else {
  scale_color <- bayesplot:::geom_ignore()
  scale_fill <- bayesplot:::geom_ignore()
}

p <-
  ggplot(datas$outer) +
  aes_(x = ~ x, y = ~ parameter) +
  layer_vertical_line +
  layer_inner +
  layer_point +
  layer_outer +
  layer_outer2 +
  layer_bottom +
  scale_color +
  scale_fill +
  # scale_y_discrete(
  #   limits = unique(rev(data$parameter)),
  #   expand = expansion(
  #     add = c(0, .5 + 1/(2 * nlevels(data$parameter))),
  #     mult = c(.1, .1)
  #   )
  # ) +
  xlim(x_lim)
p

# rnks ---
f_plot_coefs_overall(rnks, suffix = 'trans_bt')

f_select <- function(suffix, op = 1) {
  rnks %>% 
    mutate(across(league, ~forcats::fct_reorder(.x, op * rnk))) %>% 
    rename_all(~sprintf('%s_%s', .x, suffix)) %>% 
    mutate(dummy = 0L)
}

probs <-
  full_join(
    f_select(1, -1),
    f_select(2, -1)
  ) %>%
  select(-dummy) %>% 
  # filter(player_1 != player_2) %>% 
  mutate(
    p = logit2prob(estimate_1 - estimate_2)
  )
probs
probs$league_1 %>% levels()
probs$league_2 %>% levels()

r <- 45
# seq_first <- c(rep(league_ids$league, each = 2), '')
# seq_mid <- rep(league_ids$league, each = 2)
# seq <- sort(c(seq_first, seq_mid))
# labs <- c(rep(league_ids$league, each = 4), '')
# idx <- (seq_along(labs) %% 2) == 1L
# # idx[length(idx) - 1] <- TRUE
# labs[idx] <- ''
# labs

probs_filt <-
  probs %>% 
  filter(rnk_1 <= rnk_2)
probs_filt

p <-
  probs_filt %>% 
  ggplot() +
  aes(x = league_2, y = league_1) +
  geom_tile(aes(fill = p), alpha = 0.7,  height = 0.95, width = 0.95, show.legend = FALSE) +
  geom_text(aes(label = scales::percent(p, accuracy = 1)), size = pts(14)) +
  # geom_text(
  #   data = probs_filt %>% filter(rnk_2 == (rnk_1 + 1)) %>% ungroup(),
  #   # angle = 45,
  #   hjust = 0,
  #   size = pts(8),
  #   aes(label = league_2)
  # ) +
  # coord_flip() +
  scale_fill_viridis_c(option = 'H', limits = c(0.4, 0.85)) +
  # scale_x_binned(
  #   labels = labs,
  #   breaks = seq
  # ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  # coord_flip(clip = 'off') +
  # coord_cartesian(clip = 'off') +
  theme(
    plot.title = ggtext::element_markdown(size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_text(angle = 90),
    # axis.labels.x = element_text(angle = 45),
    # axis.ticks.x = element_line(color = c('grey80', rep(c('grey80', NA), t = length(labs)))),
  ) +
  labs(
    title = 'P(League A > League B)',
    y = 'League A',
    x = 'League B'
  )
p
ggsave(
  plot = p,
  filename = file.path(dir_proj, 'bt_probs.png'),
  width = 16,
  height = 8,
  type = 'cairo'
)


# library(grid)
# # print(
# #   p + theme(text = element_text(angle = -r)),
# #   vp = grid::viewport(
# #     angle = r,
# #     width = unit(.75, 'npc'),
# #     height = unit(.75, 'npc')
# #   )
# # )
# 
# # method 2 ----
# grid::grid.newpage()
# grid::pushViewport(grid::viewport(angle = -r))
# g <-
#   ggplotGrob(
#     p +
#       theme(
#         text = element_text(angle = r),
#         axis.text = element_text(hjust = 1)
#       )
#   )
# 
# grid::grid.draw(g)
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
