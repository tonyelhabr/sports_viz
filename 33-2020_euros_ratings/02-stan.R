
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

posterior_v <- as.array(fit_stan)
# nms <- dimnames(posterior_v)
# nms_alpha <- nms$parameters %>% str_subset('alpha')

p <-
  do_mcmc_areas(
    x = posterior_v,
    prob = 0.8,
    pars = nms_alpha,
    pars_mapping = pars_mapping
  )

p_bt <-
  p +
  theme(
    plot.title = ggtext::element_markdown(size = 18),
    plot.tag = ggtext::element_markdown(size = 10),
    plot.tag.position = c(.01, 0.015),
    plot.caption = ggtext::element_markdown(size = 10)
  ) +
  labs(
    title = 'Bayesian Bradley-Terry Power Rankings',
    tag = '**Viz**: Tony ElHabr<br/>**Data**: @canzhiye',
    x = 'Logg Odds',
    caption = 'Log odds for "player" in "match" = log(P[player wins match] / (1 - P[player wins match]))<br/>Shaded area represents middle 80th confidence interval. Median shown by line.'
  )
p_bt
ggsave(
  plot = p_bt,
  filename = file.path(dir_proj, 'coefs_bt.png'),
  width = 8,
  height = 8,
  type = 'cairo'
)
# posterior2 <- rstan::extract(fit_stan, inc_warmup = TRUE, permuted = FALSE)
# p <- mcmc_trace(posterior2,  pars = nms_alpha, n_warmup = 300,
#                 facet_args = list(nrow = 2, labeller = label_parsed))
# p
f_plot_coefs_overall(rnks, suffix = 'trans_bt')