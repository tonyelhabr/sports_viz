
source('46-euro_fouls/01-setup.R')
library(ebbr)
library(rPref)

fouls <- all_actions %>% filter(type_name == 'foul')
turnovers <- all_actions %>% filter(turnover)
euro_fouls <- all_actions %>%
  filter(
    type_name == 'foul',
    time_between_poss <= 5,
    time_since_poss_start <= 3
  )
euro_fouls


do_count_euro_fouls <- function(cols) {
  col_syms <- syms(cols)
  n_euro_fouls_by_x <- euro_fouls %>%
    count(
      competition_id,
      season_id,
      !!!col_syms,
      name = 'n_euro_fouls',
      sort = TRUE
    )
  
  n_fouls_by_x <- fouls %>%
    count(
      competition_id,
      season_id,
      !!!col_syms,
      name = 'total_fouls',
      sort = TRUE
    )
  
  n_euro_fouls_by_x %>%
    left_join(
      n_fouls_by_x
    ) %>% 
    mutate(
      prop_fouls = n_euro_fouls / total_fouls
    )
}

euro_fouls_by_player <- do_count_euro_fouls(
  c('player_id')
) %>% 
  left_join(players_filt) %>%
  left_join(games_by_team)


df <- euro_fouls_by_player %>% 
  transmute(
    season_id,
    player_id, 
    player_name,
    pos_11,
    pos_grp,
    n_euro_fouls,
    total_fouls,
    prop_fouls,
    total_mp_scaled = total_mp * inv_prop_matches,
    prop_fouls_scaled = prop_fouls * inv_prop_matches,
    n_euro_fouls_scaled = n_euro_fouls * inv_prop_matches,
    total_fouls_scaled = total_fouls * inv_prop_matches
  )
df

df_adj <- df %>% 
  filter(pos_grp %in% c('D', 'M')) %>% 
  add_ebb_estimate(
    n_euro_fouls,
    total_fouls,
    method = 'mle',
    prior_subset = total_fouls >= 10
  ) %>% 
  mutate(
    prnk1 = percent_rank(.fitted),
    prnk2 = percent_rank(-total_fouls_scaled),
    prnk3 = percent_rank(total_mp_scaled),
    prnk = prnk1 + prnk2 + prnk3,
    size = (1 - (1 - prnk1))^2 * (1 - (1 - prnk2))^2
  )
df_adj %>% 
  group_by(player_id, player_name, pos_grp, pos_11) %>% 
  summarize(
    n = n(),
    across(prnk, sum)
  ) %>% 
  ungroup() %>% 
  filter(n >= 4) %>% 
  mutate(prnk_avg = prnk / n) %>% 
  group_by(pos_11) %>% 
  slice_max(prnk_avg, n = 3, with_ties = FALSE) %>% 
  ungroup()


# f1 <- df_adj %>% 
#   lm(total_fouls_scaled ~ total_mp_scaled, data = .) %>% 
#   broom::augment()
# 
# f1 %>% 
#   ggplot() +
#   # aes(x = .fitted, y = total_fouls_scaled) +
#   aes(x = total_mp_scaled, y = total_fouls_scaled, color = -.resid) +
#   geom_abline(
#     data = tibble(intercept = rep(0, 3), slope = c(0.5, 1, 1.5)/90),
#     aes(intercept = intercept, slope = slope)
#   ) +
#   # scale_radius(range = c(0.1, 3)) +
#   scale_x_continuous(
#     limits = c(0, 3800),
#     breaks = 1:4 * 10 * 90,
#     labels =  1:4 * 10,
#     expand = c(0, 0.05)
#   ) +
#   # scale_color_viridis_c(option = 'D', direction = -1) +
#   scale_color_gradient2(midpoint = median(f1$.resid), mid = 'green', high = 'cyan', low = 'red') +
#   # scale_color
#   geom_point(show.legend = FALSE)

df_adj_pref <- df_adj %>% 
  psel(
    low(total_mp_scaled) | high(total_fouls_scaled), top = nrow(.)
  ) %>% 
  rename(z = .level)

player_names <- c('James Ward-Prowse', 'Declan Rice', 'Conor Coady', 'Aaron Cresswell')
df_adj_pref_filt <- df_adj_pref %>% 
  filter(player_name %in% player_names) %>% 
  mutate(
    lab = sprintf("%s '%s", str_replace_all(player_name, '(^.*\\s)(.*$)', '\\2'), season_id)
  )
add_players <- function(...) {
  list(
    ...,
    geom_point(
      data = df_adj_pref_filt,
      shape = 19,
      size = 4,
      color = 'black'
    ),
    ggrepel::geom_text_repel(
      data = df_adj_pref_filt,
      aes(label = lab)
    )
  )
}

p1 <- df_adj_pref %>% 
  ggplot() +
  aes(x = total_mp_scaled, y = total_fouls_scaled) +
  scale_x_continuous(
    limits = c(0, 4500),
    breaks = 1:4 * 10 * 90,
    labels = 1:4 * 10,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    expand = c(0.02, 0)
  ) +
  geom_abline(
    data = tibble(intercept = rep(0, 3), slope = c(0.5, 1, 1.5)/90),
    aes(intercept = intercept, slope = slope)
  ) +
  geom_point(
    aes(fill = z),
    shape = 21,
    size = 1.5, 
    show.legend = FALSE
  ) +
  scale_fill_viridis_c(option = 'H') +
  add_players() +
  theme(
    # panel.background = element_rect(),
    panel.border = element_rect(fill = NA),
    panel.grid.major = element_blank()
  ) +
  labs(
    x = "90's played",
    y = 'Total Fouls'
  )
p1


res <- df_adj %>% psel(low(.fitted) | high(total_fouls_scaled), top = nrow(df_adj))
res
res %>% 
  ggplot() +
  # aes(x = .fitted, y = total_fouls_scaled) +
  aes(x = .fitted, y = total_fouls_scaled, color = .level) +
  # scale_size_area(max_size = 3) +
  scale_alpha_continuous(range = c(0.25, 1)) +
  geom_point()

# xi <- mean(df_adj$.fitted)
# yi <- mean(df_adj$total_fouls_scaled)
# xi; yi
# yi/xi
# fit_poly <- lm(total_fouls_scaled ~ .fitted + I(.fitted^2), df_adj)
# fit_poly
df_adj %>% 
  ggplot() +
  # aes(x = .fitted, y = total_fouls_scaled) +
  aes(x = prnk1, y = prnk2, alpha = size, size = size) +
  # scale_size_area(max_size = 3) +
  geom_point(
    data = df_adj %>% filter(prnk1 >= 0.5, prnk2 >= 0.5),
    color = 'green'
  ) +
  geom_point(
    data = df_adj %>% filter(prnk1 <= 0.5, prnk2 >= 0.5),
    color = 'red'
  ) +
  geom_point(
    data = df_adj %>% filter(prnk1 <= 0.5, prnk2 <= 0.5),
    color = 'blue'
  ) +
  geom_point(
    data = df_adj %>% filter(prnk1 >= 0.5, prnk2 <= 0.5),
    color = 'magenta'
  ) +
  # geom_smooth(
  #   method = 'lm',
  #   formula = formula(y ~ x + I(x^2))
  # ) +
  # geom_function(
  #   fun = function(x) 696-351.1*x + 275.3*(x^2)
  # ) +
  # geom_vline(
  #   aes(xintercept = mean(.fitted^2))
  # ) +
  # geom_hline(
#   aes(yintercept = mean(total_fouls_scaled))
# ) +
# geom_abline(
#   aes(slope = yi/xi, intercept = 0)
# ) +
# coord_cartesian(xlim = c(0, 1)) +
geom_point(
  data = coady,
  size = 3,
  color = 'red'
) +
  geom_point(
    data = silva,
    size = 3,
    color = 'magenta'
  )

