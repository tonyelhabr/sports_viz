
library(tidyverse)

dir_proj <- '30-var'
source(file.path(dir_proj, 'helpers.R'))
res <- 
  2019L:2020L %>% 
  tibble(season = .) %>% 
  mutate(data = map(season, ~do_scrape(.x))) %>% 
  unnest_wider(data)
agg <- res %>% select(season, agg) %>% unnest(agg)
agg
subjective_calls <-
  agg %>% 
  select(season, team, matches('^subjective_decision')) %>% 
  pivot_longer(
    -c(season, team),
    names_to = 'side',
    values_to = 'n'
  ) %>% 
  mutate(across(side, ~str_remove(.x, 'subjective_decisions_'))) %>% 
  mutate(n_sign = ifelse(side == 'against', -n, n)) 
subjective_calls

subjective_calls_agg <-
  subjective_calls %>% 
  group_by(season, team) %>% 
  summarize(
    n_net = sum(n_sign)
  ) %>% 
  ungroup() %>% 
  arrange(season, desc(n_net)) %>% 
  mutate(grp = tidytext::reorder_within(team, n_net, season))
subjective_calls_agg %>% filter(season == 2020L)


subjective_calls_viz <-
  subjective_calls %>% 
  left_join(subjective_calls_agg) %>% 
  left_join(
    subjective_calls %>% 
      select(season, team, side, n_sign) %>% 
      pivot_wider(names_from = side, values_from = n_sign)
  )
subjective_calls_viz

f_segment <- partial(
  geom_segment,
  size = 1.25,
  arrow = arrow(length = unit(5, 'pt'), type = 'closed'),
  show.legend = FALSE,
  ... = 
)

subjective_calls_viz %>% 
  ggplot() +
  aes(y = grp, x = n_sign) +
  # geom_col(aes(fill = side)) +
  f_segment(
    data = subjective_calls_viz %>% filter(abs(`for`) >= abs(against)) %>% filter(side == 'for'),
    aes(x = 0, xend = `for`, yend = grp, color = side),
    nudge_y = 0.5,
    color = 'green'
  ) +
  f_segment(
    data = subjective_calls_viz %>% filter(abs(`for`) < abs(against)) %>% filter(side == 'against'),
    aes(x = 0, xend = `against`, yend = grp, color = side),
    nudge_y = 0.5,
    color = 'red'
  ) +
  f_segment(
    data = subjective_calls_viz %>% filter(abs(`for`) >= abs(against)) %>% filter(side == 'against'),
    aes(x = `for`, xend = n_net, yend = grp, color = side),
    nudge_y = -0.5,
    color = 'red'
  ) +
  f_segment(
    data = subjective_calls_viz %>% filter(abs(`for`) < abs(against)) %>% filter(side == 'for'),
    aes(x = `against`, xend = n_net, yend = grp, color = side),
    nudge_y = -0.5,
    color = 'green'
  ) +
  tidytext::scale_y_reordered() +
  # scale_fill_manual(values = c('for' = 'green', 'against' = 'red')) +
  facet_wrap(~season, scales = 'free_y')

subjective_calls_agg %>% 
  ggplot() +
  aes(y = grp, x = n_net) +
  geom_col(
    subjective_calls
  ) +
  scale_fill_manual(values = )