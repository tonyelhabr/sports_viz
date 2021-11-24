
library(tidyverse)
library(ffsched) # remotes::install_github('tonyelhabr/ffsched')
library(gt)
library(broom)

dir_proj <- '45-202122_ff_luck'
# ffsched::scrape_espn_ff_scores(league_id = 899513, season = 2020, export = FALSE)
scores <- 
  tibble(
    season = c(2020, 2021) %>% as.integer(),
    max_week = c(12, 14)
  ) %>%
  mutate(
    data = map2(
      season, max_week, 
      ~ffsched::scrape_espn_ff_scores(
        league_id = 899513, 
        season = ..1,
        weeks = ..2, 
        export = FALSE
      )
    )
  ) %>% 
  select(season, data) %>% 
  unnest(data)
scores
write_rds(scores, file.path(dir_proj, 'ff_scores.rds'))
scores

scores_by_team <-
  scores %>% 
  filter(week <= 11) %>% 
  select(season, team_id, team, week, pf, pa) %>% 
  mutate(
    pd = pf - pa,
    w = if_else(pf > pa, 1L, 0L),
    l = 1 - w
  )

agg <- scores_by_team %>% 
  arrange(season, team_id, week) %>% 
  group_by(season, team_id, team) %>% 
  mutate(
    across(c(pf, pa, pd,  w, l), cumsum)
  ) %>% 
  ungroup() %>% 
  group_by(season, week) %>% 
  mutate(
    rnk1 = min_rank(-w)
  ) %>% 
  group_by(season, week, rnk1) %>% 
  mutate(
    rnk2 = row_number(-pf) - 1
  ) %>% 
  ungroup() %>% 
  mutate(
    rnk = rnk1 + rnk2
  ) %>% 
  select(-c(rnk1, rnk2)) %>% 
  arrange(desc(pa))
agg %>% filter(season == 2021)

# agg %>% 
#   ggplot() +
#   aes(x = pd, y = rnk) +
#   geom_point() +
#   geom_smooth(method = 'lm')
fit <- agg %>% 
  lm(
    formula(pd ~ pf + I(week*pf) + I((week*pf)^2)), data = .
  )
fit %>% broom::tidy()

preds <- fit %>% 
  broom::augment() %>% 
  select(.fitted:last_col()) %>% 
  bind_cols(agg) %>% 
  group_by(season, week) %>% 
  mutate(
    pf_rnk = row_number(-pf)
  ) %>% 
  ungroup() %>% 
  mutate(diff_rnk = rnk - pf_rnk) %>% 
  # arrange(desc(abs(diff_rnk)))
  arrange(desc(abs(.std.resid)))
preds %>% filter(week >= 4) %>% head(20)

den <- max(agg$rnk)
fit2 <- agg %>% 
  mutate(prnk = (rnk - 1 + 0.1) / !!den) %>% 
  # betareg::betareg(
  #   formula(prnk ~ pd + I(week*pd) + I((week*pd)^2)), data = .
  # )
  betareg::betareg(
    formula(prnk ~ pd + I(week*pd) + I((week*pd)^2)), data = .
  )
fit2 %>% broom::tidy()
fit2 %>% 
  broom::augment() %>% 
  mutate(
    .fitted_rnk = den * .fitted +  + 1 - 0.1
  ) %>% 
  select(.fitted:last_col()) %>% 
  bind_cols(agg) %>% 
  mutate(
    .resid_rnk = .fitted_rnk - rnk
  ) %>% 
  arrange(desc(abs(.resid_rnk))) %>% 
  # arrange(-.fitted_rnk) %>% 
  filter(week >= 4)

library(gt)


