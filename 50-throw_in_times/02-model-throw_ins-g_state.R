
library(tidyverse)
library(arrow)
library(xgboost)

dir_proj <- '50-throw_in_times'
throw_ins <- file.path(dir_proj, 'throw_ins.parquet') %>%
  arrow::read_parquet() %>% 
  filter(is_after_sub == 0L)

x_mat <- throw_ins %>% 
  select(minute, period_id, g_state) %>% 
  as.matrix()

fit <- xgboost::xgboost(
  data = xgboost::xgb.DMatrix(
    x_mat,
    weight = throw_ins$wt,
    label = throw_ins$time_diff
  ),
  # objective = 'count:poisson',
  objective = 'reg:squarederror',
  # params = list(monotone_constraints  = '(0, 0, 0)')
  nrounds = 1000,
  early_stopping_rounds = 50,
  print_every_n = 100
)

newdata_grid <- crossing(
  minute = 1L:50L,
  period_id = 2L,
  g_state = 1L:3L
)

g_states <- c(
  '1' = 'Behind',
  '2' = 'Tied',
  '3' = 'Ahead'
)

preds <- predict(
  fit,
  newdata = newdata_grid %>% as.matrix()
) %>% 
  tibble(.pred = .) %>% 
  bind_cols(newdata_grid) %>% 
  mutate(
    across(
      g_state,
      ~factor(
        unname(g_states[as.character(.x)]),
        levels  = unname(g_states)
      )
    ),
    across(
      minute,
      list(
        lab = ~ifelse(period_id == 2L, 45L + .x, .x)
      )
    )
  )

arrow::write_parquet(preds, file.path(dir_proj, 'preds-throw_ins-g_state.parquet'))


library(DALEXtra)

set.seed(42)
do_pdp <- function(df) {
  x_mat <- as.matrix(df)
  explainer <- explain_xgboost(
    fit,
    data = x_mat,
    y = throw_ins$time_diff,
    colorize = FALSE
  )

  pdp <- model_profile(
    explainer,
    variables = 'minute'
  )

  clean_pdp <- pdp %>%
    pluck('agr_profiles') %>%
    as_tibble() %>%
    select(
      minute = `_x_`,
      .pred = `_yhat_`
    )
  clean_pdp %>% 
    mutate(
      period_id = df$period_id[1],
      g_state = df$g_state[1],
      .before = 1
    )
}

pdps <- throw_ins %>% 
  select(minute, period_id, g_state) %>% 
  filter(period_id == 2) %>% 
  group_split(period_id, g_state) %>% 
  map_dfr(do_pdp) %>% 
  mutate(
    across(
      g_state,
      ~factor(
        unname(g_states[as.character(.x)]),
        levels  = unname(g_states)
      )
    ),
    across(
      minute,
      list(
        lab = ~ifelse(period_id == 2L, 45L + .x, .x)
      )
    )
  )

gam_preds_90 <- pdps %>% 
  group_nest(g_state) %>% 
  transmute(
    g_state,
    gam_pred_90 = map_dbl(
      data, 
      ~mgcv::gam(
        formula(.pred ~ s(minute_lab, bs = 'cs')),
        data = .x
      ) %>% 
        predict(
          newdata = tibble(minute_lab = 90)
        )
    )
  )

pdps %>% 
  ggplot() +
  aes(
    x = minute_lab,
    y = .pred,
    color = g_state
  ) +
  geom_vline(
    linetype = '11',
    size = 1,
    color = 'white',
    aes(
      xintercept = 90
    )
  ) +
  geom_smooth(
    size = 1.5,
    method = 'gam',
    formula = formula(y ~ s(x, bs = 'cs')),
    se = FALSE
  ) +
  ggtext::geom_richtext(
    fill = NA_character_,
    label.color = NA_character_,
    family = 'Karla',
    hjust = 0,
    size = pts(20),
    data = gam_preds_90 %>% filter(g_state != 'Ahead'),
    aes(
      x = 92,
      y = gam_pred_90 + 0.2,
      label = sprintf('**%s: %.1f**', g_state, gam_pred_90)
    )
  ) +
  ggtext::geom_richtext(
    fill = NA_character_,
    label.color = NA_character_,
    family = 'Karla',
    hjust = 0,
    size = pts(20),
    data = gam_preds_90 %>% filter(g_state == 'Ahead'),
    aes(
      x = 92,
      y = gam_pred_90 - 0.2,
      label = sprintf('**%s: %.1f**', g_state, gam_pred_90)
    )
  ) +
  guides(
    color = 'none'
  ) +
  scale_color_manual(
    values = pal
  ) +
  scale_x_continuous(
    limits = c(45, 100),
    breaks = c(45, 70, 90)
  ) +
  theme(
    panel.grid.major.x = element_blank()
  )
