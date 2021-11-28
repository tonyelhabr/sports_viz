
library(tidyverse)
library(ffsched) # remotes::install_github('tonyelhabr/ffsched')
library(gt)
library(broom)

dir_proj <- '45-202122_ff_luck'
path_scores <- file.path(dir_proj, 'ff_scores.rds')
if(!file.exists(path_scores)) {
  scores <- 
    tibble(
      season = c(2020, 2021) %>% as.integer(),
      max_week = c(12, 14)
    ) %>%
    # slice(2) %>% 
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
  
  write_rds(scores, path_scores)
}

scores <- read_rds(path_scores)

scores_by_team <-
  scores %>% 
  filter(week <= 11) %>% 
  select(season, team_id, team, week, pf, pa) %>% 
  mutate(
    pd = pf - pa,
    w = if_else(pf > pa, 1L, 0L),
    l = 1 - w
  )

cumu <- scores_by_team %>% 
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
cumu %>% filter(season == 2021)

den <- max(cumu$rnk)
cumu_prnk <- 
  cumu %>% 
  mutate(
    prnk = (rnk - 1) / (!!den - 1),
    across(
      prnk,
      ~case_when(
        .x == 0 ~ 0.001,
        .x == 1 ~ 0.999,
        TRUE ~ .x
      )
    )
  )
mean(cumu_prnk$prnk)

library(betareg)
fit <- cumu_prnk %>% 
  betareg::betareg(
    formula(prnk ~ pf*week | pf*week),
    data = .
  )
fit
fit %>% broom::tidy()

preds <- fit %>% 
  broom::augment() %>% 
  mutate(
    .fitted_rnk = den * .fitted + 1/den
  ) %>%
  select(.fitted:last_col()) %>% 
  bind_cols(cumu) %>% 
  group_by(season, week) %>% 
  mutate(
    pf_avg = mean(pf),
    .fitted_rnk_rescaled = den * (1/den + (.fitted - min(.fitted)) / (max(.fitted) - min(.fitted)))
  ) %>% 
  ungroup() %>% 
  mutate(
    .resid_rnk = .fitted_rnk - rnk,
    .resid_rnk_rescaled = .fitted_rnk_rescaled - rnk
  ) %>% 
  arrange(desc(abs(.resid_rnk_rescaled)))

preds %>% 
  filter(week >= 5) %>% 
  arrange(desc(.resid_rnk_rescaled)) %>% 
  relocate(.resid_rnk)

preds_gt <- preds %>% 
  filter(week >= 5) %>% 
  arrange(desc(abs(.resid_rnk))) %>% 
  head(10) %>% 
  left_join(
    tibble(
      person = c('juan', 'tony'),
      team = c('SAG BOI', 'Tony El Tigre')
    ) %>% 
      mutate(
        path_img = file.path(dir_proj, sprintf('%s.webp', person))
      )
  ) %>% 
  select(
    season, week, path_img, pf, pf_avg, rnk, .fitted_rnk, .resid_rnk
  )
preds_gt

.gt_theme_538 <- function(data,...) {
  data %>%
    # gt::opt_all_caps()  %>%
    gt::opt_table_font(
      font = list(
        gt::google_font('Karla'),
        gt::default_fonts()
      )
    ) %>%
    # gt::tab_style(
    #   # style = gt::cell_borders(
    #   #   sides = 'bottom', color = 'transparent', weight = gt::px(2)
    #   # ),
    #   locations = gt::cells_body(
    #     columns = TRUE,
    #     # This is a relatively sneaky way of changing the bottom border
    #     # Regardless of data size
    #     rows = nrow(data$`_data`)
    #   )
    # )  %>%
    gt::tab_options(
      column_labels.background.color = 'white',
      table.border.top.width = gt::px(3),
      # table.border.top.color = 'transparent',
      # table.border.bottom.color = 'transparent',
      table.border.bottom.width = gt::px(3),
      column_labels.border.top.width = gt::px(3),
      # column_labels.border.top.color = 'transparent',
      column_labels.border.bottom.width = gt::px(3),
      column_labels.border.bottom.color = 'black',
      data_row.padding = gt::px(1),
      footnotes.font.size = 10,
      footnotes.padding = gt::px(0),
      source_notes.font.size = 10,
      table.font.size = 16,
      heading.align = 'left',
      ...
    )
}

tb_unlucky <- preds_gt %>% 
  gt::gt() %>%
  gt::cols_label(
    .list =
      list(
        season = 'Season',
        week = 'Week',
        path_img = ' ',
        pf = gt::html('Points For'),
        pf_avg = gt::html('League Avg.<br/>Points For'),
        rnk = gt::html('Actual<br/>Rank'),
        .fitted_rnk = gt::html('Predicted<br/>Rank'),
        .resid_rnk = gt::html('Rank<br/>Difference')
      )
  ) %>%
  # https://github.com/rstudio/gt/issues/510
  gt::text_transform(
    locations = gt::cells_body(columns = path_img),
    fn = function(x) map_chr(x, ~{gt::local_image(filename =  as.character(.x))})
  ) %>% 
  gt::fmt_number(
    decimals = 0,
    columns = c(4, 5)
  ) %>%
  gt::fmt_number(
    decimals = 1,
    columns = c(7, 8)
  ) %>%
  gt::cols_align(
    align = 'right',
    columns = C(1:2, 4:8)
  ) %>%
  gt::tab_header(
    title = 'Destined for the Beer Mile?',
    subtitle = gt::html('Tony\'s 2020 season and Juan\'s 2021 seasons have been the unluckiest.<br/>This past week was rock bottom for Juan.')
  ) %>%
  gt::tab_footnote(
    gt::md("<i>Rank is predicted using beta regression using points for and week.</i>"),
    locations = gt::cells_column_labels(columns = .fitted_rnk)
  ) %>%
  gt::data_color(
    columns = .resid_rnk,
    colors = scales::col_numeric(
      # palette = c("white", "yellow", "navyblue"),
      palette = ggsci::rgb_material("cyan", n = 100, reverse = TRUE),
      domain = c(min(preds$.resid_rnk), 0)
    )
  ) %>% 
  # gt::tab_source_note(
  #   gt::md('**Table**: Tony ElHabr')
  # ) %>% 
  .gt_theme_538()
tb_unlucky
gtsave(tb_unlucky, filename = file.path(dir_proj, 'ff_unlucky.png'))
