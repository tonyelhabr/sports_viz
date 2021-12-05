
library(tidyverse)
library(ffsched) # remotes::install_github('tonyelhabr/ffsched')
library(gt)
library(broom)
library(betareg)

dir_proj <- '45-202122_ff_luck'
path_scores <- file.path(dir_proj, 'ff_scores.rds')
if(!file.exists(path_scores)) {
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
  
  write_rds(scores, path_scores)
}

scores <- read_rds(path_scores)

scores_by_team <- scores %>% 
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
    pf_avg = mean(pf)
  ) %>% 
  ungroup() %>% 
  mutate(
    .resid_rnk = .fitted_rnk - rnk
  ) %>% 
  select(
    season, week, team, pf, pf_avg, rnk, .fitted_rnk, .resid_rnk
  ) %>% 
  arrange(desc(abs(.resid_rnk)))

preds_gt <- preds %>% 
  filter(week >= 5) %>% 
  # arrange(desc(abs(.resid_rnk))) %>% 
  mutate(
    r1 = row_number(.resid_rnk),
    r2 = row_number(desc(.resid_rnk))
  ) %>% 
  mutate(
    grp = case_when(
      r1 <= 10 ~ 'unluckiest',
      r2 <= 10 ~ 'luckiest',
      TRUE ~ NA_character_
    )
  ) %>% 
  filter(!is.na(grp)) %>% 
  left_join(
    tibble(
      person = c('Juan', 'Tony', 'Andrew E.', 'Manny', 'Drake', 'Enrique', 'Stephen'),
      path_img = file.path(dir_proj, sprintf('%s.jpg', c('juan', 'tony', 'andrew', 'blank', 'blank', 'enrique', 'blank'))),
      team = c('SAG BOI', 'Tony El Tigre', 'The Juggernaut', 'I\'m Also Sad', 'Patronizing Pandas', 'Cussy Commandos', 'Rocket Power')
    )
  ) %>% 
  select(-c(r1, r2, team)) %>% 
  relocate(person, path_img, .after = 'week')
preds_gt

.gt_theme_538 <- function(data,...) {
  data %>%
    gt::opt_table_font(
      font = list(
        gt::google_font('Karla'),
        gt::default_fonts()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = 'bottom', color = 'transparent', weight = gt::px(2)
      ),
      locations = gt::cells_column_labels(
        columns = gt::everything()
      )
    )  %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = 'bottom', color = 'black', weight = px(1)
      ),
      locations = gt::cells_row_groups()
    ) %>%
    gt::tab_options(
      column_labels.background.color = 'white',
      heading.border.bottom.style = 'none',
      table.border.top.width = gt::px(3),
      table.border.top.style = 'none', #transparent
      table.border.bottom.style = 'none',
      # column_labels.font.weight = 'normal',
      column_labels.border.top.style = 'none',
      column_labels.border.bottom.width = gt::px(0),
      # column_labels.border.bottom.color = 'black',
      # row_group.border.top.style = 'none',
      # row_group.border.top.color = 'black',
      # row_group.border.bottom.width = px(1),
      # row_group.border.bottom.color = 'white',
      stub.border.color = 'white',
      stub.border.width = gt::px(0),
      data_row.padding = gt::px(1), # px(3),
      source_notes.font.size = 10,  # 12,
      source_notes.border.lr.style = 'none',
      table.font.size = 16,
      heading.align = 'left',
      
      footnotes.font.size = 12,
      footnotes.padding = gt::px(0),
      ...
    ) %>% opt_css(
      "tbody tr:last-child {
    border-bottom: 2px solid #ffffff00;
      }
    
    ",
    add = TRUE
    )
}

make_table <- function(.grp = 'luckiest') {
  preds_gt_filt <- preds_gt %>% 
    dplyr::filter(grp == .grp) %>% 
    select(-grp)
  if(.grp == 'unluckiest') {
    title <- '<b>Destined for the Beer Mile?</b>'
    subtitle <- 'Tony\'s 2020 season and Juan\'s 2021 seasons have been the unluckiest.<br/>This past week was rock bottom for Juan.'
    reverse <- TRUE
    color <- 'red'
  } else {
    title <- '<b>Lucky Mother Fuckers</b>'
    subtitle <- 'Andrew E. has most often been higher in the standings than he deserved to be.'
    reverse <- FALSE
    color <- 'green'
  }
  
  tb <- preds_gt_filt %>% 
    gt::gt() %>%
    gt::cols_label(
      .list =
        list(
          season = 'Season',
          week = 'Week',
          person = 'Player',
          path_img = ' ',
          pf = gt::html('Points For'),
          pf_avg = gt::html('League Avg.<br/>Points For'),
          rnk = gt::html('Actual<br/>Rank'),
          .fitted_rnk = gt::html('Expected<br/>Rank'),
          .resid_rnk = gt::html('<b>Rank<br/>Difference</b>')
        )
    ) %>%
    # https://github.com/rstudio/gt/issues/510
    gt::text_transform(
      locations = gt::cells_body(columns = path_img),
      fn = function(x) map_chr(x, ~{gt::local_image(filename = as.character(.x))})
    ) %>% 
    gt::fmt_number(
      decimals = 0,
      columns = c(pf, pf_avg, rnk)
    ) %>%
    gt::fmt_number(
      decimals = 1,
      columns = c(.fitted_rnk, .resid_rnk)
    ) %>%
    gt::cols_align(
      align = 'right',
      columns = c(pf, pf_avg, rnk, .fitted_rnk, .resid_rnk)
    ) %>%
    gt::tab_footnote(
      gt::md('<i>Weeks 5 through 11 for the 2020 and 2021 fantasy seasons considered.</i>'),
      locations = gt::cells_title('title')
    ) %>%
    gt::tab_footnote(
      gt::md('<i>Expected rank is a function of points for and week.</i>'),
      locations = gt::cells_column_labels(columns = .fitted_rnk)
    ) %>%
    gt::data_color(
      columns = .resid_rnk,
      colors = scales::col_numeric(
        palette = ggsci::rgb_material(color, n = 100, reverse = reverse),
        domain = c(min(preds_gt_filt$.resid_rnk), max(preds_gt_filt$.resid_rnk))
      )
    ) %>% 
    gt::tab_header(
      title = gt::html(title),
      subtitle = gt::html(subtitle)
    ) %>%
    .gt_theme_538()
  gtsave(tb, filename = file.path(dir_proj, sprintf('ff_%s.png', .grp)))
  tb
}
c('unluckiest', 'luckiest') %>% walk(make_table)
