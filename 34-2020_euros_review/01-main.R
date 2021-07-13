
library(tidyverse)
dir_proj <- '34-2020_euros_review'

results <-
  file.path('32-2020_euros', 'results.csv') %>% 
  read_csv(
    col_types = cols(
      .default = col_integer(),
      country = col_character()
    )
  ) %>%
  set_names(c('country', 'start', 'group', 'r16', 'qf', 'sf', 'finals')) %>% 
  select(-c(start, group)) %>% 
  mutate(
    champ = ifelse(country == 'Italy', 1L, NA_integer_)
  ) %>% 
  pivot_longer(
    -country,
    names_to = 'round',
    values_to = 'result',
  ) %>% 
  mutate(across(result, ~replace_na(.x, 0L))) %>% 
  group_by(round) %>% 
  mutate(n_pos = sum(result)) %>% 
  ungroup() %>% 
  mutate(p = n_pos / 24)
results

f_read <- function(x, ...) {
  file.path(dir_proj, sprintf('%s.csv', x)) %>% 
    read_csv(...) %>% 
    janitor::clean_names()
}
df_analyst <- f_read('the-analyst')
df_analyst

df_benz <- 
  f_read('sim_results') %>% 
  select(country = team, r16:last_col()) %>% 
  pivot_longer(
    -country,
    names_to = 'round'
  )
df_benz

df_ku <- 
  f_read('kuleuven') %>% 
  select(country = team, r16:champ) %>% 
  rename(finals = final) %>% 
  mutate(
    across(country, str_trim),
    across(country, ~case_when(.x == 'Czechia' ~ 'Czech Republic', TRUE ~ .x)),
    across(-c(country), ~parse_number(.x) / 100)
  ) %>% 
  pivot_longer(
    -country,
    names_to = 'round'
  )
df_analyst %>% count(round)

df_ze <-
  f_read('zeileis') %>% 
  pivot_longer(
    -country,
    names_to = 'round'
  ) %>% 
  mutate(across(value, ~.x / 100))
df_ze

f_rename <- function(df, suffix = str_remove(deparse(substitute(df)), '^df_')) {
  df %>% rename(!!sym(sprintf('value_%s', suffix)) := value)
}
df_analyst %>% distinct(country) %>% anti_join(df_ze %>% distinct(country), .)

df_wide <-
  list(
    f_rename(df_analyst),
    f_rename(df_benz),
    f_rename(df_ku),
    f_rename(df_ze)
  ) %>% 
  reduce(inner_join)

df <-
  df_wide %>% 
  pivot_longer(
    matches('value'),
  ) %>% 
  mutate(across(name, ~str_remove(.x, 'value_')))
df

df %>% 
  inner_join(results) %>% 
  select(-c(n_pos, p)) %>% 
  write_csv(file.path(dir_proj, 'preds.csv'), na = '')

agg_baseline <-
  results %>% 
  group_by(round) %>% 
  summarize(
    baseline = mean((result - p)^2)
  ) %>% 
  ungroup() 
agg_baseline

agg_init <-
  df %>% 
  inner_join(results) %>% 
  group_by(name, round) %>% 
  summarize(
    score = mean((result - value)^2)
  ) %>% 
  ungroup() %>% 
  left_join(agg_baseline)
agg_init

lvls <- c('r16', 'qf', 'sf', 'finals', 'champ')
agg <-
  agg_init %>% 
  mutate(score = 1 - (score / baseline)) %>% 
  group_by(round) %>% 
  mutate(rnk = row_number(desc(score))) %>% 
  ungroup() %>% 
  mutate(across(round, ~ordered(.x, levels = lvls))) %>% 
  arrange(round)
agg

lvl_labs <-
  tibble(
    round = lvls,
    lab = c('R16', 'Quarters', 'Semis', 'Title', 'Champ') %>% ordered()
  )
lvl_labs

nms <- c('Luke Benz', 'Achim Zeileis', 'KU Leuven', 'The Analyst')
name_labs <-
  tibble(
    name = c('benz', 'ze', 'ku', 'analyst'),
    lab = nms
  ) %>% 
  mutate(
    rnk = row_number(),
    path_img = file.path(dir_proj, sprintf('%s.png', name))
  )
name_labs

agg_wide <-
  agg %>% 
  select(-baseline) %>% 
  pivot_wider(
    names_from = round,
    values_from = c(score, rnk)
  ) %>% 
  # select(name, matches('_r16$'), matches('_qf$'), matches('_sf$'), matches('_finals$'), matches('_champ$')) %>% 
  select(
    name,
    rnk_r16,
    score_r16,
    rnk_qf,
    score_qf,
    rnk_sf,
    score_sf,
    rnk_finals,
    score_finals,
    rnk_champ,
    score_champ
  ) %>% 
  left_join(name_labs) %>% 
  arrange(rnk) %>% 
  relocate(rnk, path_img, lab)
agg_wide

.gt_theme_538 <- function(data,...) {
  data %>%
    gt::opt_all_caps()  %>%
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
      locations = gt::cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    gt::tab_options(
      column_labels.background.color = 'white',
      footnotes.padding = gt::px(0),
      table.border.top.width = gt::px(3),
      table.border.top.color = 'transparent',
      table.border.bottom.color = 'black',
      table.border.bottom.width = gt::px(3),
      column_labels.border.top.width = gt::px(3),
      column_labels.border.top.color = 'transparent',
      column_labels.border.bottom.width = gt::px(3),
      column_labels.border.bottom.color = 'black',
      data_row.padding = gt::px(3),
      footnotes.font.size = 10,
      source_notes.font.size = 10,
      table.font.size = 16,
      heading.align = 'left',
      ...
    ) 
}

add_color_at <- function(table, r, pal) {
  col <- sprintf('rnk_%s', r)
  table %>% 
    gt::data_color(
      columns = all_of(col),
      colors = scales::col_numeric(palette = ggsci::rgb_material(pal, n = 100), domain = range(agg_wide[[col]]), reverse = TRUE)
    )
}

add_spanner_at <- function(table, r, lab) {
  col <- sprintf('rnk_%s', r)
  table %>% 
    gt::tab_spanner(
      label = lab,
      columns = one_of(sprintf('%s_%s', c('rnk', 'score'), r))
    )
}

img <- 
  fs::dir_ls(file.path('32-2020_euros', 'flags')) %>% 
  tibble(path_img = .) %>% 
  mutate(country = path_img %>% basename() %>% tools::file_path_sans_ext())
img

df_champ_wide <-
  df %>% 
  filter(round == 'champ') %>% 
  select(-round) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  arrange(desc(analyst + benz + ku + ze)) %>% 
  # mutate(across(-country, ~sprintf('%.1f%%', 100 * .x))) %>% 
  left_join(img) %>% 
  select(country, path_img, benz, ze, ku, analyst)
df_champ_wide

tb_champ <-
  df_champ_wide %>% 
  head(10) %>% 
  gt::gt() %>% 
  gt::cols_label(
    .list = 
      list(
        country = ' ',
        path_img = ' ',
        benz = nms[1],
        ze = nms[2],
        ku = nms[3],
        analyst = nms[4]
      )
  ) %>% 
  gt::text_transform(
    locations = gt::cells_body(columns = path_img),
    fn = function(x) map_chr(x, ~{gt::local_image(filename =  as.character(.x))})
  ) %>%
  .gt_theme_538() %>% 
  gt::tab_header(
    title = gt::md('**Who Ya Got?**'),
    subtitle = 'Pre-Tournament 2020 EURO Title Chances'
  ) %>% 
  gt::cols_align(
    align = 'right',
    columns = 3:6
  ) %>% 
  gt::fmt_percent(
    columns = 3:6,
    decimals = 1
  ) %>% 
  gt::cols_width(
    matches('benz|ze|ku|analyst') ~ gt::px(50)
  ) %>% 
  gt::data_color(
    columns = 3:6,
    colors = scales::col_numeric(palette = c("white", "#3fc1c9"), domain = NULL)
  ) %>% 
  gt::tab_style(
    style = list(
      gt::cell_borders(
        sides = c('t', 'b'),
        color = 'black',
        weight = gt::px(1)
      )
    ),
    locations = list(
      gt::cells_body(
        # columns = everything(),
        rows = 4
      )
    )
  )
tb_champ
gt::gtsave(tb_champ, file.path(dir_proj, 'tb_champ.png'))

tb <-
  agg_wide %>% 
  # mutate(across(matches('^score_'), ~sprintf('(%+.1f%%)', 100 * .x))) %>% 
  mutate(across(matches('^score_'), ~sprintf('%+.1f%%', 100 * .x))) %>% 
  select(-name) %>% 
  gt::gt() %>% 
  gt::cols_label(
    .list = 
      list(
        rnk = ' ',
        path_img = ' ',
        lab = ' ',
        rnk_r16 = ' ',
        score_r16 = ' ',
        rnk_qf = ' ',
        score_qf = ' ',
        rnk_sf = ' ',
        score_sf = ' ',
        rnk_finals = ' ',
        score_finals = ' ',
        rnk_champ = ' ',
        score_champ = ' '
      )
  ) %>% 
  add_spanner_at('r16', 'R16') %>% 
  add_spanner_at('qf', 'Quarters') %>% 
  add_spanner_at('sf', 'Semis') %>% 
  add_spanner_at('finals', 'Finals') %>% 
  add_spanner_at('champ', 'Champ') %>% 
  # https://github.com/rstudio/gt/issues/510
  gt::text_transform(
    locations = gt::cells_body(columns = path_img),
    fn = function(x) map_chr(x, ~{gt::local_image(filename =  as.character(.x))})
  ) %>% 
  gt::cols_align(
    align = 'right',
    # columns = matches('^rnk')
    columns = matches('^(rnk|score)')
  ) %>%
  gt::cols_align(
    align = 'left',
    # columns = matches('^score_|lab')
    columns = all_of('lab')
  ) %>%
  gt::cols_align(
    align = 'center',
    columns = matches('path_img')
  ) %>%
  gt::tab_style(
    style = gt::cell_text(size = gt::px(11)),
    locations = gt::cells_body(
      columns = matches('^score_')
    )
  ) %>%
  # gt::cols_merge(
  #   columns = c(rnk_r16, score_r16),
  #   pattern = '{1} <span style="size:10pt">{2}</span>'
  # ) %>% 
  gt::tab_style(
    style = list(
      gt::cell_borders(
        sides = 'left',
        color = 'black',
        weight = gt::px(3)
      )
    ),
    locations = list(
      gt::cells_body(
        columns = matches('^rnk_')
      )
    )
  ) %>% 
  add_color_at('r16', 'light-green') %>%
  add_color_at('qf', 'purple') %>% 
  add_color_at('sf', 'cyan') %>% 
  add_color_at('finals', 'pink') %>% 
  add_color_at('champ', 'yellow') %>% 
  .gt_theme_538() %>% 
  gt::tab_header(
    title = gt::md('**Analyzing the Analysts**'),
    subtitle = 'Mean Brier Skill Score for Pre-Tournament 2020 EURO Predictions'
  ) %>% 
  gt::tab_footnote(
    gt::md('Brier skill score is a measure of accuracy for probabilistic predictions. **Higher** brier score is better.<br/>Max score is 100%. A skill score <0% means that the predictions are worse than picking at random.'),
    locations = gt::cells_title(groups = 'subtitle')
  ) %>% 
  gt::tab_footnote('https://arxiv.org/abs/2106.05799', locations = gt::cells_body(columns = 'lab', rows = 1)) %>% # https://twitter.com/AchimZeileis
  gt::tab_footnote('https://github.com/lbenz730/euro_cup_2021', locations = gt::cells_body(columns = 'lab', rows = 2)) %>% # https://twitter.com/recspecs730
  gt::tab_footnote('https://theanalyst.com/eu/2021/06/euro-2020-predictions/', locations = gt::cells_body(columns = 'lab', rows = 3)) %>% # https://twitter.com/OptaAnalyst
  gt::tab_footnote('https://dtai.cs.kuleuven.be/sports/blog', locations = gt::cells_body(columns = 'lab', rows = 4)) %>% # https://twitter.com/KU_Leuven
  # This no work! https://github.com/rstudio/gt/issues/648
  gt::tab_style(
    locations = gt::cells_column_spanners(),
    style = gt::cell_borders(sides = 'all', color = '#fff', weight = gt::px(0))
  ) %>%
  gt::cols_width(
    matches('^rnk_|^score') ~ gt::px(50)
  ) %>%
  gt::tab_style(
    locations = gt::cells_column_labels(),
    style = list(
      'padding-bottom: 0px; padding-top: 0px; padding-left: 0px; padding-right: 0px'
    )
  )
tb
gt::gtsave(tb, file.path(dir_proj, 'tb_compare.png'))

results %>% filter(round == 'champ')
df %>% 
  filter(round == 'champ') %>% 
  filter(country == 'Italy')

