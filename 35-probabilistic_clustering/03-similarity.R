
library(tidyverse)
dir_proj <- '35-probabilistic_clustering'
source(file.path(dir_proj, 'helpers.R'))

do_similarity <- function(.player = 'Jadon Sancho', .n = 2, .k = 6, .f = 'umap', .g = 'gmm', .pos = c('F', 'AM', 'M')) {
  res <- do_uncertainty(.n = .n, .k = .k, .f = .f, .g = .g)
  df_tidy <-
    res$uncertainty %>% 
    select(where(is.character), matches('^comp_')) %>% 
    pivot_longer(matches('^comp_')) %>% 
    mutate(across(name, ~str_remove(.x, '^comp_') %>% as.integer()))
  df_tidy
  
  .rename <- function(data, suffix) {
    data %>% 
      select(-pos) %>% 
      rename_with(~sprintf('%s_%s', .x, suffix)) %>% 
      mutate(dummy = 0)
  }
  
  df_tidy_filt <- df_tidy %>% filter(player == .player)
  df_tidy_filt
  
  sims_init <-
    full_join(
      df_tidy_filt %>% .rename(1), 
      df_tidy %>% filter(pos %in% .pos) %>% .rename(2)
    ) %>% 
    filter(name_1 == name_2) %>% 
    filter(player_1 != player_2)
  sims_init
  

  
  sims <-
    sims_init %>% 
    group_by_if(is.character) %>% 
    summarize(
      d = sqrt(sum((value_1 - value_2)^2))
    ) %>% 
    ungroup() %>% 
    group_by_at(vars(matches('_1'))) %>% 
    mutate(score = 1 - ((d - 0) / (max(d) - 0))) %>% 
    ungroup()
  sims
  
  # # For blog
  # sims_init %>% 
  #   select(matches('^player'), matches('^name'), matches('^value')) %>% 
  #   rename(comp_1 = name_1, comp_2 = name_2) %>% 
  #   arrange(player_1, player_2, comp_1)
  # sims <-
  #   sims_init %>% 
  #   group_by(player_1, player_2) %>% 
  #   summarize(
  #     d = sqrt(sum((value_1 - value_2)^2))
  #   ) %>% 
  #   ungroup() %>% 
  #   mutate(score = 1 - ((d - 0) / (max(d) - 0))) %>% 
  #   mutate(rnk = row_number(desc(score))) %>% 
  #   arrange(rnk) %>% 
  #   select(player = player_2, d, score, rnk)
  # sims
  
  sims_clean <-
    sims %>% 
    # filter(player_1 == .player) %>% 
    arrange(d) %>% 
    mutate(rnk = row_number(desc(score))) %>% 
    select(-matches('_1')) %>% 
    rename_all(~str_remove(.x, '_2$'))
  sims_clean
}

scores <-
  crossing(
    .f = fs,
    .n = c(2, 6, 12)
  ) %>% 
  # slice(3) %>% 
  mutate(
    scores = 
      map2(
        .f, .n, 
        ~do_similarity(
          .player = 'Lionel Messi', .f = ..1, .n = ..2
        )
      )
  ) %>% 
  unnest(scores)
scores


# players_fbref <-
#   c(
#     'Lorenzo Insigne',
#     'Lionel Messi',
#     'Ousmane DembÃ©lÃ©',
#     'Kingsley Coman',
#     'Jack Grealish',
#     'Neymar',
#     'Ãngel Di MarÃ­a',
#     'Adnan Januzaj',
#     'Riyad Mahrez',
#     'Hakan ÃalhanoÄlu'
#   )

players_fbref <-
c("Kylian Mbappé", "Karim Benzema", "Cristiano Ronaldo", "Memphis Depay", "Ciro Immobile", "Paulo Dybala", "Robert Lewandowski", "Kelechi Iheanacho", "Luis Muriel", "Joaquín Correa")

# Reference: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset4=theme-code3
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
      table.border.top.width = gt::px(3),
      table.border.top.color = 'transparent',
      table.border.bottom.color = 'transparent',
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

scores_filt <-
  scores %>% 
  filter(.n == 2) %>% 
  # filter(player %in% players_fbref) %>% 
  left_join(players_fbref %>% tibble(player = .) %>% mutate(rnk_fbref = row_number())) %>% 
  drop_na(rnk_fbref) %>% 
  select(.f, player, rnk_fbref, rnk) %>% 
  pivot_wider(names_from = .f, values_from = c(rnk)) %>% 
  rename_with(~sprintf('rnk_%s', .x), c(pca, umap)) %>% 
  arrange(rnk_fbref)
scores_filt

scores %>% 
  filter(.n == 2) %>% 
  group_by(.f) %>% 
  slice_head(n = 10) %>% 
  ungroup() %>% 
  select(.f, player, rnk) %>% 
  pivot_wider(names_from = .f, values_from = c(player)) %>% 
  rename_with(~sprintf('player_%s', .x), c(pca, umap)) %>% 
  arrange(rnk) %>% 
  gt::gt() %>% 
  gt::cols_label(
    .list = 
      list(
        rnk = 'Rank',
        player_pca = gt::html('PCA<br/>Player'),
        player_umap = gt::html('UMAP<br/>Plauer')
      )
  ) %>% 
  gt::cols_align(
    align = 'right',
    columns = vars(rnk)
  ) %>%
  .gt_theme_538() %>% 
  gt::tab_header(
    title = '2-Component Similarity Rankings',
    subtitle = 'Similarity to Lionel Messi'
  ) -> gt_scores_filt_2
gt::gtsave(gt_scores_filt_2, file.path(dir_proj, 'gt_similarity_lionel_messi_wo_fbref.png'))

gt_scores_filt <-
  scores_filt %>% 
  gt::gt() %>% 
  gt::cols_label(
    .list = 
      list(
        player = 'Player',
        rnk_fbref = gt::html('<b>FBREF<br/>Rank</b>'),
        rnk_pca = gt::html('PCA<br/>Rank'),
        rnk_umap = gt::html('UMAP<br/>Rank')
      )
  ) %>% 
  gt::cols_align(
    align = 'right',
    columns = vars(rnk_fbref, rnk_pca, rnk_umap)
  ) %>%
  .gt_theme_538() %>% 
  # gt::tab_footnote(
  #   locations = gt::cells_column_labels(columns = vars(player)),
  #   footnote = gt::md('Hakan ÃalhanoÄlu (fbref\'s 10th most similar player) is not shown.')
  # ) %>% 
  # gt::tab_footnote(
  #   locations = gt::cells_column_labels(columns = vars(rnk_umap)),
  #   footnote = gt::md('Alexis SÃ¡nchez is ranked #1 most similar by UMAP.')
  # ) %>% 
  gt::tab_header(
    title = '2-Component Similarity Rankings',
    subtitle = 'Similarity to Lionel Messi'
  ) # %>% 
  # gt::tab_source_note(
  #   gt::md('<br/>**Table theme** (538 style): @thomas_mock')
  # )
gt_scores_filt
gt::gtsave(gt_scores_filt, file.path(dir_proj, 'gt_similarity_lionel_messi.png'))

# scores_filt %>% 
#   ggplot() +
#   aes(x = ordered(.n), y = score, color = .f) +
#   geom_point()

