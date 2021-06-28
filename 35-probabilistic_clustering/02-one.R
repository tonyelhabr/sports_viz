
library(tidyverse)
dir_proj <- '35-probabilistic_clustering'
source(file.path(dir_proj, 'helpers.R'))

# fit one thing ----
dist <- function (x1, x2, y1, y2, power = 2) {
  ((x2 - x1)^2 + (y2 - y1)^2)^power
}

do_one <- function(f) {
  if(f == 'pca') {
    .nudge_x <- 0
    .nudge_y <- -10
  } else if (f == 'umap') {
    .nudge_x <- 1.5
    .nudge_y <- 1
  }
  
  lab_f <- toupper(f)
  k <- 6
  n <- 2
  fit <- do_clust(.n = n, .k = k, .f = f, .g = 'mclust', return = 'fit')
  nms_comp <- sprintf('comp_%d', 1:n)
  centers <-
    fit$parameters$mean %>% 
    as_tibble(rownames = 'component') %>% 
    pivot_longer(-component, names_to = '.class') %>% 
    pivot_wider(names_from = component, values_from = value) %>% 
    mutate(across(.class, ~str_remove(.x, 'V') %>% as.integer())) %>% 
    set_names(c('.class', nms_comp))
  centers
  
  preds_init <- predict(fit)
  class <-
    tibble(.class = preds_init$classification) %>% 
    bind_cols(df_filt %>% select(pos))
  
  cors <-
    class %>% 
    fastDummies::dummy_cols(c('.class', 'pos'), remove_selected_columns = TRUE) %>% 
    corrr::correlate(method = 'spearman', quiet = TRUE) %>% 
    filter(term %>% str_detect('pos')) %>% 
    select(term, matches('^[.]class'))
  
  cols_idx <- 2:(k+1)
  cors_mat <- as.matrix(cors[,cols_idx]) + 1
  rownames(cors_mat) <- cors$term
  cols <- names(cors)[cols_idx]
  colnames(cors_mat) <- cols
  cols_idx_min <- clue::solve_LSAP(cors_mat, maximum = TRUE)
  cols_min <- cols[cols_idx_min]
  pairs <-
    tibble::tibble(
      .class = cols_min %>% str_remove('^[.]class_') %>% as.integer(),
      pos = cors$term %>% str_remove('pos_')
    )
  pairs
  
  centers_clean <-
    centers %>% 
    left_join(pairs)
  centers_clean
  
  uncertainty <- 
    bind_cols(
      fit$data %>% as_tibble() %>% set_names(nms_comp), 
      .class = fit$classification,
      uncertainty = fit$uncertainty
    ) %>% 
    left_join(pairs %>% rename(pos_pred = pos)) %>% 
    bind_cols(df_filt %>% select(where(is.character))) %>% 
    mutate(wrong = pos != pos_pred)
  uncertainty_filt <- uncertainty # %>% filter(pos != 'G')
  
  lims <-
    uncertainty_filt %>% 
    summarize(
      across(all_of(nms_comp), list(min = min, max = max))
    ) %>% 
    pivot_longer(
      matches('.*')
    )
  lims
  
  # uncertainty %>% 
  #   # filter(wrong) %>% 
  #   filter(pos == 'DM') %>% 
  #   filter(comp == 'eng Premier League')
  # uncertainty %>% 
  #   filter(player %>% str_detect('Semedo'))
  
  most_wrong <-
    uncertainty_filt %>% 
    filter(wrong) %>% 
    # filter(comp == 'eng Premier League') %>% 
    filter(
      (pos == 'F' & pos_pred == 'AM') |
        (pos == 'M' & pos_pred == 'AM')
    ) %>% 
    group_by(pos, pos_pred) %>% 
    slice_max(uncertainty, with_ties = FALSE) %>% 
    ungroup()
  most_wrong
  
  add_clust_constants <- function(...) {
    list(
      ...,
      theme_tony(),
      theme(
        plot.subtitle = ggtext::element_markdown(size = 12),
        panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'none'
      ),
      coord_cartesian(
        xlim = c(lims$value[1] * 1.1, lims$value[2] * 1.1), 
        ylim = c(lims$value[3] * 1.1, lims$value[4] * 1.1)
      ),
      labs(
        title = sprintf('GMM clusters after 2-component %s', lab_f),
        tag = lab_tag,
        caption = 'Keepers (G) excluded.', #  because they are fairly distant and distort the plot.',
        y = 'UMAP Component 2',
        x = 'UMAP Component 1'
      )
    )
  }

  viz_uncertainty_init <-
    uncertainty_filt %>% 
    ggpubr::ggscatter(
      x = 'comp_1',
      y = 'comp_2',
      color = 'pos_pred',
      shape = 'pos_pred',
      size = 0,
      point = FALSE,
      mean.point = TRUE,
      ellipse = TRUE,
      ellipse.type = 'norm',
      ellipse.alpha = 0, # 0.2,
      ellipse.level = 0.4
    ) +
    geom_point(
      aes(size = uncertainty, color = pos_pred)
    ) + 
    scale_size(range = c(0.5, 2.5)) + 
    ggrepel::geom_text_repel(
      data = centers_clean, #  %>% filter(pos != 'G'),
      hjust = 'left',
      seed = 42,
      family = 'Karla',
      fontface = 'bold',
      # direction = 'y',
      segment.colour = 'black',
      nudge_x = .nudge_x,
      nudge_y = .nudge_y,
      # gravity = 10,
      force = 20,
      size = pts(24),
      # label.size = NA,
      aes(x = comp_1, y = comp_2, label = pos, color = pos)
    ) +
    scale_color_manual(values = pal_clusts) +
    add_clust_constants() +
    labs(
      subtitle = 'Probabilistic classification reflects ambiguity between clusters.'
    )
  viz_uncertainty_init
  
  if(f == 'umap') {
    viz_uncertainty_init <-
      viz_uncertainty_init +
      ggtext::geom_richtext(
        data = tibble(),
        label.color = NA,
        hjust = 0,
        family = 'Karla',
        size = pts(14),
        aes(x = -3, y = 3, label = 'Larger point size = more uncertainty.<br/>Ellipsoids illustrate cluster covariances.')
      )
  }
  
  if(f == 'umap') {
    f_text <-
      partial(
        ggrepel::geom_text_repel,
        # hjust = 'left',
        seed = 42,
        family = 'Karla',
        fontface = 'bold',
        # direction = 'y',
        segment.colour = 'black',
        size = pts(14),
        # label.size = NA,
        aes(x = comp_1, y = comp_2, label = sprintf('%s (%s, %s) classified as %s', player, pos, squad, pos_pred)),
        ... = 
      )
    
    
    viz_uncertainty <-
      viz_uncertainty_init +
      geom_point(
        data = most_wrong,
        size = 3,
        color = 'black'
      ) +
      # f_text(
      #   data = most_wrong %>% filter(pos == 'DM'),
      #   nudge_y = 1,
      #   nudge_x = 1.5
      # ) +
      f_text(
        data = most_wrong %>% filter(pos == 'F'),
        nudge_y = 3,
        nudge_x = 0
      ) +
      f_text(
        data = most_wrong %>% filter(pos == 'M'),
        nudge_y = -1,
        nudge_x = 2
      )
  } else {
    viz_uncertainty <- viz_uncertainty_init
  }
  
  ggsave(
    plot = viz_uncertainty,
    filename = file.path(dir_proj, sprintf('viz_uncertainty_%s.png', f)),
    width = 10,
    height = 10,
    type = 'cairo'
  )
  
  # uncertainty %>% 
  #   filter(abs(comp_1) <= 1) %>% 
  #   filter(comp_2 %>% between(-2, 0)) %>% 
  #   arrange(desc(uncertainty)) %>% 
  #   filter(comp == 'eng Premier League')
  
  .player <- 'Jadon Sancho'
  uncertainty_scaled <- 
    uncertainty_filt %>% 
    recipes::recipe(formula(~.), data = .) %>% 
    recipes::step_normalize(recipes::all_numeric_predictors()) %>% 
    recipes::prep() %>% 
    recipes::juice()
  uncertainty_scaled <- 
    uncertainty_filt %>% 
    mutate(
      across(matches('^comp_'), ~(.x - min(.x)) / (max(.x) - min(.x)))
    )
  uncertainty_scaled
  player1 <-
    uncertainty_scaled %>% 
    filter(player == .player)
  
  sim <-
    player1 %>% 
    select(comp_1, comp_2, pos) %>% 
    bind_cols(
      uncertainty_scaled %>% 
        filter(player != .player) %>% 
        select(comp_1_other = comp_1, comp_2_other = comp_2, player, squad, comp, pos_other = pos)
    ) %>% 
    filter(pos == pos_other) %>% 
    mutate(
      d = dist(comp_1, comp_1_other, comp_2, comp_2_other)
    ) %>% 
    arrange(d) %>% 
    mutate(
      score = (max(d) - d) / (max(d) - 0)
    )
  sim
  sim %>% arrange(score)
  
  viz_wrong <-
    uncertainty %>% 
    ggplot() +
    aes(x = comp_1, y = comp_2) +
    geom_point(
      data = . %>% filter(!wrong),
      alpha = 0.5,
      aes(color = wrong)
    ) +
    geom_point(
      data = . %>% filter(wrong),
      alpha = 0.5,
      aes(color = wrong)
    ) +
    scale_color_manual(values = c(`TRUE` = 'red', `FALSE` = 'grey20')) +
    add_clust_constants() +
    labs(
      caption = ' ',
      subtitle = 'Mis-classified positions'
    )
  viz_wrong
  
  ggsave(
    plot = viz_wrong,
    filename = file.path(dir_proj, sprintf('viz_misclassified_%s.png', f)),
    width = 10,
    height = 10,
    type = 'cairo'
  )
  
  list(
    viz_uncertainty = viz_uncertainty,
    viz_wrong = viz_wrong,
    sim = sim
  )
}

res_umap <- do_one('umap')
res_pca <- do_one('pca')


