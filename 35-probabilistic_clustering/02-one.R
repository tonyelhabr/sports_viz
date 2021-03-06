
library(tidyverse)
dir_proj <- '35-probabilistic_clustering'
source(file.path(dir_proj, 'helpers.R'))

# fit one thing ----
dist <- function (x1, x2, y1, y2, power = 2) {
  ((x2 - x1)^2 + (y2 - y1)^2)^power
}

do_one <- function(f) {
  # f <- 'umap'
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
  lab_caption <- sprintf('Components (%s): %d, Clusters (GMM): %d', lab_f, n, k)
  
  res <- do_uncertainty(.n = n, .k = k, .f = f, .g = 'gmm')
  centers <- res$centers
  
  require(yardstick) # for autoplot
  cm <-
    uncertainty %>% 
    mutate(across(matches('pos'), factor)) %>% 
    yardstick::conf_mat(pos, pos_pred)
  viz_cm_init <- autoplot(cm, type = 'heatmap')
  
  do_theme_set()
  viz_cm <-
    viz_cm_init +
    theme_tony() + # still need to do this for caption
    guides(fill = FALSE) +
    theme(
      panel.grid.major = element_blank()
    ) +
    labs(
      title = 'Confusion Matrix',
      tag = lab_tag,
      caption = lab_caption
    )
  viz_cm
  
  ggsave(
    plot = viz_cm,
    filename = file.path(dir_proj, sprintf('viz_cm_%s.png', f)),
    width = 6,
    height = 6,
    type = 'cairo'
  )
  
  uncertainty <-
    res$uncertainty %>% 
    mutate(wrong = pos != pos_pred)

  .pos_drop <- c('G', 'D')
  uncertainty_filt <- 
    uncertainty %>% 
    filter(!(pos_pred %in% .pos_drop))
  
  nms_comp <- sprintf('comp_%d', 1:n)
  lims <-
    uncertainty_filt %>% 
    summarize(
      across(all_of(nms_comp), list(min = min, max = max))
    ) %>% 
    pivot_longer(
      matches('.*')
    )
  lims
  
  most_wrong <-
    uncertainty_filt %>% 
    filter(wrong) %>% 
    filter(
      (pos == 'F' & pos_pred == 'AM') |
        (pos == 'M' & pos_pred == 'AM')
    ) %>% 
    group_by(pos, pos_pred) %>% 
    slice_max(.uncertainty, with_ties = FALSE) %>% 
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
      labs(
        title = sprintf('GMM clusters after 2-component %s', lab_f),
        tag = lab_tag,
        caption = lab_caption,
        y = sprintf('%s Component 2', lab_f),
        x = sprintf('%s Component 1', lab_f)
      )
    )
  }
  
  f_label <- partial(
    ggrepel::geom_text_repel,
    hjust = 'left',
    seed = 42,
    family = 'Karla',
    fontface = 'bold',
    segment.colour = 'grey20',
    force = 20,
    size = pts(24),
    aes(x = comp_1, y = comp_2, label = pos, color = pos),
    ... =
  )
  
  plot_uncertainty <- function(data) {
    data %>% 
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
        ellipse.level = 0.9
      ) +
      geom_point(
        aes(size = .uncertainty, color = pos_pred)
      ) + 
      scale_size(range = c(0.5, 2.5)) + 
      scale_color_manual(values = pal_clusts) +
      add_clust_constants() +
      labs(
        subtitle = 'Probabilistic classification reflects ambiguity between clusters.'
      )
  }
  viz_uncertainty_full <- uncertainty %>% plot_uncertainty()
  viz_uncertainty_full
  
  lab_ellipse <- 'Larger point size = more uncertainty.<br/>(Ellipsoids illustrate cluster covariance).'
  if(f == 'umap') {
    
    .pos_attack <- c('F', 'AM')
    viz_uncertainty_full <-
      viz_uncertainty_full +
      f_label(
        nudge_x = 1.5,
        nudge_y = 1,
        data = centers %>% filter(!(pos %in% .pos_attack))
      ) +
      f_label(
        nudge_x = -1,
        nudge_y = -3,
        data = centers %>% filter((pos %in% .pos_attack))
      ) +
      ggtext::geom_richtext(
        data = tibble(),
        label.color = NA,
        hjust = 0,
        family = 'Karla',
        size = pts(14),
        aes(x = -6, y = -15, label = lab_ellipse)
      )
    viz_uncertainty_full

    viz_uncertainty_filt_init <-
      uncertainty_filt %>% 
      plot_uncertainty() +
      coord_cartesian(
        xlim = c(lims$value[1] * 1.1, lims$value[2] * 1.1), 
        ylim = c(lims$value[3] * 1.1, lims$value[4] * 1.1)
      ) +
      labs(
        caption = sprintf('%s<br/>Keepers (G) and defenders (D) excluded.', lab_caption)
      )
    
    f_text <-
      partial(
        ggrepel::geom_text_repel,
        # hjust = 'left',
        seed = 42,
        family = 'Karla',
        fontface = 'bold',
        # direction = 'y',
        segment.colour = 'grey20',
        colour = 'grey20',
        size = pts(14),
        # label.size = NA,
        aes(x = comp_1, y = comp_2, label = sprintf('%s (%s, %s) classified as %s', player, pos, squad, pos_pred)),
        ... = 
      )
    
    viz_uncertainty_filt <-
      viz_uncertainty_filt_init +
      geom_point(
        data = most_wrong,
        size = 3,
        color = 'grey20'
      ) +
      # f_text(
      #   data = most_wrong %>% filter(pos == 'DM'),
      #   nudge_y = 1,
      #   nudge_x = 1.5
      # ) +
      f_text(
        data = most_wrong %>% filter(pos == 'F'),
        nudge_y = 1.5,
        nudge_x = 3
      ) +
      f_text(
        data = most_wrong %>% filter(pos == 'M'),
        nudge_y = -1,
        nudge_x = 2
      )
    viz_uncertainty_filt
    
    ggsave(
      plot = viz_uncertainty_filt,
      filename = file.path(dir_proj, sprintf('viz_uncertainty_%s_filt.png', f)),
      width = 8,
      height = 8,
      type = 'cairo'
    )
  } else if (f == 'pca') {
      
    viz_uncertainty_full <-
      viz_uncertainty_full +
      f_label(
        nudge_x = 8,
        nudge_y = 10,
        data = centers %>% filter(!(pos %in% 'G'))
      ) +
      f_label(
        nudge_x = 1,
        nudge_y = 2,
        data = centers %>% filter((pos %in% 'G'))
      ) +
      ggtext::geom_richtext(
        data = tibble(),
        label.color = NA,
        hjust = 1,
        family = 'Karla',
        size = pts(14),
        aes(x = 25, y = -15, label = lab_ellipse)
      )
    viz_uncertainty_full
  }
  
  ggsave(
    plot = viz_uncertainty_full,
    filename = file.path(dir_proj, sprintf('viz_uncertainty_%s_full.png', f)),
    width = 8,
    height = 8,
    type = 'cairo'
  )
  
  viz_wrong <-
    uncertainty_filt %>% 
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
      caption = sprintf('%s<br/>Keepers (G) and defenders (D) excluded.', lab_caption),
      subtitle = '<span style="color:red">Red</span>: mis-classified points'
    )
  viz_wrong
  
  ggsave(
    plot = viz_wrong,
    filename = file.path(dir_proj, sprintf('viz_misclassified_%s.png', f)),
    width = 8,
    height = 8,
    type = 'cairo'
  )
}

# do_one('umap')
do_one('pca')


