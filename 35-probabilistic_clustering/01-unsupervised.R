
library(tidyverse)
dir_proj <- '35-probabilistic_clustering'
source(file.path(dir_proj, 'helpers.R'))
path_metrics_unsup <- file.path(dir_proj, 'metrics_unsup.rds')
k_max <- 12
k_min <- 2
k_buff <- 2

mets_unsup <- read_rds(path_metrics_sup)
if(FALSE) {
  mets_unsup <-
    crossing(
      n = seq.int(2, 15, by = 1),
      k = seq.int(k_min, k_max),
      f = fs,
      g = gs
    ) %>%
    mutate(metrics = pmap(
      list(n, k, f, g),
      ~ do_clust(
        .n = ..1,
        .k = ..2,
        .f = ..3,
        .g = ..4,
        return = 'summary'
      )
    ))
  mets_unsup
  write_rds(mets_unsup, path_metrics_unsup)
}

mets_unsup_clean <-
  bind_rows(
    mets_unsup %>% 
      filter(g == 'kmeans') %>% 
      mutate(metric = 'wss') %>% 
      hoist(
        metrics,
        'value' = 'totss', # 'tot.withinss'
      ),
    mets_unsup %>% 
      filter(g == 'mclust') %>% 
      mutate(metric = 'bic') %>% 
      hoist(
        metrics,
        'value' = 'BIC'
      )
  ) %>% 
  select(-metrics) %>% 
  mutate(
    grp = sprintf('%s + %s', f, g),
    across(c(k), ordered)
  )
mets_unsup_clean

# mets_unsup %>% 
#   filter(g == 'mclust') %>% 
#   hoist(
#     metrics,
#     'bic' = 'BIC',
#     'll' = 'logLik'
#   ) %>% 
#   select(-metrics) %>% 
#   select(bic, ll) %>% 
#   corrr::correlate()

plot_metric_unsup <- function(.g) {
  # .g <- 'kmeans'
  .g <- 'mclust'
  metric <- switch(
    .g,
    kmeans = 'wss',
    mclust = 'bic'
  )
  path <- file.path(dir_proj, sprintf('viz_%s_%s.png', .g, metric))
  if(.g == 'kmeans') {
    .lab_y <- 'Within Sum-of-Squares (WSS)'
    .lab_caption <- 'Lower is "better".'
  } else if(.g == 'mclust') {
    .lab_y <- 'BIC'
    .lab_caption <- 'Higher is "better".'
  }
  mets_unsup_clean_filt <-
    mets_unsup_clean %>%
    filter(g == .g) 

  viz <-
    mets_unsup_clean_filt %>% 
    ggplot() +
    aes(x = k, y = value, color = grp, alpha = n, group = sprintf('%s, %s', grp, n)) +
    geom_line(size = 1.2) +
    geom_point(size = 2, show.legend = FALSE) +
    ggrepel::geom_label_repel(
      data = mets_unsup_clean_filt %>% filter(k == max(k)),
      hjust = 'left',
      seed = 42,
      family = 'Karla',
      fontface = 'bold',
      direction = 'y',
      segment.colour = 'black',
      nudge_x = 1,
      size = pts(10),
      label.size = NA,
      show.legend = FALSE,
      aes(label = sprintf('%s%s', n, ifelse(n == max(n), '  components', '')))
    ) +
    # scale_y_log10() +
    scale_alpha(range = c(0.25, 1)) +
    guides(
      alpha = FALSE,
      color = guide_legend(
        title = '',
        override.aes = list(size = 5)
      )
    ) +
    scale_color_manual(values = pal) +
    scale_x_discrete(
      breaks = seq.int(k_min - 1, k_max + k_buff),
      labels = c('', as.character(seq.int(k_min, k_max)), rep('', k_buff))
    ) +
    coord_cartesian(ylim = c(NA, NA), xlim = c(k_min - 1, k_max + k_buff)) +
    theme_tony() +
    theme(
      legend.position = 'top',
      legend.text = element_text(size = 14),
      # axis.text.y = element_blank(),
      # axis.ticks.y = element_blank(),
      # panel.grid.major.y = element_blank(),
      plot.title = ggtext::element_markdown(size = 16),
      plot.subtitle = ggtext::element_markdown(size = 12)
    ) +
    facet_wrap(~grp, scales = 'free_y') +
    labs(
      title = 'Unsupervised dimensionality reduction + clustering performance',
      subtitle = 'Loss (within sum of squares) as a function of components (line, transparency) and clusters (x-axis).',
      tag = lab_tag,
      caption = sprintf('%s<br/>Y-axis units are not nessarily meaningful, so they are not shown.', .lab_caption),
      y = .lab_y,
      x = '# of Clusters'
    )
  viz
  ggsave(
    plot = viz,
    filename = path,
    width = 9,
    height = 6,
    type = 'cairo'
  )
  viz
}

viz_kmeans_wss <- plot_metric_unsup('kmeans')
viz_kmeans_wss
viz_mclust_bic <- plot_metric_unsup('mclust')
viz_mclust_bic

