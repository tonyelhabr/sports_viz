
library(tidyverse)
dir_proj <- '35-probabilistic_clustering'
source(file.path(dir_proj, 'helpers.R'))
path_metrics_sup <- file.path(dir_proj, 'metrics_sup.rds')

mets_sup <- read_rds(path_metrics_sup)
if(FALSE) {
  mets_sup <-
    crossing(
      n = seq.int(2, 15, by = 1),
      f = fs,
      g = gs
    ) %>%
    mutate(metrics = pmap(
      list(n, f, g),
      ~ do_clust(
        .n = ..1,
        .f = ..2,
        .g = ..3,
        .k = 6,
        return = 'metrics'
      )
    ))
  write_rds(mets_sup, path_metrics_sup)
}
mets_sup

mets_sup_clean <-
  mets_sup %>% 
  hoist(
    metrics,
    'acc' = 'acc',
    'll' = 'll'
  ) %>% 
  select(-metrics) %>% 
  mutate(
    grp = sprintf('%s + %s', f, g),
    across(c(n), ordered)
  )
mets_sup_clean

# evaluate supervised ----
plot_metric_sup <- function(metric = c('acc', 'll')) {
  metric <- match.arg(metric)
  col_sym <- sym(metric)
  path <- file.path(dir_proj, sprintf('viz_%s.png', metric))
  if(metric == 'acc') {
    .ylim = c(0, 1)
    .lab_y <- 'Multi-class Accuracy'
  } else if(metric == 'll') {
    .ylim <- c(-NA, NA)
    .lab_y <- 'Log Loss'
  }
  # browser()
  viz <-
    mets_sup_clean %>% 
    ggplot() +
    aes(x = n, y = !!col_sym, color = grp, group = grp) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    ggrepel::geom_label_repel(
      data = mets_sup_clean %>% drop_na(!!col_sym) %>% filter(n == max(n)),
      hjust = 'left',
      seed = 42,
      family = 'Karla',
      fontface = 'bold',
      direction = 'y',
      segment.colour = 'black',
      nudge_x = 1,
      size = pts(14),
      label.size = NA,
      aes(label = grp)
    ) +
    guides(color = FALSE) +
    scale_color_manual(values = pal) +
    scale_x_discrete(
      breaks = seq.int(1, 17),
      labels = c('', as.character(seq.int(2, 15)), rep('', 2))
    ) +
    coord_cartesian(ylim = .ylim, xlim = c(1, 17)) +
    theme_tony() +
    theme(
      plot.title = ggtext::element_markdown(size = 16),
      plot.subtitle = ggtext::element_markdown(size = 14)
    ) +
    labs(
      title = 'Which dimensionality reduction + clustering combo is best?',
      subtitle = 'Prediction for 6 classes (soccer positions)',
      caption = 'Positions: G, D, DM, M, AM, F',
      tag = lab_tag,
      y = .lab_y,
      x = '# of Components'
    )
  if(metric == 'acc') {
    viz <- viz + scale_y_continuous(labels = scales::percent)
  }
  
  ggsave(
    plot = viz,
    filename = path,
    width = 9,
    height = 6,
    type = 'cairo'
  )
  viz
}


viz_acc <- plot_metric_sup('acc')
viz_acc
viz_ll <- plot_metric_sup('ll')
viz_ll