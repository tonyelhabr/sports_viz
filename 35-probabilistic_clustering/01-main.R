
library(tidyverse)
library(tidymodels)
dir_proj <- '35-probabilistic_clustering'
dir_data <- file.path(dir_proj, 'data')
fs::dir_create(dir_data)

theme_tony <- function() {
  font <- 'Karla'
  gray_light <- 'gray80'
  gray_mid <- 'gray50'
  gray_dark <- 'gray20'
  ggplot2::theme_minimal() + #  %+replace% 
    ggplot2::theme(
      text = ggplot2::element_text(family = font),
      title = ggplot2::element_text(font, size = 14, color = gray_dark),
      plot.title = ggplot2::element_text(font, face = 'bold', size = 18, color = gray_dark),
      plot.title.position = 'plot',
      plot.subtitle = ggplot2::element_text(font, face = 'bold', size = 14, color = gray_dark),
      axis.text = ggplot2::element_text(font, size = 14),
      axis.title = ggplot2::element_text(size = 14, face = 'bold', hjust = 0.99),
      axis.line = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = gray_light),
      panel.grid.minor = ggplot2::element_line(color = gray_light),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      plot.background = ggplot2::element_rect(fill = 'white', color = NA),
      plot.caption = ggplot2::element_text(font, size = 12, color = gray_dark, hjust = 1),
      plot.caption.position = 'plot',
      plot.tag = ggplot2::element_text(font, size = 12, color = gray_dark, hjust = 0),
      plot.tag.position = c(.01, 0.02),
      
      legend.text = ggplot2::element_text(size = 14),
      strip.text = ggplot2::element_text(color = gray_dark, size = 14),
      strip.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = 'white', color = NA)
    )
}
theme_set_tony <- function() {
  extrafont::loadfonts('win', quiet = TRUE)
  font <- 'Karla'
  ggplot2::theme_set(theme_tony())
  ggplot2::update_geom_defaults('text', list(family = font, size = 4))
}

df <- 
  file.path(dir_proj, 'FBRef 2020-21 T5 League Data.xlsx') %>% 
  readxl::read_excel() %>% 
  janitor::clean_names() %>% 
  mutate(
    across(where(is.integer), ~replace_na(.x, 0L)),
    across(where(is.double), ~replace_na(.x, 0)),
    across(
      pos,
      ~case_when(
        .x %in% c('DF,MF', 'MF,DF') ~ 'DM',
        .x %in% c('DF,FW', 'FW,DF') ~ 'M',
        .x %in% c('MF,FW', 'FW,MF') ~ 'AM',
        .x == 'DF' ~ 'D',
        .x == 'MF' ~ 'M',
        .x == 'FW' ~ 'F',
        .x == 'GK' ~ 'G',
        TRUE ~ .x
      )
    )
  )
df
# df %>% skimr::skim(min)
df_filt <- df %>% filter(min > 10 * 90)
df_filt %>% count(pos, sort = TRUE)
df_filt %>% 
  filter(pos == 'W') %>% 
  select(player, squad, comp, min) %>% 
  arrange(desc(min))

rec_init <-
  recipes::recipe(formula(~.), data = df_filt) %>% 
  update_role(all_nominal_predictors(), new_role = 'id') %>% 
  step_normalize(all_numeric_predictors())
rec_init

# df %>% visdat::vis_miss()
path_summary <- file.path(dir_proj, 'summary.rds')
path_metrics <- file.path(dir_proj, 'metrics.rds')
set.seed(42)

require(mclust)
fs <- c('pca', 'umap')
gs <- c('kmeans', 'mclust')
do_clust <- function(.n, .k, .f, .g, return = c('metrics', 'data', 'summary', 'clusters', 'assignments', 'fit'), overwrite = FALSE, ...) {
  # .f <- match.arg(.f)
  # .g <- match.arg(.g)
  return <- match.arg(return)
  path <- file.path(dir_data, sprintf('%s-%s-%s-n=%s-k=%s.rds', return, .n, .k, .f, .g))
  suffix <- glue::glue('for `.n = {.n}`, `.k = {.k}`, `.f = {.f}`, `.g = {.g}`')
  if(file.exists(path) & !overwrite) {
    cat(glue::glue('{Sys.time()}: Returning early {suffix}'), sep = '\n')
    return(read_rds(path))
  }
  f <- if(.f == 'pca') {
    step_pca
  } else if (.f == 'umap') {
    embed::step_umap
  }
  g <- if(.g == 'kmeans') {
    kmeans
  } else if (.g == 'mclust') {
    mclust::Mclust
  }
  cat(glue::glue('{Sys.time()}: Processing {suffix}'), sep = '\n')
  rec <-
    rec_init %>% 
    f(all_numeric_predictors(), num_comp = .n)
  
  jui <- rec %>% prep() %>% juice()
  jui_num <- jui %>% select(where(is.numeric))
  if(return == 'data') {
    res <- jui_num
  } else {
    fit <- g(jui_num, .k, ...)
    if(return == 'summary') {
      res <- broom::glance(fit)
    } else if (return == 'clusters') {
      res <- broom::tidy(fit)
    } else if (return == 'assignments') {
      res <- broom::augment(fit)
    } else if (return == 'fit') {
      res <- fit
    } else if (return == 'metrics') {
      
      
      if(.g == 'kmeans') {
        class <- 
          tibble(.class = fit$cluster) %>% 
          bind_cols(df_filt %>% select(pos))
        class  
      } else if (.g == 'mclust') {
        
        preds_init <- predict(fit)
        
        class <-
          tibble(.class = preds_init$classification) %>% 
          bind_cols(df_filt %>% select(pos))
      }
      cors <-
        class %>% 
        fastDummies::dummy_cols(c('.class', 'pos'), remove_selected_columns = TRUE) %>% 
        corrr::correlate(method = 'spearman') %>% 
        filter(term %>% str_detect('pos')) %>% 
        select(term, matches('^[.]class'))
      
      cols_idx <- 2:(.k+1)
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
      
      cmat_init <-
        class %>% 
        left_join(
          pairs %>% select(.class, pos_pred = pos)
        ) %>% 
        select(-.class) %>% 
        count(pos, pos_pred) %>% 
        mutate(across(matches('pos'), factor)) %>% 
        pivot_wider(names_from = 'pos_pred', values_from = 'n')
      cmat_init
      order <- cmat_init$pos
      cmat_init <- cmat_init %>% select(all_of(order)) 
      cmat <- cmat_init %>% as.matrix()
      .sum <- partial(sum, na.rm = TRUE, ... =)
      acc <- .sum(diag(cmat)) / .sum(cmat)
      
      if(.g == 'kmeans') {
        res <- list(acc = acc, ll = NULL, cmat = cmat)
      } else if(.g == 'mclust') {
        probs_init <-
          preds_init$z %>% 
          as_tibble() %>% 
          set_names(sprintf('.class_%d', 1:.k)) %>% 
          bind_cols(df_filt %>% select(pos)) %>% 
          mutate(idx = row_number()) %>% 
          relocate(idx)
        probs_init
        
        probs <-
          probs_init %>% 
          pivot_longer(
            -c(idx, pos),
            names_to = '.class'
          ) %>% 
          mutate(across(.class, ~str_remove(.x, '^[.]class_') %>% as.integer())) %>% 
          left_join(
            pairs %>% 
              select(.class, pos_pred = pos)
          ) %>% 
          select(-.class) %>% 
          pivot_wider(names_from = pos_pred, values_from = value) %>% 
          select(-idx)
        probs
        
        # MLmetrics::MultiLogLoss(
        #   y_true = probs$pos %>% factor(), 
        #   y_pred = probs %>% select(2:last_col()) %>% as.matrix()
        # )  
        
        ll <-
          probs %>% 
          mutate(across(pos, factor)) %>% 
          yardstick::mn_log_loss(truth = pos, 2:last_col()) %>% 
          pull(.estimate)
        res <- list(acc = acc, ll = ll, cmat = cmat)
      }
    }
  }
  write_rds(res, path)
  res
}

# mets <- read_rds(path_metrics)
mets <-
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
write_rds(mets, path_metrics)
mets

mets_clean <-
  mets %>% 
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
mets_clean

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

lab_tag <- '**Viz**: Tony ElHabr'
pal <- c('pca + kmeans'  = '#ef426f', 'pca + mclust' = '#00b2a9', 'umap + kmeans' = '#ff8200', 'umap + mclust' = '#7a5195')

plot_metric <- function(metric = c('acc', 'll')) {
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
    mets_clean %>% 
    ggplot() +
    aes(x = n, y = !!col_sym, color = grp, group = grp) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    ggrepel::geom_label_repel(
      data = mets_clean %>% drop_na(!!col_sym) %>% filter(n == max(n)),
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
viz_acc <- plot_metric('acc')
viz_ll <- plot_metric('ll')


# summ <- read_rds(path_summary)
summ <-
  crossing(
    n = seq.int(2, 15, by = 1),
    k = seq.int(2, 12),
    f = fs,
    g = gs
  ) %>%
  mutate(summary = pmap(
    list(n, k, f, g),
    ~ do_clust(
      .n = ..1,
      .k = ..2,
      .f = ..3,
      .g = ..4,
      return = 'summary'
    )
  ))
summ
write_rds(summ, path_summary)

summ %>% 
  filter(g == 'kmeans') %>% 
  unnest(summary) %>% 
  mutate(across(c(k, n), ordered)) %>% 
  ggplot() +
  aes(x = k, y = tot.withinss) +
  geom_line(aes(color = n, group = n), show.legend = TRUE) +
  facet_wrap(~f, scales = 'free_x')

summ %>% 
  filter(g == 'mclust') %>% 
  unnest(summary) %>% 
  mutate(across(c(k, n), ordered)) %>% 
  ggplot() +
  aes(x = k, y = BIC) +
  geom_line(aes(color = n, group = n), show.legend = TRUE) +
  facet_wrap(~f, scales = 'free_x')

n <- 2
k <- 6
jui <- do_clust(.n = n, .k = k, .f = 'umap', .g = 'mclust', return = 'data')
fit <- do_clust(.n = n, .k = k, .f = 'umap', .g = 'mclust', return = 'fit')
?mclust:::summary.Mclust
summary(fit, parameters = TRUE)
plot(fit, what = 'BIC')
plot(fit, what = 'classification', addEllipses = TRUE)
plot(fit, what = 'uncertainty')
plot(fit, what = 'density')
pal_clusts <- c('#003f5c', '#444e86', '#955196', '#dd5182', '#ff6e54', '#ffa600')
# ggtheme argument doesn't work
centers <-
  fit$parameters$mean %>% 
  as_tibble(rownames = 'component') %>% 
  pivot_longer(-component) %>% 
  pivot_wider(names_from = component, values_from = value)
centers
viz_clusts <- factoextra::fviz_mclust(fit, 'uncertainty', legend = 'none', palette = pal_clusts)
viz_clusts + 
  theme_tony() +
  theme(
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = 'none'
  ) +
  scale_size(range = c(0.1, 2)) +
  # guides(color = FALSE, size = FALSE, alpha = FALSE) +
  labs(
    y = 'UMAP Component 2',
    x = 'UMAP Component 1'
  )

mclust:::plot.Mclust

jui
preds_init <- predict(fit)
preds_init

# probs ----
probs <-
  preds_init$z %>% 
  as_tibble() %>% 
  set_names(sprintf('.class_%d', 1:k)) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  bind_cols(tibble(.class = preds_init$classification)) %>% 
  bind_cols(df_filt) %>% 
  mutate(idx = row_number()) %>% 
  relocate(idx)
probs

probs_uncertain <-
  probs %>% 
  select(idx, player:comp, matches('^[.]class')) %>% 
  pivot_longer(
    matches('^[.]class_')
  ) %>% 
  filter(value > 0 & value < 1) %>% 
  mutate(mid = abs(0.5 - value)) %>% 
  arrange(mid)
probs_uncertain
preds %>% 
  count(.class, pos) %>% 
  pivot_wider(names_from = .class, values_from = n)

cl <- fit %>% broom::tidy()
as <- fit %>% broom::augment()
summary(fit, parameters = TRUE)
mclust:::summary.Mclust
cl
as
df %>% count(pos)
