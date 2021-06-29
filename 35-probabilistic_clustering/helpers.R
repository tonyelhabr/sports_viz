

library(tidyverse)

dir_proj <- '35-probabilistic_clustering'
dir_data <- file.path(dir_proj, 'data')
fs::dir_create(dir_data)

theme_tony <- function() {
  font <- 'Karla'
  gray_light <- 'gray80'
  gray_mid <- 'gray50'
  gray_dark <- 'gray20'
  theme_minimal() +
    theme(
      text = element_text(family = font),
      title = element_text(font, size = 14, color = gray_dark),
      plot.title = ggtext::element_markdown(font, face = 'bold', size = 18, color = gray_dark),
      plot.title.position = 'plot',
      plot.subtitle = element_text(font, face = 'bold', size = 14, color = gray_mid),
      axis.text = element_text(font, size = 14),
      axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
      axis.line = element_blank(),
      panel.grid.major = element_line(color = gray_light),
      panel.grid.minor = element_line(color = gray_light),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.margin = margin(10, 10, 10, 10),
      plot.background = element_rect(fill = 'white', color = NA),
      plot.caption = ggtext::element_markdown(font, size = 12, color = gray_dark, hjust = 1),
      plot.caption.position = 'plot',
      plot.tag = ggtext::element_markdown(font, size = 12, color = gray_dark, hjust = 0),
      plot.tag.position = c(.01, 0.02),
      legend.text = element_text(size = 14),
      strip.text = element_text(color = gray_dark, size = 14),
      strip.background = element_blank(),
      panel.background = element_rect(fill = 'white', color = NA)
    )
}
theme_set_tony <- function() {
  extrafont::loadfonts('win', quiet = TRUE)
  font <- 'Karla'
  theme_set(theme_tony())
  update_geom_defaults('text', list(family = font, size = 4))
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
options(width = 90)
df_filt %>% dim()

# df %>% skimr::skim(min)
df_filt <- df %>% filter(min > 10 * 90)
df_filt
df_filt %>% count(pos, sort = TRUE)
df_filt %>% 
  filter(pos == 'W') %>% 
  select(player, squad, comp, min) %>% 
  arrange(desc(min))

rec_init <-
  recipes::recipe(formula(~.), data = df_filt) %>% 
  recipes::update_role(recipes::all_nominal_predictors(), new_role = 'id') %>% 
  recipes::step_normalize(recipes::all_numeric_predictors())
rec_init

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
  f <- ifelse(.f == 'pca', recipes::step_pca, embed::step_umap)
  g <- ifelse(.g == 'kmeans', kmeans, mclust::Mclust)
  # f <- if(.f == 'pca') {
  #   recipes::step_pca
  # } else if (.f == 'umap') {
  #   embed::step_umap
  # }
  # g <- if(.g == 'kmeans') {
  #   kmeans
  # } else if (.g == 'mclust') {
  #   mclust::Mclust
  # }
  cat(glue::glue('{Sys.time()}: Processing {suffix}'), sep = '\n')
  rec <-
    rec_init %>% 
    f(recipes::all_numeric_predictors(), num_comp = .n)
  
  jui <- rec %>% recipes::prep() %>% recipes::juice()
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
        corrr::correlate(method = 'spearman', quiet = TRUE) %>% 
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
      class_clean <-
        class %>% 
        left_join(
          pairs %>% select(.class, pos_pred = pos)
        ) %>% 
        select(-.class)
      
      cmat_init <-
        class_clean %>% 
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
          arrange(idx) %>% 
          select(-idx) %>% 
          bind_cols(class_clean %>% select(pos_pred)) %>% 
          relocate(pos, pos_pred)
        probs
        
        # MLmetrics::MultiLogLoss(
        #   y_true = probs$pos %>% factor(), 
        #   y_pred = probs %>% select(2:last_col()) %>% as.matrix()
        # )  
        
        ll <-
          probs %>% 
          select(-pos_pred) %>% 
          mutate(across(pos, factor)) %>% 
          yardstick::mn_log_loss(truth = pos, 2:last_col()) %>% 
          pull(.estimate)
        res <- list(acc = acc, ll = ll, cmat = cmat, probs = probs)
      }
    }
  }
  write_rds(res, path)
  res
}

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

lab_tag <- '**Viz**: Tony ElHabr'
pal <- c('pca + kmeans'  = '#ef426f', 'pca + mclust' = '#00b2a9', 'umap + kmeans' = '#ff8200', 'umap + mclust' = '#7a5195')
pal_clusts <-c('D' = '#003f5c', 'DM' = '#58508d', 'M' = '#bc5090', 'AM' = '#ff6361', 'F' = '#ffa600', 'G' = 'grey50')

do_uncertainty <- function(.n, .k, ...) {
  fit <- do_clust(.n = .n, .k = .k, return = 'fit', ...)

  nms_comp <- sprintf('comp_%d', 1:.n)
  as <-
    fit %>% 
    broom::augment() %>% 
    mutate(
      across(.class, as.integer)
    ) %>% 
    set_names(c(nms_comp, '.class', 'uncertainty')) %>% 
    bind_cols(df_filt %>% select(pos))
  
  cors <-
    as %>% 
    select(-matches('^comp_')) %>% 
    fastDummies::dummy_cols(c('.class', 'pos'), remove_selected_columns = TRUE) %>% 
    corrr::correlate(method = 'spearman', quiet = TRUE) %>% 
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
  
  uncertainty <- 
    as %>% 
    left_join(pairs %>% rename(pos_pred = pos)) %>% 
    bind_cols(df_filt %>% select(where(is.character), -pos))
  
  cl <- broom::tidy(fit)
  centers <-
    cl %>% 
    select(component, matches('^mean[.]')) %>% 
    set_names(c('.class', nms_comp))
  centers
  
  centers_clean <-
    centers %>% 
    left_join(pairs)
  centers_clean
  list(uncertainty = uncertainty, centers = centers_clean)
}
