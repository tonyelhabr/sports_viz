
library(tidyverse)
library(tidymodels)
dir_proj <- '35-probabilistic_clustering'
dir_data <- file.path(dir_proj, 'data')
fs::dir_create(dir_data)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 18, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown('Karla', face = 'bold', size = 16, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  plot.caption = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 1),
  # plot.caption = element_text('Karla', size = 12, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.01),
  legend.text = element_text(color = 'gray20', size = 12),
  # strip.text = element_text(color = 'gray20', size = 14),
  # strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

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
set.seed(42)

# require(mclust)
fs <- c('pca', 'umap')
gs <- c('kmeans', 'mclust')
do_clust <- function(.n, .k, .f, .g, return = c('summary', 'clusters', 'assignments', 'fit'), overwrite = FALSE, ...) {
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
  clusts <- g(jui_num, .k, ...)
  if(return == 'summary') {
    res <- broom::glance(clusts)
  } else if (return == 'clusters') {
    res <- broom::tidy(clusts)
  }  else if (return == 'assignments') {
    res <- broom::augment(clusts)
  } else if (return == 'fit') {
    res <- clusts
  }
  write_rds(res, path)
  res
}

if(!file.exists(path_summary)) {
  summ <-
    crossing(
      n = seq.int(2, 10, by = 1),
      k = seq.int(1, 10, by = 1),
      f = fs,
      g = gs
    ) %>% 
    mutate(summary = pmap(list(n, k, f, g), ~do_clust(.n = ..1, .k = ..2, .f = ..3, .g = ..4, overwrite = TRUE)))
  write_rds(summ, path_summary)
} else {
  summ <- read_rds(path_summary)
}
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


rec <-
  rec_init %>% 
  embed::step_umap(all_numeric_predictors(), num_comp = 10)

n <- 10
k <- 6
fit <- do_clust(.n = n, .k = k, .f = 'umap', .g = 'mclust', return = 'fit')
jui <- fit$data %>% as_tibble()
preds_init <- predict(fit)

class <-
  tibble(.class = preds_init$classification) %>% 
  bind_cols(df_filt %>% select(pos))

cors <-
  class %>% 
  fastDummies::dummy_cols(c('.class', 'pos'), remove_selected_columns = TRUE) %>% 
  corrr::correlate(method = 'spearman') %>% 
  filter(term %>% str_detect('pos')) %>% 
  select(term, matches('^[.]class'))
cors
cols_idx <- 2:(k+1)
cors_mat <- as.matrix(cors[,cols_idx]) + 1
cors_mat
rownames(cors_mat) <- cors$term
cols <- names(cors)[cols_idx]
colnames(cors_mat) <- cols
cols_idx_min <- clue::solve_LSAP(cors_mat, maximum = TRUE)
cols_min <- cols[cols_idx_min]
pairs <-
  tibble::tibble(
    .class = cors$term,
    pos = cols_min
  )
pairs

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

df_filt %>% filter(pos == 'DF,FW') %>% select(player, squad, comp)
df_filt %>% filter(pos == 'FW,DF') %>% select(player, squad, comp)
cl <- fit %>% broom::tidy()
as <- fit %>% broom::augment()
summary(fit, parameters = TRUE)
mclust:::summary.Mclust
cl
as



df %>% count(pos)


viz_totwss <-
  summ %>% 
  unnest(summary) %>% 
  mutate(across(c(k, n), ordered)) %>% 
  ggplot() +
  aes(x = k, y = tot.withinss) +
  geom_line(aes(color = n, group = n), show.legend = TRUE)
viz_totwss

viz_totwss <-
  summ %>% 
  unnest(summary) %>% 
  mutate(grp = ordered(n)) %>% 
  ggplot() +
  aes(x = k, y = logLik) +
  geom_line(aes(color = grp, group = grp), show.legend = TRUE)
viz_totwss
