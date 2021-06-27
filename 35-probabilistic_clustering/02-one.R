
library(tidyverse)
dir_proj <- '35-probabilistic_clustering'
source(file.path(dir_proj, 'helpers.R'))

# fit one thing ----
k <- 6
do_clust_final <- partial(do_clust, .n = 2, .k = k, 'umap', .g = 'mclust', ... = )
jui <- do_clust_final(return = 'data')
fit <- do_clust_final(return = 'fit')
metrics_sup <- do_clust_final(return = 'metrics', overwrite = TRUE)

# ggtheme argument doesn't work
centers <-
  fit$parameters$mean %>% 
  as_tibble(rownames = 'component') %>% 
  pivot_longer(-component, names_to = '.class') %>% 
  pivot_wider(names_from = component, values_from = value) %>% 
  mutate(across(.class, ~str_remove(.x, 'V') %>% as.integer()))
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

lims <-
  bind_cols(
    fit$data %>% as_tibble(),
    fit$classification %>% tibble(.class = .)
  ) %>% 
  left_join(pairs) %>% 
  filter(pos != 'G') %>% 
  summarize(
    across(matches('^umap'), list(min = min, max = max))
  ) %>% 
  pivot_longer(
    matches('.*')
  )
lims

# debugonce(factoextra::fviz_cluster)
# debugonce(ggpubr::ggscatter)
# debugonce(factoextra:::.add_outliers)
# debugonce(factoextra::fviz_mclust)
# viz_clusts_init <- 
#   factoextra::fviz_mclust(
#     fit, 
#     what = 'uncertainty',
#     legend = 'none', 
#     palette = unname(pal_clusts), 
#     stand = FALSE
#   )
# viz_clusts_init

uncertainty <- 
  bind_cols(
    fit$data %>% as_tibble(), 
    .class = fit$classification,
    uncertainty = fit$uncertainty
  ) %>% 
  # mutate(name = row_number())
  left_join(pairs)

viz_clusts <-
  ggpubr::ggscatter(
    uncertainty,
    'umap_1',
    'umap_2',
    color = 'pos',
    shape = 'pos',
    size = 0,
    point = FALSE,
    # label = NULL,
    # font.label = 12,
    # repel = FALSE,
    mean.point = TRUE,
    ellipse = TRUE,
    ellipse.type = 'norm',
    ellipse.alpha = 0, # 0.2,
    ellipse.level = 0.4
  ) +
  geom_point(
    aes(size = uncertainty, color = pos)
  ) + 
  scale_size(range = c(0.1, 3)) + 
  ggrepel::geom_text_repel(
    data = centers_clean %>% filter(pos != 'G'),
    hjust = 'left',
    seed = 42,
    family = 'Karla',
    fontface = 'bold',
    direction = 'y',
    segment.colour = 'black',
    nudge_x = 1.5,
    # nudge_y = 1,
    # gravity = 10,
    size = pts(24),
    # label.size = NA,
    aes(x = umap_1, y = umap_2, label = pos, color = pos)
  ) +
  theme_tony() +
  scale_color_manual(values = pal_clusts) +
  theme(
    panel.grid.major = element_blank(),
    legend.position = 'none'
  ) +
  coord_cartesian(
    xlim = c(lims$value[1] * 1.1, lims$value[2] * 1.1), 
    ylim = c(lims$value[3] * 1.1, lims$value[4] * 1.1)
  ) +
  labs(
    title = 'GMM clusters after 2-component UMAP',
    subtitle = 'Probabilistic clustering (larger point size = more uncertainty) reflects ambiguity between clusters.',
    caption = 'Keepers (G) excluded.', #  because they are fairly distant and distort the plot.',
    y = 'UMAP Component 2',
    x = 'UMAP Component 1'
  )
viz_clusts

# probs ----
metrics_sup
df_filt %>% 
  filter(pos == 'G', comp == 'eng Premier League') %>% 
  # filter(squad == 'Chelsea') %>% 
  select(player, pos, squad, mp, min) %>% 
  arrange(squad)
metrics_sup$probs

probs_tidy <-
  metrics_sup$probs %>% 
  mutate(idx = row_number()) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  pivot_longer(
    -c(idx, matches('pos'))
  )
probs_tidy

probs_uncertain <-
  probs_tidy %>% 
  filter(value > 0 & value < 1) %>% 
  mutate(mid = abs(0.5 - value)) %>% 
  arrange(mid)
probs_uncertain

probs_uncertain %>% count(idx, sort = TRUE)


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
