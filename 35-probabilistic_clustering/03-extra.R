
library(tidyverse)
dir_proj <- '35-probabilistic_clustering'
source(file.path(dir_proj, 'helpers.R'))

viz_cors <-
  df_filt %>% 
  select(-age) %>% 
  select(where(is.numeric)) %>% 
  cor() %>% 
  # corrplot::corrplot()
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = 'upper') +
  theme_tony() +
  theme(
    legend.position = 'none',
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = 'Correlation plot for 100+ variables',
    subtitle = '(This is not all that helpful.)',
    # tag = lab_tag,
    caption = ' '
  )
viz_cors

ggsave(
  plot = viz_cors,
  filename = file.path(dir_proj, 'viz_cors.png'),
  width = 8,
  height = 8,
  type = 'cairo'
)

y <- df_filt %>% select(pos)
preds <-
  y %>% 
  mutate(D = 1L, M = 0, AM = 0, `F` = 0, G = 0, DM = 0) %>% 
  select(-pos) %>% 
  as.matrix()
mll_dummy <-
  MLmetrics::MultiLogLoss(
    y_pred = preds,
    y_true = y$pos
  )
mll_dummy

