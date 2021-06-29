
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
    subtitle = 'This is not all that helpful.',
    # tag = lab_tag,
    caption = ' '
  )
viz_cors

ggsave(
  plot = viz_cors,
  filename = file.path(dir_proj, 'viz_cors.png')
)
