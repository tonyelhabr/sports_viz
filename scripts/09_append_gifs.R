
library(tidyverse)
file <- 'ucl_2020_psg_mun'
path_export_gif <- here::here('plots', sprintf('%s.gif', file))
path_export_pc_gif <- path_export_gif %>% str_replace('[.]gif$', '_pc.gif')
path_export_vaep_gif <- path_export_gif %>% str_replace('[.]gif$', '_vaep.gif')
viz_pc_mgif <- path_export_pc_gif %>% magick::image_read()
viz_vaep_mgif <- path_export_vaep_gif %>% magick::image_read()

res_gif <- magick::image_append(c(viz_pc_mgif[1], viz_vaep_mgif[1]), stack = TRUE)
n_frame <- 253
for(i in 2:n_frame){
  combo_gif <- magick::image_append(c(viz_pc_mgif[i], viz_vaep_mgif[i]), stack = TRUE)
  res_gif <- c(res_gif, combo_gif)
}
magick::image_write(res_gif, path = path_export_gif)
