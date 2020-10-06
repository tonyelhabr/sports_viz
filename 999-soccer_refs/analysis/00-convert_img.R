
path_orig <- fs::path(.dir_data_raw, 'anthony-taylor-cropped.jpg')
path_new <- path_orig %>% str_replace('jpg', 'svg')
res <- path_orig %>% rsvg::rsvg() %>% png::writePNG(path_new)
