
dir_proj <- '01-nba_seed_p'
dir_data_raw <- fs::path(dir_proj, 'data-raw')
dir_data <- fs::path(dir_proj, 'data')
fs::dir_create(dir_data_raw)
fs::dir_create(dir_data)
tm_logo_url_info <- readxl::read_excel(fs::path(dir_data_raw, 'tm_logo_urls.xlsx'))
tm_logo_url_info

download_safely <- safely(download.file)

paths_logo_dl <-
  tm_logo_url_info %>% 
  filter(!logo_download_manual) %>% 
  mutate(already_downloaded = map_lgl(logo_path_local, fs::file_exists)) %>% 
  filter(!already_downloaded) %>% 
  mutate(res = map2(logo_url, logo_path_local, ~download_safely(url = ..1, destfile = ..2, quiet = TRUE, mode = 'wb')))
paths_logo_dl

paths_logo_png <-
  tm_logo_url_info %>%
  mutate(already_converted = map_lgl(logo_path_png, fs::file_exists)) %>% 
  filter(!already_converted) %>% 
  mutate(res = map2(logo_path_local, logo_path_png, ~rsvg::rsvg(..1) %>% png::writePNG(..2)))
paths_logo_png