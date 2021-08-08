
# library(tidyverse)
library(dplyr)
library(stringr)
library(magrittr)
library(readr)
library(purrr)
library(tidyr)

# https://bitbucket.org/nicholasehamilton/ggtern/issues/13/ggtern-breaks-ggplots-plot
# https://stackoverflow.com/questions/68344592/ggtern-error-error-geom-point-requires-the-following-missing-aesthetics-x-and/68478744#68478744
library(ggplot2) # should be less than 3.3
# library(ggtern)
packageVersion('ggplot2')

packageVersion('ggtern')

dir_proj <- '38-soccer_twitter'
dir_data <- file.path(dir_proj, 'data')
fs::dir_create(dir_data)

token <- xengagement::get_twitter_token()
token

users <-
  c('pwawrzynow', 'slothfulwave612', 'Advt_played', 'lambertsmarc', 'luistscosta', 'AKaragjyzi', 'DomC0801', 'MishraAbhiA', 'HemmenKees', 'Eoin_OBrien_', 'trevillion_', 'DyslexicDdue', 'ExpectedChelsea', 'ArabAnalytics', 'Peter__OL', '__ElJdP', 'TonyElHabr', 'mckayjohns', 'Soumyaj15209314', 'markrstats', 'victorrenaud5', 'DanielKatona17', 'DSamangy', 'utdarena', 'SanchoQuinn', 'GoalAnalysis', 'biscuitchaser', 'watmanAFC', 'venkyReddevil', 'jonollington', 'johnspacemuller', 'placeholder1966', 'maramperninety', 'abhisheksh_98', 'arielle_dror', 'FC_rstats', 'FC_Python', 'R_by_Ryo', 'Blades_analytic', 'NinadB_06', 'Ben8t', 'SimplyWink', 'joedgallagher', 'Worville', 'experimental361', 'AurelNz', 'wiscostretford', 'petermckeever', 'CallmeAlfredo', 'thecomeonman', 'amonizfootball', 'etmckinley')
# xengagement::retrieve_tweets()

f_get <- function(user, overwrite = FALSE, n = 3200, ...) {
  path <- file.path(dir_data, sprintf('%s.rds', user))
  if(file.exists(path) & !overwrite) {
    return(read_rds(path))
  }
  res <- rtweet::get_timeline(user, n = n, ...)
  write_rds(res, path)
  res
}

tls_wide <- users %>% map_dfr(f_get)
tls_wide

tls <-
  tls_wide %>%
  select(
    user = screen_name,
    pic = profile_image_url,
    user_id,
    status_id,
    created_at,
    text,
    n_char = display_text_width,
    # media_url,
    favorite_count,
    followers_count,
    reply_to_user_id,
    # media_url
    ext_media_t.co
  ) %>% 
  group_by(user) %>% 
  mutate(
    across(created_at, list(prev = lag))
  ) %>% 
  ungroup() %>% 
  mutate(
    n_media = map_int(ext_media_t.co, ~str_detect(.x, 't\\.co') %>% sum()) %>% coalesce(0L),
    fav_follow_ratio = favorite_count / log(followers_count),
    dur = as.numeric(created_at_prev - created_at, 'hours')
  )
tls

# tls_wide %>% filter(screen_name == 'TonyElHabr') -> z
# z %>% filter(status_id == '1393553741199069192') %>% glimpse()
# z %>% filter(status_id == '1410277898381103111') %>% glimpse()

# tls %>% distinct(user) %>% anti_join(tibble(user = users), .)
# tls %>% skimr::skim()

tls_filt <- tls %>% filter(created_at >= '2020-08-08')

fracs <-
  tls_filt %>% 
  group_by(user, pic) %>% 
  summarize(
    n = n(),
    across(favorite_count, mean),
    across(followers_count, first),
    across(n_char, sum),
    across(n_media, sum),
    across(fav_follow_ratio, mean),
    across(dur, mean, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  left_join(
    tls_filt %>% 
      filter(is.na(reply_to_user_id) | (reply_to_user_id == user_id)) %>% 
      group_by(user) %>% 
      summarize(across(n_media, list(self = sum))) %>% 
      ungroup()
  ) %>% 
  mutate(
    frac_media = n_media / n,
    char_per_tweet = n_char / n,
    char_per_media = n_char / n_media
  ) %>% 
  arrange(char_per_media)
fracs

# fracs %>% filter(dur < 24) %>% ggplot() + aes(x = dur) + geom_density()
fracs_filt <- fracs # %>% filter(n > 100) %>% filter(dur < 24)
fracs_filt

# fracs_filt %>% ggplot() + aes(frac_media) + geom_histogram()
# GGally::ggpairs(fracs_filt %>% select(-user))
# fracs_filt %>% arrange(desc(fav_follow_ratio))

# venn ----
venn <-
  fracs_filt %>% 
  # filter(user %>% str_detect('Dyslex', negate = TRUE)) %>% 
  mutate(across(where(is.numeric), percent_rank)) %>% 
  # filter(user %>% str_detect('aram|arena|helsea|Habr')) %>% 
  select(user, pic, dur, frac_media, followers_count) %>% 
  mutate(
    grp = case_when(
      dur <= 1/3 & frac_media >= 2/3 & followers_count >= 2/3 ~ 'hi freq + hi viz + hi foll',
      dur <= 1/3 & frac_media >= 2/3 & followers_count <= 1/3 ~ 'hi freq + hi viz + lo foll',
      dur <= 1/3 & frac_media >= 2/3 & between(followers_count, 1/3, 2/3) ~ 'hi freq + hi viz + mi foll',
      
      dur <= 1/3 & frac_media <= 1/3 & followers_count >= 2/3 ~ 'hi freq + lo viz + hi foll',
      dur <= 1/3 & frac_media <= 1/3 & between(followers_count, 1/3, 2/3) ~ 'hi freq + lo viz + mi foll',
      dur <= 1/3 & frac_media <= 1/3 & followers_count <= 1/3 ~ 'hi freq + lo viz + lo foll',
      
      dur <= 1/3 & between(frac_media, 1/3, 2/3) & followers_count >= 2/3 ~ 'hi freq + mi viz + hi foll',
      dur <= 1/3 & between(frac_media, 1/3, 2/3) & between(followers_count, 1/3, 2/3) ~ 'hi freq + mi viz + mi foll',
      dur <= 1/3 & between(frac_media, 1/3, 2/3) & followers_count <= 1/3 ~ 'hi freq + mi viz + lo foll',
      
      dur >= 2/3 & frac_media >= 2/3 & followers_count >= 2/3 ~ 'lo freq + hi viz + hi foll',
      dur >= 2/3 & frac_media >= 2/3 & followers_count <= 1/3 ~ 'lo freq + hi viz + lo foll',
      dur >= 2/3 & frac_media >= 2/3 & between(followers_count, 1/3, 2/3) ~ 'lo freq + hi viz + mi foll',
      
      dur >= 2/3 & frac_media <= 1/3 & followers_count >= 2/3 ~ 'lo freq + lo viz + hi foll',
      dur >= 2/3 & frac_media <= 1/3 & between(followers_count, 1/3, 2/3) ~ 'lo freq + lo viz + mi foll',
      dur >= 2/3 & frac_media <= 1/3 & followers_count <= 1/3 ~ 'lo freq + lo viz + lo foll',
      
      dur >= 2/3 & between(frac_media, 1/3, 2/3) & followers_count >= 2/3 ~ 'lo freq + mi viz + hi foll',
      dur >= 2/3 & between(frac_media, 1/3, 2/3) & between(followers_count, 1/3, 2/3) ~ 'lo freq + mi viz + mi foll',
      dur >= 2/3 & between(frac_media, 1/3, 2/3) & followers_count <= 1/3 ~ 'lo freq + mi viz + lo foll',
      
      between(dur, 1/3, 2/3) & frac_media >= 2/3 & followers_count >= 2/3 ~ 'mi freq + hi viz + hi foll',
      between(dur, 1/3, 2/3) & frac_media >= 2/3 & followers_count <= 1/3 ~ 'mi freq + hi viz + lo foll',
      between(dur, 1/3, 2/3) & frac_media >= 2/3 & between(followers_count, 1/3, 2/3) ~ 'mi freq + hi viz + mi foll',
      
      between(dur, 1/3, 2/3) & frac_media <= 1/3 & followers_count >= 2/3 ~ 'mi freq + lo viz + hi foll',
      between(dur, 1/3, 2/3) & frac_media <= 1/3 & between(followers_count, 1/3, 2/3) ~ 'mi freq + lo viz + mi foll',
      between(dur, 1/3, 2/3) & frac_media <= 1/3 & followers_count <= 1/3 ~ 'mi freq + lo viz + lo foll',
      
      between(dur, 1/3, 2/3) & between(frac_media, 1/3, 2/3) & followers_count >= 2/3 ~ 'mi freq + mi viz + hi foll',
      between(dur, 1/3, 2/3) & between(frac_media, 1/3, 2/3) & between(followers_count, 1/3, 2/3) ~ 'mi freq + mi viz + mi foll',
      between(dur, 1/3, 2/3) & between(frac_media, 1/3, 2/3) & followers_count <= 1/3 ~ 'mi freq + mi viz + lo foll',
      TRUE ~ 'other'
    )
  )

f_dl <- function(user, pic, overwrite = FALSE) {
  path <- file.path(dir_proj, 'img', sprintf('%s.jpg', user))
  if(file.exists(path) & !overwrite) {
    return((path))
  }
  download.file(pic, destfile = path, quiet = TRUE, mode = 'wb')
}
venn %>% mutate(res = map2(user, pic, f_dl))
venn %>% select(-pic) %>% arrange(user) %>% slice(11:25)
options(tibble.print_max = 30)
venn %>% count(grp, sort = TRUE)

# plot ----
fracs_filt %>% 
  ggplot() +
  aes(x = dur, y = char_per_tweet) +
  geom_point(aes(size = frac_media))

fracs_filt %>% 
  ggtern::ggtern(
    aes(x = char_per_tweet, y = dur, z = followers_count)
  ) + 
  geom_text(
    data = . %>% filter(user %>% str_detect('aram|arena|helsea|Habr')),
    aes(label = user)
  ) +
  # geom_point() +
  geom_point(
    data = . %>% filter(user %>% str_detect('aram|arena|helsea|Habr')),
    color = 'red'
  ) +
  theme_minimal()
p_init
fracs_filt %>% mutate(across(where(is.numeric), percent_rank)) %>% arrange(desc(char_per_tweet)) # filter(user %>% str_detect('Dyslex'))


g <- p_init %>% ggplot_build()  
df <- g$data[[1]] %>% as_tibble()
g$plot + ggimage::geom_image(
  data = df2,
  aes(image = pic)
)
