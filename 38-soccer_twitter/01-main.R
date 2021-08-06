
library(tidyverse)

dir_proj <- '38-soccer_twitter'
dir_data <- file.path(dir_proj, 'data')
fs::dir_create(dir_data)

token <- xengagement::get_twitter_token()
token
users <-
  c('pwawrzynow', 'slothfulwave612', 'Advt_played', 'lambertsmarc', 'luistscosta', 'AKaragjyzi', 'DomC0801', 'MishraAbhiA', 'HemmenKees', 'Eoin_OBrien_', 'trevillion_', 'DyslexicDdue', 'ExpectedChelsea', 'ArabAnalytics', 'Peter__OL', '__ElJdP', 'TonyElHabr', 'mckayjohns', 'Soumyaj15209314', 'markrstats', 'victorrenaud5', 'DanielKatona17', 'DSamangy', 'utdarena', 'SanchoQuinn', 'GoalAnalysis', 'biscuitchaser', 'watmanAFC', 'VenkyReddevil', 'jonollington', 'johnspacemuller', 'placeholder1966', 'maramperninety', 'abhisheksh_98', 'arielle_dror', 'FC_rstats', 'FC_Python', 'R_by_Ryo', 'Blades_analytic', 'NinadB_06', 'Ben8t', 'SimplyWink', 'joedgallagher', 'Worville', 'experimental361', 'AurelNz', 'wiscostretford', 'petermckeever', 'CallmeAlfredo', 'thecomeonman')
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

tls_wide <- 
  users %>% 
  map_dfr(f_get)
tls_wide

tls <-
  tls_wide %>%
  select(
    user = screen_name,
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
    n_media = map_int(ext_media_t.co, length),
    fav_follow_ratio = favorite_count / log(followers_count),
    dur = as.numeric(created_at_prev - created_at, 'hours')
  )
tls %>% count(n_media)
tls_wide %>% filter(screen_name == 'TonyElHabr') -> z
z %>% filter(status_id == '1393553741199069192') %>% glimpse()
z %>% filter(status_id == '1410277898381103111') %>% glimpse()

# tls %>% distinct(user) %>% anti_join(tibble(user = users), .)
# tls %>% skimr::skim()

tls_filt <- tls %>% filter(created_at >= '2020-08-08')
fracs <-
  tls_filt %>% 
  group_by(user) %>% 
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
    char_per_media = n_char / n_media
  ) %>% 
  arrange(char_per_media)
fracs

fracs_filt <- fracs # %>% filter(n > 100) %>% filter(dur < 24)
fracs_filt

fracs_filt %>% ggplot() + aes(frac_media) + geom_histogram()

GGally::ggpairs(fracs_filt %>% select(-user))
fracs_filt %>% arrange(desc(fav_follow_ratio))

fracs_filt_long <-
  fracs_filt %>% 
  pivot_longer(
    -user
  )


fracs_filt_long %>% 
  ggplot() +
  aes(x = dur, y = char_per_media) +
  geom_point(aes(size = n))

