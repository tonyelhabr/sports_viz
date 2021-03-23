
library(tidyverse)
user <- 'OptaJoe'
dir_proj <- sprintf('18-%s', user)
grps <-
  file.path(dir_proj, 'grps.csv') %>% 
  read_csv() %>% 
  fill(catg) %>% 
  rename(suffix = word)
path <- file.path(dir_proj, sprintf('%s_timeline.rds', user))
overwrite <- FALSE
if(!file.exists(path) & !overwrite) {
  token <- xengagement::get_twitter_token()
  tweets <- 
    xengagement::retrieve_tweets(
      method = 'all',
      token = token,
      user = user,
      path = path
    )
  tweets
} else {
  tweets <- path %>% read_rds()
}

tweets_slim <-
  tweets %>% 
  select(status_id, created_at, text) %>%
  distinct(text, .keep_all = TRUE) %>% 
  # select(text) %>% 
  mutate(
    text_trim = 
      text %>% 
      tolower() %>% 
      # Remove leading and trailing spaces.
      str_trim() %>% 
      # Remove ew line characters.
      str_replace_all('[\\r\\n]', ' ') %>% 
      # Remove urls.
      str_remove_all('(ht)tp(s?)://\\S+\\b') %>% 
      # Remove hashtags and twitter handles at the end.
      str_remove_all('\\s+(#[@])[A-z]+\\s?$') %>% 
      # # Remove twitter handles at the end.
      # str_remove_all('\\s+[@][A-z_]+\\s?$') %>% 
      # Pad a character at the end.
      paste0('.') %>% 
      # Remove if there are more than one (especially if they have been padded).
      str_replace_all('[.]{1,2}', '.'),
    # don't technically need this number but it could be interesting to do analysis on
    number = 
      text_trim %>% 
      str_sub(1, 10) %>% 
      str_replace_all('(^[-:.\\\\\\/0-9]+)([ ][--][ ])(.*$)', '\\1'),
    suffix1 = 
      text_trim %>% 
      str_replace_all('(^.*)([.][\\s]+?\\w+[.!?])(.*$)', '\\2') %>% 
      str_remove_all('[.]|\\s'),
    suffix2 = 
      text_trim %>% 
      str_replace_all('(^.*)([.]?[\\s]+?\\w+[.!?])(.*$)', '\\2') %>% 
      str_remove_all('[.]|\\s'),
    across(matches('^suffix'), list(str_length), .names = 'n_char_{col}'),
    is_good1 = if_else(n_char_suffix1 <= 20, TRUE, FALSE),
    is_good2 = if_else(n_char_suffix2 <= 20, TRUE, FALSE),
    suffix = case_when(
      suffix1 == suffix2 & is_good1 ~ suffix1,
      !is_good1 & !is_good2 ~ NA_character_,
      is_good1 ~ suffix1,
      is_good2 ~ suffix2,
      TRUE ~ NA_character_
    )
  ) %>% 
  # Keep text for debugging.
  select(-text_trim) %>% 
  reloate(text_trim, .after = last_col())
tweets_slim

n_tweet <-
  tweets_slim %>% 
  count(suffix, sort = TRUE) %>% 
  drop_na(suffix)
n_tweet


