

.dir_proj <- '18-OptaJoe'
library(tidyverse)
if(FALSE) {
token <- xengagement::get_twitter_token()
tweets <- 
  xengagement::retrieve_tweets(
    method = 'all',
    token = token,
    user = 'OptaJoe',
    dir = .dir_proj
  )
tweets

max_id <- tweets_new %>% slice_min(created_at) %>% pull(status_id)
max_id
tweets_new2 <-
  rtweet::get_timeline(
    max_id = max_id,
    token = token,
    user = 'OptaJoe'
  )
tweets_new2

users <- 'OptaJoe'
debugonce(academictwitteR::get_user_tweets)
debugonce(academictwitteR:::get_tweets)
tweets_user <-
  academictwitteR::get_user_tweets(
    users,
    "2020-01-01T00:00:00Z",
    "2020-01-05T00:00:00Z",
    token,
    data_path = 'OptaJoe4/'
  )
tweets_user

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
    # suffix = 
    #   text_trim %>% 
    #   str_replace_all('(^.*[.])(.*[.!?])', '\\2'),
    suffix1 = 
      text_trim %>% 
      str_replace_all('(^.*)([.][\\s]+?\\w+[.!?])(.*$)', '\\2') %>% 
      str_remove_all('[.]|\\s'),
    suffix2 = 
      text_trim %>% 
      str_replace_all('(^.*)([.]?[\\s]+?\\w+[.!?])(.*$)', '\\2') %>% 
      str_remove_all('[.]|\\s'),
    across(matches('^suffix'), list(str_length), .names = 'n_char_{col}'),
    # n_char_suffix1 = str_length(suffix1),
    # n_char_suffix2 = str_length(suffix2),
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
  # relocate(text_trim, text)
  # select(-matches('text'))
  select(-text_trim)
tweets_slim

n_tweet <-
  tweets_slim %>% 
  count(suffix, sort = TRUE) %>% 
  drop_na(suffix)
n_tweet


grps <- file.path(.dir_proj, 'grps.csv') %>% read_csv() %>% fill(catg)

n_tweet %>% filter(n >= 8)

tweets_slim %>% 
  filter(suffix == 'collection') %>% 
  select(text) %>% 
  mutate(across(text, ~str_sub(.x, -50)))

tweets_slim %>% 
  filter(suffix == 'arrival')

afinn <- tidytext::get_sentiments('afinn')
afinn$word

features <-
  n_tweet %>% 
  select(text = suffix) %>% 
  textfeatures::textfeatures(normalize = FALSE, word_dims = 0) %>% 
  select(matches('^sent')) %>% 
  bind_cols(n_tweet)
features %>% skimr::skim()

n_tweet %>% 
  slice_head(n = 30) %>% 
  mutate(
    across(suffix, ~fct_reorder(.x, n))
  ) %>% 
  ggplot() +
  aes(x = n, y = suffix) +
  geom_col()


tweets_slim %>% 
  count(equal = suffix1 == suffix2)
tweets_slim %>% 
  filter(suffix1 != suffix2) %>% 
  filter(n_char_suffix1 <= 20 & n_char_suffix2 <= 20)


tweets_slim %>% 
  count(suffix2, sort = TRUE)

tweets_slim %>% 
  filter(suffix2 == 'blog') -> x
  count(suffix2, sort = TRUE) 

tweets_slim %>% 
  count(suffix2, sort = TRUE) %>% 
  # filter(n > 1L) %>% 
  count(n, name = 'nn') %>% 
  ggplot() +
  aes(x = n, y = nn) +
  geom_col() +
  scale_y_log10()

tweets_slim %>% filter(n_char_suffix1 > 20) %>% nrow()
tweets_slim %>% filter(n_char_suffix2 > 20) %>% nrow()

tweets_slim %>% 
  mutate(
    good1 = if_else(n_char_suffix1 <= 20, T, F),
    good2 = if_else(n_char_suffix2 <= 20, T, F)
  ) %>% 
  relocate(good1, good2) %>% 
  rio::export('temp.xlsx')
  filter(n_char_suffix1)

tweets_slim %>% 
  filter(n_char_suffix > 15) %>% 
  # rio::export('temp.xlsx')
  slice(2) %>% 
  pull(text_trim)
  glimpse()

tweets_slim %>% 
  filter(n_char_number > 9) %>% 
  slice(3) %>% 
  mutate(
    number = text %>% str_sub(1, 10) %>% str_replace_all('(^[-:.\\\\\\/0-9]+)([ ][-].*$)', '\\1')
  )


tweets_slim %>% 
  count(suffix, sort = TRUE) %>% 
  filter(n > 1L)
