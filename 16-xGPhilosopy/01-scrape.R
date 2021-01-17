
library(tidyverse)

user <- 'xGPhilosophy'
path_tweets <- file.path(sprintf('%s_timeline.rds', user))
path_epl_tm_accounts <- file.path('epl_tm_accounts.rds')
path_champ_tm_accounts <- file.path('epl_tm_accounts.rds')
path_tm_mapping <- file.path('team_account_mapping.csv') # Manual mapping
tm_mapping <- 
  path_tm_mapping %>% 
  read_csv(
    col_types = cols(
      tm_xgp = col_character(),
      user_id = col_character()
    )
  )
tm_mapping

if(FALSE) {
  n <- 3200
  tweets <- rtweet::get_timeline(user = user, n = n)
  # earliest_tweet <- tweets %>% slice_min(created_at)
  epl_tm_accounts <- rtweet::lists_members(786067957373894656)
  champ_tm_accounts <- rtweet::lists_members(786070708405272576)
  write_rds(tweets, path_tweets)
  write_rds(epl_tm_accounts, path_epl_tm_accounts)
  write_rds(champ_tm_accounts, path_champ_tm_accounts)
} else {
  tweets <- path_tweets %>% read_rds()
  epl_tm_accounts <- path_epl_tm_accounts %>% read_rds()
  champ_tm_accounts <- path_champ_tm_accounts %>% read_rds()
}

tm_accounts <- bind_rows(epl_tm_accounts, champ_tm_accounts)
tm_accounts

tm_accounts_mapping <-
  tm_mapping %>% 
  left_join(
    tm_accounts %>% 
      select(user_id, followers_count, created_at)
  ) %>% 
  select(-user_id) %>% 
  rename(tm = tm_xgp)
tm_accounts_mapping %>% arrange(-followers_count)

# We have to make an assumption about how many followers the account had when it started to make tweets frequently, which really wasn't until Jan. 2020
first_follower_count <- 10000
latest_tweet <- tweets %>% slice_max(created_at)
latest_follower_count <- latest_tweet$followers_count
follower_count_diff <- latest_follower_count - first_follower_count
# latest_date <- latest_tweet$created_at %>% lubridate::date()
# created_date <- latest_tweet$account_created_at %>% lubridate::date()

.f_replace <- function(x, i) {
  x %>% str_replace('(^.*)\\s\\(([0-9.]+)\\)\\s([0-9]+)[-]([0-9]+)\\s\\(([0-9.]+)\\)\\s(.*$)', sprintf('\\%d', i))
}
.f_tm <- function(x) {
  x %>% str_remove_all('⭐️') %>% str_trim()
}
# tweets %>% filter(text %>% str_detect('follower')) %>% select(created_at, text)
scores <-
  tweets %>%
  select(
    status_id,
    created_at,
    # reply_count,
    # followers_count,
    retweet_count,
    retweet_favorite_count,
    favorite_count,
    # favourites_count,
    # quote_count,
    # quoted_favorite_count,
    # quoted_retweet_count,
    text
  ) %>% 
  mutate(
    idx = row_number(created_at),
    estimated_follower_count = !!first_follower_count + round((idx / max(idx)) * !!follower_count_diff, 0)
  ) %>% 
  # Drop half time scores, and just anything with commas or new lines since those aren't score line tweets.
  filter(text %>% str_detect('^HT|\\,|\\n', negate = TRUE)) %>%
  # We know that a score line tweet has this.
  filter(text %>% str_detect('\\(')) %>%
  mutate(
    created_date = created_at %>% lubridate::date(),
    across(
      text,
      list(
        tm_h = ~ .f_replace(.x, 1) %>% .f_tm(),
        xg_h = ~ .f_replace(.x, 2) %>% as.numeric(),
        g_h = ~ .f_replace(.x, 3) %>% as.integer(),
        g_a = ~ .f_replace(.x, 4) %>% as.integer(),
        xg_a = ~ .f_replace(.x, 5) %>% as.numeric(),
        tm_a = ~ .f_replace(.x, 6) %>% .f_tm()
      ),
      .names = '{fn}'
    )
  ) %>%
  select(-text) %>% 
  # Drop non-score line tweets that weren't caught by previous filter.
  drop_na(xg_h, g_h, g_a, xg_a) %>% 
  mutate(
    # This is the only weird naming case that I identified.
    across(c(tm_h, tm_a), ~if_else(.x == 'Spurs', 'Tottenham', .x))
  ) %>% 
  arrange(created_at)
scores

scores_aug <-
  scores %>% 
  # head(1) %>% 
  left_join(
    tm_accounts_mapping %>% rename_all(~sprintf('%s_h', .x))
  ) %>% 
  mutate(
    date_diff_h = !!latest_date - lubridate::date(created_at_h),
    date_diff_h_latest = !!latest_date - created_date,
    across(matches('date_diff_h'), as.numeric),
    # frac_h = ((date_diff_h - date_diff_h_latest) / date_diff_h),
    estimated_follower_count_h = ((date_diff_h - date_diff_h_latest) / date_diff_h) * followers_count_h
  ) %>% 
  select(-matches('date_diff_h')) %>% 
  left_join(
    tm_accounts_mapping %>% rename_all(~sprintf('%s_a', .x))
  ) %>% 
  mutate(
    date_diff_a = !!latest_date - lubridate::date(created_at_a),
    date_diff_a_latest = !!latest_date - created_date,
    across(matches('date_diff_a'), as.numeric),
    # frac_a = ((date_diff_a - date_diff_a_latest) / date_diff_a),
    estimated_follower_count_a = ((date_diff_a - date_diff_a_latest) / date_diff_a) * followers_count_a
  )
scores_aug

scores %>% arrange(desc(estimated_follower_count))
scores %>% arrange(desc(favorite_count))

tms <-
  bind_rows(
    scores %>% 
      count(tm = tm_h),
    scores %>% 
      count(tm = tm_a)
  ) %>% 
  group_by(tm) %>% 
  summarize(across(n, sum)) %>% 
  ungroup() %>% 
  arrange(desc(n))
tms

