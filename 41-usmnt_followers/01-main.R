
library(tidyverse)
# library(rtweet)
# library(xengagement)
# token <- get_twitter_token()

dir_proj <- '41-usmnt_followers'
path_followers <- file.path(dir_proj, 'watke_followers.rds')

watke <- 'watke_'
watke_folowers <- get_followers(user, n = 18000)
write_rds(folowers, path_watke_followers)
watke_folowers <- read_rds(path_followers)


usmnt <- 'USMNT'
usmnt_followers <- get_followers(usmnt, n = 2200000, retryonratelimit = TRUE)
path_usmnt_followers <- file.path(dir_proj, 'usmnt_followers.rds')
write_rds(usmnt_followers, path_usmnt_followers)

lasso <- 'TedLasso'
lasso_followers <- get_followers(lasso, n = 525000, retryonratelimit = TRUE)
path_lasso_followers <- file.path(dir_proj, 'lasso_followers.rds')
write_rds(lasso_followers, path_lasso_followers)

# bind_rows(
#   watke_folowers %>% mutate(user = 'watke'),
#   lasso_followers %>% mutate(user = 'lasso'),
#   usmnt_followers %>% mutate(user = 'usmnt')
# ) %>% 
#   count(user)

df <-
  list(
    'watke' = watke_folowers$user_id,
    'lasso' = lasso_folowers$user_id,
    'usmnt' = usmnt_followers$user_id
  ) %>% 
  reduce(full_join)

df %>% 
  mutate(
    grp = case_when(
      is.na(watke) & is.na(lasso) ~ 'usmnt only',
      is.na(watke) & is.na(usmnt) ~ 'lasso only',
      is.na(lasso) & is.na(usmnt) ~ 'watke only',
      is.na(watke) ~ 'lasso + usmnt',
      is.na(lasso) ~ 'watke + usmnt',
      is.na(usmnt) ~ 'watke + lasso',
      TRUE ~ 'all'
    )
  )