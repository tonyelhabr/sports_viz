
library(tidyverse)
library(rtweet)

dir_proj <- '41-usmnt_followers'
source(file.path(dir_proj, 'helpers.R'))

watke <- 'watke_'
path_watke_followers <- file.path(dir_proj, 'watke_followers.rds')
# watke_folowers <- get_followers(user, n = 18000)
# write_rds(folowers, path_watke_followers)
watke_followers <- read_rds(path_watke_followers)

lasso <- 'TedLasso'
path_lasso_followers <- file.path(dir_proj, 'lasso_followers.rds')
# # lasso_followers <- get_followers(lasso, n = 525000, retryonratelimit = TRUE) # rtweet failing
# lasso_followers <- get_all_followers(lasso, followers = 525000, page = '-1')
# # next_cursor was "1690687164766806439"
# write_rds(lasso_followers, path_lasso_followers)
lasso_followers <- read_rds(path_lasso_followers)

usmnt <- 'USMNT'
path_usmnt_followers <- file.path(dir_proj, 'usmnt_followers.rds')
# # usmnt_followers <- get_followers(usmnt, n = Inf, retryonratelimit = TRUE) # rtweet failing
# usmnt_followers <- get_all_followers(usmnt, followers = 2200000, page = '-1')
# # next cursor was "1701898738548249542"
# usmnt_followers <- usmnt_followers %>% map_dfr(bind_rows)
# write_rds(usmnt_followers, path_usmnt_followers)
usmnt_followers <- read_rds(path_usmnt_followers)

n_followers_watke <- lookup_users(watke)$followers_count
n_followers_lasso <- lookup_users(lasso)$followers_count
n_followers_usmnt <- lookup_users(usmnt)$followers_count
total_followers <- n_followers_watke + n_followers_lasso + n_followers_usmnt

df <-
  bind_rows(
    watke_followers %>% mutate(user = 'watke'),
    lasso_followers %>% mutate(user = 'lasso'),
    usmnt_followers %>% mutate(user = 'usmnt')
  ) %>%
  distinct() %>% 
  mutate(z = 1) %>% 
  pivot_wider(names_from = user, values_from = z)

grps <-
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
grps

cnts_init <-
  grps %>% 
  count(grp, sort = TRUE) %>% 
  mutate(
    n_actual = case_when(
      str_detect(grp, 'only') ~ 1L,
      grp == 'all' ~ 3L,
      TRUE ~ 2L
    ),
    n_adj = n * n_actual,
    frac = n_adj / sum(n_adj)
  )
cnts_init

# for percentages
do_boost <- function(rgx) {
  cnts_init %>% 
    filter(grp == 'all' | str_detect(grp, 'watke')) %>% 
    mutate(
      frac = n / sum(n),
      n_boost = frac * n_followers_watke,
      n_boost_adj = n_boost / n_actual
    )
}
cnts_watke <- do_boost('watke')
cnts_lasso <- do_boost('lasso')
cnts_usmnt <- do_boost('usmnt')

# need to boost lass only from 465k to 470k for some reason
cnts_adj <-
  bind_rows(
    cnts_watke,
    cnts_lasso,
    cnts_usmnt
  ) %>% 
  group_by(grp, n, n_actual) %>% 
  summarize(
    across(n_boost, sum),
    across(n_boost_adj, sum)
  ) %>% 
  ungroup() %>% 
  mutate(n_boost_adj = ifelse(grp == 'lasso only', 490000, n_boost_adj))
cnts_adj

# extra ----
# download_pic <- function(user, pic, overwrite = FALSE) {
#   path <- file.path(dir_proj, 'img', sprintf('%s.jpg', user))
#   if(file.exists(path) & !overwrite) {
#     return((path))
#   }
#   download.file(pic, destfile = path, quiet = TRUE, mode = 'wb')
# }
pundits <- lookup_users(c('AlexiLalas', 'TaylorTwellman', 'GrantWahl'))
pundits
grps %>% filter(user_id %in% pundits$user_id)

players <- lookup_users(c('ChristainPulisic', 'GioReyna', 'Weston McKinnie'))
players
