
library(tidyverse)
library(rtweet)
library(xengagement)
token <- get_twitter_token()

dir_proj <- '41-usmnt_followers'
source(file.path(dir_proj, 'helpers.R'))

download_pic <- function(user, pic, overwrite = FALSE) {
  path <- file.path(dir_proj, 'img', sprintf('%s.jpg', user))
  if(file.exists(path) & !overwrite) {
    return((path))
  }
  download.file(pic, destfile = path, quiet = TRUE, mode = 'wb')
}

watke <- 'watke_'
path_watke_followers <- file.path(dir_proj, 'watke_followers.rds')
# watke_folowers <- get_followers(user, n = 18000)
# write_rds(folowers, path_watke_followers)
watke_followers <- read_rds(path_watke_followers)

lasso <- 'TedLasso'
path_lasso_followers <- file.path(dir_proj, 'lasso_followers.rds')
# lasso_followers <- get_followers(lasso, n = 525000, retryonratelimit = TRUE)
# last lasso next_cursor was "1690687164766806439"
# lasso_followers <- get_all_followers(lasso, followers = 525000, page = '-1')
# write_rds(lasso_followers, path_lasso_followers)
lasso_followers <- read_rds(path_lasso_followers)

usmnt <- 'USMNT'
path_usmnt_followers <- file.path(dir_proj, 'usmnt_followers.rds')
# usmnt_followers <- get_followers(usmnt, n = Inf, retryonratelimit = TRUE)
usmnt_followers <- get_all_followers(usmnt, followers = 2200000, page = '-1')
usmnt_followers <- usmnt_followers[1:29] %>% map_dfr(bind_rows)
# next cursor was "1701898738548249542"
write_rds(usmnt_followers, path_usmnt_followers)
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
#   count(user)

# df <-
#   list(
#     tibble('watke' = watke_followers$user_id),
#     tibble('lasso' = lasso_followers$user_id),
#     tibble('usmnt' = usmnt_followers$user_id)
#   ) %>%
#   reduce(full_join)

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
cnts_watke <-
  cnts_init %>% 
  filter(grp == 'all' | str_detect(grp, 'watke')) %>% 
  mutate(
    frac = n / sum(n),
    n_boost = frac * n_followers_watke,
    n_boost_adj = n_boost / n_actual
  )
cnts_watke

cnts_lasso <-
  cnts_init %>% 
  filter(grp == 'all' | str_detect(grp, 'lasso')) %>% 
  mutate(
    frac = n / sum(n),
    n_boost = frac * n_followers_lasso,
    n_boost_adj = n_boost / n_actual
  )
cnts_lasso

cnts_usmnt <-
  cnts_init %>% 
  filter(grp == 'all' | str_detect(grp, 'usmnt')) %>% 
  mutate(
    frac = n / sum(n),
    n_boost = frac * n_followers_usmnt,
    n_boost_adj = n_boost / n_actual
  )
cnts_usmnt

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
cnts_adj_watke <-
  cnts_adj %>% 
  filter(grp == 'all' | str_detect(grp, 'watke')) %>% 
  mutate(
    frac = n_boost_adj / sum(n_boost_adj)
  )
cnts_adj_watke

cnts_adj_lasso <-
  cnts_adj %>% 
  filter(grp == 'all' | str_detect(grp, 'lasso')) %>% 
  mutate(
    frac = n_boost_adj / sum(n_boost_adj)
  )
cnts_adj_lasso

cnts_adj_usmnt <-
  cnts_adj %>% 
  filter(grp == 'all' | str_detect(grp, 'usmnt')) %>% 
  mutate(
    frac = n_boost_adj / sum(n_boost_adj)
  )
cnts_adj_usmnt


total_followers - sum(cnts_adj$n_boost)

sum(cnts_init$n_adj)
frac_boost <- total_followers / sum(cnts_init$n_adj)
frac_boost
cnts <- 
  cnts_init %>% 
  mutate(n_boost = frac * total_followers, n_boost_adj = n_boost / n_actual)
cnts
sum(cnts$n_boost)
total_followers
grps %>% filter(grp == 'all')
df %>% filter(!is.na(watke))
grps %>% filter(grp == 'watke + usmnt') %>% filter(!is.na(lasso))

# extra ----
pundits <- lookup_users(c('AlexiLalas', 'TaylorTwellman', 'GrantWahl'))
pundits
grps %>% 
  filter(user_id %in% pundits$user_id)

players <- lookup_users(c('ChristainPulisic', 'GioReyna', 'Weston McKinnie'))
players
