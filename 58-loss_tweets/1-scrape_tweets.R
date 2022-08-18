library(dplyr)
library(worldfootballR)
library(janitor)
library(readr)
library(lubridate)
library(rtweet)
library(qs)

dir_proj <- '58-loss_tweets'
dir_data <- file.path(dir_proj, 'data')
dir.create(dir_data)
matches <- worldfootballR::fb_team_match_results('https://fbref.com/en/squads/19538871/2021-2022/Manchester-United-Stats') |> 
  as_tibble() |> 
  janitor::clean_names()

matches |>
  select(date, venue, opponent, result) |> 
  write_csv(file.path(dir_data, 'manu_matches.csv'))

match_dates <- matches |> 
  filter(comp == 'Premier League') |> 
  select(date) |> 
  pull(date) |> 
  lubridate::date()

first_match_date <- min(match_dates)

token <- xengagement::get_twitter_token()
## this doesn't work?!?
# max_id <- NULL
# last_date <- Sys.Date()
# res <- list()
# i <- 1
# while(last_date > first_match_date) {
#   message(
#     sprintf('%d: Last date is %s.', i, last_date)
#   )
#   tweets <- rtweet::get_timeline(user = 'ManUtd', n = 3200, max_id = max_id)
#   res[[i]] <- tweets
#   last_tweet <- tweets |>
#     slice_min(created_at, n = 1, with_ties = FALSE)
#   last_date <- lubridate::date(last_tweet$created_at)
#   max_id <- last_tweet$status_id
#   i <- i + 1
# }
tweets <- rtweet::get_timeline(user = 'ManUtd', n = 3200)
qs::qsave(tweets, file.path(dir_data, 'manu_tweets.qs'))

## use this for as many tweets as possible, searching thru the data set manually
matchday_tweets <- tweets |> 
  filter(!is_retweet) %>%
  filter(!is_quote) %>%
  mutate(
    date = created_at |> lubridate::date()
  ) |> 
  select(status_id, created_at, date, text) |> 
  filter(
    date %in% match_dates
  ) |> 
  arrange(desc(created_at))
# manual searching like from:ManUtd since:2022-02-20 until:2022-02-20

