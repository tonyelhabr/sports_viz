library(tidyverse)
library(lubridate)
library(rtweet)
library(xengagement)
library(worldfootballR)

dir_proj <- '58-loss_tweets'
matches <- worldfootballR::fb_team_match_results('https://fbref.com/en/squads/19538871/2021-2022/Manchester-United-Stats') |> 
  as_tibble() |> 
  janitor::clean_names()
match_dates <- matches |> 
  filter(comp == 'Premier League') |> 
  select(date) |> 
  pull(date) |> 
  lubridate::date()
match_dates
first_match_date <- min(match_dates)

token <- xengagement::get_twitter_token()

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
qs::qsave(tweets, file.path(dir_proj, 'data', 'tweets.qs'))

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
# from:ManUtd since:2022-02-20 until:2022-02-20

tonythemes::theme_set_tony()
manu_tweets <- file.path(dir_proj, 'statuses.csv') |> read_csv()
outcome_lvls <- c('Win', 'Draw or Loss')
player_counts <- manu_tweets |> 
  mutate(
    across(outcome, ~ifelse(.x != 'win', outcome_lvls[[2]], outcome_lvls[[1]]))
  ) |> 
  separate_rows(
    players,
    sep = ','
  ) |> 
  count(outcome, player = players, sort = TRUE) |> 
  mutate(
    across(outcome, ~ordered(.x, outcome_lvls)),
    grp = tidytext::reorder_within(player, n, outcome)
  )

y_labels <- player_counts %>% distinct(grp, player)

p <- player_counts |> 
  ggplot() +
  aes(x = n, y = grp) +
  geom_col() +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 1)
  ) +
  scale_y_discrete(name = '', labels = y_labels %>% select(grp, player) %>% deframe()) +
  facet_wrap(~outcome, scales = 'free') +
  theme(
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = 'Appearances in end-of-match @ManUtd score tweets',
    y = NULL,
    x = '# of matches'
  )
p
ggsave(
  p,
  filename = file.path(dir_proj, 'manu_tweets.png'),
  width = 10,
  height = 8
)
