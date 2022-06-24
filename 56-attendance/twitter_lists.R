
library(rtweet)
library(purrr)
library(dplyr)

token <- create_token(
  app =             Sys.getenv('TWITTER_APP'),
  consumer_key =    Sys.getenv('TWITTER_CONSUMER_API_KEY'),
  consumer_secret = Sys.getenv('TWITTER_CONSUMER_API_SECRET'),
  access_token =    Sys.getenv('TWITTER_ACCESS_TOKEN'),
  access_secret =   Sys.getenv('TWITTER_ACCESS_TOKEN_SECRET')
)

mls_club_list <- 8929253
team_accounts <- list(
  'mls_usl' = 1524400157474836481,
  'epl' = 786067957373894656, 
  'champ' = 786070708405272576
) |> 
  map_dfr(rtweet::lists_members, token = token, .id = 'league')