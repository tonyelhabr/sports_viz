
library(tidyverse)
token <- xengagement::get_twitter_token()
pollsters <- rtweet::lists_members("105140588")
pollsters$protected
list_ids <-
  c(
    '1335885096063295488',
    '1288082572195639296',
    '1287444819015618561',
    '1283739792702713856',
    '1081734288368898048',
    '910757441855459328',
    '193445218',
    '90205656',
    '85315110'
  )

subs <- rtweet::lists_subscriptions('anenglishgoat')
subs

members <-
  list_ids %>% 
  setNames(., . ) %>% 
  map_dfr(~rtweet::lists_members(.x, token = token), .id = 'id') %>% 
  distinct(user_id, .keep_all = TRUE)
members

users_to_exclude <-
  c(
    'premierleague',
    'SpursOfficial',
    'Arsenal',
    'ManCity',
    'sterling7',
    'kylewalker2',
    'HKane',
    'benmendy23',
    'dele_official',
    'RobHolding95',
    'm8arteta'
  )
users_to_exclude

members_clean <-
  members %>% 
  filter(!(screen_name %in% users_to_exclude)) %>% 
  arrange(desc(followers_count))
members_clean
members_clean %>% count(id, sort = TRUE)

members_clean %>% 
  tail()

memberships <- rtweet::lists_memberships('TonyElHabr')
memberships

# x <-
#   httr::GET('https://api.twitter.com/1.1/lists/show.json?list_id=1288082572195639296')
# content <- httr::content(x)
# content
# httr::GET('https://api.twitter.com/1.1/lists/show.json?slug=team&owner_screen_name=twitter') %>% httr::content()
