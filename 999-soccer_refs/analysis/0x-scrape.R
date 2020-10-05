
library(tidyverse)
library(rvest)
path <- 'c:/users/aelhabr/downloads/2020-2021 Tottenham Hotspur Stats, Premier League _ FBref.com.html'
page <- path %>% xml2::read_html()
page
nodes <- page %>% rvest::html_nodes('td')
nodes
page %>% rvest::html_nodes('th')
page %>% rvest::html_nodes(xpath = '//*[@id="stats_standard_3232"]/thead')
page %>% rvest::html_nodes('table') # %>% pluck(20)
page %>% rvest::html_nodes('table') %>% pluck(11) %>% rvest::html_attr('href')
page %>% rvest::html_nodes('table') %>% pluck(11) %>% rvest::html_table()
page %>% rvest::html_nodes('table') %>% pluck(11) %>% rvest::html_nodes('tbody') %>% rvest::html_children() %>% rvest::html_attrs()

page %>% rvest::html_nodes('#all_matchlogs_all') %>% rvest::html_nodes('tr') %>% html_attr('href') # %>% rvest::html_table()


# whoscored ----
# 2012-13
# raw url: https://www.whoscored.com/Regions/252/Tournaments/2/Seasons/3389/Stages/6531/RefereeStatistics/England-Premier-League-2012-2013
# request url: https://www.whoscored.com/RefereesFeed/6531/TournamentStatsByReferee?fieldString=Overall
# 2013-14
#  raw url: https://www.whoscored.com/Regions/252/Tournaments/2/Seasons/3853/Stages/7794/RefereeStatistics/England-Premier-League-2013-2014
# request url: https://www.whoscored.com/RefereesFeed/7794/TournamentStatsByReferee?fieldString=Overall
# 2014-2015
# raw url: https://www.whoscored.com/Regions/252/Tournaments/2/Seasons/4311/Stages/9155/RefereeStatistics/England-Premier-League-2014-2015
# request url: https://www.whoscored.com/RefereesFeed/9155/TournamentStatsByReferee?fieldString=Overall
url <- 'https://www.whoscored.com/Regions/252/Tournaments/2/Seasons/8228/Stages/18685/RefereeStatistics/England-Premier-League-2020-2021'
url <- 'https://www.whoscored.com/RefereesFeed/18685/TournamentStatsByReferee?fieldString=Overall'
resp <- url %>% httr::GET()
resp
cont <- resp %>% httr::content()
cont
cont %>% html_node('body') %>% html_node('div')
