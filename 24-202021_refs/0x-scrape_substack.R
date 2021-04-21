
library(tidyverse)
library(rvest)
url <- 'https://spacespacespaceletter.com'
page <- sprintf('%s/archive/', url) %>% xml2::read_html()
body <-
  page %>% 
  html_node('body')
nodes <-
  body %>% 
  html_node('[class="container main-content"]') %>% 
  html_node('[class="posts"]') %>% 
  html_nodes('article') %>% 
  html_nodes('[class="post-card"]')
nodes %>% 
  html_nodes('h1') %>% 
  html_attrs()
  
res <-
  1:15 %>% 
  map_chr(
    ~html_nodes(
      page,
      xpath = sprintf('/html/body/div[1]/div/article[%d]/div[2]/h1/a', .x)
    ) %>% 
      html_attr('href')
  )
res

links <-
  res %>% 
  tibble(post = .) %>% 
  mutate(
    url = sprintf('%s%s', !!url, post)
  )
links    

page <- links$url[1] %>% read_html()
page
body <- page %>% html_nodes(xpath = '/html/body/div[1]/article') %>% html_text()
body

