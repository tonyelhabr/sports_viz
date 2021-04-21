
library(tidyverse)
library(rvest)
url <- 'https://spacespacespaceletter.com/whats-tackles-defense-is-about-denying-space/'
page <- url %>% xml2::read_html()
page
page %>% 
  html_node('body') %>% 
  html_nodes('p')
