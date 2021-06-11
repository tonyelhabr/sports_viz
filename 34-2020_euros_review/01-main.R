

library(tidyverse)
library(rvest)

url <- 'https://theanalyst.com/na/2021/06/euro-2020-predictions/'
page <- url %>% xml2::read_html()
node <- page %>% html_element(xpath = '/html/body/div[1]/div[2]/theanalyst-widget/div/div/div/table') # %>% html_element('data-table')
node
xpath <- '/html/body/div[1]/div[2]/theanalyst-widget/div/div/div/table'