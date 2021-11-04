
library(tidyverse)
library(RSelenium)
library(janitor)

# shell('docker run -d -p 4445:4444 selenium/standalone-chrome') # standalone-firefox:2.53.0')
shell('docker run -d -p 4445:4444 selenium/standalone-firefox')
state <- shell('docker ps', intern = TRUE)
state %>% str_split("\\s+") %>% map(length)
state %>% as_tibble() %>% janitor::row_to_names(1) %>% janitor::clean_names()
state
shell('docker kill a41d65842260')
address = '192.168.99.100'
# port = 4445L
# base_url <- 'https://understat.com/'
base_url <- 'https://whoscored.com/'
# address = 'localhost'
port = 4445L

dr <-
  remoteDriver(
    browserName = 'firefox',
    remoteServerAddr = address,
    port = port
  )
dr$open()
dr$navigate(base_url)
dr$screenshot(display = TRUE)
dr
