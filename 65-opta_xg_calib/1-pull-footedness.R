
generate_league_url <- function(start_year) {
  season_str <- sprintf('%04d-%04d', start_year, start_year + 1)
  sprintf(
    'https://fbref.com/en/comps/9/%s/stats/%s-Premier-League-Stats', season_str, season_str
  )
}

league_urls <- c(
  'https://fbref.com/en/comps/9/stats/Premier-League-Stats',
  generate_league_url(c(2017:2021))
)

library(rvest)
library(xml2)
library(purrr)
retrieve_league_players <- function(url) {
  url <- 'https://fbref.com/en/comps/9/2021-2022/stats/2021-2022-Premier-League-Stats'
  session <- chromote_session(url)
  nodes <- session$find_nodes('table')
  elements <- map_chr(nodes, function(node_id) {
    json <- session$call_node_method(node_id)
    json$result$value
  })
  html <- paste0('<html>', paste0(elements, collapse = '\n'), '</html>')
  res <- xml_children(xml_children(read_html(html)))
  session$html
  res[[3]] |> 
    html_elements('tbody > tr > td > a') |> 
    html_attr('href')
}

url <- 'https://fbref.com/en/comps/9/2021-2022/stats/2021-2022-Premier-League-Stats'
worldfootballR:::.fb_single_league_stats
session <- worldfootballR:::worldfootballr_chromote_session(url)
nodes <- session$find_nodes("table")
elements <- purrr::map_chr(nodes, function(node_id) {
  json <- session$call_node_method(node_id)
  json$result$value
})
html <- paste0("<html>", paste0(elements, collapse = "\n"), "</html>")
res <- xml2::xml_children(xml2::xml_children(xml2::read_html(html)))
# tables <- worldfootballR:::worldfootballr_html_table(session)
# tables[[3]]
res[[3]] |> rvest::html_elements("tbody > tr > td > a") |> rvest::html_attr('href')
player_url <- 'https://fbref.com/en/players/d5dd5f1f/Pierre-Emerick-Aubameyang'
page <- read_html(player_url)
page |> 
  html_elements(css = '#meta > div > p') |> 
  html_text2()
