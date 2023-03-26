library(rvest)
library(purrr)
library(stringr)
url <- 'https://dataviz.theanalyst.com/opta-power-rankings/'
s <- read_html_live(url)

tb <- s$html_elements('table') |> 
  html_table() |> 
  pluck(1)

debugonce(s$click)
s$click("button")

# divs <- s$html_elements('div')
# idx <- divs |> 
#   html_text2() |> 
#   str_which('^\\<[0-9]+ of [0-9]+\\>$')
# css_cls <- divs[idx] |> html_attr('class')
# last_button <- s$html_elements(sprintf('.%s > button:nth-child(3)', css_cls))
# s$click(last_button)

root_id <- s$session$DOM$getDocument(0)$root$nodeId
button_node_ids <- unlist(s$session$DOM$querySelectorAll(root_id, "button")$nodeIds)
node_id <- button_node_ids[length(button_node_ids)]
js_fun <- paste0("function() { return this", ".click()", "}")
object_id <- s$session$DOM$resolveNode(node_id)$object$objectId
s$session$Runtime$callFunctionOn(js_fun, objectId = object_id)

library(readr)
library(dplyr)
opta <- read_csv('../club-rankings/data.csv')
df
ft8 <- read_csv('https://projects.fivethirtyeight.com/soccer-api/club/spi_global_rankings.csv')
ft8_leagues <- ft8 |> 
  group_by(league) |> 
  summarize(
    across(
      rank,
      list(
        min = min,
        max = max,
        median = median
      )
    )
  ) |> 
  ungroup() |> 
  arrange(rank_median)
ft8_leagues |> 
  filter(league == 'English League Two')

library(worldfootballR)
team_urls <- fb_teams_urls('https://fbref.com/en/comps/11/Serie-A-Stats')
team_urls
