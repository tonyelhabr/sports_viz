library(httr)
library(jsonlite)
library(dplyr)
library(tibble)
library(tidyr)
library(janitor)

library(httr)

cookies = c(
  `_ga` = "GA1.1.685929884.1635478427",
  `_hjSessionUser_2585474` = "eyJpZCI6IjViYmQyMDc2LTA5NjAtNWJjNy1iMmJlLTQyZTBjZTVhM2MwZCIsImNyZWF0ZWQiOjE2MzkyMzIyNzI1NjcsImV4aXN0aW5nIjp0cnVlfQ==",
  `_ga_G0V1WDW9B2` = "GS2.1.s1759357286$o417$g1$t1759357316$j30$l1$h1057953792",
  `_ga_SQ24F7Q7YW` = "GS2.1.s1756607699$o33$g0$t1756607700$j59$l0$h0",
  `_ga_K2ECMCJBFQ` = "GS2.1.s1756607699$o27$g0$t1756607700$j59$l0$h0",
  g_state = '{"i_l":0}',
  guser = '{"name":"Tony ElHabr","image":"https://lh3.googleusercontent.com/a/ACg8ocLwMiWLhlszK44bxUCAaQOBmvF3l9yidNl70dGV565bqeRR2H-Ogw=s96-c","id":"102041985920062361465","email":"anthonyelhabr@gmail.com"}',
  `_cc_id` = "54e2801c6b6817ee756a3f3ab439a84b",
  cto_bundle = "P_AXtF9ZRkczczNBREx0UGZjRDQ0eEJpb1QlMkZNTDNwWHd0WUZTRWlNc3haeDJOSE5zU3E5ZldBY0lYUDhJUzRxSzRLZmY3OHJOTzRiWWQ3dVZOQVdDa09rUmU0MkJVbiUyRmlpdFVJR1ljaEhncHpwckxaeG5EUmUlMkI3bDRaUXcwSWJHd0NTJTJCS0ppcFVTVGZUeW9LSHIxVDR1YkVNZ2FUZGFlRFFuUmx4MTBYcEd5UkxIdyUzRA",
  `__gads` = "ID=8e8d79495cb23300:T=1729553579:RT=1729553579:S=ALNI_Macdf4iPUX3WMkEAQzZLz_lFPw1xw",
  `__gpi` = "UID=00000f2f4e363fd6:T=1729553579:RT=1729553579:S=ALNI_MaUJqlZqT_kMMlAIpHil5R2X2z57A",
  NEXT_LOCALE = "en",
  `_gcl_au` = "1.1.932920787.1756607681",
  `u:location` = '{"countryCode":"US","regionId":"TX","ip":"127.0.0.1","ccode3":"USA_TX","ccode3NoRegion":"USA","timezone":"America/Chicago"}',
  `_hjSession_2585474` = "eyJpZCI6IjgwNmYzZmNiLTM0ZjUtNDVjZS1hM2I3LTFhYWMzOTU5MzAzYyIsImMiOjE3NTkzNTcyODczMjgsInMiOjAsInIiOjAsInNiIjowLCJzciI6MCwic2UiOjAsImZzIjowLCJzcCI6MH0=",
  g_csrf_token = "de437a0b2eef4746",
  user = "ubdUaW8ix9rT8IW8H6eeKg.9z1Ezxfeo_LXAOECH2qxE0KyVulEmcMzqYcZpbBk9fRDQEj_9perSR0jZi940RjWB7anOfmaJ0cpc1pqG70maLd6Agv1pmrSNT1tdADt7J6oybmRWWuC8SSVf2gVTCltyCzDLpP9LzN7IGKJBJl16bamZueNZ8A8FbebyIQ2Nx_Rw9W18VpXrAn-3pavuVNw4XkzyV0c6Y3ZNR917mGD9ne921m4aAw0dUty6MCBiWgfYjgSf7nZlr9Edi6OBRdCbVLY7ym-E6lBWmnrRMiqsqOF3xHAAoVY2-lDNh7ZCq96HTqmOC7ISfSOAy6kRi1XmQwp3p4KWG2WG_Y6HBs7c-LDYTI_fhv49RTSVn7VW7FNRTk50N_kRwvHBaEDQwr2XquxtqSG3TrjMx6pRwGYSi9KtoTsT0EmCIrDvhKAA86aBjqS2YcXNxdOjp8IKvvgZw-p0rHJvsDKIk7DNW1BX8L_loaAHQJHc4OqSl1MQnSBaYF0Mss5VYL9SNMqgiF_7ooFQkW1MZG66VMjkCO4DWD8YHgQlXlyim0R7wYLSEL968rmYSKmuhRYWjS_jHegu2TxsTs9m8buX1gaE4v8ktyBI6jHFJioMtmbGa1WA4dEslAvY5lLv-T-LUyPB_PS0civkMby0WbHlu9uHEvhPK7EBe5BOp9Wk33No6H3UsRD2jQuUTvlwil-U3yk_-nq9tsmyTwG4QyZ6kX3EfeJjsiaLpWbwRjRrrsirgrIV9kmznOlBa8VmpARugd_pHJN1c1HCaq6cVoEAJSYRism-Wmvzm6E78lm4QX-eY6szd-m_YY9LFHvTsRK8dGzNyDFYqDfgcn6A7sFcB77bLfC7UE6eLQH9bIhB8q8BPUA8-X_7E8QOAzboiCkoliumk10KwAWUoqnRPsVNcqdIVFLPjuqG1sen7Fda1uKDWt6F_4R8aDzZUFALigaMFKaqnl6ZQqoCP4sVk0A9l-5cmSWzEa3wp34mQkrzMWGBhQ5wp-QBQSarAY2kS5VkD-jFiAVYlYU32uDQvrC8HLpsBIYz82dxkp_rlKiopGZ2JnefN7f9hcFKrBqxSt9fdBugFA7SmfibNmCNt5UFcyLqVCMOU7Z-ZK8VFeieepd4L5N6JMxzrpExb0v9f8YE-cIHq91oyTRA-FD34Nh4dZ7cMIqbgU6qyOJhs3x12Gl21P3nS11bZxzDPZWLp44pFb7rMqS0IKDtQllqrjQrpjWL_9SpHDI-MQilTFlB3YJc9wMyDAxYUZHNmp0TkdaZkXs01ItKF8susgkXvIPrlUrKpqAs87ZCvYb9Qjxisqp-T9Uft6gE3w6NqLYA80RfIxkDUe2ObVtguRkZtaIFjuZddyFQpNexmc4XbAeMCjmEq4nGr000_BBMT0-yy5nMEHMzcCLY5V90ymeeMeT6iAZztoIBssJQpG1H3Y43R54--Ri1GhJ1SwJyhZYeXAeY4JoFArx2PiVNwQ8i2OnGsKTPhz5bY2Yj6gC1jRLsyqm3xEbWEGv40zWlZIhFp8uClS5xskr1isk1ut_SutAv3sI0lMjvSS4vSRA4sihNjEjw3LPeY_XZrc1uI5DCHgTG5U91BkJAxuAZbOAjxpLes59cGU5bZFO23Y458mcUgcMvhCnlgLXYUSSiUpCuHcXwR8gFUefeD92fjjL41mVVLJyHYA9DYlbyMj1fMpeq0_Xq8s8J-f9uGHuqjmYKIXEY3vPYwBKfMXHETxhCUb4mICY1sq6lsevdbC9COTaGqw1sBT7Gcx6Q5SP9kARy63s9EpthtskL27QeEksJBfEyS52F9z6d2xC6pIKmddP3e36rkx-OMBta-Dz9EG74ZIgSQKIcXqaPnQk49vExiZ1dGwbOxfe8pwvy_6wckSIgB8nbTXCCOTlEH4hZ5xn28kI_Mu65HNmerRGE8WoaULE77pYA5UPkRXVw4fdQ1WkwsOTS-VLpVc.1759357297706.2592000000.i3Uhcd6xHEBxHQyLw19ajutZNSgJagLRkIPeYBeouSI",
  FCNEC = '[["AKsRol9cPiyWt0ZEcLU0Te3cA1aP_4ASLDIT6E8tU2_SAUOpOIsN0XcDONJxQ2_ljtKGUrJwNJpb_cvU7YeAOs8x5OBrTZ1VGyE91Be6yvpDKeywM_OaV5iL3q2hbg8y2iM2zLqKl0jFrYYDQxkOUuizOuwPTW3Myg=="]]'
)

headers = c(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:143.0) Gecko/20100101 Firefox/143.0",
  Accept = "*/*",
  `Accept-Language` = "en-US,en;q=0.5",
  `Accept-Encoding` = "gzip",
  Referer = "https://www.fotmob.com/leagues/130/overview/mls",
  `x-mas` = "eyJib2R5Ijp7InVybCI6Ii9hcGkvZGF0YS9sZWFndWVzP2lkPTEzMCZjY29kZTM9VVNBX1RYIiwiY29kZSI6MTc1OTM1NzMxNjk4MiwiZm9vIjoicHJvZHVjdGlvbjo1NzNhZDYzMDMzNTE5NDdlMDJiMTFkNWEwM2JhNTYxNTAwOTVmZWY5In0sInNpZ25hdHVyZSI6IkZCNzI4MjVGN0Q5RTZFMDJGMUQ1NzUyNkZGMTZGQkRDIn0=",
  Connection = "keep-alive",
  `Sec-Fetch-Dest` = "empty",
  `Sec-Fetch-Mode` = "cors",
  `Sec-Fetch-Site` = "same-origin"
)

params = list(
  id = "130",
  ccode3 = "USA_TX"
)

resp <- httr::GET(url = "https://www.fotmob.com/api/data/leagues", httr::add_headers(.headers=headers), query = params, httr::set_cookies(.cookies = cookies))
cont <- httr::content(resp, as = 'text')
result <- jsonlite::fromJSON(cont)
table_init <- result$table$data
tables <- dplyr::bind_rows(table_init$tables)
fotmob_standings <- tables |>
  as_tibble() |> 
  slice(1:2) |> 
  as_tibble() |> 
  select(conference = leagueName, table) |> 
  unnest_wider(table) |> 
  select(conference, xg) |> 
  # bind_rows() |> 
  # as_tibble() |> 
  # select(xg) |> 
  unnest_longer(xg) |> 
  unnest_longer(xg) |> 
  unnest_wider(xg) |> 
  dplyr::transmute(
    team = name,
    team_id = id,
    conference,
    logo_url = sprintf('https://images.fotmob.com/image_resources/logo/teamlogo/%s.png', id),
    idx,
    pts,
    gd = goalConDiff,
    played,
    wins,
    draws,
    xpts = xPoints, 
    xgd = xg - xgConceded
  )

qs::qsave(fotmob_standings, file.path(DATA_DIR, 'fotmob_standings.qs'))
