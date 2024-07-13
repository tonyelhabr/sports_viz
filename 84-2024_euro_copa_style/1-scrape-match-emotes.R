library(dplyr)
library(tidyr)
library(purrr)
library(janitor)

library(httr)
library(fotmob) ## tonyelhabr/fotmob

library(qs)

PROJ_DIR <- '84-2024_euro_copa_style'
source(file.path(PROJ_DIR, 'helpers.R'))
EMOTES_DATA_DIR <- file.path(PROJ_DIR, 'data', 'emotes')
dir.create(EMOTES_DATA_DIR, showWarnings = FALSE, recursive = TRUE)

raw_matches <- fotmob::fotmob_get_league_matches(league_id = 44, season = 2024) |> 
  janitor::clean_names()

matches <- raw_matches |> 
  dplyr::select(match_id = id, home, away, status) |>
  tidyr::unnest_wider(c(home, away, status), names_sep = '_')

completed_matches <- matches |> 
  dplyr::filter(status_finished)

HEADERS = c(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:127.0) Gecko/20100101 Firefox/127.0",
  `Accept` = "application/json",
  `Accept-Language` = "en-US,en;q=0.5",
  `Accept-Encoding` = "gzip, deflate, br, zstd",
  `Referer` = "https://www.fotmob.com/",
  `x-spotim-page-view-id` = "47742a5d-3100-4829-8c7c-2673f0c3c47d",
  `x-spotim-device-uuid` = "6575b185-8c7b-45ff-9085-5332ba09c963",
  `x-spotim-device-v2` = "d_taBGIaozD9es1HqSFlC8",
  `x-access-token` = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6IiIsInZlcmlmaWVkIjpmYWxzZSwidXNlcl9pZCI6InVfNGxOSHR5ZVhLVlJmIiwiZGlzcGxheV9uYW1lIjoiR3JlZW5Ub290aCIsInVzZXJfbmFtZSI6IkdyZWVuVG9vdGgiLCJyZWdpc3RlcmVkIjpmYWxzZSwiaW1hZ2VfaWQiOiIjR3JlZW4tVG9vdGgiLCJyb2xlcyI6W10sInNzb19kYXRhIjpudWxsLCJwcm92aWRlcnMiOm51bGwsInJlcHV0YXRpb24iOnt9LCJsb2NhdGlvbiI6IiIsImlzX21vZGVyYXRpb25fdmlld2VyIjpmYWxzZSwic3BvdF9pZCI6InNwX2c4MllTWWlRIiwibGFzdF9jaGVjayI6MTcyMDEwNDQ5MSwidmVyc2lvbiI6MiwieC1zcG90aW0tdG9rZW4iOiIwMTI0MDcwNElBSldFci40NzI4YmVkZDViNDAxMTEyM2MzMjJkMDg5YjA0YjEzNmE5N2FjNTY4ZWYxZDJlMDQ5ODVhODQwNDQ2OWExMjY5IiwicGVybWlzc2lvbnMiOm51bGwsInNwb3RpbS1kZXZpY2UtdjIiOiJkX3RhQkdJYW96RDllczFIcVNGbEM4IiwibmV0d29yayI6eyJuZXR3b3JrX2lkIjoibmV0X2ZvdG1vYl9zc28iLCJuZXR3b3JrX25hbWUiOiJmb3Rtb2Jfc3NvIiwibmV0d29ya19pbWFnZV9pZCI6IjliZjJjMmJhZmRlZjhhMDRiNDVkODQ3ODhlZmM1YTg1IiwibmV0d29ya19jb2xvciI6IiJ9LCJzcG90X25hbWUiOiIiLCJkb21haW4iOiIiLCJyb2xlc19udW1iZXIiOjAsInRlbXBfdXNlciI6ZmFsc2UsImV4cCI6MTc0ODUyNjQ5MSwic3ViIjoidV80bE5IdHllWEtWUmYifQ.Pf8jLW1lsBq3JpdCRoCbP1EnrQPpM3um8q_6nVH0D8A",
  `Content-Type` = "application/json",
  `x-spot-id` = "sp_g82YSYiQ",
  # `x-post-id` = "4377207", ## "4377209",
  `Origin` = "https://www.fotmob.com",
  `Connection` = "keep-alive",
  `Sec-Fetch-Dest` = "empty",
  `Sec-Fetch-Mode` = "cors",
  `Sec-Fetch-Site` = "cross-site",
  `Priority` = "u=6",
  `TE` = "trailers"
)

get_match_reactions <- function(match_id) {
  headers <- c(
    HEADERS,
    `x-post-id` = as.character(match_id)
  )
  resp <- httr::GET(
    url = 'https://api-2-0.spot.im/v1.0.0/reactions/stats/default',
    httr::add_headers(.headers = headers)
  )
  httr::content(resp)
}


get_and_save_match_reactions <- function(match_id) {
  manage_io_operations(
    get_match_reactions,
    match_id = match_id,
    .name = match_id
  )
}

extract_emote_scores <- function(x) {
  x |> 
    purrr::pluck('stats', 'all') |> 
    purrr::map(
      \(.x) {
        .x$score
      }
    )
}

match_emote_scores <- purrr::map(
  completed_matches$match_id,
  \(.x) {
    res <- get_and_save_match_reactions(.x) |> 
      extract_emote_scores()
    append(
      list(match_id = .x),
      res
    )
  }
) |> 
  dplyr::bind_rows() |> 
  tidyr::pivot_longer(
    -c(match_id),
    names_to = 'emote',
    values_to = 'n'
  ) |> 
  dplyr::mutate(n = dplyr::coalesce(n, 0L))
match_emote_scores

match_emote_props <- match_emote_scores |> 
  dplyr::group_by(match_id) |> 
  dplyr::mutate(
    prop = n / sum(n)
  ) |> 
  dplyr::ungroup() |> 
  tidyr::pivot_wider(
    names_from = emote,
    values_from = c(n, prop)
  ) |> 
  dplyr::mutate(
    total = n_angry + n_happy + n_sad + n_surprised,
    .after = match_id
  )

# angry_props <- match_emote_scores |> 
#   dplyr::group_by(match_id) |> 
#   dplyr::summarize(
#     total = sum(n),
#     angry_n = sum((emote == 'angry') * n)
#   ) |> 
#   dplyr::ungroup() |> 
#   dplyr::mutate(
#     angry_prop = angry_n / total
#   ) |> 
#   dplyr::arrange(dplyr::desc(angry_prop))

completed_matches |> 
  dplyr::select(
    match_id,
    home_name,
    away_name
  ) |> 
  dplyr::inner_join(
    match_emote_props,
    by = dplyr::join_by(match_id)
  ) |> 
  dplyr::arrange(dplyr::desc(prop_angry)) |> 
  head(20)

