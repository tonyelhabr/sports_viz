library(dplyr)
library(httr2)

resp <- request("https://www.fotmob.com/api/fixtures") |>
  req_url_query(
    id = "47",
    season = "2024/2025"
  ) |>
  req_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:133.0) Gecko/20100101 Firefox/133.0",
    Accept = "*/*",
    `Accept-Language` = "en-US,en;q=0.5",
    `Accept-Encoding` = "gzip, deflate, br, zstd",
    Referer = "https://www.fotmob.com/leagues/47/overview/premier-league?season=2024-2025",
    `x-mas` = "eyJib2R5Ijp7InVybCI6Ii9hcGkvZml4dHVyZXM/aWQ9NDcmc2Vhc29uPTIwMjQlMkYyMDI1IiwiY29kZSI6MTczMjk3MTEzNjAwNSwiZm9vIjoiZTk3M2ZjNzNmIn0sInNpZ25hdHVyZSI6IkUwRDY4NTgzRTIyQTRFOUNBNTcwMEE4NTk0QkU5RUY0In0=",
    `Alt-Used` = "www.fotmob.com",
    Connection = "keep-alive",
    Cookie = '_ga=GA1.1.685929884.1635478427; _hjSessionUser_2585474=eyJpZCI6IjViYmQyMDc2LTA5NjAtNWJjNy1iMmJlLTQyZTBjZTVhM2MwZCIsImNyZWF0ZWQiOjE2MzkyMzIyNzI1NjcsImV4aXN0aW5nIjp0cnVlfQ==; _ga_G0V1WDW9B2=GS1.1.1732971107.383.1.1732971134.0.0.0; _ga_SQ24F7Q7YW=GS1.1.1720365483.27.0.1720365483.0.0.0; _ga_K2ECMCJBFQ=GS1.1.1720362840.21.0.1720362843.0.0.0; g_state={"i_l":0}; guser=%7B%22name%22%3A%22Tony%20ElHabr%22%2C%22image%22%3A%22https%3A%2F%2Flh3.googleusercontent.com%2Fa%2FACg8ocKh2DgGFrBhKHP9SMprrulRtQRgDWI1muKW8bp0UetEjy5aSfzp%3Ds96-c%22%2C%22id%22%3A%22102041985920062361465%22%2C%22email%22%3A%22anthonyelhabr%40gmail.com%22%7D; _cc_id=54e2801c6b6817ee756a3f3ab439a84b; cto_bundle=CtffN19ZRkczczNBREx0UGZjRDQ0eEJpb1QyJTJCWkY2aUFvbUlQYU8zSHZsb0dnRmhyNGJFN0hDMTRyM29yTlYwZW51SHRBSVJXUXdSa01QNXNUUTdqQyUyRnhqaHpTVHE0bzJhY0VkOHFaR2Z1TG8yVlFrQUNnUkpLUEtGMGlBNE82eExQNFVOdzJtQ3BCOVQyJTJCTkZQN0UzTkZVV1ElM0QlM0Q; __gads=ID=8e8d79495cb23300:T=1729553579:RT=1729553579:S=ALNI_Macdf4iPUX3WMkEAQzZLz_lFPw1xw; __gpi=UID=00000f2f4e363fd6:T=1729553579:RT=1729553579:S=ALNI_MaUJqlZqT_kMMlAIpHil5R2X2z57A; __eoi=ID=f14a8b2eaa62f80f:T=1729553579:RT=1729553579:S=AA-AfjakVCNH79hX0ZrM-Qe3_YRj; u:location=%7B%22countryCode%22%3A%22US%22%2C%22ccode3%22%3A%22USA%22%2C%22timezone%22%3A%22America%2FChicago%22%2C%22ip%22%3A%222603%3A8080%3Af01%3Ae22b%3A2c15%3Abaa8%3Aea34%3A2018%22%2C%22regionId%22%3A%22TX%22%2C%22regionName%22%3A%22Texas%22%2C%22metroCode%22%3A%22635%22%7D; _hjSession_2585474=eyJpZCI6IjY4ODBlNDJmLThmNDItNDExMS1hNWJiLTdmMTE2ZTUzNDM0MSIsImMiOjE3MzI5NzExMDc0MTUsInMiOjAsInIiOjAsInNiIjowLCJzciI6MCwic2UiOjAsImZzIjowLCJzcCI6MX0=; panoramaId_expiry=1733575913718; panoramaId=b1fe2ec7eb94c21ccafd2a3f296f16d5393845b526a4ab95443084d3a1b5ae0b; panoramaIdType=panoIndiv; g_csrf_token=ae486706615e7c7a; user=TVWweUToMgxBXcL18EA7xw.m09-MmqmYYIMC-wXzPJidwdgR6w6x0S2HlQlyW1dOwkh8rLboh5uiNeB-M-oJYsHqmC3BXv-OWTHVZpErN4qE6E_tgfvpcX5ng4sFfcXSt-NP-zNhR-5lqgqTaNdh8u-fjsz_SrorSzVqzjqSNuGI_UGcft7T6teqHJBq-KaOOZt6SXjEcmGbuwIkbLKIYqg3uo8mwWnQvx4Mt5cJcNSj5wdZ8h9Phpe4gRA-7AY3t0wjCA626NW08nk81uQyy2kZ0cbfjCc-42D2MvuSDoi3pkkvkGchNBmiTRKqU-TMT18xPCjNUK-k5_j0ieexaEKB0KMcKkNrOFSP-jU24Eh6GL3iISxhp6A_TeXU2Gee-C5i6ftDkbK9j8XPZYsgR-zHpFxEacqQ_RcvBHZCrZxTrhf4tqWjP5FWZLIcLCoqi01ytqrXQELl2mw8F9_wHl-1mM6WCt0mGD0fBJSsP3wJxXnxy9fjQbt9QCLyDBr4PYXQ689dmd2ItpML01kBGZvqcadF-J1i12bWPlnQrQ20FfmItbQXqE4WTxVtwPiaBRSgSJor3kEiZ3tsiTiSJVXx6TcZlhgsRbO25EVltGAYMVrXy2LRm4pvvKhzehu5HXyKaQLpQtVZhXGtF5oneTbGUL3I6OiX1r1usQEgmOAHeeGzgy3eCWX03gq_ACTjYgJWS6gWHJGyRH-qzMao60p_OGjaM9uh_-VBqIHaCtbf2v3oOui9zPUyQliZwvNxFdBKTN5PbGm8BfeG_XcJHHYybtxT3b60J3b1x_BpJccZCljCOni_I-XYZXDwQdBU8Pzypjb7_JhkMfE8JwGVwypVulE0XOq7S6kzLUZnzhS2NyNGabdQ7oSbzycB3R-_--1_NYZ2e0akptAqnGlCXU5mCtEv3XEElq-BAXPTVwosgw-T21sPv-k83rZFSqU-TwdxtwLSlBtL_hJ3r-kqcsbRxIatbG2Sqm5zBi6AjKMLHCQnboyh7S1oCu7zDrqNFyiAfMmqzvKQwKZPv2WGhsul-UTgqm_JvTEIJbIr9o_0KaBvc6wVbW9t3a1u0AAil_z8rwoLO4_LdG26BKRUvreCjKxYyM5IRhlN7O774m1pGK8SkJvllFIyPQjKuG50UlLGmkc1tnTatvm6RrvhnkZf_cCC4KJl2HnTn58S9cpgBqjTlLLNMrg8VhXHm46mwr102nNRChPbLwgzcRsmkeAjRdB_K1CfKVwDt9FFL5bGPKX9eOEGigUon23TF01BjaekI6IP9ayQJZ2GOEA05zBae7_x2vdOchpJpQDaySZWvtVAyQFW2DSL5GEb8DFqKjUiU1USBXkbt_jsl4341rOBArRJ5aW1wb0azTzj87EO5FPcwWrhsn5_WcS5EassumYE1XnKhbntuiVap_FH_AO0V0GRlB0xhBHeCoUWSqD8A5nUUbKnUdF6iFkeUcCRLcR6P3O0w1hjzKkXNsWAKg4PxsDqFglYjrtX12stEaOZS8v1bJDVG5o0GcwmolF1GlP8leioLK-wuSUv5l-20euVZqxtyYppow_vtyrCU8AXHfDXNadEOUFWrBca9DsA5y0Cg259Gi_PLtk1EA8ZYqy5Yrzi-Jq99BILDc8jxdgvPWIcikAHTfHrsZjwUx7Lm-X_stN_VaziFGtv7wTPTScrZFhwtZVcuj1106XNxDt_TM69io1QB_NT-fpm9XSlwx9QOzDhkAAibfXsqqUcUOxxyYdVsibPEmxg3UICiHp0XVk9uPTeXUT4gS6HdLt4WSfoeG5O7mfktZdb6jk5OcybuuehVHI0i032dbWRoG9uwaDbuEUgL6yI8RBMitsgY_4VP7RtP-B6O37Y_w-F-E4Aldr8QoYFOwcWQ22nMqhtRrAOpNVuEXlWj3FrG_SsE3cC3MbLv_Z8df4ek6PblIPMfuf5N3VQ9vLdH7a6u2bJLQUjnzRdj6BWiW3G4tSEjk.1732971119048.2592000000.0JGwUR-NMETzyez3pGHvgnnyhpRsNU6F4GqKhUFzqiQ; FCNEC=%5B%5B%22AKsRol97C__44fJ_AVybx3SIl_HA04r0SMWCGrObVuOvocDZpGofkDyZ0iCETv_kjdHX9ZJQM-yGYhVNV0utU3gTfmjCQ8ZTx1A1eLpKxoohgkFcMaRDzCwE8yj_yGNBM13LbbbvFYXXpGM8iTuuImk8I0ehr1xAng%3D%3D%22%5D%5D',
    `Sec-Fetch-Dest` = "empty",
    `Sec-Fetch-Mode` = "cors",
    `Sec-Fetch-Site` = "same-origin"
  ) |>
  req_perform()
cont <- resp |> resp_body_json()

raw_fixtures <- tibble::enframe(cont, name = 'row', value = 'value') |> 
  tidyr::unnest_wider(value) |> 
  tidyr::unnest_wider(c(opponent, home, away, status), names_sep = '_') |> 
  janitor::clean_names()
raw_fixtures
raw_fixtures |> glimpse()
finished_raw_fixtures <- raw_fixtures |> 
  filter(status_finished == TRUE)
# team_id = dplyr::if_else(opponent_id == away_id, home_id, away_id),
# team_name = dplyr::if_else(opponent_id == away_id, home_name,  away_name),
home_away_fixtures <- finished_raw_fixtures |> 
  dplyr::transmute(
    match_id = as.integer(id),
    home_id = as.integer(home_id),
    home_name,
    away_id = as.integer(away_id),
    away_name,
    home_g = home_score,
    away_g = away_score
  )

restacked_fixtures <- dplyr::bind_rows( 
  dplyr::transmute(
    home_away_fixtures,
    match_id,
    pov = 'home',
    team_id = home_id,
    team_name = home_name,
    opponent_id = away_id,
    opponent_name = away_name,
    team_g = home_g,
    opponent_g = away_g
  ),
  dplyr::transmute(
    home_away_fixtures,
    match_id,
    pov = 'away',
    team_id = away_id,
    team_name = away_name,
    opponent_id = home_id,
    opponent_name = home_name,
    team_g = away_g,
    opponent_g = home_g
  )
) |> 
  dplyr::mutate(
    pts = dplyr::case_when(
      team_g > opponent_g ~ 3L,
      team_g < opponent_g ~ 0L,
      team_g == opponent_g  ~ 1L
    )
  ) |> 
  dplyr::arrange(match_id, pov)

counterfactual_restacked_fixtures <- restacked_fixtures |> 
  dplyr::mutate(
    is_one_score = abs(team_g - opponent_g) == 1,
    counterfactual_team_g = dplyr::if_else(is_one_score, opponent_g, team_g),
    counterfactual_opponent_g = dplyr::if_else(is_one_score, team_g, opponent_g),
    counterfactual_pts = dplyr::case_when(
      counterfactual_team_g > counterfactual_opponent_g ~ 3L,
      counterfactual_team_g < counterfactual_opponent_g ~ 0L,
      counterfactual_team_g == counterfactual_opponent_g  ~ 1L
    )
  )

calculate_table <- function(fixtures) {
  fixtures |> 
    dplyr::group_by(team_id, team_name) |> 
    dplyr::summarize(
      mp = dplyr::n(),
      gf = sum(team_g),
      ga = sum(opponent_g),
      pts = sum(pts)
    ) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      gd = gf - ga,
      rnk = dplyr::row_number(
        100000 * dplyr::desc(pts) + 1000 * dplyr::desc(gd) + dplyr::desc(gf)
      )
    ) |> 
    dplyr::arrange(rnk)
}

actual_table <- restacked_fixtures |> calculate_table()
counterfactual_table <- counterfactual_restacked_fixtures |> 
  dplyr::transmute(
    match_id,
    team_id,
    team_name,
    team_g = counterfactual_team_g,
    opponent_g = counterfactual_opponent_g,
    pts = counterfactual_pts
  ) |> 
  calculate_table()

both_tables <- dplyr::inner_join(
  actual_table |> 
    dplyr::select(
      team_id,
      team_name,
      actual_pts = pts,
      actual_rnk = rnk
    ),
  counterfactual_table |> 
    dplyr::select(
      team_id,
      team_name,
      counterfactual_pts = pts,
      counterfactual_rnk = rnk
    ),
  dplyr::join_by(team_id, team_name)
)
qs::qsave(both_tables, file.path(PROJ_DIR, 'both_tables.qs'))
