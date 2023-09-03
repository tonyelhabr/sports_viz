#' Load pre-saved FBref match summary data
#'
#' Loading version of \code{fb_match_summary}. Only some leagues available.
#'
#' @inheritParams load_match_results
#' @importFrom purrr possibly map_dfr
#' @importFrom cli cli_alert
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @return returns a dataframe
#' @examples
#' \donttest{
#' try({
#' load_fb_match_summary(
#'   country = "ENG",
#'   gender = "M",
#'   tier = "1st"
#' )
#'
#' load_fb_match_summary(
#'   country = c("ITA", "ESP"),
#'   gender = "M",
#'   tier = "1st",
#'   season_end_year = 2019
#' )
#' })
#' }
#' @export
load_fb_match_summary <- function(country, gender, tier, season_end_year = NA) {

  urls <- sprintf(
    "https://github.com/JaseZiv/worldfootballR_data/releases/download/fb_match_summary/%s_%s_%s_match_summary.rds",
    country,
    gender,
    tier
  )
  
  res <- purrr::map_dfr(urls, worldfootballR:::.file_reader)
  
  if(nrow(res) == 0) {
    cli::cli_alert("Data not loaded. Please check parameters.")
    return(res)
  } else {
    cli::cli_alert("Data last updated {attr(res, 'scrape_timestamp')} UTC")
  }
  
  if (!all(is.na(season_end_year))) {
    res <- res %>%
      dplyr::filter(.data[["Season_End_Year"]] %in% season_end_year)
  }
  
  res
  
}

#' Load pre-saved FBref match summary data
#'
#' Loading version of \code{fb_match_summary}. Only some leagues available.
#'
#' @inheritParams load_match_results
#' @inheritParams fb_advanced_match_stats
#' @importFrom purrr possibly map_dfr
#' @importFrom cli cli_alert
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @return returns a dataframe
#' @examples
#' \donttest{
#' try({
#' load_fb_match_summary(
#'   country = "ENG",
#'   gender = "M",
#'   tier = "1st"
#' )
#'
#' load_fb_match_summary(
#'   country = c("ITA", "ESP"),
#'   gender = "M",
#'   tier = "1st",
#'   season_end_year = 2019
#' )
#' })
#' }
#' @export
load_fb_advanced_match_stats <- function(country, gender, tier, stat_type, team_or_player, season_end_year = NA) {
  
  urls <- sprintf(
    "https://github.com/JaseZiv/worldfootballR_data/releases/download/fb_match_shooting/%s_%s_%s_advanced_match_stats.rds",
    country,
    gender,
    tier,
    stat_type,
    team_or_player
  )
  
  res <- purrr::map_dfr(urls, worldfootballR:::.file_reader)
  
  if(nrow(res) == 0) {
    cli::cli_alert("Data not loaded. Please check parameters.")
    return(res)
  } else {
    cli::cli_alert("Data last updated {attr(res, 'scrape_timestamp')} UTC")
  }
  
  if (!all(is.na(season_end_year))) {
    res <- res %>%
      dplyr::filter(.data[["Season_End_Year"]] %in% season_end_year)
  }
  
  res
  
}