library(qs)
library(purrr)
library(fotmob)

DATA_DIR <- file.path(PROJ_DIR, 'data')
LEAGUE_MATCHES_DATA_DIR <- file.path(DATA_DIR, 'league_matches')
purrr::walk(
  c(
    LEAGUE_MATCHES_DATA_DIR
  ),
  \(.x) {
    dir.create(.x, showWarnings = FALSE, recursive = TRUE)
  }
)

manage_io_operations <- function(
    f, 
    ...,
    .name, 
    .data_dir,
    .overwrite = FALSE, 
    .sleep = TRUE,
    .delay_range = c(1, 2)
) {
  
  path <- file.path(.data_dir, paste0(.name, '.qs'))
  
  if (file.exists(path) & isFALSE(.overwrite)) {
    return(qs::qread(path))
  }
  if (isTRUE(.sleep)) {
    Sys.sleep(runif(1, .delay_range[1], .delay_range[2]))
  }
  message(sprintf('%s: Getting %s.', Sys.time(), .name))
  res <- f(...)
  qs::qsave(res, path)
  res
}


get_and_save_league_matches <- function(league_id, ...) {
  manage_io_operations(
    fotmob::fotmob_get_league_matches,
    league_id = league_id,
    ...,
    # .overwrite = TRUE,
    .name = league_id,
    .data_dir = LEAGUE_MATCHES_DATA_DIR
  )
}

get_and_save_completed_euro_copa_matches <- function() {
  raw_matches <- c(
    'Copa America 2024' = 44, 
    'EURO 2024' = 50
  ) |> 
    purrr::imap(
      \(.x, .y) {
        res <- get_and_save_league_matches(
          league_id = .x, 
          season = 2024,
          .overwrite = FALSE
        )
        dplyr::mutate(
          res,
          competition_name = .y
        )
      }
    ) |> 
    dplyr::bind_rows() |> 
    janitor::clean_names()
  
  matches <- raw_matches |> 
    dplyr::select(match_id = id, home, away, status) |>
    tidyr::unnest_wider(c(home, away, status), names_sep = '_')
  
  matches |> 
    dplyr::filter(status_finished)
}
