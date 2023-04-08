library(dplyr)
library(readr)
library(glue)
library(magick)

generate_club_rankings_url <- function(x) {
  sprintf(
    'https://github.com/tonyelhabr/club-rankings/releases/download/club-rankings/%s-rankings.csv',
    x
  )
}

generate_logo_url <- function(id) {
  sprintf('https://omo.akamai.opta.net/image.php?secure=true&h=omo.akamai.opta.net&sport=football&entity=team&description=badges&dimensions=150&id=%s', id)
}

club_rankings_palette <- c(
  '538' = '#ED713B',
  'Opta' = '#7B1582' # '#00ADEF'
)

label_538 <- glue::glue('<b><span style="color:{club_rankings_palette[["538"]]}">538</span></b>')
label_opta <- glue::glue('<b><span style="color:{club_rankings_palette[["Opta"]]}">Opta</span></b>')

compared_rankings <- generate_club_rankings_url('compared') |> 
  read_csv() |> 
  mutate(
    across(league_538, ~coalesce(league_alternative, .x))
  ) |> 
  select(-league_alternative) |> 
  mutate(
    drank = rank_538 - rank_opta
  )

compared_latest_rankings <- compared_rankings |> 
  # filter(date == .env$club_rankings_date)
  slice_max(date, n = 1, with_ties = TRUE)

club_rankings_date <- sprintf('%s (morning)', compared_latest_rankings$date[1])
baseline_caption <- sprintf('*Updated at %s.*', club_rankings_date)

widen_image <- function(path, ratio = NULL, ratio_resolution = 0.25, height_resolution = 100) {
  img <- magick::image_read(path)
  info <- magick::image_info(img)
  w <- info$width
  h <- info$height
  r <- h / w
  new_ratio <- if (is.null(ratio)) {
    ratio_resolution * ceiling(ratio / ratio_resolution)
  } else {
    if (ratio > r) {
      stop(
        sprintf('`ratio` must be smaller than %s', round(r, 2))
      )
    }
    ratio
  }
  
  new_h <- height_resolution * ceiling(h / height_resolution)
  new_w <- new_h / new_ratio
  dw <- round((new_w - w) / 2)
  if (dw < 0) {
    stop(
      'New width smaller than original width'
    )
  }
  dh <- round((new_h - h) / 2)
  new_img <- magick::image_border(img, 'white', sprintf('%sx%s', dw, dh))
  magick::image_write(new_img, path)
}

