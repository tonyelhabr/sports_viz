
club_rankings_date <- '2023-04-01'
baseline_caption <- '***Updated**: 2023-04-01.*'
generate_club_rankings_url <- function(x) {
  sprintf(
    'https://github.com/tonyelhabr/club-rankings/releases/download/club-rankings/%s-rankings.csv',
    x
  )
}

generate_logo_url <- function(id) {
  sprintf('https://omo.akamai.opta.net/image.php?secure=true&h=omo.akamai.opta.net&sport=football&entity=team&description=badges&dimensions=150&id=%s', id)
}

palette <- c(
  '538' = '#ED713B',
  'Opta' = '#7B1582' # '#00ADEF'
)

label_538 <- glue::glue('<b><span style="color:{palette[["538"]]}">538</span></b>')
label_opta <- glue::glue('<b><span style="color:{palette[["Opta"]]}">Opta</span></b>')
