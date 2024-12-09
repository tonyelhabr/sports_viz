library(tidyverse)
library(lubridate)
library(worldfootballR)
library(jsonlite)
library(janitor)
library(gt)
library(gtExtras)

dir_proj <- '60-card_reasons'
dir_data <- file.path(dir_proj, 'data')
dir.create(dir_data, showWarnings = FALSE)

fotmob_api_url <- 'https://www.fotmob.com/api'
scrape_ticker <- function(match_id, overwrite = FALSE) {
  
  path <- file.path(dir_data, sprintf('%s.json.gz', match_id))
  if (file.exists(path)) {
    if (isFALSE(overwrite)) {
      cli::cli_inform('Returning early for {match_id}.')
      return(path)
    } else {
      cli::cli_inform('Creating a backup for {match_id}.')
      new_path <- file.path(dirname(path), sprintf('%s_%s.%s', tools::file_path_sans_ext(basename(path)), strftime(Sys.time(), '%Y%m%d%H%M%S'), tools::file_ext(path)))
      fs::file_copy(path, sprintf(new_path))
    }
  }
  Sys.sleep(0.5)
  cli::cli_inform('Scraping data for {match_id}.')
  url <- sprintf('%s/matchDetails?matchId=%s', fotmob_api_url, match_id)
  resp <- jsonlite::fromJSON(url)
  ticker_url <- sprintf('http://%s', resp$content$liveticker$url)
  download.file(ticker_url, destfile = path, quiet = TRUE)
  path
}
slowly_scrape_ticker <- slowly(scrape_ticker, rate = rate_delay(pause = 0.5), quiet = FALSE)

read_json_gz <- function(path) {
  suppressWarnings(lines <- path |> gzfile() |> readLines())
  jsonlite::fromJSON(lines)
}
possibly_read_json_gz <- possibly(read_json_gz, otherwise = list('Events' = tibble()), quiet = FALSE)

country_abbs <- c('ENG', 'FRA', 'GER', 'ITA', 'ESP', 'USA')
league_id_mapping <- tibble(
  league_id = c(
    c(47,  53, 54,  55,  87, 130),
    c(48, 110, 146, 86, 140, 8972)
  ),
  country = rep(country_abbs, 2),
  tier = c(rep('1st', length(country_abbs)), rep('2nd', length(country_abbs))),
  league_name = c(
    c('Premier League', 'Ligue 1', 'Bundesliga', 'Serie A', 'La Liga', 'MLS'),
    c('Championship', 'Ligue 2', 'Bundesliga 2', 'Serie B', 'La Liga 2', 'USL')
  ),
  logo_file = c(
    c('epl', 'ligue-1', 'bundesliga', 'serie-a', 'la-liga', 'mls'),
    c('efl', 'ligue-2', 'bundesliga-2', 'serie-b', 'la-liga-2', 'usl')
  )
)

league_start_dates <- league_id_mapping |> 
  mutate(
    data = map2(
      country, tier,
      ~load_match_results(
        country = ..1, 
        gender = 'M', 
        season_end_year = 2021, 
        tier = ..2
      )
    )
  ) |>
  unnest(data) |> 
  group_by(league_id) |> 
  slice_min(Date, n = 1, with_ties = FALSE) |> 
  select(league_id, date = Date)

league_start_date_mapping <- setNames(league_start_dates$date, league_start_dates$league_id)

scrape_tickers_for_league <- function(league_id, overwrite = FALSE) {
  path <- file.path(dir_data, sprintf('%s.qs', league_id))
  
  if (file.exists(path)) {
    if (isFALSE(overwrite)) {
      cli::cli_inform('Returning early for {path}.')
      return(qs::qread(path))
    } else {
      cli::cli_inform('Creating a backup for {path}.')
      new_path <- file.path(dirname(path), sprintf('%s_%s.%s', tools::file_path_sans_ext(basename(path)), strftime(Sys.time(), '%Y%m%d%H%M%S'), tools::file_ext(path)))
      fs::file_copy(path, sprintf(new_path))
    }
  }
  
  first_date <- as.Date(league_start_date_mapping[[as.character(league_id)]])
  
  matches_by_date <- load_fotmob_matches_by_date(league_id = league_id)
  filt_matches_by_date <- matches_by_date |> 
    filter(date >= first_date)
  
  tickers <- filt_matches_by_date |> 
    distinct(league_id = primary_id, match_id) |> 
    mutate(
      data = map(
        match_id,
        ~scrape_ticker(.x) |> 
          possibly_read_json_gz() |> 
          pluck('Events')
      )
    ) |> 
    unnest(data) |> 
    clean_names() |> 
    as_tibble() |> 
    select(league_id, match_id, incident_code, elapsed, elapsed_plus, description, event_id)
  qs::qsave(tickers, path)
  tickers
}

tickers <- league_id_mapping |> 
  pull(league_id) |> 
  map_dfr(~scrape_tickers_for_league(.x, overwrite = FALSE)) |> 
  mutate(
    idx = row_number(),
    .before = 1
  )

card_incident_codes <- c('YC', 'RC', 'Y2C')
card_events <- tickers |> 
  filter(incident_code %in% card_incident_codes) |> 
  inner_join(
    league_id_mapping,
    by = 'league_id'
  )

words <- card_events |> 
  tidytext::unnest_tokens('word', description) |> 
  anti_join(tidytext::stop_words, by = 'word') |> 
  mutate(
    stem = word |> SnowballC::wordStem()
  )

ns <- tickers |> 
  group_by(league_id) |> 
  summarize(
    n_matches = n_distinct(match_id),
    n_cards = sum(incident_code %in% card_incident_codes)
  )

.gt_theme_538 <- function(data, ...) {
  data |>
    gt::opt_table_font(
      font = list(
        gt::google_font('Titillium Web'),
        gt::default_fonts()
      )
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = 'bottom', color = 'transparent', weight = gt::px(3)
      ),
      locations = gt::cells_body(
        columns = everything(),
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  |> 
    gt::tab_options(
      column_labels.background.color = 'white',
      heading.border.bottom.style = 'none',
      table.border.top.width = gt::px(3),
      table.border.top.style = 'none',
      table.border.bottom.style = 'none',
      column_labels.font.weight = 'normal',
      column_labels.border.top.style = 'none',
      column_labels.border.bottom.width = gt::px(2),
      column_labels.border.bottom.color = 'black',
      row_group.border.top.style = 'none',
      row_group.border.top.color = 'black',
      row_group.border.bottom.width = gt::px(1),
      row_group.border.bottom.color = 'white',
      stub.border.color = 'white',
      stub.border.width = gt::px(0),
      data_row.padding = gt::px(0), # gt::px(0)
      source_notes.font.size = 10,
      source_notes.border.lr.style = 'none',
      table.font.size = 12, # 15
      heading.align = 'left',
      
      column_labels.font.size = 12,
      heading.padding = gt::px(0),
      column_labels.padding = gt::px(0),
      
      ...
    )
}

plot_frequency <- function(.stems, .label = .stems[1], title = NULL, subtitle = NULL, palette = 'ggsci::red_material') {
  interesting_stems <- words |> 
    filter(stem %in% .stems) |> 
    distinct(idx, league_id, stem)
  
  interesting_ns <- tickers |> 
    semi_join(
      interesting_stems,
      by = 'idx'
    ) |> 
    group_by(league_id) |> 
    summarize(
      n_interesting = n()
    )
  
  df <- ns |> 
    inner_join(
      interesting_ns,
      by = 'league_id'
    ) |> 
    mutate(
      cards_per_match = n_cards / n_matches,
      interesting_cards_per_match = n_interesting / n_matches,
      prop = interesting_cards_per_match / cards_per_match,
      rnk = row_number(desc(prop))
    ) |> 
    inner_join(
      league_id_mapping,
      by = 'league_id'
    ) |> 
    relocate(league_id, country, tier) |> 
    arrange(-interesting_cards_per_match)
  col_label <- glue::glue('With <b>"{.label}"</b><br/>in description')
  
  interesting_decimals <- ifelse(max(df$interesting_cards_per_match) > 1, 1, 2)
  prop_decimals <- ifelse(max(df$prop) > 0.1, 1, 2)

  tb <- df |> 
    transmute(
      rnk,
      path = file.path(dir_proj, 'logos', sprintf('%s.png', logo_file)),
      country, tier, league_name,
      cards_per_match,
      across(
        interesting_cards_per_match,
        ~ifelse(
          round(.x, interesting_decimals) == 0, 
          sprintf('<0.%s1', ifelse(interesting_decimals == 2, '0', '')), 
          round(.x, interesting_decimals)
        )
      ),
      prop
    ) |> 
    arrange(rnk) |> 
    gt::gt() |> 
    gt::cols_label(
      rnk = '#',
      path = 'League',
      country = ' ',
      interesting_cards_per_match = gt::html(col_label),
      cards_per_match = 'Total',
      prop = ' '
    ) |> 
    .gt_theme_538() |> 
    gt::cols_merge(
      columns = c(country, tier, league_name)
    ) |> 
    text_transform(
      locations = gt::cells_body(columns = country),
      fn = function(x) {
        country <- word(x, 1)
        tier <- word(x, 2)
        league_name <- str_remove(x, sprintf('%s %s', country, tier))
        glue::glue("{league_name} <span style='font-size:0.7em; color:#777'>{country}, {tier}</span>")
      }
    ) |> 
    gt::text_transform(
      locations = gt::cells_body(columns = path),
      fn = function(x) {
        gt::local_image(
          filename = x,
          height = 25
        )
      }
    ) |>
    gt::tab_spanner(
      columns = c(interesting_cards_per_match, cards_per_match),
      label = 'Cards per match'
    ) |> 
    gt::cols_align(
      columns = path,
      align = 'center'
    ) |> 
    gt::cols_align(
      columns = interesting_cards_per_match,
      align = 'right'
    ) |> 
    gt::fmt_number(
      columns = cards_per_match,
      decimals = 1
    ) |>
    gt::fmt_percent(
      columns = prop,
      decimals = prop_decimals
    ) |> 
    gt::tab_source_note(
      source_note = gt::html('<i><b>Data</b>: Matches since beginning of 2020/21 season. (2021 season for U.S. leagues.)</i>')
    ) |> 
    gt::tab_header(
      title = gt::md(glue::glue('**{title}**')),
      subtitle = gt::md(glue::glue('Frequency of "**{.label}**" in text descriptions of yellow and red cards in match news feed.'))
    ) |> 
    gtExtras::gt_color_rows(
      columns = prop,
      palette = palette
    )

  gt::gtsave(
    tb,
    vwidth = 450,
    zoom = 2,
    filename = file.path(dir_proj, sprintf('%s.png', .label))
  )
  invisible(tb)
}

plot_frequency(
  'hand', 
  title = "If you ain't cheating, you ain't trying", 
  palette = 'ggsci::amber_material'
)
plot_frequency(
  'violent', 
  title = 'USL? More like UFC', 
  palette = 'ggsci::pink_material'
)
plot_frequency(
  'bad',
  title = 'Tier 2 leagues got that dawg in them', 
  palette = 'ggsci::cyan_material'
)
plot_frequency(
  c('wast', 'timewast'), 
  'timewasting', 
  title = 'EPL referees = low tolerance for BS',
  palette = 'ggsci::purple_material'
)
