library(tidyverse)
library(lubridate)
library(worldfootballR)
library(jsonlite)
library(janitor)

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
    c(47, 53, 54, 55, 87, 130),
    c(48,110,146, 86,140,8972)
  ),
  country = rep(country_abbs, 2),
  tier = c(rep('1st', length(country_abbs)), rep('2nd', length(country_abbs))),
  league_name = c(
    c('English Premier League', 'Ligue 1', 'Bundesliga', 'Serie A', 'La Liga', 'MLS'),
    c('EFL Championship', 'Ligue 2', 'Bundesliga 2', 'Serie B', 'La Liga 2', 'USL')
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
  # slice(c(7)) |> 
  pull(league_id) |> 
  map_dfr(~scrape_tickers_for_league(.x, overwrite = FALSE)) |> 
  mutate(
    idx = row_number(),
    .before = 1
  )
tickers

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

word_counts <- words |> 
  count(league_name, word, sort = TRUE)

word_tfidf <- word_counts |> 
  tidytext::bind_tf_idf(term = word, document = league_name, n = n) |> 
  arrange(desc(tf_idf))

word_tfidf |> 
  filter(word %in% names(interesting_stems))
word_tfidf |> 
  filter(word == 'foul')

words |> 
  filter(lead(word) == 'foul') |> 
  count(league_id, word, sort = TRUE) |> 
  head(20)

stem_counts <- words |> 
  count(stem, sort = TRUE)

ns <- tickers |> 
  group_by(league_id) |> 
  summarize(
    n_matches = n_distinct(match_id),
    n_cards = sum(incident_code %in% card_incident_codes)
  )

interesting_stems <- list(
  'foul',
  'tackle',
  # 'ground',
  'celebration' = 'celebr',
  'excessive' = 'excess',
  'dissent' = 'dissent',
  'clumsy' = c('clumsi', 'clumsili'),
  'timewasting' = c('wast', 'timewast'),
  'violent' = 'violent',
  'conduct' = 'conduct'
)

quantify_frequency <- function(stems) {
  interesting_stems <- words |> 
    filter(stem %in% stems) |> 
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
      interesting_cards_per_card = n_interesting / n_cards,
      rnk = row_number(desc(interesting_cards_per_card))
    ) |> 
    inner_join(
      league_id_mapping,
      by = 'league_id'
    ) |> 
    relocate(league_id, country, tier) |> 
    arrange(-interesting_cards_per_card)

  df |> 
    inner_join(
      league_id_mapping,
      by = c('')
    )
    transmute(
      rnk,
      country,
      tier,
      league_name
      cards_per_match,
      interesting_cards_per_card
    )
    gt::gt() |> 
    gt::cols_label(
      rn = '#',
      path = 'League',
      league_name = ' ',
      venue = 'Venue',
      capacity = 'Cap.',
      trunc_attendance_prop = '%'
    ) |> 
    .gt_theme_538() |> 
    gt::text_transform(
      locations = gt::cells_body('path'),
      fn = function(x) {
        gt::local_image(
          filename = x,
          height = 25
        )
      }
    ) 
}

quantify_frequency(c('hand'))
quantify_frequency(c('foul'))
# quantify_frequency(c('tackle'))
quantify_frequency(c('challeng'))
quantify_frequency(c('late'))
quantify_frequency(c('ground'))
quantify_frequency(c('celebr'))
quantify_frequency(c('excess'))
quantify_frequency(c('dissent'))
quantify_frequency(c('clumsi', 'clumsili'))
quantify_frequency(c('wast', 'timewast'))
quantify_frequency(c('conduct'))
quantify_frequency(c('violent'))

## new ----
elapsed_times <- tickers |> 
  group_by(league_id, match_id) |> 
  summarize(
    across(
      c(elapsed, elapsed_plus), 
      ~max(.x, na.rm = TRUE)
    )
  ) |> 
  ungroup() |> 
  mutate(
    across(
      elapsed_plus, 
      ~case_when(
        .x < 0L ~ 0L,
        .x > 10L ~ 10L,
        TRUE ~ .x
      )
    ),
    total_elapsed = elapsed + elapsed_plus
  ) |> 
  filter(elapsed >= 90L)

compute_card_per90_about_threshold <- function(threshold) {
  # threshold <- 85L
  elapsed_after <- elapsed_times |> 
    mutate(diff = total_elapsed - !!threshold) |> 
    group_by(league_id) |> 
    summarize(
      n_matches = n(),
      across(diff, sum)
    )
  
  cards_after <- card_events |> 
    filter(elapsed >= !!threshold) |> 
    count(league_id, league_name) |> 
    inner_join(
      elapsed_after, 
      by = 'league_id'
    ) |> 
    mutate(
      cards_per_min = n / diff
    )
  
  elapsed_before <- elapsed_times |> 
    mutate(diff = !!threshold - 1) |> 
    group_by(league_id) |> 
    summarize(
      n_matches = n(),
      across(diff, sum)
    )
  
  cards_before <- card_events |> 
    filter(elapsed < !!threshold) |> 
    count(league_id, league_name) |> 
    inner_join(
      elapsed_before, 
      by = 'league_id'
    ) |> 
    mutate(
      cards_per_min = n / diff
    )
  
  inner_join(
    cards_before |> 
      transmute(
        league_id, 
        league_name,
        n_matches, 
        n_cards_before = n, 
        cards_per90_min_before = 90 * cards_per_min
      ),
    cards_after |> 
      transmute(
        league_id, 
        n_cards_after = n, 
        cards_per90_min_after = 90 * cards_per_min
      ),
    by = 'league_id'
  ) |> 
    mutate(
      ratio = cards_per90_min_after / cards_per90_min_before
    ) |> 
    mutate(
      rank = row_number(desc(ratio)),
      .before = 1
    ) |> 
    arrange(desc(ratio))
}

card_ratios <- c(45, 60, 75, 80, 85, 86, 87, 88, 89, 90) |> 
  map_dfr(
    ~compute_card_per90_about_threshold(.x) |> 
      mutate(threshold = .x, .before = 1)
  )
card_ratios |> filter(league_id == 8972)
card_ratios |> filter(league_id == 47)

card_elapsed_times <- card_events |> 
  inner_join(
    elapsed_times |> select(league_id, match_id, total_elapsed),
    by = c('league_id', 'match_id')
  ) |> 
  filter(elapsed <= total_elapsed)

card_times_by_league <- card_elapsed_times |> 
  group_by(league_name) |> 
  summarize(
    across(
      elapsed,
      list(
        median = median,
        q75 = ~quantile(.x, 0.75),
        q90 = ~quantile(.x, 0.9)
      )
    )
  ) |> 
  ungroup() |> 
  arrange(desc(elapsed_q75))

card_times_by_league |> 
  pull(league_name)
library(ggridges)
card_elapsed_times |>
  mutate(
    across(league_name, ~ordered(.x, levels = rev(card_times_by_league$league_name)))
  ) |> 
  ggplot() +
  aes(
    x = elapsed, 
    y = league_name, 
    alpha = elapsed,
    fill = stat(x),
  ) +
  geom_density_ridges_gradient(
    scale = 1.5,
    rel_min_height = 0.01,
    quantiles = 4,
    quantile_lines = TRUE
  ) +
  scale_fill_viridis_c(option = 'C', end = 1, begin = 0) +
  coord_cartesian(
    xlim = c(0, 100)
  )
usa_t2_league_id <- 8972
matches_by_date <- load_fotmob_matches_by_date(league_id = usa_t2_league_id)
match_ids <- matches_by_date |> 
  filter(year(date) == 2022) |> 
  distinct(match_id)
matches_by_date |> filter(match_id == 3802716) |> glimpse()
card_events |> 
  semi_join(
    match_ids,
    by = 'match_id'
  ) |> 
  filter(league_id == usa_t2_league_id) |> 
  count(match_id, sort = TRUE)
card_events |> 
  filter(match_id == 3802716)
