library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(janitor)
library(qs)
library(httr)
library(lubridate)
library(countrycode)
library(gt)

dir_proj <- '63-wc2022_player_height'
rosters_path <- file.path(dir_proj, 'rosters.qs')
matches_path <- file.path(dir_proj, 'matches.qs')

if (!file.exists(rosters_path)) {
  table_url <- 'https://www.espn.com/soccer/table/_/league/fifa.world'
  table_page <- rvest::read_html(table_url)
  
  team_hrefs <- table_page |>
    rvest::html_element('table') |>
    rvest::html_elements('tr > td .AnchorLink') |>
    rvest::html_attr('href') |>
    unique() |>
    sort() |> 
    str_replace('team/', 'team/squad/')
  
  scrape_roster <- function(team_href) {
    Sys.sleep(runif(1))
    message(sprintf('Scraping at %s', team_href))
    # team_href <- '/soccer/team/squad/_/id/212/uruguay'
    url <- sprintf('https://www.espn.com%s', team_href)
    page <- read_html(url)
    tbs <- page |> 
      html_table() |> 
      ## uruguay has an empty column names that makes it impossible to do a mutate, even on just one column.
      ##   thus, use clean_names
      map(
        ~.x |> janitor::clean_names() |> mutate(across(everything(), as.character))
      ) |> 
      bind_rows()
  }
  
  rosters <- team_hrefs |>
    map_dfr(scrape_roster)
  qs::qsave(rosters, rosters_path)
} else {
  rosters <- qs::qread(rosters_path)
}

heights <- rosters |>
  filter(as.integer(app) > 0) |> 
  select(country = nat, name, ht) |> 
  mutate(
    across(
      ht, 
      list(
        feet = ~str_remove(.x, "['].*$") |> as.integer(), 
        inches = ~str_replace_all(.x, "(.*\\s)([0-9]+)(.*$)", '\\2') |> as.integer()
      ), 
      .names = '{fn}'
    ),
    total_inches = 12 * feet + inches
  )

agg_heights <- heights |>
  group_by(country) |> 
  summarize(
    across(
      total_inches,
      mean
    )
  ) |> 
  arrange(desc(total_inches))

if (!file.exists(matches_path)) {
  matches_resp <- httr::GET('https://api.fifa.com/api/v3/calendar/matches?language=en&count=500&idSeason=255711')
  results <- content(matches_resp) |> pluck('Results')
  
  ## for incomplete matches (anything beyond the group stage at the moment), there will be `NULL`s
  ##   which causes `pluck()` to throw an error. using a `.default` of `NA` fixes the issue.
  pluck2 <- partial(pluck, .default = NA_character_, ... = )
  
  map_pluck_chr <- function(x, ...) {
    map_chr(x, pluck2, ...)
  }
  
  map_pluck_results_chr <- function(...) {
    results |> map_pluck_chr(...)
  }
  
  matches <- tibble(
    date = map_pluck_results_chr('Date') |> ymd_hms() |> date(),
    result_id = map_pluck_results_chr('Properties', 'IdIFES') |> as.integer(),
    home_ioc = map_pluck_results_chr('Home', 'IdCountry'),
    away_ioc = map_pluck_results_chr('Away', 'IdCountry'),
    home_score = map_pluck_results_chr('HomeTeamScore') |> as.integer(),
    away_score = map_pluck_results_chr('AwayTeamScore') |> as.integer()
  )
  qs::qsave(matches, matches_path)
} else {
  matches <- qs::qread(matches_path)
}

country_mapping <- countrycode::codelist |> 
  select(ioc, country = country.name.en) |> 
  bind_rows(
    tibble(
      ioc = c('ENG', 'WAL', 'IRN'),
      country = c('England', 'Wales', 'Iran')
    )
  )

agg_heights_with_ioc <- agg_heights |> 
  inner_join(country_mapping, by = 'country')

double_matches <- bind_rows(
  matches |> transmute(date, result_id, side = 'home', team_ioc = home_ioc, opp_ioc = away_ioc, team_score = home_score, opp_score = away_score),
  matches |> transmute(date, result_id, side = 'away', team_ioc = away_ioc, opp_ioc = home_ioc, team_score = away_score, opp_score = home_score)
)

logo_path <- function(x) {
  file.path(dir_proj, 'flags', sprintf('%s.png', x))
}


.gt_theme_538 <- function(data, ...) {
  data |>
    opt_table_font(
      font = list(
        google_font('Titillium Web'),
        default_fonts()
      )
    ) |>
    tab_style(
      style = cell_borders(
        sides = "top", color = "black", weight = px(0)
      ),
      locations = gt::cells_column_labels(
        columns = gt::everything()
      )
    ) |>
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "black", weight = px(1)
      ),
      locations = cells_row_groups()
    ) |>
    tab_options(
      column_labels.background.color = "white",
      heading.border.bottom.style = "none",
      table.border.top.width = px(3),
      table.border.top.style = "none", # transparent
      table.border.bottom.style = "none",
      column_labels.font.weight = "normal",
      column_labels.border.top.style = "none",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "black",
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "white",
      stub.border.color = "white",
      stub.border.width = px(0),
      data_row.padding = px(3),
      source_notes.font.size = 12,
      source_notes.border.lr.style = "none",
      table.font.size = 14,
      heading.title.font.size = 16,
      heading.subtitle.font.size = 14,
      heading.align = "left",
      ...
    ) |>
    opt_css(
      "tbody tr:last-child {
    border-bottom: 2px solid #ffffff00;
      }
    ",
    add = TRUE
    )
}


df <- double_matches |> 
  filter(!is.na(team_ioc)) |> 
  inner_join(
    agg_heights_with_ioc |> 
      select(team_country = country, team_ioc = ioc, team_inches_mean = total_inches),
    by = 'team_ioc'
  ) |> 
  inner_join(
    agg_heights_with_ioc |> 
      select(opp_country = country, opp_ioc = ioc, opp_inches_mean = total_inches),
    by = 'opp_ioc'
  ) |> 
  mutate(
    outcome = case_when(
      is.na(team_score) ~ NA_character_, #  'question',
      team_score > opp_score ~ 'W', #  'check-square',
      team_score < opp_score ~ 'L', # 'square-xmark',
      team_score == opp_score ~  'D' # flag'
    ),
    diff_inches_mean = team_inches_mean - opp_inches_mean
  ) |> 
  arrange(diff_inches_mean)
df

tb <- df |> 
  slice_min(diff_inches_mean, n = 10, with_ties = FALSE) |> 
  transmute(
    date,
    team_country,
    team_logo = logo_path(team_country),
    opp_country,
    opp_logo = logo_path(opp_country),

    team_inches_mean,
    opp_inches_mean,
    diff_inches_mean,
    outcome
  ) |> 
  gt::gt() |> 
  .gt_theme_538() |> 
  gt::cols_label(
    date = 'Date',
    team_country = 'Team',
    team_logo = ' ',
    opp_country = 'Opponent',
    opp_logo = ' ',
    team_inches_mean = 'Team',
    opp_inches_mean = 'Opp.',
    diff_inches_mean = gt::md('**Diff.**'),
    outcome = 'Outcome'
  ) |>
  gt::tab_spanner(
    columns = c(team_inches_mean, opp_inches_mean),
    label = 'Avg. Height (in.)'
  ) |>
  # gt::tab_style(
  #   locations = gt::cells_column_labels(),
  #   style = list(
  #     'padding-bottom: 0px; padding-top: 0px; padding-left: 5px; padding-right: 5px'
  #   )
  # ) |> 
  gt::fmt_number(
    columns = ends_with("inches_mean"),
    decimals = 1
  ) |> 
  gt::cols_align(
    columns = date,
    align = 'left'
  ) |> 
  text_transform(
    locations = cells_body(columns = outcome), 
    fn = function(x) {
      color <- case_when(
        x == 'W' ~ 'green',
        x == 'L' ~ 'red',
        x == 'D' ~ 'black',
        TRUE ~ 'white'
      )
      sprintf(
        '<span style="color:%s"><b>%s</b></span>',
        color,
        x
      )
    }
  ) |> 
  gt::cols_align(
    columns = outcome,
    align = 'center'
  ) |> 
  gt::text_transform(
    locations = gt::cells_body(columns = c(team_logo, opp_logo)),
    fn = function(x) {
      gt::local_image(
        filename = x,
        height = 25
      )
    }
  ) |> 
  tab_header(
    title = md('**Short kings vs. Tall boys**'),
    subtitle = md("*Which matchups have the biggest height mismatches?*")
  ) |> 
  tab_source_note(
    source_note = md('***Data**: ESPN. **Updated**: 2022-11-26.*')
  )
tb

gtsave(
  tb,
  filename = file.path(dir_proj, 'avg_player_height.png')
)
