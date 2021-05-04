
# pep ----
# https://www.goal.com/en-us/news/what-is-pep-guardiolas-champions-league-record-barcelona/1sdxaf8qyk8ii1o5fgx059hqzt
# https://www.netsportworld.com/football/pep-guardiola-trophies/
library(tidyverse)
dir_proj <- '27-pep'
salaries <- 
  file.path(dir_proj, 'salaries.csv') %>% 
  read_csv() %>% 
  select(year, team, rank_salary = rank)
salaries

placings <-
  file.path(dir_proj, 'placings.csv') %>% 
  read_csv() %>% 
  select(season, team, league_placing, ucl_placing, other_winnings) %>% 
  mutate(
    year = season %>% str_sub(1, 4) %>% as.integer()
  )
placings

teams <-
  tibble(
    team_clubelo = c('Barcelona', 'Bayern', 'ManCity'),
    club_clubelo = c('Barcelona', 'Bayern', 'Man City'),
    team = c('Barcelona', 'Bayern Munich', 'Manchester City'),
    url_logo = c(
      'https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/83.png&w=28',
      'https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/132.png&w=28',
      'https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/382.png&w=28'
    )
  )

# Just download these by going to them in the browser.
# resp <- sprintf('http://api.clubelo/%s', team) %>% download.file(destfile = 'temp.csv', quiet = TRUE)
import_clubelo <- function(team) {
  df <- 
    file.path(dir_proj, sprintf('%s.csv', team[1])) %>% 
    read_csv() %>% 
    janitor::clean_names() %>% 
    select(club_clubelo = club, rank_elo = rank, elo, from, to) %>% 
    left_join(teams %>% select(club_clubelo, team)) %>% 
    select(-club_clubelo)
  df
}

elo <-
  teams$team_clubelo %>% 
  map_dfr(import_clubelo)
elo
elo %>% count(team)

windows <-
  tibble(
    start = c('2012-08-19', '2013-08-09', '2014-08-22', '2015-08-14', '2016-08-13', '2017-08-12', '2018-08-12', '2019-08-10'),
    end = c('2013-06-01', '2014-05-10', '2015-05-23', '2016-05-14', '2017-05-21', '2018-05-13', '2019-05-12', '2020-07-26')
  ) %>% 
  mutate(across(c(start, end), lubridate::ymd)) %>% 
  bind_cols(placings %>% select(season, team))
windows

elo_windows <-
  data.table::as.data.table(elo %>% mutate(from2 = from, to2 = to))[
    data.table::as.data.table(windows), 
    on=.(team = team, from2 >= start, to2 <= end)
  ] %>% 
  as_tibble() %>% 
  # Can't coerce this until now cuz early years have "None"
  mutate(across(rank_elo, as.integer)) %>% 
  rename(start = from2, end = to2) %>% 
  arrange(season, from)
elo_windows

elo_calcs <-
  elo_windows %>% 
  mutate(
    dur_match = as.numeric(to - from, unit = 'days') %>% as.integer(),
    dur_season = as.numeric(end - start, unit = 'days') %>% as.integer()
  ) %>% 
  group_by(season) %>% 
  mutate(n = n(), dur_match_total = sum(dur_match)) %>% 
  ungroup() %>% 
  mutate(frac_season = dur_match / dur_match_total)
elo_calcs
elo_windows %>% filter(is.na(elo))
elo %>% filter(team == 'Manchester City')

elo_agg <-
  elo_calcs %>% 
  group_by(team, season) %>% 
  summarize(
    elo = sum(elo * frac_season),
    rank_elo = sum(rank_elo * frac_season)
  ) %>% 
  ungroup()
elo_agg

stats_agg <-
  elo_agg %>% 
  left_join(placings) %>% 
  inner_join(salaries) %>%
  left_join(teams %>% select(team, url_logo)) %>%
  select(
    season,
    team,
    url_logo,
    rank_elo,
    rank_salary,
    ucl_placing,
    other_winnings
  )

# Reference: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset4=theme-code3
.gt_theme_538 <- function(data,...) {
  data %>%
    gt::opt_all_caps()  %>%
    gt::opt_table_font(
      font = list(
        gt::google_font('Karla'),
        gt::default_fonts()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = 'bottom', color = 'transparent', weight = gt::px(2)
      ),
      locations = gt::cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    gt::tab_options(
      column_labels.background.color = 'white',
      table.border.top.width = gt::px(3),
      table.border.top.color = 'transparent',
      table.border.bottom.color = 'transparent',
      table.border.bottom.width = gt::px(3),
      column_labels.border.top.width = gt::px(3),
      column_labels.border.top.color = 'transparent',
      column_labels.border.bottom.width = gt::px(3),
      column_labels.border.bottom.color = 'black',
      data_row.padding = gt::px(3),
      footnotes.font.size = 10,
      source_notes.font.size = 10,
      table.font.size = 16,
      heading.align = 'left',
      ...
    ) 
}

res <-
  stats_agg %>% 
  mutate(
    across(rank_elo, round, 1),
    across(other_winnings, ~coalesce(.x, '') %>% str_replace_all('\\,', '<br/>'))
  ) %>% 
  gt::gt() %>% 
  gt::cols_label(
    .list = 
      list(
        season = 'Season',
        team = 'Team',
        url_logo = ' ',
        rank_elo = 'ELO Rank',
        rank_salary = 'Team Salary Rank',
        # league_placing = 'League Placing',
        ucl_placing = 'UCL Placing',
        other_winnings = 'Non-UCL Titles'
      )
  ) %>%
  gt::text_transform(
    locations = gt::cells_body(
      vars(url_logo)
    ),
    fn = function(x) {
      gt::web_image(
        url = x,
        height = 25
      )
    }
  ) %>% 
  gt::fmt_markdown(columns = vars(other_winnings)) %>% 
  gt::tab_style(
    style = list(
      gt::cell_text(size = gt::px(8))
    ),
    locations = gt::cells_body(
      columns = vars(other_winnings)
    )
  ) %>%
  gt::cols_align(
    align = 'right',
    columns = vars(season, rank_elo, rank_salary, league_placing, ucl_placing)
  ) %>% 
  gt::tab_footnote(
    footnote = gt::md('**Source**: http://clubelo.com/ (time-weighted average)'),
    locations = gt::cells_column_labels(columns = vars(rank_elo))
  ) %>% 
  gt::tab_footnote(
    footnote = gt::md('**Source**: https://globalsportssalaries.com/ (based on average annual salary per player, relative to all soccer clubs)'),
    locations = gt::cells_column_labels(columns = vars(rank_salary))
  ) %>% 
  .gt_theme_538() %>% 
  gt::tab_source_note(
    gt::md('**Table theme** (538 style): @thomas_mock')
  )
res
gt::gtsave(res, file.path(dir_proj, 'pep.png'))

# Pep GODiola or FRAUDiola? Yes, Pep's Man City teams have not met their lofty #UCL expectations, but is he really playing with the most stacked deck? The last time his team had the largest salary was in 2012-13, just one year after he last won a UCL title.

# man city v chelsea ----
# https://en.wikipedia.org/wiki/List_of_European_Cup_and_UEFA_Champions_League_finals

# This will be the fifth time since 2007-08 that the #UCL finalists come from the same league. Have the domestic league face-offs during the season foreshadowed the title match result? Sort of.
