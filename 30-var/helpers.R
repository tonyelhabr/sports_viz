
library(tidyverse)
library(rvest)

do_scrape <- function(season, overwrite = FALSE, dir = '30-var') {
  
  .path <- function(name) {
    file.path(dir, sprintf('%s_%s.rds', name, season))
  }
  
  path_agg <- .path('var_by_team')
  path_incidents <- .path('var_incidents')
  
  path_agg_exists <- file.exists(path_agg)
  path_incidents_exists <- file.exists(path_incidents)
  if(path_agg_exists & path_incidents_exists & !overwrite) {
    return(list(agg = read_rds(path_agg), incidents = read_rds(path_incidents)))
  }
  
  url <- 
    switch(
      as.character(season),
      '2020' = 'https://www.espn.com/soccer/english-premier-league/story/4182135/how-var-decisions-affected-every-premier-league-club-in-2020-21',
      '2019' = 'https://www.espn.com/soccer/english-premier-league/story/3929823/how-var-decisions-have-affected-every-premier-league-club'
    )
  
  page <- url %>% xml2::read_html()
  h2 <- page %>% html_nodes('h2')
  teams <- h2 %>% html_text() %>% .[1:20] %>% str_remove('\\s[+-]?[0-9]+$')
  page %>% html_nodes('p') %>% html_nodes('strong')
  nodes_p <- page %>% html_nodes('p')
  text_p <- nodes_p %>% html_text()
  idx_filt <- text_p %>% str_which('^Overturns')
  nodes_filt <- nodes_p[idx_filt]
  
  lst <-
    nodes_filt %>% 
    html_text2(preserve_nbsp = TRUE) %>% 
    str_split(pattern = '\n')
  lst

  f <- function(x, rgx = 'Overturns[:] ', f = as.integer, default = 0L) {
    res_init <- x %>% keep(~str_detect(.x, rgx))
    if(length(res_init) == 0L) {
      return(default)
    }
    res_init %>% str_remove(rgx) %>% f()
  }
  # `possibly` wasn't really working as I wanted since stuff like integer(0) is being returned.
  
  agg <-
    tibble(
      overturns = lst %>% map_int(~f(.x, 'Overturns[:] ')),
      goals_for = lst %>% map_int(~f(.x, 'Leading to goals for[:] ')),
      goals_disallowed_for = lst %>% map_int(~f(.x, 'Disallowed goals for[:] ')),
      goals_against = lst %>% map_int(~f(.x, 'Leading to goals against[:] ')),
      goals_disallowed_against = lst %>% map_int(~f(.x, 'Disallowed goals against[:] ')),
      net_goals = lst %>% map_int(~f(.x, 'Net goal score[:] ')),
      subjective_decisions_for = lst %>% map_int(~f(.x, 'Subjective decisions for[:] ')),
      subjective_decisions_against = lst %>% map_int(~f(.x, 'Subjective decisions against[:] ')),
      net_subjective_for = lst %>% map_int(~f(.x, 'Net subjective score[:] ')),
      misc = lst %>% map_int(~f(.x, 'Miscellaneous[:] ')),
      penalties_for_against = lst %>% map_chr(~f(.x, 'Penalties for \\/ against[:] ', f = as.character, default = NA_character_)),
      overturns_rejected = lst %>% map_int(~f(.x, 'Rejected overturns[:] ')),
      mistaken_identity = lst %>% map_int(~f(.x, 'Mistaken Identity[:] '))
    ) %>% 
    separate(penalties_for_against, into = c('penalties_for', 'penalties_against'), sep = '\\s\\/\\s') %>% 
    bind_cols(tibble(team = teams), .)
  agg
  
  idx_game <- text_p %>% str_which('^Game')
  idx_game
  
  find_between <- function(i) {
    idx1 <- idx_filt[i]
    idx2 <- idx_filt[i+1]
    idx2 <- ifelse(is.na(idx2), rev(idx_game)[1], idx2)
    between(idx_game, idx1, idx2) %>% idx_game[.]
  }
  
  idx_between <-
    seq_along(idx_filt) %>% 
    map(find_between)
  
  nodes_game <- nodes_p[idx_game]
  lst_game <-
    nodes_game %>% 
    html_text2(preserve_nbsp = TRUE) %>% 
    str_split(pattern = '\n')
  lst_game
  
  f_chr <- partial(f, f = as.character, default = NA_character_, ... = )
  
  values_game <-
    lst_game %>% 
    enframe('idx_intra', 'value') %>% 
    bind_cols(
      idx_between %>% 
        enframe('idx_team', 'idx_opp') %>% 
        # select(-idx_opp) %>% 
        unnest(idx_opp)
    ) %>% 
    unnest(value) %>%
    left_join(tibble(team = teams, idx_team = seq_along(teams))) %>% 
    mutate(idx = row_number()) %>% 
    group_by(idx_intra) %>% 
    mutate(idx_inter = row_number(idx)) %>% 
    ungroup()
  values_game
  
  games_idx <- values_game %>% filter(idx_inter == 1L)
  incidents_idx <- values_game %>% filter(idx_inter != 1L)
  
  incidents_init <-
    games_idx %>% 
    select(game = value, idx_intra, team) %>% 
    left_join(incidents_idx %>% select(idx, idx_intra, incident = value)) %>% 
    arrange(idx) %>% 
    group_by(idx_intra) %>% 
    mutate(idx_inter = row_number(idx)) %>% 
    ungroup() %>% 
    mutate(
      across(game, ~str_remove(.x, 'Game[:] ')),
      across(incident, ~str_remove(.x, 'Incident[:] '))
    )
  incidents_init
  
  f_replace <- function(x, i) {
    str_replace(x, '(^.*)\\s\\(([AH])[:;]\\s([A-Z][a-z]+)[.]?\\s([0-9]+)\\)$', sprintf('\\%d', i))
  }
  
  m_mapper <- function(x) {
    switch(
      x,
      'Sept' = 9,
      'Oct' = 10,
      'Nov' = 11,
      'Dec' = 12,
      'Jan' = 1,
      'Feb' = 2,
      'March' = 3,
      'April' = 4,
      'May' = 5,
      -1
    ) %>% 
      as.integer()
  }
  
  incidents <-
    incidents_init %>% 
    mutate(
      across(
        game,
        list(
          team_opp = ~f_replace(.x, 1),
          side = ~f_replace(.x, 2),
          m = ~f_replace(.x, 3),
          d = ~f_replace(.x, 4) %>% as.integer()
        ),
        .names = '{fn}'
      )
    ) %>% 
    select(-game) %>% 
    mutate(
      across(m, Vectorize(m_mapper)),
      y = ifelse(m <= 5L, !!season + 1, !!season),
      date = sprintf('%s-%s-%s', y, m, d) %>% lubridate::ymd(),
      idx = row_number(idx)
    ) %>% 
    select(idx, date, team, team_opp, side, incident)
  incidents
  
  write_rds(agg, path_agg)
  write_rds(incidents, path_incidents)
  list(agg = agg, incidents = incidents)
}

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 24, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown('Karla', face = 'bold', size = 18, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  # plot.caption = ggtext::element_markdown('Karla', size = 16, color = 'gray20', hjust = 1),
  plot.caption = element_text('Karla', size = 12, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.01),
  legend.text = element_text(color = 'gray20', size = 14),
  strip.text = element_text(color = 'gray20', size = 14),
  strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

f_segment <- partial(
  geom_segment,
  size = 1.1,
  arrow = arrow(length = unit(5, 'pt'), type = 'closed'),
  show.legend = FALSE,
  ... = 
)

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

f_text <- partial(
  ggtext::geom_richtext,
  # fill = NA, 
  label.color = NA,
  family = 'Karla',
  size = pts(12),
  ... =
)
