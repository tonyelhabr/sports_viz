
library(tidyverse)
extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = element_text('Karla', face = 'bold', size = 18, color = 'gray20', hjust = 0.5),
  # plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', face = 'bold', size = 14, color = 'gray20'),
  axis.text = element_text('Karla', size = 14),
  # axis.title = element_text(size = 24, face = 'bold'),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  # axis.text = element_text('Karla', size = 12, face = 'bold', color = 'gray20'),
  # axis.title.x = element_text(hjust = 0.95),
  # axis.title.y = element_text(hjust = 0.95),
  # axis.line = element_line(color = 'gray80'),
  axis.line = element_blank(),
  # panel.grid.major = element_line(color = 'gray80'),
  # panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.major = element_blank(),
  # panel.grid.minor.x = element_blank(),
  # panel.grid.minor.y = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  plot.caption = element_text('Karla', size = 14, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.01),
  legend.text = element_text(size = 14),
  # legend.background = element_rect(fill = '#F3F4F6', color = '#F3F4F6'),
  # legend.position = c(.85, .85))
  # strip.text = element_text(size = 18),
  strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

dir_proj <- '20-202021_epl_xg_stats'
season <- 2020
path_matches <- here::here(dir_proj, sprintf('matches_%s.rds', season))
path_shots <- here::here(dir_proj, sprintf('shots_%s.rds', season))
overwrite <- FALSE
# Reference: https://gist.github.com/Torvaney/42cd82addb3ba2c4f33ec3247e66889c
if(!file.exists(path_shots) & overwrite) {
  if(!file.exists(path_matches) & overwrite) {
    extract_json <- function(html, varname) {
      # Extract a JSON variable from the raw html
      html %>% 
        rvest::html_nodes('script') %>% 
        rvest::html_text(trim = TRUE) %>% 
        keep(str_detect, varname) %>% 
        str_extract("(?<=JSON.parse\\(').*(?='\\))") %>% 
        stringi::stri_unescape_unicode() %>% 
        jsonlite::fromJSON()
    }
    
    unnest_df <- function(df, name) {
      # Hack to convert list/nested dfs to a single df while keeping sane column 
      # names
      if (!is.data.frame(df)) {
        return(tibble(!!name := df))
      }
      
      tbl <- as_tibble(df)
      colnames(tbl) <- str_c(name, '_', colnames(tbl))
      
      tbl
    }
    
    fetch_matches <- function(league = 'EPL', season = 2020) {
      # Fetch a dataframe of matches for an individual league/season's page
      # league_url %>% 
      sprintf('https://understat.com/league/%s/%s', toupper(league), season) %>% 
        xml2::read_html() %>% 
        extract_json('datesData') %>% 
        imap_dfc(unnest_df) %>% 
        rename(match_id = id) %>% 
        type_convert()
    }
    
    matches <- fetch_matches(season = season)
    write_rds(matches, path_matches)
  } else {
    matches <- path_matches %>% read_rds()
  }
  shots <-
    matches %>%
    filter(isResult) %>% 
    pull(match_id) %>% 
    map_dfr(slowly(possibly(understatr::get_match_shots, tibble())))
  
  write_rds(shots, path_shots)
} else {
  shots <- path_shots %>% read_rds()
}

team_stats <- understatr::get_league_teams_stats('EPL', year = season)

# Need this for prettier team names.
team_mapping <- 
  xengagement::team_accounts_mapping %>% 
  filter(league %in% c('epl', 'champ'))
team_mapping

# team_mapping %>% filter(team %>% str_detect('Wolves'))
.fix_team_col <- function(data) {
  data %>% 
    left_join(
      team_mapping %>% 
        select(team_new = team, team = team_understat)
    ) %>% 
    select(-team) %>% 
    rename(team = team_new)
}

# For ordering teams on y-axis later.
standings <-
  team_stats %>% 
  group_by(team = team_name) %>% 
  summarize(
    across(pts, sum)
  ) %>% 
  ungroup() %>%
  arrange(rnk = row_number(desc(pts))) %>% 
  .fix_team_col()
standings

shots_slim <-
  shots %>% 
  mutate(
    across(c(match_id, minute), as.integer),
    penalty = if_else(situation == 'Penalty', TRUE, FALSE)
  ) %>% 
  arrange(date, match_id, minute) %>% 
  select(
    season, date, match_id, minute, team_h = h_team, team_a = a_team, side = h_a, xg = xG, penalty, result # , g_h = h_goals, g_a = a_goals
  )
shots_slim

xg_penalty <- shots_slim %>% filter(penalty) %>% pull(xg) %>% mean()
xg_penalty
rgx_g <- '^(np)?[x]?g'

# Pad such that minutes after last event for games with last shot before 90 min (e.g. last shot at 87th minute) gets counted.
shots_redux_init <-
  bind_rows(
    shots_slim %>% 
      filter(side == 'h') %>% 
      rename(xg_h = xg) %>% 
      mutate(g_h = if_else(result == 'Goal', 1L, 0L)),
    shots_slim %>% 
      filter(side == 'a') %>% 
      rename(xg_a = xg) %>% 
      mutate(g_a = if_else(result == 'Goal', 1L, 0L))
  ) %>% 
  arrange(season, date, match_id, minute) %>% 
  mutate(
    npxg_h = ifelse(penalty & side == 'h', NA_real_, xg_h),
    npxg_a = ifelse(penalty & side == 'a', NA_real_, xg_a)
  )
shots_redux_init

shots_redux_pad <-
  shots_redux_init %>% 
  group_by(season, date, match_id) %>% 
  slice_max(minute, with_ties = FALSE) %>% 
  ungroup() %>% 
  filter(minute < 90) %>% 
  mutate(result = 'dummy') %>% 
  select(season, date, match_id, team_h, team_a, side, result)

shots_redux <-
  bind_rows(shots_redux_init, shots_redux_pad) %>% 
  arrange(season, date, match_id, minute) %>% 
  group_by(season, date, match_id) %>% 
  mutate(
    dur = minute - dplyr::lag(minute, default = 0L),
    across(matches(sprintf('%s_', rgx_g)), list(cumu = ~cumsum(coalesce(.x, 0))))
  ) %>% 
  # ungroup() %>% 
  # mutate(
  #   g_state_h = g_h_cumu - g_a_cumu,
  #   g_state_a = g_a_cumu - g_h_cumu
  # ) %>% 
  # # filter(result != 'Goal') %>% 
  mutate(
    g_state_h = dplyr::lag(g_h_cumu, 1) - dplyr::lag(g_a_cumu, 1),
    g_state_a = dplyr::lag(g_a_cumu, 1) - dplyr::lag(g_h_cumu, 1)
  ) %>%
  ungroup() %>%
  mutate(
    # The coalesce is for the first event in a match.
    across(matches('^g_'), ~coalesce(.x, 0) %>% as.integer())
  )
shots_redux
# shots_redux %>% filter(result == 'Goal' | lead(result) == 'Goal')
# shots_redux %>% filter(result == 'dummy')
# shots_redux %>% filter(penalty)

.f_rename <- function(side) {
  suffix <- sprintf('_%s$', side)
  side_opp <- ifelse(side == 'h', 'a', 'h')
  suffix_opp <- sprintf('_%s$', side_opp)
  shots_redux %>% 
    select(-matches('_cumu$')) %>% 
    rename_with(~str_replace(.x, suffix, ''), c(matches(suffix))) %>% 
    rename_with(~str_replace(.x, suffix_opp, '_opp'), c(matches(suffix_opp))) %>% 
    mutate(side = !!side)
}

shots_by_team <- 
  bind_rows(.f_rename('h'), .f_rename('a')) %>% 
  .fix_team_col()
shots_by_team

# shots_by_team$team %>% levels()
# shots_by_team %>% filter(is.na(team))

# # g_state_simple column gets coerced to a character later on.
.factor_g_state_simple_col <- function(data) {
  data %>%
    mutate(
      across(g_state_simple, ~factor(.x, levels = c('<0', '0', '>0')))
    )
}

states <-
  shots_by_team %>% 
  select(-g_state_opp) %>% 
  # mutate(
  #   across(g_state, ~case_when(.x <= -3L ~ '<=-3', .x >= 3L ~ '>=+3', .x > 0 ~ sprintf('+%d', .x), TRUE ~ as.character(.x)))
  # ) %>%
  # mutate(
  #   across(g_state, ~factor(.x, levels = c('<=-3', -2:0, sprintf('+%d', 1:2), '>=+3')))
  # ) %>%
  # filter(g_state <= 2L & g_state >= -2L) %>%
  mutate(
    across(g_state, list(simple = ~case_when(.x < 0L ~ '<0', .x > 0L ~ '>0', TRUE ~ '0')))
  ) %>%
  # .factor_g_state_simple_col() %>%
  mutate(
    across(g_state, ~case_when(.x > 0 ~ sprintf('+%d', .x), TRUE ~ as.character(.x)))
  ) %>%
  mutate(
    across(g_state, ~factor(.x, levels = c(-2:0, sprintf('+%d', 1:2))))
  ) %>% 
  # Just to have something at the end that I don't comment out when testing.
  arrange(season, date, match_id, minute, team)
states

# lvls <- team_mapping$team %>% rev()
lvls <- standings$team %>% rev()

.aggregate_states_by_team <- function(col) {
  col_sym <- col %>% sym()
  col_other <- switch(col, 'g_state' = 'g_state_simple', 'g_state_simple' = 'g_state')
  col_other_sym <- col_other %>% sym()
  states %>% 
    select(-!!col_other_sym) %>% 
    group_by(season, team, !!col_sym) %>% 
    summarize(
      shots = sum(!is.na(xg)),
      shots_opp = sum(!is.na(xg_opp)),
      across(c(dur, matches(rgx_g)), ~sum(.x, na.rm = TRUE))
    ) %>% 
    ungroup() %>% 
    mutate(
      npxg_ps = npxg / shots,
      npxg_opp_ps = npxg_opp / shots_opp
    ) %>% 
    mutate(
      across(c(matches(rgx_g), -any_of(col), matches('^shots')), list(p90 = ~90 * .x / dur))
    ) %>% 
    mutate(
      shots_p90_diff = shots_p90 - shots_opp_p90,
      npxg_p90_diff = npxg_p90 - npxg_opp_p90,
      npxg_ps_diff = npxg_ps - npxg_opp_ps
    ) %>% 
    mutate(
      across(team, ~factor(.x, levels = lvls))
    ) %>% 
    relocate(season, team)
}

states_by_team <- .aggregate_states_by_team('g_state')
states_by_team_simple <- .aggregate_states_by_team('g_state_simple')

states_by_team_simple_slim <-
  states_by_team_simple %>% 
  select(season, team, g_state_simple, npxg_p90) # npxg_p90_diff) 
states_by_team_simple_slim

states_by_team_simple_wide <-
  states_by_team_simple_slim %>% 
  pivot_wider(
    values_from = -any_of(c('season', 'team', 'g_state_simple')),
    names_from = g_state_simple
  ) %>% 
  mutate(
    `<` = `<0` - `0`,
    `>` = `>0` - `0`
  )
states_by_team_simple_wide

states_by_team_simple_clean <-
  states_by_team_simple_slim %>% 
  left_join(
    states_by_team_simple_wide %>% 
      select(-matches('0')) %>%
      rename(`<0` = `<`, `>0` = `>`) %>% 
      pivot_longer(
        matches('^[<>]'),
        names_to = 'g_state_simple',
        values_to = 'diff'
      )
  ) %>% 
  mutate(
    lab = case_when(
      g_state_simple == '0' ~ sprintf('%.2f', npxg_p90), #  npxg_p90_diff),
      # TRUE ~ sprintf('%+.2f (%+.2f)', npxg_p90_diff, diff)
      TRUE ~ sprintf('%+.2f', diff)
    )
  ) %>% 
  .refactor_g_state_simple_col()

viz_npxg_p90_diff_simple <-
  states_by_team_simple_clean %>% 
  muttate(
    lab = case_when(
      # team_name %in% tms_filt ~ glue::glue('<span style="color:{tm_colors[[team_name]]}">{team_name}</span>'),
      (team_name %in% tms_filt) ~ glue::glue('<b><span style="color:{tm_colors[[team_name]]}">{team_name}</span></b>') %>% as.character(), 
      TRUE ~ team_name
    )
 ) %>% 
  ggplot() +
  aes(y = team, x = g_state_simple) +
  geom_tile(aes(fill = diff)) +
  geom_text(aes(label = lab), family  = 'Karla', size = 4) +
  guides(
    fill = 
      guide_legend(
        title = 'Non-Penalty xG Differential Per 90 Min.\nRelative to Game State = 0',
        title.position = 'top',
        nrow = 1
      )
  ) +
  scale_fill_gradient2(
    midpoint = 0,
    low = '#e9a3c9',
    mid = '#f7f7f7',
    high = '#a1d76a',
    na.value = 'grey80'
  ) +
  labs(
    y = NULL,
    x = 'Game State',
    title = '2020/21 Premier League xG By Game State',
    subtitle = 'Some teams (Man City and Tottenham) accumulate xG at a higher rate when playing in an even match, while others (Man United, Chelsea, West Ham) have higher xG rates when the behind or ahead.',
    caption = 'Rates are time-based averages (accounting for varying time spent in a given game state).',
    tag = '**Viz**: Tony ElHabr | **Data**: understat'
  ) + 
  theme(
    plot.title.position = 'plot',
    legend.title = element_text(size = 10, hjust = 0.5),
    plot.title = element_text(size = 22, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0, color = 'grey50'),
    panel.grid.major = element_blank(),
    legend.position = 'top'
  )
viz_npxg_p90_diff_simple

# more ----
# options(tibble.print_min = 23)
states_by_team_agg <-
  # states_by_team %>% 
  states_by_team_simple %>% 
  rename(g_state = g_state_simple) %>% 
  group_by(season, g_state) %>% 
  # mutate() %>% 
  summarize(
    cnt = sum(!is.na(dur)),
    # dur_total = sum(dur),
    across(matches('_diff$'), list(mean = ~sum(dur * .x, na.rm = TRUE) / sum(dur)))
  ) %>% 
  ungroup()
states_by_team_agg %>% mutate(across(matches('_mean'), round, 2))

# Not really needed. Just for identify the most extreme cases.
states_by_team_oe <-
  # states_by_team %>% 
  states_by_team_simple %>% 
  rename(g_state = g_state_simple) %>% 
  left_join(states_by_team_agg) %>% 
  mutate(
    shots_p90_oe = shots_p90_diff - shots_p90_diff_mean,
    npxg_p90_oe = npxg_p90_diff - npxg_p90_diff_mean
  ) %>% 
  group_by(season, g_state) %>% 
  mutate(
    across(matches('_oe$'), list(rnk = ~row_number(desc(.x)), rnk_inv = ~row_number(.x)))
  ) %>% 
  ungroup() %>%
  arrange(season, g_state)
states_by_team_oe

states_by_team_oe %>% 
  filter(
    shots_p90_oe_rnk == 1L |
      npxg_p90_oe_rnk == 1L |
      shots_p90_oe_rnk_inv == 1L | npxg_p90_oe_rnk_inv == 1L
  )

.plot_heatmap <- function(col) {
  # states_by_team %>% 
  states_by_team_simple %>% 
    rename(g_state = g_state_simple) %>% 
    # filter(g_state %in% c('-1', '0', '+1')) %>% 
    ggplot() +
    aes(y = team, x = g_state) +
    geom_tile(aes(fill = {{ col }})) +
    labs(
      y = NULL
    ) +
    theme(
      axis.text.y = element_blank(),
      # plot.title.position = 'plot',
      plot.subtitle = element_text(hjust = 0.5),
      # title = element_text(hjust = 0.5),
      panel.grid.major = element_blank(),
      legend.position = 'top'
    )
}

viz_npxg_ps_diff <-
  .plot_heatmap(npxg_ps_diff) +
  geom_text(
    family = 'Karla',
    size = 4,
    aes(label = scales::number(npxg_ps_diff, accuracy = 0.01))
  ) +
  guides(
    fill = guide_legend(
      title = 'Non-Penalty xG Differential Per Shot',,
      nrow = 1,
      title.position = 'top',
      title.hjust = 0.5
    )
  ) +
  scale_fill_gradient2(
    midpoint = 0,
    low = '#5ab4ac',
    mid = '#f5f5f5',
    high = '#d8b365'
  ) +
  labs(
    x = ' ',
    tag = '**Viz**: Tony ElHabr | **Data**: understat',
    subtitle = 'Teams shoot less efficiently when they\'re down'
  )
viz_npxg_ps_diff

viz_shots_p90_diff <-
  .plot_heatmap(shots_p90_diff) +
  geom_text(
    family = 'Karla',
    size = 4,
    aes(label = scales::number(shots_p90_diff, accuracy = 0.1))
  ) +
  # guides(fill = guide_legend(title = 'shots/90', nrow = 1)) +
  guides(
    fill = guide_legend(
      title = 'Shot Differential Per 90 Min.',
      nrow = 1,
      title.position = 'top',
      title.hjust = 0.5
    )
  ) +
  scale_fill_gradient2(
    midpoint = 0,
    low = '#67a9cf',
    mid = '#f7f7f7',
    high = '#ef8a62'
  ) +
  labs(
    x = 'Game State',
    caption = 'Game states with goal differentials less/greater than +-2 not shown.',
    subtitle = 'because they shoot more frequently'
  )
viz_shots_p90_diff

# viz_teams <-
#   states_by_team %>%
#   distinct(team) %>% 
#   ggplot() +
#   aes(y = team) +
#   geom_blank() +
#   theme(
#     axis.text.y = element_text(hjust = 0.5)
#   ) +
#   labs(
#     x = NULL,
#     y = NULL
#   )
# viz_teams

viz_teams <-
  states_by_team %>%
  distinct(team) %>%
  mutate(rnk = row_number(team)) %>%  # row_number(desc(pts))) %>% 
  ggplot() +
  aes(y = team, x = 0) +
  geom_tile(fill = NA, show.legend = FALSE) +
  # scale_fill_manual(values = c(`1` = '#ffffff')) +
  geom_text(aes(label = team), hjust = 0.5, family = 'Karla', size = 5) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL
  )
viz_teams

# library(patchwork)
res <- 
  patchwork::wrap_plots(viz_npxg_ps_diff, viz_teams, viz_shots_p90_diff, widths = c(8, 2, 8)) +
  patchwork::plot_annotation(
    title = 'Why is there a game state bias with xG?',
    theme = theme(
      plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 18, hjust = 0.5)
    )
  )
res
