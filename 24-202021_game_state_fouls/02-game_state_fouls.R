
library(tidyverse)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = element_text('Karla', face = 'bold', size = 24, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', face = 'bold', size = 16, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  plot.caption = element_text('Karla', size = 14, color = 'gray20', hjust = 0),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 14, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.01),
  legend.text = element_text(size = 14),
  strip.text = element_text(size = 14),
  strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

dir_proj <- '24-202021_game_state_fouls'
path_events <- file.path(dir_proj, 'events.rds')
path_meta <- file.path(dir_proj, 'meta.rds')

leagues_mapping <-
  tibble(
    league = c('epl'),
    league_understat = c('EPL'),
    league_whoscored = c('England-Premier-League'),
    league_538 = c('Barclays Premier League')
  )

team_mapping <-
  xengagement::team_accounts_mapping %>% 
  select(team, team_538, team_whoscored)
team_mapping

probs_init <-
  read_csv(
    'https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv'
  ) %>% 
  rename(league_538 = league, team_538_h = team1, team_538_a = team2, probtie_538 = probtie) %>% 
  left_join(leagues_mapping) %>% 
  select(-league_538) %>% 
  filter(league == 'epl' & season >= 2019) %>% 
  select(-c(league_id)) %>% 
  mutate(across(season, as.integer)) %>% 
  rename_with(~stringr::str_replace(.x, '1$', '_538_h'), matches('1$')) %>% 
  rename_with(~stringr::str_replace(.x, '2$', '_538_a'), matches('2$')) %>% 
  left_join(team_mapping %>% select(team_h = team, team_538_h = team_538)) %>% 
  left_join(team_mapping %>% select(team_a = team, team_538_a = team_538)) %>% 
  rename(prob_h = prob_538_h, prob_a = prob_538_a, probtie = probtie_538) %>% 
  select(season, date, matches('^team_'), matches('^score_'), matches('^prob'))
probs_init

.f_rename_538 <- function(side) {
  suffix <- sprintf('_%s$', side)
  side_opp <- ifelse(side == 'h', 'a', 'h')
  suffix_opp <- sprintf('_%s$', side_opp)
  probs_init %>% 
    rename_with(~str_replace(.x, suffix, ''), c(matches(suffix))) %>% 
    rename_with(~str_replace(.x, suffix_opp, '_opp'), c(matches(suffix_opp))) %>% 
    mutate(side = !!side)
}

probs_redux <- 
  bind_rows(.f_rename_538('h'), .f_rename_538('a')) %>% 
  select(-matches('team_538')) %>% 
  mutate(prob_diff = prob - prob_opp)
probs_redux

probs_grps <-
  probs_redux %>% 
  mutate(
    across(
      prob,
      list(
        grp4 = ~case_when(
          .x > prob_opp & prob_opp > probtie  ~ 'fav_small',
          .x > prob_opp~ 'fav_big',
          .x < prob_opp & .x < probtie ~ 'dog_big',
          .x < prob_opp ~ 'dog_small',
        ),
        grp2 = ~case_when(
          .x > prob_opp ~ 'fav',
          .x < prob_opp ~ 'dog',
          TRUE ~ NA_character_
        )
      )
    )
  )
probs_grps
probs_grps %>% count(prob_grp4)

meta <- 
  path_meta %>% 
  read_rds() %>% 
  mutate(across(start_date, ~lubridate::ymd_hms(.x) %>% lubridate::date())) %>% 
  rename(date = start_date)

events_teams <-
  path_events %>% 
  read_rds() %>% 
  group_by(match_id, team = team_name) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(match_id, team, side) %>% 
  pivot_wider(
    names_from = side,
    values_from = team,
    names_prefix = 'team_'
  ) %>% 
  rename(team_h = team_home, team_a = team_away)
events_teams

events <- 
  path_events %>% 
  read_rds() %>% 
  rename(team = team_name, league_whoscored = league) %>% 
  left_join(leagues_mapping) %>% 
  select(-league_whoscored) %>% 
  mutate(across(side, ~case_when(.x == 'home' ~ 'h', .x == 'away' ~ 'a'))) %>% 
  left_join(meta %>% select(match_id, date)) %>% 
  mutate(
    across(season, ~ str_sub(.x, 1, 4) %>% as.integer()),
    across(c(minute, second), as.integer)
  ) %>% 
  left_join(events_teams) %>% 
  mutate(team_opp = ifelse(team == team_h, team_a, team_h)) %>% 
  arrange(league, season, match_id, expanded_minute, second, team) %>% 
  relocate(league, season, match_id, date)
events

events_first <- events %>% filter(period_name == 'PreMatch', type_name == 'FormationSet', side == 'h')
events_last <- events %>% filter(period_name == 'SecondHalf', type_name == 'End', side == 'h')

.add_time_col <- function(data) {
  data %>% 
    mutate(time = sprintf('%s:%s', expanded_minute, second) %>% lubridate::ms() %>% as.double())
}

fouls <-
  events %>% 
  filter(type_name == 'Foul') %>% 
  mutate(
    action = if_else(outcome_type_name == 'Successful', 'drawn', 'committed')
  ) %>%
  .add_time_col() %>%
  select(
    league,
    season,
    match_id,
    date,
    id,
    time,
    expanded_minute,
    second,
    team,
    team_opp,
    # team_h,
    # team_a,
    side,
    player_name,
    action
  )
fouls

goals <- events %>% filter(type_name == 'Goal')

# main ----
.f_select <- function(data, ...) {
  data %>%
    select(
      league,
      season,
      date,
      match_id,
      team_h,
      team_a,
      side,
      expanded_minute,
      second,
      ...
    )
}

rgx_g <- '^g_[ha]'
goals_redux_init <-
  bind_rows(
    # Could generalize this even more.
    bind_rows(
      goals %>% 
        filter(side == 'h' & is.na(is_own_goal)),
      goals %>% 
        filter(side == 'a' & is_own_goal)
    ) %>% 
      .f_select() %>% 
      mutate(g_h = 1L),
    bind_rows(
      goals %>% 
        filter(side == 'a' & is.na(is_own_goal)),
      goals %>% 
        filter(side == 'h' & is_own_goal)
    ) %>% 
      .f_select() %>% 
      mutate(g_a = 1L)
  ) %>% 
  mutate(across(matches(sprintf('%s$', rgx_g)), ~coalesce(.x, 0L))) %>% 
  arrange(league, season, date, match_id, expanded_minute, second) %>% 
  group_by(league, season, date, match_id) %>% 
  mutate(
    across(matches(sprintf('%s$', rgx_g)), list(cumu = ~cumsum(.x)))
  ) %>% 
  ungroup()
goals_redux_init

goals_redux <-
  bind_rows(
    events_first %>% 
      .f_select() %>% 
      mutate(g_h = 0L, g_a = 0L, g_h_cumu = 0L, g_a_cumu = 0L, comment = 'init'),
    goals_redux_init %>% mutate(comment = NA_character_),
    events_last %>% 
      .f_select() %>% 
      mutate(g_h = 0L, g_a = 0L, comment = 'end')
  ) %>% 
  arrange(league, season, date, match_id, expanded_minute, second) %>% 
  group_by(match_id) %>% 
  fill(g_h_cumu, g_a_cumu, .direction = 'down') %>% 
  ungroup() %>% 
  mutate(
    g_state_h = dplyr::lag(g_h_cumu, 1) - dplyr::lag(g_a_cumu, 1),
    g_state_a = dplyr::lag(g_a_cumu, 1) - dplyr::lag(g_h_cumu, 1)
  ) %>%
  ungroup() %>%
  mutate(
    # The coalesce is for the first event in a match.
    across(matches('^g_'), ~coalesce(.x, 0) %>% as.integer())
  )
goals_redux

.f_rename <- function(side) {
  suffix <- sprintf('_%s$', side)
  side_opp <- ifelse(side == 'h', 'a', 'h')
  suffix_opp <- sprintf('_%s$', side_opp)
  goals_redux %>% 
    # select(-matches('_cumu$')) %>% 
    rename_with(~str_replace(.x, suffix, ''), c(matches(suffix))) %>% 
    rename_with(~str_replace(.x, suffix_opp, '_opp'), c(matches(suffix_opp))) %>% 
    mutate(side = !!side)
}

states <- 
  bind_rows(.f_rename('h'), .f_rename('a')) %>% 
  mutate(
    across(g_state, list(grp = ~case_when(.x < 0L ~ 'behind', .x > 0L ~ 'ahead', TRUE ~ 'neutral')))
  ) %>%
  arrange(league, season, date, match_id, team, expanded_minute, second) %>% 
  .add_time_col()
states

final_scores <- 
  states %>% 
  filter(comment == 'end') %>% 
  select(match_id, team, team_opp, g_h_final = g_h_cumu, g_a_final = g_a_cumu, g_state_final = g_state)
final_scores

# pre-joining ----
# This returns the same number of results as a lag join (as expected).
fouls_by_time <-
  data.table::as.data.table(fouls)[
    data.table::as.data.table(
      states %>% 
        select(match_id, team, time, g_state, g_state_grp) %>% 
        group_by(match_id) %>% 
        mutate(across(time, list(hi = ~dplyr::lead(.x)))) %>% 
        ungroup() %>% 
        rename(time_lo = time)
    ), 
    on=.(match_id = match_id, team = team, time >= time_lo, time < time_hi)
  ] %>% 
  as_tibble() %>%
  drop_na(id) %>% 
  rename(time_lo = time, time_hi = time.1) %>% 
  .add_time_col() %>%
  group_by(match_id, team) %>%
  arrange(time, .by_group = TRUE) %>%
  mutate(time_diff = time - lag(time, default = 0)) %>%
  ungroup() %>% 
  mutate(across(matches('^time'), as.integer)) %>% 
  relocate(
    league,
    season,
    date,
    match_id,
    time,
    time_diff,
    time_lo,
    time_hi,
    expanded_minute,
    second,
    team
  ) %>%
  rename(team_whoscored = team, team_whoscored_opp = team_opp) %>% 
  left_join(team_mapping %>% select(team, team_whoscored = team_whoscored)) %>% 
  left_join(team_mapping %>% select(team_opp = team, team_whoscored_opp = team_whoscored)) %>% 
  # left_join(probs_grps) %>% 
  select(-matches('^team.*whoscored')) %>% 
  arrange(league, season, date, match_id, time, team)
fouls_by_time
fouls_by_time %>% count(id, sort = TRUE) %>% filter(n > 1L)

.add_n_foul_p90_col <-
  function(data,
           suffix = '',
           sep = ifelse(suffix == '', '', '_'),
           col = sprintf('n_foul%s%s', sep, suffix),
           col_res = sprintf('n_foul_p90%s%s', sep, suffix)) {
    data %>%
      mutate(!!sym(col_res) := 90 * !!sym(col) / (time_diff / 60))
  }

.postprocess_redux <- function(data) {
  data %>% 
    mutate(
      across(c(n_foul, n_foul_opp), ~coalesce(.x, 0L)),
      across(c(n_foul_p90, n_foul_p90_opp), ~coalesce(.x, 0))
    ) %>%
    mutate(
      n_foul_diff = n_foul - n_foul_opp, 
      n_foul_p90_diff = n_foul_p90 - n_foul_p90_opp
    )
}

fouls_by_state <-
  fouls_by_time %>%
  group_by(
    league,
    season,
    date,
    team,
    team_opp,
    side,
    match_id,
    action,
    g_state_grp,
    time_lo,
    time_hi
  ) %>%
  # Summing `time_diff`s is wrong, since it won't account for time after last foul and before `time_hi`...
  summarize(
    n_foul = sum(time_diff > 0L)
  ) %>% 
  ungroup() %>% 
  # ...so re-calculate `time_diff`
  mutate(time_diff = time_hi - time_lo) %>% 
  left_join(final_scores) %>% 
  left_join(probs_grps) %>% 
  .add_n_foul_p90_col() %>% 
  arrange(league, season, date, match_id, time_lo, time_hi, team, team_opp)
fouls_by_state

.rejoin <- function(side1 = 'h') {
  side2 <- ifelse(side1 == 'h', 'a', 'h')
  left_join(
    fouls_by_state %>% filter(side == !!side1),
    fouls_by_state %>% 
      filter(side == !!side2) %>% 
      select(
        match_id,
        time_lo,
        time_hi,
        action,
        n_foul,
        n_foul_p90
      ) %>%
      rename(n_foul_opp = n_foul, n_foul_p90_opp = n_foul_p90) 
  )
}

fouls_by_state_redux <- 
  bind_rows(.rejoin('h'), .rejoin('a')) %>% 
  .postprocess_redux() %>% 
  arrange(league, season, date, match_id, time_lo, time_hi, team, team_opp)
fouls_by_state_redux

fouls_by_state_redux_filt <-
  fouls_by_state_redux %>% 
  filter(action == 'committed') %>% 
  # filter(time_diff > (60 * 5)) %>% 
  filter(season == 2020L) %>% 
  filter(g_state_grp != 'neutral')
fouls_by_state_redux_filt


fouls_by_team <-
  fouls_by_state_redux_filt %>%
  group_by(league, season, team, g_state_grp, prob_grp2) %>% 
  summarize(
    n = sum(!is.na(n_foul)),
    across(c(n_foul, n_foul_opp, time_diff), sum)
  ) %>% 
  ungroup() %>% 
  .add_n_foul_p90_col() %>% 
  .add_n_foul_p90_col(suffix = 'opp') %>% 
  .postprocess_redux() %>% 
  arrange(league, season, team) %>% 
  filter(time_diff > (60 * 90))  %>% 
  group_by(league, season, g_state_grp, prob_grp2) %>% 
  mutate(
    rnk_inv = row_number(n_foul_p90_diff)
  ) %>% 
  ungroup()
fouls_by_team

# fouls_by_team %>% filter(team == 'Man City')
# fouls_by_team %>% filter(team == 'Liverpool')
# fouls_by_team %>% filter(team == 'Burnley')
# fouls_by_team %>% filter(team == 'Tottenham')
prob_grp2_pretty <- c('Underdog', 'Favorite')
g_state_grp_pretty <- c('Behind', 'Ahead')
labs_prob_grps <-
  tibble(
    prob_grp2 = c('dog', 'fav'),
    prob_grp2_pretty = prob_grp2_pretty
  ) %>% 
  mutate(idx_prob = row_number())
labs_prob_grps

labs_g_state <-
  tibble(
    g_state_grp = c('behind', 'ahead'),
    g_state_grp_pretty = g_state_grp_pretty 
  ) %>% 
  mutate(idx_g_state = row_number())
labs_g_state

lvls_grp <- c('(1) Underdog Playing with a Lead', '(2) Favorite Playing from Behind', 'Underdog Playing from Behind', 'Favorite Playing From Behind')
.case_when <- function(x, lvls) {
  data %>% 
    mutate(
      !!sym(col_res) :=
        case_when(
          prob_grp2 == 'Underdog' & g_state_grp == 'Ahead' ~ lvls[1]
          prob_grp2 == 'Favorite' & g_state_grp == 'Behind' ~ lvls[2],
          prob_grp2 == 'Underdog' & g_state_grp == 'Behind' ~ lvls[3],
          prob_grp2 == 'Favorite' & g_state_grp == 'Ahead' ~ lvls[4],
        )
    )
}

labs_grps <-
  crossing(
    prob_grp2_pretty = prob_grp2_pretty,
    g_state_grp_pretty = g_state_grp_pretty 
  ) %>% 
  mutate(
    grp = case_when(
      prob_grp2 == 'Underdog' & g_state_grp == 'Ahead' ~ '(1) Underdog Playing with a Lead'
      prob_grp2 == 'Favorite' & g_state_grp == 'Behind' ~ '(2) Favorite Playing from Behind',
      prob_grp2 == 'Underdog' & g_state_grp == 'Behind' ~ 'Underdog Playing from Behind',
      prob_grp2 == 'Favorite' & g_state_grp == 'Ahead' ~ 'Favorite Playing From Behind',
    ),
    idx = case_when(
      prob_grp2 == 'Underdog' & g_state_grp == 'Ahead' ~ '(1) Underdog Playing with a Lead'
      prob_grp2 == 'Favorite' & g_state_grp == 'Behind' ~ '(2) Favorite Playing from Behind',
      prob_grp2 == 'Underdog' & g_state_grp == 'Behind' ~ 'Underdog Playing from Behind',
      prob_grp2 == 'Favorite' & g_state_grp == 'Ahead' ~ 'Favorite Playing From Behind',
    )
  ) %>% 
  
  mutate(idx_g_state = row_number())
labs_grps

# # Reference: https://themockup.blog/posts/2020-10-11-embedding-images-in-ggplot/
# .link_to_img <- function(x, width = 50) {
#   glue::glue("<img src='{x}' width='{width}'/>")
# }

dir_img <- file.path(dir_proj, 'img')
fouls_by_team_pretty <-
  fouls_by_team %>% 
  left_join(
    xengagement::team_accounts_mapping %>%
      # mutate(href = .link_to_img(url_logo_espn)) %>% 
      mutate(path_local = file.path(dir_img, sprintf('%s.png', team))) %>% 
      select(team, path_local, url = url_logo_espn, color_pri)
  ) %>% 
  left_join(labs_prob_grps) %>% 
  left_join(labs_g_state) %>% 
  select(-c(prob_grp2, g_state_grp)) %>% 
  rename(prob_grp2 = prob_grp2_pretty, g_state_grp = g_state_grp_pretty) %>% 
  mutate(
    across(prob_grp2, ~fct_reorder(.x, idx_prob)),
    across(g_state_grp, ~ fct_reorder(.x, idx_g_state))
  ) %>% 
  mutate(
    grp = sprintf('%s %s', prob_grp2, ifelse(g_state_grp == 'Ahead', 'Playing with the Lead', 'Playing from Behind')),
    grp = case_when(
      prob_grp2 == 'Underdog' & g_state_grp == 'Ahead' ~ 'Underdog Playing with a Lead'
    )
    # across(grp, ~fct_reorder(.x, idx_prob + 2 * idx_g_state))
    across(grp, ~ordered(.x, levels = c('Underdog Playing with the Lead', )))
  ) %>% 
  select(-c(idx_prob, idx_g_state))
fouls_by_team_pretty
fouls_by_team_pretty$grp %>% levels()
bind_rows(
fouls_by_team_pretty %>% 
  filter(grp == 'Underdog Playing with the Lead' & n_foul_p90_diff > 0),
fouls_by_team_pretty %>% 
  filter(grp == 'Favorite Playing from Behind' & n_foul_p90_diff > 0),
fouls_by_team_pretty %>% 
  filter(grp == 'Underdog Playing from Behind' & n_foul_p90_diff < 0),
) %>% 
  count(team) %>% 
  filter(n == 3L)

# Do this once.
# fs::dir_create(dir_img)
# fouls_by_team_pretty %>% 
#   distinct(url, path_local) %>% 
#   mutate(res = map2(url, path_local, ~download.file(url = ..1, destfile = ..2, quiet = TRUE, mode = 'wb')))
# asp_ratio <- 1.618
# .pts <- function(x) {
#   as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
# }

.plot_team <- function(.team, height = 8) {
  # .team <- 'Liverpool'
  viz <-
    fouls_by_team_pretty %>% 
    # filter(prob_grp2 == 'Underdog', g_state_grp == 'Ahead') %>% 
    ggplot() +
    aes(y = rnk_inv, x = n_foul_p90_diff) +
    geom_vline(aes(xintercept = 0), linetype = 2) +
    geom_segment(
      data = fouls_by_team_pretty %>% filter(team == .team), #  %>% filter(prob_grp2 == 'Underdog', g_state_grp == 'Ahead'),
      show.legend = FALSE,
      size = 1.25,
      aes(x = n_foul_p90_diff, xend = 0, y = rnk_inv, yend = rnk_inv, color = I(color_pri))
    ) +
    geom_text(
      data = fouls_by_team_pretty, #  %>% filter(team == .team), #  %>% filter(prob_grp2 == 'Underdog', g_state_grp == 'Ahead'),
      aes(label = team), 
      show.legend = FALSE
    ) +
    # ggimage::geom_image(
    #   data = fouls_by_team_pretty %>% filter(team == .team) %>% filter(prob_grp2 == 'Underdog', g_state_grp == 'Ahead'),
    #   aes(image = path_local), # , width = I(time_diff)), by= 'height'
    # ) +
    # scale_size(range = c(0.01, 0.1)) +
    facet_wrap(~grp, scales = 'free_y', ncol = 2, labeller = grp) +
    theme(
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank()
    ) +
    labs(
      x = 'Fouls per 90 min. Relative to Opponent',
      y = NULL
    )
  viz
  
  ggsave(
    plot = viz,
    filename = file.path(dir_proj, sprintf('viz_%s.png', tolower(.team))),
    height = height,
    width = height * asp_ratio,
    type = 'cairo'
  )
  viz
}

fouls_by_team %>% distinct(team) %>% pull(team) %>% walk(.plot_team)

team <- 'Tottenham'
color_pri <- xengagement::team_accounts_mapping %>% filter(team == !!team) %>% pull(color_pri)
viz <-
  fouls_by_team_pretty %>% 
  ggplot() +
  aes(y = rnk_inv, x = n_foul_p90_diff) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  geom_segment(
    data = fouls_by_team_pretty %>% filter(team == !!team),
    show.legend = FALSE,
    size = 2,
    aes(x = n_foul_p90_diff, xend = 0, y = rnk_inv, yend = rnk_inv, color = I(color_pri))
  ) +
  # geom_text(
  #   data = fouls_by_team_pretty,
  #   aes(label = team), 
  #   show.legend = FALSE
  # ) +
  ggimage::geom_image(
    data = fouls_by_team_pretty %>% filter(team != !!team),
    size = 0.1,
    aes(image = path_local), # , width = I(time_diff)), by= 'height'
  ) +
  ggimage::geom_image(
    data = fouls_by_team_pretty %>% filter(team == !!team),
    size = 0.2,
    aes(image = path_local), # , width = I(time_diff)), by= 'height'
  ) +
  scale_size(range = c(0.01, 0.1)) +
  facet_wrap(~grp, scales = 'free_y', ncol = 2) +
  theme(
    plot.title = element_text(size = 22, hjust = 0),
    plot.subtitle = ggtext::element_markdown(size = 12, hjust = 0),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = 'Relative Foul Frequency By Game State',
    subtitle = glue::glue('Under Mourinho, <b><span style="color:{color_pri};">Tottenham</span></b> played more aggressively based on game state. Spurs are the only team that fouled<br/>(1) more often than their opponent when not favored to win ("Underdog") and playing with the lead, and <br/>(2) more often when the favorite and playing from behind, and,<br/>otherwise, fouled less often.'),
    x = 'Fouls per 90 min. Relative to Opponent',
    y = NULL
  )
viz

# h <- 6
ggsave(
  plot = viz,
  filename = file.path(dir_proj, 'viz_foul_p90_diff.png'),
  # height = (2 * h),
  # width = h * asp_ratio,
  height = 12,
  width = 12,
  type = 'cairo'
)

