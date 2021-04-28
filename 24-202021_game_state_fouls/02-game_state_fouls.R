
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
  plot.caption = ggtext::element_markdown('Karla', size = 14, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 14, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.01),
  legend.text = element_text(color = 'gray20', size = 14),
  strip.text = element_text(color = 'gray20', size = 14),
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

# by game state ----
.group_by_team <- function(...) {
  fouls_by_state_redux_filt %>%
    group_by(league, season, team, ...) %>% 
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
    group_by(league, season, ...) %>% 
    mutate(
      rnk_inv = row_number(n_foul_p90_diff)
    ) %>% 
    ungroup()
}
fouls_by_team <- .group_by_team(g_state_grp)

lvls_grp <-
  c(
    'Playing with a Lead',
    'Playing from Behind'
  )
pal <- c('#003f5c', '#ffa600')

.map2_colors <- function(lab, color) {
  glue::glue('<b><span style="color:{color};">{lab}</span></b>')
}
labs_md <- .map2_colors(lvls_grp, pal)
labs_md

lab_grps <-
  crossing(
    g_state_grp = c('behind', 'ahead')
  ) %>% 
  mutate(grp = g_state_grp) %>% 
  mutate(
    idx_grp =
      case_when(
        g_state_grp == 'ahead' ~ 1L,
        g_state_grp == 'behind' ~ 2L
      ),
    grp = fct_reorder(grp, idx_grp)
  )
lab_grps

dir_img <- file.path(dir_proj, 'img')
fouls_by_team_pretty <-
  fouls_by_team %>% 
  left_join(
    xengagement::team_accounts_mapping %>%
      # mutate(href = .link_to_img(url_logo_espn)) %>% 
      mutate(path_local = file.path(dir_img, sprintf('%s.png', team))) %>% 
      select(team, path_local, url = url_logo_espn, color_pri)
  ) %>% 
  left_join(lab_grps)
fouls_by_team_pretty

labs_xy <-
  fouls_by_team_pretty %>% 
  group_by(idx_grp, grp) %>% 
  summarize(
    across(c(rnk_inv, n_foul_p90_diff), list(min = min, max = max))
  ) %>% 
  ungroup() %>% 
  left_join(tibble(lab = labs_md) %>% mutate(idx_grp = row_number()))
labs_xy

# Do this once.
if(FALSE) {
  fs::dir_create(dir_img)
  fouls_by_team_pretty %>%
    distinct(url, path_local) %>%
    mutate(res = map2(url, path_local, ~download.file(url = ..1, destfile = ..2, quiet = TRUE, mode = 'wb')))
}

.pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

lab_tag <- '**Viz**: Tony ElHabr | **Data**: 2020-21 Premier League through Matchweek 32'
viz_by_state <-
  fouls_by_team_pretty %>% 
  ggplot() +
  aes(y = rnk_inv, x = n_foul_p90_diff) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  geom_segment(
    data = fouls_by_team_pretty,
    show.legend = FALSE,
    size = 1,
    color = 'black',
    aes(x = n_foul_p90_diff, xend = 0, y = rnk_inv, yend = rnk_inv)
  ) +
  ggimage::geom_image(
    data = fouls_by_team_pretty,
    size = 0.05,
    aes(image = path_local),
  ) +
  ggtext::geom_richtext(
    # fill = NA, 
    label.color = NA,
    size = .pts(14),
    family = 'Karla',
    fontface = 'bold',
    inherit.aes = FALSE,
    data = labs_xy,
    hjust = 0,
    aes(y = rnk_inv_max, x = min(n_foul_p90_diff_min) + 0.1, label = lab)
  ) +
  facet_wrap(~grp, scales = 'free_y', ncol = 2) +
  theme(
    strip.text = element_blank(),
    panel.background = element_rect(color = 'grey80', size = 2),
    # strip.background = element_rect(color = '#000000', size = 10),
    plot.title = element_text(size = 18, hjust = 0),
    plot.subtitle = ggtext::element_markdown(size = 12, hjust = 0),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = 'Team Foul Rate Relative to Opponent',
    subtitle = glue::glue('Which teams foul more/less frequently than their opponent when playing <b><span style="color:{pal[1]};">with the lead</span></b> or <b><span style="color:{pal[2]};">from behind</span></b>?'),
    # caption = '',
    tag = lab_tag,
    x = 'Fouls per 90 min. Relative to Opponent',
    y = NULL
  )
viz_by_state

h <- 8
asp_ratio <- 1.618
ggsave(
  plot = viz_by_state,
  filename = file.path(dir_proj, 'viz_foul_p90_diff_by_state.png'),
  height = h,
  width = h * asp_ratio,
  type = 'cairo'
)

# by prob_grp2 ----
fouls_by_team <- .group_by_team(prob_grp2)
lvls_grp <-
  c(
    'Underdog',
    'Favorite'
  )
pal <- c('#7a5195', '#ef5675')

labs_md <- .map2_colors(lvls_grp, pal)
labs_md

lab_grps <-
  crossing(
    prob_grp2 = c('dog', 'fav')
  ) %>% 
  mutate(grp = prob_grp2) %>% 
  mutate(
    idx_grp =
      case_when(
        prob_grp2 == 'dog' ~ 1L,
        prob_grp2 == 'fav' ~ 2L
      ),
    grp = fct_reorder(grp, idx_grp)
  )
lab_grps

fouls_by_team_pretty <-
  fouls_by_team %>% 
  left_join(
    xengagement::team_accounts_mapping %>%
      mutate(path_local = file.path(dir_img, sprintf('%s.png', team))) %>% 
      select(team, path_local, url = url_logo_espn, color_pri)
  ) %>% 
  left_join(lab_grps)
fouls_by_team_pretty

labs_xy <-
  fouls_by_team_pretty %>% 
  group_by(idx_grp, grp) %>% 
  summarize(
    across(c(rnk_inv, n_foul_p90_diff), list(min = min, max = max))
  ) %>% 
  ungroup() %>% 
  left_join(tibble(lab = labs_md) %>% mutate(idx_grp = row_number()))
labs_xy

viz_by_prob <-
  fouls_by_team_pretty %>% 
  ggplot() +
  aes(y = rnk_inv, x = n_foul_p90_diff) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  geom_segment(
    data = fouls_by_team_pretty,
    show.legend = FALSE,
    size = 1,
    color = 'black',
    aes(x = n_foul_p90_diff, xend = 0, y = rnk_inv, yend = rnk_inv)
  ) +
  ggimage::geom_image(
    data = fouls_by_team_pretty,
    size = 0.05,
    aes(image = path_local),
  ) +
  ggtext::geom_richtext(
    # fill = NA, 
    label.color = NA,
    size = .pts(16),
    family = 'Karla',
    fontface = 'bold',
    inherit.aes = FALSE,
    data = labs_xy,
    hjust = 0,
    aes(y = rnk_inv_max, x = min(n_foul_p90_diff_min) + 0.1, label = lab)
  ) +
  facet_wrap(~grp, scales = 'free_y', ncol = 2) +
  theme(
    strip.text = element_blank(),
    panel.background = element_rect(color = 'grey80', size = 2),
    # strip.background = element_rect(color = '#000000', size = 10),
    plot.title = element_text(size = 18, hjust = 0),
    plot.subtitle = ggtext::element_markdown(size = 12, hjust = 0),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = 'Team Foul Rate Relative to Opponent',
    subtitle = glue::glue('Which teams foul more/less frequently than they are the pre-game <b><span style="color:{pal[1]};">underdog</span></b> vs. <b><span style="color:{pal[2]};">favorite</span></b>?'),
    # caption = 'Favorites and underdogs determined based on @FiveThirtyEight\'s pre-game win probabilities.',
    tag = lab_tag,
    x = 'Fouls per 90 min. Relative to Opponent',
    y = NULL
  )
viz_by_prob

ggsave(
  plot = viz_by_prob,
  filename = file.path(dir_proj, 'viz_foul_p90_diff_by_prob.png'),
  height = h,
  width = h * asp_ratio,
  type = 'cairo'
)

# by4 ----
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
  group_by(league, season, g_state_grp, prob_grp2) %>% 
  mutate(
    rnk_inv = row_number(n_foul_p90_diff)
  ) %>% 
  ungroup()
fouls_by_team

lvls_grp <-
  c(
    '(1) Underdog Playing with a Lead',
    '(2) Favorite Playing from Behind',
    '(3) Underdog Playing from Behind',
    '(4) Favorite Playing with a Lead'
  )
# paletteer::paletteer_d('ggsci::default_nejm', n = 4) %>% datapasta::vector_paste()
# c("#BC3C29FF", "#0072B5FF", "#E18727FF", "#20854EFF")
# c("#BC3C29FF", "#0072B5FF", "#E18727FF", "#20854EFF")
# paletteer::paletteer_d('LaCroixColoR::PassionFruit', n = 4) %>% datapasta::vector_paste()
pal <- c('#BC3C29FF', '#0072B5FF', '#E18727FF', '#20854EFF') %>% str_remove('FF$')
# pal <- c('#003f5c', '#7a5195', '#ef5675', '#ffa600')

labs_md <- .map2_colors(lvls_grp, pal)

lab_grps <-
  crossing(
    prob_grp2 = c('dog', 'fav'),
    g_state_grp = c('behind', 'ahead')
  ) %>% 
  mutate(grp = sprintf('%s_%s', prob_grp2, g_state_grp)) %>% 
  mutate(
    idx_grp =
      case_when(
        prob_grp2 == 'dog' & g_state_grp == 'ahead' ~ 1L,
        prob_grp2 == 'fav' & g_state_grp == 'behind' ~ 2L,
        prob_grp2 == 'dog' & g_state_grp == 'behind' ~ 3L,
        prob_grp2 == 'fav' & g_state_grp == 'ahead' ~ 4L
      ),
    grp = fct_reorder(grp, idx_grp)
  ) %>% 
  arrange(idx_grp)
lab_grps

fouls_by_team_pretty <-
  fouls_by_team %>% 
  left_join(
    xengagement::team_accounts_mapping %>%
      # mutate(href = .link_to_img(url_logo_espn)) %>% 
      mutate(path_local = file.path(dir_img, sprintf('%s.png', team))) %>% 
      select(team, path_local, url = url_logo_espn, color_pri)
  ) %>% 
  left_join(lab_grps)
fouls_by_team_pretty

labs_xy <-
  fouls_by_team_pretty %>% 
  group_by(idx_grp, grp) %>% 
  summarize(
    across(c(rnk_inv, n_foul_p90_diff), list(min = min, max = max))
  ) %>% 
  ungroup() %>% 
  left_join(tibble(lab = labs_md) %>% mutate(idx_grp = row_number()))
labs_xy

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
    size = 0.05,
    aes(image = path_local), # , width = I(time_diff)), by= 'height'
  ) +
  ggimage::geom_image(
    data = fouls_by_team_pretty %>% filter(team == !!team),
    size = 0.15,
    aes(image = path_local), # , width = I(time_diff)), by= 'height'
  ) +
  ggtext::geom_richtext(
    # fill = NA, 
    label.color = NA,
    size = .pts(14),
    family = 'Karla',
    fontface = 'bold',
    inherit.aes = FALSE,
    data = labs_xy,
    hjust = 0,
    aes(y = rnk_inv_max, x = min(n_foul_p90_diff_min) + 0.1, label = lab)
  ) +
  facet_wrap(~grp, scales = 'free_y', ncol = 2) +
  theme(
    strip.text = element_blank(),
    panel.background = element_rect(color = 'grey80', size = 2),
    plot.title = element_text(size = 18, hjust = 0),
    plot.subtitle = ggtext::element_markdown(size = 12, hjust = 0),
    plot.caption = ggtext::element_markdown(size = 12, hjust = 1),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = 'Will Tottenham Continue Bus Parking Without Mourinho?',
    subtitle = glue::glue('Under Mourinho, <b><span style="color:{color_pri};">Tottenham</span></b> played more aggressively based on game state. Spurs are the only team that has fouled:<br/><br/><b><span style="color:{pal[1]};">(1) more often than their opponent when the underdog and playing with the lead</span></b>,<br/><b><span style="color:{pal[2]};">(2) more often when the favorite and playing from behind</span></b>,<br/><b><span style="color:{pal[3]};">(3) less often when the underdog and playing from ahead</span></b>, and<br/><b><span style="color:{pal[4]};">(4) less often when the favorite and playing with the lead</span></b>.'),
    # caption = glue::glue('Teams with less than 90 minutes in a given state (e.g. Man City as the underdog playing from behind) are not shown.<br/>Favorites and underdogs determined based on @FiveThirtyEight\'s pre-game win probabilities.'),
    caption = glue::glue('Teams with less than 90 minutes in a given state<br/>(e.g. Man City as the underdog playing from behind) are not shown.'
    tag = lab_tag %>% str_replace('\\s|\\s', '<br/>')
    x = 'Fouls per 90 min. Relative to Opponent',
    y = NULL
  )
viz

ggsave(
  plot = viz,
  filename = file.path(dir_proj, 'viz_foul_p90_diff.png'),
  height = 12,
  width = 11,
  type = 'cairo'
)

# Leeds has the largest difference in foul rate relative to their opponent when ahead vs. when behind. They have the 2nd lowest rate when winning and the 5th highest rate when losing. Is this the magic behind Bielsa ball,,,,?


