## data ----
## https://tonyelhabr.rbind.io/posts/fbref-gamestate-expected-goal-difference/
library(worldfootballR) ## version: 0.6.4.9

## data manipulation
library(dplyr)
library(lubridate)
library(tidyr)

COUNTRY <- 'USA'
GENDER <- 'M'
TIER <- '1st'
SEASON_END_YEAR <- 2023

raw_shots <- worldfootballR::load_fb_match_shooting(
  country = COUNTRY,
  gender = GENDER,
  tier = TIER,
  season_end_year = SEASON_END_YEAR
)

raw_match_summaries <- worldfootballR::load_fb_match_summary(
  country = COUNTRY,
  gender = GENDER,
  tier = TIER,
  season_end_year = SEASON_END_YEAR
)

extract_fbref_match_id <- function(match_url) {
  basename(dirname(match_url))
}

## distinct because for some reason there are alternate URLs for games, e.g.
## 1. (good) https://fbref.com/en/matches/5c57984f/Atlanta-United-San-Jose-Earthquakes-February-25-2023-Major-League-Soccer
## 2. (bad) https://fbref.com/en/matches/5c57984f/Atlanta-United-San-Jose-Clash-February-25-2023-Major-League-Soccer

match_summaries <- raw_match_summaries |> 
  dplyr::filter(
    Matchweek == 'Major League Soccer (Regular Season)'
  ) |> 
  dplyr::group_by(MatchURL) |> 
  dplyr::mutate(
    match_summary_rn = dplyr::row_number(dplyr::desc(Event_Time)),
    match_has_no_penalties = all(Event_Type != 'Penalty')
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    match_has_no_goals = Away_Score == 0 & Home_Score == 0
  ) |> 
  ## Drop non-shot events, e.g. card and substitution events. 
  ##   Always keep the first timeline event, so that we're not accidentally dropping matches.
  dplyr::filter(
    Event_Type %in% c('Goal', 'Own Goal', 'Penalty') | 
      ## don't drop games with no goals
      (match_has_no_goals & match_has_no_penalties & match_summary_rn == 1)
  ) |> 
  dplyr::transmute(
    match_id = extract_fbref_match_id(MatchURL),
    season = Season_End_Year,
    gender = Gender,
    tier = Tier,
    date = lubridate::ymd(Match_Date),
    home_team = Home_Team ,
    away_team = Away_Team,
    period = as.integer(Event_Half),
    ## ensure that minutes always has a value
    minutes = dplyr::case_when(
      period == 1L & Event_Time > 45L ~ 45L, 
      period == 2L & Event_Time > 90L ~ 90L,
      .default = Event_Time
    ) |> as.integer(),
    minutes_added = dplyr::case_when(
      period == 1L & Event_Time > 45 ~ Event_Time - 45L, 
      period == 2L & Event_Time > 90 ~ Event_Time - 90L,
      .default = NA_integer_
    ),
    home_g = as.integer(gsub('[:].*$', '', Score_Progression)), ## after event
    away_g = as.integer(gsub('^.*[:]', '', Score_Progression)),
    is_own_goal = Event_Type == 'Own Goal',
    team = Team,
    player = Event_Players
  ) |> 
  dplyr::distinct()

shots <- raw_shots |> 
  # dplyr::filter(Squad == 'Atlanta Utd') |> 
  # dplyr::filter(
  #   MatchURL == 'https://fbref.com/en/matches/5c57984f/Atlanta-United-San-Jose-Earthquakes-February-25-2023-Major-League-Soccer'
  # ) |>
  dplyr::transmute(
    # MatchURL,
    match_id = extract_fbref_match_id(MatchURL),
    period = as.integer(Match_Half),
    ## convert "45+2" to "45"
    minutes = ifelse(
      grepl('[+]', Minute),
      as.integer(gsub('(^[0-9]+)[+]([0-9]+$)', '\\1', Minute)), 
      as.integer(Minute)
    ),
    ## convert "45+2" to "2"
    minutes_added = ifelse(
      grepl('[+]', Minute), 
      as.integer(gsub('(^[0-9]+)[+]([0-9]+$)', '\\2', Minute)), 
      NA_integer_
    ),
    is_home = Home_Away == 'Home',
    team = Squad,
    player = Player,
    is_goal = Outcome == 'Goal',
    xg = as.double(xG)
  ) |> 
  dplyr::distinct()
# raw_shots |> filter(MatchURL == 'https://fbref.com/en/matches/5c57984f/Atlanta-United-San-Jose-Earthquakes-February-25-2023-Major-League-Soccer')
shots |> filter(match_id == '5c57984f')
shots |> filter(match_id == '5c57984f') |> distinct()

shots_with_own_goals <- dplyr::bind_rows(
  shots |> 
    dplyr::transmute(
      match_id,
      period,
      minutes,
      minutes_added,
      is_home,
      team,
      player,
      is_goal,
      xg,
      is_own_goal = FALSE
    ),
  ## synthetic events for own goals
  match_summaries |> 
    dplyr::filter(
      is_own_goal
    ) |> 
    dplyr::transmute(
      match_id,
      period,
      minutes,
      minutes_added,
      is_home = team == home_team,
      team,
      player,
      is_goal = TRUE,
      xg = NA_real_,
      is_own_goal = TRUE
    )
)

clean_shots <- shots_with_own_goals |> 
  ## To get meta-information about the game
  dplyr::inner_join(
    match_summaries |>
      dplyr::distinct(match_id, home_team, away_team),
    by = dplyr::join_by(match_id),
    relationship = 'many-to-one'
  ) |> 
  dplyr::mutate(
    home_g = dplyr::case_when(
      ## Note that fotmob would list the away team for an own goal but fbref 
      ##   lists the home team
      (is_goal | is_own_goal) & is_home ~ 1L,
      is_own_goal & is_home ~ 1L,
      TRUE ~ 0L
    ),
    away_g = dplyr::case_when(
      (is_goal | is_own_goal) & !is_home ~ 1L,
      TRUE ~ 0L
    ),
    home_xg = dplyr::case_when(
      is_home ~ dplyr::coalesce(xg, 0),
      TRUE ~ 0L ## even for own goals
    ),
    away_xg = dplyr::case_when(
      !is_home ~ dplyr::coalesce(xg, 0),
      TRUE ~ 0L
    )
  ) |>
  dplyr::group_by(match_id) |>
  ## Differentiate between shots in the same minute.
  dplyr::mutate(
    shot_idx = dplyr::row_number((minutes + dplyr::coalesce(minutes_added, 0L)))
  ) |> 
  dplyr::ungroup() |> 
  dplyr::transmute(
    shot_id = sprintf('%s-%02d', match_id, shot_idx),
    match_id,
    period,
    minutes,
    minutes_added,
    is_home,
    is_goal,
    is_own_goal,
    player,
    home_team,
    away_team,
    home_g,
    away_g,
    home_xg,
    away_xg
  )

restacked_shots <- dplyr::bind_rows(
  clean_shots |> 
    dplyr::filter(is_home) |> 
    dplyr::transmute(
      shot_id,
      match_id,
      period,
      minutes,
      minutes_added,
      is_home,
      is_goal,
      is_own_goal,
      player,
      team = home_team,
      opponent = away_team,
      g = home_g,
      g_conceded = away_g,
      xg = home_xg,
      xg_conceded = away_xg
    ),
  clean_shots |> 
    dplyr::filter(!is_home) |> 
    dplyr::transmute(
      shot_id,
      match_id,
      period,
      minutes,
      minutes_added,
      is_home,
      is_goal,
      is_own_goal,
      player,
      team = away_team,
      opponent = home_team,
      g = away_g,
      g_conceded = home_g,
      xg = away_xg,
      xg_conceded = home_xg
    )
)

doublecounted_restacked_shots <- dplyr::bind_rows(
  restacked_shots |> dplyr::mutate(pov = 'primary', .before = 1),
  restacked_shots |> 
    ## re-assign to temporary variable names first, so that way we don't accidentlaly overwrite information
    dplyr::rename(
      team1 = team,
      team2 = opponent,
      g1 = g,
      g2 = g_conceded,
      xg1 = xg,
      xg2 = xg_conceded
    ) |> 
    ## then formally re-assign columns
    dplyr::rename(
      team = team2,
      opponent = team1,
      g = g2,
      g_conceded = g1,
      xg = xg2,
      xg_conceded = xg1
    ) |> 
    dplyr::mutate(
      is_home = !is_home
    ) |> 
    dplyr::mutate(
      pov = 'secondary',
      .before = 1
    )
) |> 
  dplyr::arrange(match_id, shot_id, pov)

cumu_doublecounted_restacked_shots <- doublecounted_restacked_shots |> 
  dplyr::group_by(match_id, team) |> 
  dplyr::mutate(
    dplyr::across(
      c(g, g_conceded),
      list(cumu = cumsum)
    )
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    gamestate = g_cumu - g_conceded_cumu
  )

gamestate_shots <- cumu_doublecounted_restacked_shots |> 
  dplyr::inner_join(
    match_summaries |> 
      dplyr::distinct(
        match_id,
        season,
        date,
        home_team,
        away_team
      ),
    by = dplyr::join_by(match_id)
  ) |> 
  dplyr::transmute(
    pov,
    match_id,
    season,
    date,
    home_team,
    away_team,
    team,
    player,
    shot_id,
    period,
    minutes,
    minutes_added,
    time = minutes + dplyr::coalesce(minutes_added, 0L),
    g,
    g_conceded,
    g_cumu,
    g_conceded_cumu,
    xg,
    xgd = xg - xg_conceded,
    gamestate,
    g_sum_cumu = g_cumu + g_conceded_cumu
  ) |> 
  dplyr::group_by(match_id, team) |> 
  dplyr::arrange(shot_id, .by_group = TRUE) |> 
  dplyr::mutate(
    pre_shot_gamestate = dplyr::lag(gamestate),
    pre_shot_g_sum_cumu = dplyr::lag(g_sum_cumu)
  ) |> 
  dplyr::ungroup()

LAST_MIN_BUFFER <- 3
last_min_pad <- gamestate_shots |>
  dplyr::select(
    match_id,
    season,
    date,
    team,
    # pre_shot_gamestate,
    period,
    time
  ) |> 
  dplyr::group_by(match_id, team, period) |>
  dplyr::slice_max(time, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    g = 0,
    g_conceded = 0,
    xg = 0,
    xgd = 0,
    last_regular_min = ifelse(period == 1L, 45L, 90L),
    time = pmax(last_regular_min + LAST_MIN_BUFFER, time + 1)
  )

padded_gamestate_shots <- dplyr::bind_rows(
  gamestate_shots,
  last_min_pad
) |> 
  dplyr::arrange(match_id, time) |> 
  dplyr::group_by(match_id, team) |> 
  tidyr::fill(
    g_cumu,
    g_conceded_cumu,
    g_sum_cumu,
    gamestate,
    .direction = 'down'
  ) |>
  dplyr::mutate(
    pre_shot_gamestate = dplyr::lag(gamestate),
    pre_shot_g_sum_cumu = dplyr::lag(g_sum_cumu)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    pre_shot_gamestate = dplyr::coalesce(pre_shot_gamestate, 0),
    pre_shot_g_sum_cumu = dplyr::coalesce(pre_shot_g_sum_cumu, 0)
  )

gamestate_shots_and_durations <- padded_gamestate_shots |> 
  dplyr::group_by(match_id, team) |> 
  dplyr::mutate(
    prev_period = dplyr::lag(period),
    prev_time = dplyr::lag(time)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    duration = dplyr::case_when(
      period == 1L & is.na(prev_period) ~ time - 0L,
      period == 2L & period != prev_period ~ time - 45L,
      TRUE ~ time - prev_time
    )
  )

prop_duration_init_gamestate <- gamestate_shots_and_durations |> 
  dplyr::group_by(team, match_id) |> 
  dplyr::filter(
    g_sum_cumu == 1 & (pre_shot_g_sum_cumu == 0 | is.na(pre_shot_g_sum_cumu))
  ) |> 
  dplyr::ungroup() |> 
  dplyr::select(
    match_id,
    team,
    time
  ) |> 
  dplyr::right_join(
    last_min_pad |> 
      dplyr::filter(period == 2) |> 
      dplyr::distinct(match_id, team, last_time = time),
    by = dplyr::join_by(match_id, team)
  ) |> 
  dplyr::mutate(
    time = dplyr::coalesce(time, last_time)
  ) |> 
  dplyr::group_by(team) |> 
  dplyr::summarize(
    total_duration = sum(last_time),
    duration = sum(time)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    prop_duration = duration / total_duration,
    # time_p96 = time / (34 * 96)
    duration_p96 = prop_duration * 96
  ) |> 
  dplyr::arrange(prop_duration)

g_sum_totals <- gamestate_shots_and_durations |> 
  dplyr::group_by(team) |> 
  dplyr::summarize(
    dplyr::across(
      c(g, g_conceded),
      sum
    )
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    g_sum = g + g_conceded
  ) |> 
  dplyr::arrange(dplyr::desc(g_sum))

dplyr::inner_join(
  prop_duration_init_gamestate,
  g_sum_totals
) |> 
  dplyr::transmute(
    team,
    g_sum_rank = dplyr::row_number(dplyr::desc(g_sum)),
    duration_init_gamestate_rank = dplyr::row_number(prop_duration)
  ) |> 
  dplyr::arrange(
    dplyr::desc(abs(g_sum_rank - duration_init_gamestate_rank))
  )
  corrr::correlate(method = 'spearman')

## plot ----
library(qs)
library(ggplot2)
library(ggtext)
library(glue)
library(scales)
library(magick)

PROJ_DIR <- '81-2023_mls_g_minus_xg'

FONT <- 'Oswald'
GRAYISH_COLOR <- '#9f9f9f'
BLACKISH_COLOR <- '#15202B'
theme_asa <- function(...) {
  list(
    ...,
    theme_minimal(base_family = FONT),
    theme(
      text = element_text(color = 'white', size = 20),
      axis.text.y = element_markdown(color = 'white'),
      axis.text.x = element_text(color = 'white', size = 11),
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = 'top',
      plot.title = element_markdown(size = 20, hjust = 0),
      plot.title.position = 'plot',
      plot.caption.position = 'plot',
      plot.subtitle = element_text(color = 'white', size = 14),
      plot.caption = element_markdown(color = GRAYISH_COLOR, size = 11),
      panel.border = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = GRAYISH_COLOR),
      panel.background = element_rect(fill = BLACKISH_COLOR, color = BLACKISH_COLOR),
      plot.background = element_rect(fill = BLACKISH_COLOR, color = BLACKISH_COLOR)
    )
  )
}

qs_dir <- '../itsmetoeknee/posts/fbref-gamestate-expected-goal-difference'
team_logos <- qs::qread(file.path(qs_dir, 'team_logos.qs'))

prop_duration_init_gamestate_with_logos <- prop_duration_init_gamestate |> 
  dplyr::inner_join(
    team_logos |> 
      dplyr::select(
        team,
        pts,
        path
      ),
    by = dplyr::join_by(team)
  ) |> 
  dplyr::mutate(
    label = glue::glue("<span style='font-size:13px;color:white'>{team}</span> <img src='{path}' width='14' height='14'/>")
  ) |> 
  dplyr::select(-path)

team_label_order <- prop_duration_init_gamestate_with_logos$team
prepped_prop_duration_init_gamestate <- prop_duration_init_gamestate_with_logos |> 
  dplyr::mutate(
    dplyr::across(
      team,
      \(.x) factor(.x, levels = team_label_order)
    )
  )

sysfonts::font_add_google(FONT)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

duration_plot <- prepped_prop_duration_init_gamestate |> 
  ggplot2::ggplot() +
  theme_asa() +
  ggplot2::aes(
    x = prop_duration,
    y = team
  ) +
  ggplot2::geom_col(
    data = prepped_prop_duration_init_gamestate,
    show.legend = FALSE,
    fill = '#9f9f9f'
  ) +
  ggplot2::geom_col(
    data = prepped_prop_duration_init_gamestate |> dplyr::filter(team == 'Atlanta United'),
    show.legend = FALSE,
    fill = '#01C4E7'
  ) +
  ggplot2::scale_y_discrete(
    name = '',
    labels = prepped_prop_duration_init_gamestate |>
      dplyr::distinct(team, label) |>
      tibble::deframe()
  ) +
  ggplot2::theme(
    axis.text.y = ggtext::element_markdown(margin = grid::unit(c(0, 0, 0, 0), 'pt')),
  ) +
  ggplot2::scale_x_continuous(
    labels = scales::percent,
    breaks = c(0, 1),
    limits = c(0, 1)
  ) +
  ggplot2::geom_text(
    family = FONT,
    size = 11 / ggplot2::.pt,
    # fontface = 'bold',
    color = 'white',
    hjust = -0.1,
    ggplot2::aes(
      x = prop_duration,
      y = team,
      label = scales::percent(prop_duration, accuracy = 0.1)
    )
  ) +
  ggplot2::labs(
    title = '% of Match Time Spent in 0 - 0 Game State',
    subtitle = '2023 MLS',
    x = '% of Match Time',
    y = NULL
  ) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size = 12)
  )

plot_path <- file.path(PROJ_DIR, '2023_mls_prop_duration_init_gamestate.png')
ggsave(
  duration_plot,
  filename = plot_path,
  units = 'in',
  width = 6,
  height = 8
)

add_logo <- function(
    plot_path,
    logo_path,
    logo_scale = 0.1,
    idx_x = 0.01, ## right-hand side
    idx_y = 0.99, ## top of plot
    adjust_x = ifelse(idx_x < 0.5, TRUE, FALSE),
    adjust_y = ifelse(idx_y < 0.5, TRUE, FALSE)
) {
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  logo <- magick::image_scale(
    logo_raw,
    as.character(round(plot_width * logo_scale))
  )
  
  info <- magick::image_info(logo)
  logo_width <- info$width
  logo_height <- info$height
  
  x_pos <- plot_width - idx_x * plot_width
  y_pos <- plot_height - idx_y * plot_height
  
  if (isTRUE(adjust_x)) {
    x_pos <- x_pos - logo_width
  }
  
  if (isTRUE(adjust_y)) {
    y_pos <- y_pos - logo_height
  }
  
  offset <- paste0('+', x_pos, '+', y_pos)
  
  new_plot <- magick::image_composite(plot, logo, offset = offset)
  ext <- tools::file_ext(plot_path)
  rgx_ext <- sprintf('[.]%s$', ext)
  
  magick::image_write(
    new_plot,
    plot_path
  )
}

asa_logo_path <- file.path(PROJ_DIR, 'ASAlogo.png')
add_logo(
  plot_path,
  asa_logo_path,
  logo_scale = 0.25,
  idx_x = 0.98,
  idx_y = 0.05,
  adjust_x = FALSE,
  adjust_y = FALSE
)

