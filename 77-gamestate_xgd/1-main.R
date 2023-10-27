## data scrape
library(worldfootballR) ## version: 0.6.4.9

## data manipulation
library(dplyr)
library(lubridate)
library(tidyr)

PROJ_DIR <- '77-gamestate_xgd'
COUNTRY <- 'ENG'
GENDER <- 'M'
TIER <- '1st'
SEASON_END_YEAR <- 2024

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

## Extract the from "47880eb7" from "https://fbref.com/en/matches/47880eb7/Liverpool-Manchester-City-November-10-2019-Premier-League"
extract_fbref_match_id <- function(match_url) {
  basename(dirname(match_url))
}

match_summaries <- raw_match_summaries |> 
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
  )

shots <- raw_shots |> 
  dplyr::transmute(
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
  )

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

ORDERED_GAMESTATE_LABELS <- c('Trailing', 'Tied', 'Leading')
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
    xg,
    xgd = xg - xg_conceded,
    gamestate = cut(
      gamestate,
      breaks = c(-Inf, -1, 0, Inf),
      labels = ORDERED_GAMESTATE_LABELS
    )
  ) |> 
  dplyr::group_by(match_id, team) |> 
  dplyr::arrange(shot_id, .by_group = TRUE) |> 
  dplyr::mutate(
    pre_shot_gamestate = dplyr::lag(gamestate, default = ORDERED_GAMESTATE_LABELS[2])
  ) |> 
  dplyr::ungroup()

LAST_MIN_BUFFER <- 3
last_min_pad <- gamestate_shots |>
  dplyr::select(
    match_id,
    season,
    date,
    team,
    pre_shot_gamestate,
    period,
    time
  ) |> 
  dplyr::group_by(match_id, team, period) |>
  dplyr::slice_max(time, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    xg = 0,
    xgd = 0,
    last_regular_min = ifelse(period == 1L, 45L, 90L),
    time = pmax(last_regular_min + LAST_MIN_BUFFER, time + 1)
  )

padded_gamestate_shots <- dplyr::bind_rows(
  gamestate_shots,
  last_min_pad
) |> 
  dplyr::arrange(match_id, time)

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

agg_gamestate_xgd <- gamestate_shots_and_durations |> 
  dplyr::group_by(team, pre_shot_gamestate) |> 
  dplyr::summarize(
    dplyr::across(
      c(
        xgd,
        duration
      ),
      sum
    )
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    xgd_p90 = xgd * 90 / duration
  ) |> 
  dplyr::group_by(team) |> 
  dplyr::mutate(
    prop_duration = duration / sum(duration)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::select(
    team,
    pre_shot_gamestate,
    xgd_p90,
    prop_duration
  )

## logo scraping
library(httr)
library(jsonlite)

## plotting
library(ggplot2)
library(sysfonts)
library(showtext)
library(ggtext)
library(htmltools)
library(grid)
library(scales)

TAG_LABEL <- htmltools::tagList(
  htmltools::tags$span(htmltools::HTML(enc2utf8('&#xf099;')), style = 'font-family:fb'),
  htmltools::tags$span('@TonyElHabr'),
)
PLOT_RESOLUTION <- 300
WHITISH_FOREGROUND_COLOR <- 'white'
COMPLEMENTARY_FOREGROUND_COLOR <- '#cbcbcb'
BLACKISH_BACKGROUND_COLOR <- '#1c1c1c'
COMPLEMENTARY_BACKGROUND_COLOR <- '#4d4d4d'
FONT <- 'Titillium Web'
sysfonts::font_add_google(FONT, FONT)
## https://github.com/tashapiro/tanya-data-viz/blob/main/chatgpt-lensa/chatgpt-lensa.R for twitter logo
sysfonts::font_add('fb', 'Font Awesome 6 Brands-Regular-400.otf')
showtext::showtext_auto()
showtext::showtext_opts(dpi = PLOT_RESOLUTION)

ggplot2::theme_set(ggplot2::theme_minimal())
ggplot2::theme_update(
  text = ggplot2::element_text(family = FONT),
  title = ggplot2::element_text(size = 20, color = WHITISH_FOREGROUND_COLOR),
  plot.title = ggtext::element_markdown(face = 'bold', size = 20, color = WHITISH_FOREGROUND_COLOR),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown(size = 16, color = COMPLEMENTARY_FOREGROUND_COLOR),
  axis.text = ggplot2::element_text(color = WHITISH_FOREGROUND_COLOR, size = 14),
  axis.title.x = ggtext::element_markdown(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.title.y = ggtext::element_markdown(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.line = ggplot2::element_blank(),
  strip.text = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0),
  legend.position = 'top',
  legend.text = ggplot2::element_text(size = 12, color = WHITISH_FOREGROUND_COLOR, face = 'plain'),
  legend.title = ggplot2::element_text(size = 12, color = WHITISH_FOREGROUND_COLOR, face = 'bold'),
  panel.grid.major = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor.x = ggplot2::element_blank(),
  panel.grid.minor.y = ggplot2::element_blank(),
  plot.margin = ggplot2::margin(10, 20, 10, 20),
  plot.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR),
  plot.caption = ggtext::element_markdown(size = 10, color = WHITISH_FOREGROUND_COLOR, hjust = 0, face = 'plain'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 10, color = WHITISH_FOREGROUND_COLOR, hjust = 1),
  plot.tag.position = c(0.99, 0.01),
  panel.spacing.x = grid::unit(2, 'lines'),
  panel.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR)
)
ggplot2::update_geom_defaults('text', list(color = WHITISH_FOREGROUND_COLOR, size = 12 / .pt))

GAMESTATE_PAL <- c(
  'Trailing' = '#fe218b',
  'Tied' = COMPLEMENTARY_FOREGROUND_COLOR,
  'Leading' = '#21b0fe'
)

## There is a way to get team logos from FBref, but they have a white background 
##   by default, and making the background transparent for a plot with a dark
##   background is kind of a pain in the ass. So let's pull images from fotmob.
## This function is basically a minified version of what used to exist as
##   worldfootballR::fotmob_get_league_tables(). I rely on FBref and fotmob listing
##   teams in the same order alphabetically, which works fine for the MLS. A
##   better, scalable strategy for binding team names between sources is to
##   order teams by points / placement in the standings.

get_fotmob_standings <- function() {
  url <- 'https://www.fotmob.com/api/leagues?id=47'
  resp <- httr::GET(url)
  cont <- httr::content(resp, as = 'text')
  result <- jsonlite::fromJSON(cont)
  table_init <- result$table$data
  tables <- dplyr::bind_rows(table_init$table)
  tables$all[[1]] |> 
    dplyr::transmute(
      team = name,
      team_id = id,
      pts,
      logo_url = sprintf('https://images.fotmob.com/image_resources/logo/teamlogo/%s.png', team_id)
    )
}

fotmob_standings <- get_fotmob_standings()

team_names <- agg_gamestate_xgd |> 
  dplyr::distinct(old_team = team) |> 
  dplyr::arrange(old_team) |> 
  dplyr::mutate(
    team = dplyr::case_match(
      old_team,
      'Brighton & Hove Albion' ~ 'Brighton',
      'Wolverhampton Wanderers' ~ 'Wolves',
      'Tottenham Hotspur' ~ 'Tottenham',
      'West Ham United' ~ 'West Ham',
      .default = old_team
    )
  )

team_logos <- team_names |> 
  dplyr::bind_cols(
    fotmob_standings |> 
      dplyr::transmute(
        team_fotmob = gsub(
          'AFC Bournemouth',
          'Bournemouth',
          team
        ),
        path = logo_url,
        pts
      ) |> 
      dplyr::arrange(team_fotmob)
  )

agg_gamestate_xgd_with_logos <- agg_gamestate_xgd |> 
  dplyr::rename(old_team = team) |> 
  dplyr::left_join(
    team_names,
    by = dplyr::join_by(old_team)
  ) |> 
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
    label = glue::glue("<span style='font-size:16px;color:{WHITISH_FOREGROUND_COLOR}'>{team}</span> <span style='font-size:10px;color:{COMPLEMENTARY_FOREGROUND_COLOR}'>{pts} pt{ifelse(pts > 1, 's', '')}</span> <img src='{path}' width='15' height='15'/>")
  ) |> 
  dplyr::select(-path)

# agg_gamestate_xgd_with_logos |>
#   filter(pre_shot_gamestate == 'Tied') |> 
#   mutate(
#     across(
#       c(prop_duration, pts, xgd_p90), 
#       list(rnk = \(.x) row_number(desc(.x)))
#     ),
#     d = prop_duration_rnk - pts_rnk
#   ) |> 
#   arrange(
#     desc(abs(d))
#   )


raw_table <- worldfootballR::fb_season_team_stats(
  country = COUNTRY,
  gender = GENDER,
  tier = TIER,
  season_end_year = SEASON_END_YEAR,
  stat_type = 'league_table'
)


team_label_order <- agg_gamestate_xgd_with_logos |> 
  dplyr::filter(
    pre_shot_gamestate == 'Leading'
  ) |> 
  dplyr::arrange(prop_duration) |> 
  dplyr::pull(team)
team_label_order <- c(team_label_order, '')

table <- raw_table |> 
  dplyr::select(
    team_short = Squad,
    xgd_p90_total = xGD.90
  ) |> 
  dplyr::bind_cols(
    team_names |> dplyr::select(team)
  ) |> 
  dplyr::mutate(
    dplyr::across(
      team,
      \(.x) factor(.x, levels = team_label_order)
    )
  )

prepped_agg_gamestate_xgd <- agg_gamestate_xgd_with_logos |> 
  dplyr::bind_rows(
    tibble::tibble(
      team = '',
      pre_shot_gamestate = factor(ORDERED_GAMESTATE_LABELS, levels = ORDERED_GAMESTATE_LABELS)
    )
  ) |>
  dplyr::mutate(
    dplyr::across(
      team,
      \(.x) factor(.x, levels = team_label_order)
    )
  ) |> 
  dplyr::arrange(team, desc(pre_shot_gamestate)) |> 
  dplyr::group_by(team) |> 
  dplyr::mutate(
    cumu_prop_duration = cumsum(prop_duration)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    half_cumu_prop_duration = cumu_prop_duration - 0.5 * prop_duration,
    half_cumu_prop_duration = dplyr::case_when(
      prop_duration >= 0.08 ~ half_cumu_prop_duration,
      cumu_prop_duration == 1 ~ 1 - 2 * prop_duration,
      TRUE ~ 2 * prop_duration
    )
  )

xgd_p90_plot <- prepped_agg_gamestate_xgd |> 
  ggplot2::ggplot() +
  ggplot2::aes(
    x = prop_duration,
    y = team
  ) +
  ggplot2::scale_y_discrete(
    name = '',
    labels = prepped_agg_gamestate_xgd |>
      dplyr::distinct(team, label) |>
      tibble::deframe()
  ) +
  ggplot2::theme(
    axis.text.y = ggtext::element_markdown(
      margin = grid::unit(c(0, 0, 0, 0), 'pt')
    )
  ) +
  ggplot2::geom_col(
    show.legend = FALSE,
    alpha = 0.8,
    ggplot2::aes(
      fill = pre_shot_gamestate
    )
  ) +
  ggplot2::geom_text(
    family = FONT,
    size = 9 / ggplot2::.pt,
    fontface = 'plain',
    color = COMPLEMENTARY_FOREGROUND_COLOR,
    data = table |> dplyr::arrange(desc(team)) |> head(1),
    vjust = -1,
    lineheight = 1,
    ggplot2::aes(
      x = 1.05,
      y = team,
      label = 'Overall xGD\nper 90'
    )
  ) +
  ggplot2::geom_text(
    family = FONT,
    size = 10 / ggplot2::.pt,
    fontface = 'plain',
    color = COMPLEMENTARY_FOREGROUND_COLOR,
    data = table,
    ggplot2::aes(
      x = 1.05,
      y = team,
      label = scales::number(
        xgd_p90_total, 
        accuracy = 0.01, 
        style_positive = 'plus'
      )
    )
  ) +
  ggplot2::geom_text(
    family = FONT,
    size = 11 / ggplot2::.pt,
    fontface = 'bold',
    color = WHITISH_FOREGROUND_COLOR,
    data = dplyr::filter(prepped_agg_gamestate_xgd, xgd_p90 >= 0),
    ggplot2::aes(
      x = half_cumu_prop_duration,
      y = team,
      label = scales::number(xgd_p90, accuracy = 0.01, style_positive = 'plus')
    )
  ) +
  ggplot2::geom_text(
    family = FONT,
    size = 11 / ggplot2::.pt,
    fontface = 'bold.italic',
    color = BLACKISH_BACKGROUND_COLOR,
    data = dplyr::filter(prepped_agg_gamestate_xgd, xgd_p90 < 0),
    ggplot2::aes(
      x = half_cumu_prop_duration,
      y = team,
      label = scales::number(xgd_p90, accuracy = 0.01)
    )
  ) +
  ggplot2::scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    limits = c(0, 1.15),
    expand = c(0.01, 0.01)
  ) +
  ggplot2::scale_fill_manual(
    values = GAMESTATE_PAL
  ) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    axis.title.x = ggtext::element_markdown(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.85),
    legend.position = 'top'
  ) +
  ggplot2::labs(
    title = glue::glue("xGD per 90 when <span style='color:{GAMESTATE_PAL[['Leading']]}'>Leading</span>, <span style='color:{GAMESTATE_PAL[['Tied']]}'>Tied</span>, and <span style='color:{GAMESTATE_PAL[['Trailing']]}'>Trailing</spna>"),
    subtitle = 'English Premier League, 2023/2024 Season',
    y = NULL,
    tag = TAG_LABEL,
    caption = glue::glue('**Data**: Opta via fbref *(last updated on 2023-10-25)*<br/>**xGD**: Expected goals for minus expected goals conceded'),
    x = '% of Match Time'
  )
xgd_p90_plot

xgd_p90_plot_path <- file.path(PROJ_DIR, '2023-epl-xgd-p90.png')
ggplot2::ggsave(
  xgd_p90_plot,
  filename = xgd_p90_plot_path,
  width = 8,
  height = 8
)

## https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/
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

add_logo(
  xgd_p90_plot_path,
  logo_path = file.path(PROJ_DIR, 'epl-logo-white.png'),
  logo_scale = 0.15
)

