library(dplyr)
library(qs)
library(purrr)
library(probably) ## 0.1.0.9008
packageVersion('probably') 

library(ggplot2)
library(sysfonts)
library(showtext)
library(ggtext)
library(ragg)
library(htmltools)

PROJ_DIR <- '68-opta_xg_calib_by_gamestate'
raw_shots <- qs::qread(file.path(PROJ_DIR, 'shots.qs')) |> 
  dplyr::filter(
    pov == 'primary',
    !is_own_goal
  )

## uncalibrated xG calibration ----
match_teams <- raw_shots |> 
  dplyr::distinct(
    match_id,
    home_team = ifelse(is_home, team, opponent),
    away_team = ifelse(is_home, opponent, team)
  )

shots <- raw_shots |> 
  dplyr::inner_join(
    match_teams,
    by = dplyr::join_by(match_id)
  ) |> 
  dplyr::transmute(
    match_id,
    season,
    date,
    home_team,
    away_team,
    id,
    is_penalty,
    is_goal,
    .pred_yes = xg,
    .pred_no = 1 - xg,
    game_state = cut(
      game_state,
      breaks = c(-Inf, -1, 0, Inf), 
      labels = c('trailing', 'neutral', 'leading')
    )
  ) |> 
  dplyr::group_by(match_id) |> 
  dplyr::arrange(id, .by_group = TRUE) |> 
  dplyr::mutate(
    pre_shot_game_state = dplyr::lag(game_state, default = 'neutral')
  ) |> 
  dplyr::ungroup()

CAL_N_BREAKS <- 20
CAL_CONF_LEVEL <- 0.9

## cal_table_custom_breaks ----
convert_seq_to_cuts <- function(seq) {
  list(
    lower_cut = seq[1:length(seq) - 1], 
    upper_cut = seq[2:length(seq)]
  )
}

convert_cuts_to_df <- function(cuts) {
  tibble::tibble(
    lower_cut = cuts$lower_cut,
    upper_cut = cuts$upper_cut,
    predicted_midpoint = lower_cut + (upper_cut - lower_cut) / 2
  )
}

cal_table_custom_breaks <- function(
    .data, 
    truth, 
    estimate = dplyr::starts_with(".pred"), 
    side, 
    conf_level = CAL_CONF_LEVEL, 
    event_level = c("auto", "first", "second"), 
    ..., 
    .by = NULL
) {
  
  ## mostly internals of probably:::.cal_table_breaks_impl (from probably:::.cal_table_breaks)
  truth <- rlang::enquo(truth)
  estimate <- rlang::enquo(estimate)
  group <- rlang::enquo(.by)
  
  levels <- probably:::truth_estimate_map(
    .data = .data, 
    truth = !!truth, 
    estimate = !!estimate
  )
  
  ## internals of probably:::.cal_table_breaks_grp
  cuts <- convert_seq_to_cuts(side)
  ## return lower and upper cut for plotting
  cuts_df <- convert_cuts_to_df(cuts)
  
  res <- .data |> 
    dplyr::group_by(!!group, .add = TRUE) |> 
    dplyr::group_map(
      ~{
        ## replace call to probably:::.cal_table_breaks_grp with direct call to probably:::.cal_class_grps with pre-computed cuts
        grp <- probably:::.cal_class_grps(
          .data = .x,
          truth = !!truth,
          cuts = cuts,
          event_level = event_level,
          levels = levels,
          conf_level = conf_level
        )
        dplyr::bind_cols(.y, grp)
      }
    ) |> 
    dplyr::bind_rows() |> 
    dplyr::inner_join(
      cuts_df,
      by = dplyr::join_by(predicted_midpoint)
    )
  
  if (length(levels) > 2) {
    res <- dplyr::group_by(res, !!truth, .add = TRUE)
  }
  res
}

xg_cal_table_custom_breaks <- purrr::partial(
  cal_table_custom_breaks,
  side = c(0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.15, 0.2, 0.25, 0.5, 0.78, 0.8, 1),
  conf_level = CAL_CONF_LEVEL,
  event_level = 'second'
)

calib_game_state_custom <- xg_cal_table_custom_breaks(
  shots,
  truth = is_goal,
  estimate = .pred_yes,
  .by = pre_shot_game_state
)

## plot uncalibrated xG's calibration ----
TAG_LABEL <- htmltools::tagList(
  htmltools::tags$span(htmltools::HTML(enc2utf8("&#xf099;")), style = 'font-family:fb'),
  htmltools::tags$span("@TonyElHabr"),
)
SUBTITLE_LABEL <- 'English Premier League, 2017/18 - 2022/23'
PLOT_RESOLUTION <- 300
WHITISH_FOREGROUND_COLOR <- 'white'
COMPLEMENTARY_FOREGROUND_COLOR <- '#f1f1f1'
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
  plot.title = ggplot2::element_text(face = 'bold', size = 20, color = WHITISH_FOREGROUND_COLOR),
  plot.title.position = 'plot',
  plot.subtitle = ggplot2::element_text(size = 16, color = COMPLEMENTARY_FOREGROUND_COLOR),
  axis.text = ggplot2::element_text(color = WHITISH_FOREGROUND_COLOR, size = 14),
  axis.title = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.line = ggplot2::element_blank(),
  strip.text = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0),
  panel.grid.major = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor.x = ggplot2::element_blank(),
  panel.grid.minor.y = ggplot2::element_blank(),
  plot.margin = ggplot2::margin(10, 20, 10, 20),
  plot.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR),
  plot.caption = ggtext::element_markdown(color = WHITISH_FOREGROUND_COLOR, hjust = 0, size = 10, face = 'plain'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 12, color = WHITISH_FOREGROUND_COLOR, hjust = 1),
  plot.tag.position = c(0.99, 0.01),
  panel.spacing.x = grid::unit(2, 'lines'),
  panel.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR)
)
ggplot2::update_geom_defaults('text', list(color = WHITISH_FOREGROUND_COLOR, size = 12 / .pt))
ggplot2::update_geom_defaults('point', list(color = WHITISH_FOREGROUND_COLOR))

plot_and_save_calibration <- function(
    df,
    size = 7,
    width = size, 
    height = size, 
    title = NULL,
    caption = NULL,
    filename = tempfile(),
    extra_layers
) {
  
  group_cols <- setdiff(
    colnames(df),
    c('predicted_midpoint', 'event_rate', 'events', 'total', 'lower', 'upper')
  )
  
  has_group_cols <- length(group_cols) > 0
  
  p <- df |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = predicted_midpoint, y = event_rate) +
    ggplot2::geom_abline(color = WHITISH_FOREGROUND_COLOR, linetype = 2) +
    ggplot2::geom_ribbon(
      fill = '#999999',
      alpha = 0.5,
      ggplot2::aes(ymin = lower, ymax = upper)
    ) +
    ggplot2::geom_line(color = WHITISH_FOREGROUND_COLOR) +
    ggplot2::geom_point(
      color = WHITISH_FOREGROUND_COLOR,
      ggplot2::aes(size = total),
      show.legend = FALSE
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0, 1),
      ylim = c(0, 1)
    ) +
    ggplot2::labs(
      title = paste0(c('Opta xG calibration', title), collapse = ' by '),
      subtitle = SUBTITLE_LABEL,
      y = 'Actual goal rate',
      x = 'Expected goals (xG)',
      caption = paste0(c('**Data**: Opta via fbref.<br/>Point size is proportional to number of observations.', caption), collapse = '\n'),
      tag = TAG_LABEL
    ) + 
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(color = WHITISH_FOREGROUND_COLOR)
    ) + 
    ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(group_cols)))
  
  if (isFALSE(missing(extra_layers))) {
    p <- p + extra_layers
  }
  
  ggplot2::ggsave(
    p,
    filename = file.path(PROJ_DIR, sprintf('%s_calibration.png', filename)),
    width = width,
    height = height
  )
  invisible(p)
}


xg_cal_plot_breaks <- purrr::partial(
  probably::cal_plot_breaks,
  num_breaks = CAL_N_BREAKS,
  conf_level = CAL_CONF_LEVEL,
  event_level = 'second'
)

extra_xg_cal_plot_layers <- list(
  ggplot2::geom_segment(
    data = data.frame(
      x = 0.45,
      xend = 0.35,
      y = 0.6,
      yend = 0.7,
      pre_shot_game_state = 'leading'
    ),
    arrow = grid::arrow(length = grid::unit(6, 'pt'), type = 'closed'),
    ggplot2::aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    linewidth = 1,
    color = '#009ffd'
  ),
  ggplot2::geom_segment(
    data = data.frame(
      x = 0.55,
      xend = 0.65,
      y = 0.4,
      yend = 0.3,
      pre_shot_game_state = 'leading'
    ),
    arrow = grid::arrow(length = grid::unit(6, 'pt'), type = 'closed'),
    ggplot2::aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    linewidth = 1,
    color = '#ffa400'
  ),
  ggtext::geom_richtext(
    data = data.frame(
      x = 0.65,
      y = 0.8,
      label = 'Model *under-predicts*',
      pre_shot_game_state = 'leading'
    ),
    fill = NA, label.color = NA,
    label.padding = grid::unit(rep(0, 1), 'pt'),
    color = '#009ffd',
    family = FONT,
    hjust = 1,
    vjust = 0.5,
    size = 11 / ggplot2::.pt,
    ggplot2::aes(
      x = x,
      y = y,
      label = label
    )
  ),
  ggtext::geom_richtext(
    data = data.frame(
      x = 0.35,
      y = 0.2,
      label = 'Model *over-predicts*',
      pre_shot_game_state = 'leading'
    ),
    fill = NA, label.color = NA,
    label.padding = grid::unit(rep(0, 1), 'pt'),
    color = '#ffa400',
    family = FONT,
    hjust = 0,
    vjust = 0.5,
    size = 11 / ggplot2::.pt,
    ggplot2::aes(
      x = x,
      y = y,
      label = label
    )
  )
)

shots |> 
  xg_cal_plot_breaks(
    truth = is_goal,
    estimate = .pred_yes,
    .by = pre_shot_game_state
  ) |> 
  purrr::pluck('data') |> 
  plot_and_save_calibration(
    width = 10,
    height = 10 / 2,
    extra_layers = extra_xg_cal_plot_layers,
    title = 'game state',
    filename = 'no_pre_shot_game_state_xg'
  )

calib_game_state_custom |>
  dplyr::select(
    -c(lower_cut, upper_cut)
  ) |>
  plot_and_save_calibration(
    width = 10,
    height = 10 / 2,
    title = 'game state',
    filename = 'custom_no_pre_shot_game_state_xg'
  )

## calibrate xG with game state ----
## estimate must be `.pred_{level1}` and `.pred_{level2}`
# just_shots <- shots |> 
#   dplyr::select(
#     id, 
#     pre_shot_game_state, 
#     is_goal, 
#     tidyselect::vars_select_helpers$starts_with('.pred')
#   ) |> 
#   dplyr::mutate(
#     ## probably has a bug when .by is a factor? (https://github.com/tidymodels/probably/issues/127)
#     dplyr::across(pre_shot_game_state, as.character)
#   )

beta_cal_model <- shots |> 
  dplyr::mutate(
    ## probably has a bug when .by is a factor? (https://github.com/tidymodels/probably/issues/127)
    dplyr::across(pre_shot_game_state, as.character)
  ) |> 
  probably::cal_estimate_beta(
    truth = is_goal,
    .by = pre_shot_game_state
  )

# df <- just_shots |> filter(pre_shot_game_state == 'leading')
# betacal::beta_calibration(
#   p = df$.pred_no,
#   y = df$is_goal == 'no',
#   parameters = 'abm'
# )
beta_cal_shots <- probably::cal_apply(
  shots,
  beta_cal_model
)

beta_cal_shots |>
  xg_cal_plot_breaks(
    truth = is_goal,
    estimate = .pred_yes,
    .by = pre_shot_game_state
  ) |> 
  purrr::pluck('data') |> 
  plot_and_save_calibration(
    width = 10,
    height = 10 / 2,
    title = 'game state',
    filename = 'pre_shot_game_state_xg'
  )

beta_cal_calib_game_state_custom <- xg_cal_table_custom_breaks(
  beta_cal_shots,
  truth = is_goal,
  estimate = .pred_yes,
  .by = pre_shot_game_state
)

beta_cal_calib_game_state_custom |>
  dplyr::select(
    -c(lower_cut, upper_cut)
  ) |>
  plot_and_save_calibration(
    width = 10,
    height = 10 / 2,
    title = 'game state',
    filename = 'custom_pre_shot_game_state_xg'
  )

## plot calibrated xG ----
cal_shots <- dplyr::inner_join(
  shots,
  beta_cal_shots |> 
    dplyr::select(id, starts_with('.pred')) |> 
    dplyr::rename_with(
      \(.x)  gsub('.pred', '.cal_pred', .x), 
      dplyr::starts_with('.pred')
    ),
  by = dplyr::join_by(id)
)

cal_shots_plot <- cal_shots |>
  ggplot2::ggplot() +
  ggplot2::aes(x = .pred_yes, y = .cal_pred_yes) +
  ggplot2::geom_abline(color = WHITISH_FOREGROUND_COLOR, linetype = 2) +
  # ggplot2::geom_line(color = WHITISH_FOREGROUND_COLOR) +
  ggplot2::geom_point(
    color = WHITISH_FOREGROUND_COLOR,
    shape = 21,
    show.legend = FALSE
  ) +
  ggplot2::coord_cartesian(
    xlim = c(0, 1),
    ylim = c(0, 1)
  ) +
  ggplot2::facet_wrap(~pre_shot_game_state) +
  ggplot2::labs(
    title = 'Game-state calibrated xG compared with original xG',
    subtitle = SUBTITLE_LABEL,
    y = 'Calibrated xG',
    x = 'Expected goals (xG)',
    caption = '\n',
    tag = TAG_LABEL
  ) +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(color = WHITISH_FOREGROUND_COLOR)
  )

cal_shots_plot_path <- file.path(PROJ_DIR, 'calibrated_xg_vs_orig_xg.png')
ggplot2::ggsave(
  cal_shots_plot,
  filename = cal_shots_plot_path,
  width = 10,
  height = 5
)

## calculate xPts ----
library(purrr)
library(poibin)
library(gdata)
library(tidyr)

permute_xg <- function(xg) {
  n <- length(xg)
  x <- seq.int(0, n)
  poibin::dpoibin(x, xg)
}

calculate_permuted_xg <- function(df) {
  df |> 
    dplyr::group_by(dplyr::across(c(tidyselect::vars_select_helpers$everything(), -xg))) |> 
    dplyr::summarize(dplyr::across(xg, ~list(.x))) |> 
    dplyr::mutate(
      prob = purrr::map(xg, ~permute_xg(.x))
    ) |> 
    dplyr::select(-c(xg)) |> 
    tidyr::unnest(cols = c(prob)) |> 
    dplyr::group_by(dplyr::across(-c(prob))) |>
    dplyr::mutate(
      g = dplyr::row_number() - 1L
    ) |>
    dplyr::ungroup() |> 
    dplyr::arrange(match_id, is_home)
}

summarize_pivoted_permuted_xg <- function(prob_away, prob_home) {
  outer_prod <- outer(prob_away, prob_home)
  p_draw <- sum(diag(outer_prod), na.rm = TRUE)
  p_home <- sum(gdata::upperTriangle(outer_prod), na.rm = TRUE)
  p_away <- sum(gdata::lowerTriangle(outer_prod), na.rm = TRUE)
  list(
    draw = p_draw,
    home = p_home,
    away = p_away
  )
}


## Bournemouth 0 - 1 Manchester City on 2019-03-02
## Huddersfield 0 - 0 Swansea on 2018-03-10
pad_for_matches_without_shots_from_one_team <- function(df) {
  n_teams_per_match <- df |> 
    dplyr::distinct(match_id, team) |> 
    dplyr::count(match_id, sort = TRUE)
  
  matches_with_no_shots_from_one_team <- n_teams_per_match |> 
    dplyr::filter(n == 1)
  
  dummy_opponents <- df |> 
    dplyr::distinct(match_id, season, date, team, opponent, is_home) |> 
    dplyr::semi_join(
      matches_with_no_shots_from_one_team,
      by = dplyr::join_by(match_id)
    ) |> 
    dplyr::mutate(
      dummy_team = team
    ) |> 
    dplyr::transmute(
      match_id, 
      season, 
      date, 
      team = opponent,
      opponent = dummy_team,
      dplyr::across(is_home, ~!.x),
      prob = 1,
      g = 0L
    )
  
  dplyr::bind_rows(
    df,
    dummy_opponents
  ) |> 
    dplyr::arrange(season, date, team, g)
}

summarize_permuted_xg_by_match <- function(df) {
  
  padded_df <- pad_for_matches_without_shots_from_one_team(df)
  
  pivoted <- padded_df |>
    transmute(
      match_id,
      season,
      date,
      g,
      is_home = ifelse(is_home, 'home', 'away'),
      prob
    ) |>
    tidyr::pivot_wider(
      names_from = is_home,
      names_prefix = 'prob_',
      values_from = prob,
      values_fill = 0L
    )
  
  pivoted |> 
    select(match_id, season, date, prob_away, prob_home) |>
    group_by(match_id, season, date) |> 
    summarize(
      across(starts_with('prob_'), ~list(.x))
    ) |> 
    ungroup() |> 
    inner_join(
      padded_df |> distinct(match_id, team, opponent, is_home),
      by = 'match_id'
    ) |> 
    mutate(
      prob = map2(prob_away, prob_home, summarize_pivoted_permuted_xg)
    ) |> 
    select(-starts_with('prob_')) |> 
    tidyr::unnest_wider(prob, names_sep = '_') |> 
    mutate(
      prob_win = ifelse(is_home, prob_home, prob_away),
      prob_lose = ifelse(is_home, prob_away, prob_home),
      xpts = 3 * prob_win + 1 * prob_draw
    ) |> 
    select(-c(prob_home, prob_away))
}

calculate_xpts_by_match <- purrr::compose(
  \(df) {
    dplyr::select(
      df,
      match_id,
      season,
      date,
      is_home,
      team,
      opponent,
      xg
    )
  },
  calculate_permuted_xg,
  summarize_permuted_xg_by_match,
  .dir = 'forward'
)

reg_xpts_by_match <- calculate_xpts_by_match(raw_shots)

cal_xpts_by_match <- cal_shots |> 
  dplyr::select(
    id,
    xg = .cal_pred_yes
  ) |> 
  dplyr::inner_join(
    raw_shots |> dplyr::select(-xg),
    by = join_by(id)
  ) |> 
  calculate_xpts_by_match()

match_results <- qs::qread(file.path(PROJ_DIR, 'match_results.qs'))
match_pts <- bind_rows(
  match_results |> 
    transmute(
      match_id,
      season,
      gender,
      tier,
      date,
      is_home = TRUE,
      team = home_team,
      opponent = away_team,
      g = home_g,
      g_conceded = away_g
    ),
  match_results |> 
    transmute(
      match_id,
      season,
      gender,
      tier,
      date,
      is_home = FALSE,
      team = away_team,
      opponent = home_team,
      g = away_g,
      g_conceded = home_g
    ) 
) |> 
  ungroup() |> 
  mutate(
    result = case_when(
      g > g_conceded ~ 'w',
      g < g_conceded ~ 'l',
      g == g_conceded ~ 'd'
    ),
    is_win = g > g_conceded,
    is_draw = g == g_conceded
  ) |> 
  transmute(
    match_id,
    # season,
    # gender,
    # tier,
    # date,
    team,
    result,
    pts = 3 * as.integer(is_win) + 1 * as.integer(is_draw)
  )
match_pts |> count(result)

## compare xPts ----
compared_xpts_by_match <- reg_xpts_by_match |> 
  dplyr::rename_with(
    \(.x) paste0('reg_', .x),
    c(xpts, tidyselect::vars_select_helpers$starts_with('prob'))
  ) |> 
  dplyr::inner_join(
    cal_xpts_by_match |>
      dplyr::select(match_id, team, xpts, tidyselect::vars_select_helpers$starts_with('prob')) |> 
      dplyr::rename_with(
        \(.x) paste0('cal_', .x),
        c(xpts, tidyselect::vars_select_helpers$starts_with('prob'))
      ),
    by = dplyr::join_by(match_id, team)
  ) |> 
  dplyr::inner_join(
    match_pts,
    by = dplyr::join_by(match_id, team)
  )

compared_xpts_by_match |> 
  ggplot() +
  aes(
    x = reg_xpts,
    y = cal_xpts
  ) +
  geom_point()

compared_xpts_by_season <- compared_xpts_by_match |> 
  group_by(season, team) |> 
  summarize(
    across(ends_with('pts'), sum)
  ) |> 
  ungroup() |> 
  mutate(
    reg_d = pts - reg_xpts,
    cal_d = pts - cal_xpts,
    cal_is_improvement = abs(cal_d) < abs(reg_d)
  )
compared_xpts_by_season |> count(cal_is_improvement)

compared_xpts_by_match |> 
  arrange(desc(abs(reg_xpts - cal_xpts)))

compared_xpts_by_season |> 
  arrange(desc(abs(reg_xpts - cal_xpts)))

compared_xpts_by_season |> 
  filter(team == 'Brighton & Hove Albion')

compared_xpts_by_season |> 
  select(
    season,
    team,
    pts,
    reg = reg_xpts,
    cal = cal_xpts
  ) |> 
  pivot_longer(
    c(reg, cal),
    names_to = 'method',
    values_to = 'xpts'
  ) |> 
  group_by(season, method) |> 
  summarize(
    mae = mean(abs(pts - xpts)),
    rmse = sqrt(mean((pts - xpts)^2))
  ) |> 
  ungroup()

