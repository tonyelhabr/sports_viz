library(qs)

library(dplyr)
library(tibble)
library(glue)

library(ggplot2)
library(ggtext)
library(grid)
library(scales)

PROJ_DIR <- '96-gamestate_xgd'
source(file.path(PROJ_DIR, 'config.R'))
source(file.path(PROJ_DIR, 'plot-helpers.R'))

agg_gamestate_xgd <- qs::qread(file.path(DATA_DIR, 'agg_gamestate_xgd.qs'))
agg_gamestate_xgd_with_strength <- qs::qread(file.path(DATA_DIR, 'agg_gamestate_xgd_with_strength.qs'))
agg_total_xgd_with_strength <- qs::qread(file.path(DATA_DIR, 'agg_total_xgd_with_strength.qs'))
fotmob_standings <- qs::qread('94-2025_soccer_team_strengths/data/fotmob_standings.qs')

team_logos <- agg_gamestate_xgd_with_strength |>
  dplyr::distinct(team) |>
  dplyr::arrange(team) |>
  ## Lucky for us, MLS team names line up with the fotmob names alphabetically.
  dplyr::bind_cols(
    fotmob_standings |>
      dplyr::arrange(team) |>
      dplyr::select(path = logo_url, pts)
  )

agg_gamestate_xgd_with_logos <- agg_gamestate_xgd_with_strength |>
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
    # label = glue::glue("<span style='font-size:18px;color:{ifelse(playoff_status == 'in', ALIVE_COLOR, COMPLEMENTARY_FOREGROUND_COLOR)}'>{ifelse(playoff_status == 'in', '', '<i>')}{team}</s>{ifelse(playoff_status == 'in', '', '</i>')}</span> <span style='font-size:9px;color:{COMPLEMENTARY_FOREGROUND_COLOR}'>{pts} pts</span> <img src='{path}' width='14' height='14'/>")
    label = glue::glue("<span style='font-size:18px;color:{COMPLEMENTARY_FOREGROUND_COLOR}'>{team}</span> <span style='font-size:9px;color:{COMPLEMENTARY_FOREGROUND_COLOR}'>{pts} pts</span> <img src='{path}' width='14' height='14'/>")
  ) |>
  dplyr::select(-path)

raw_table <- worldfootballR::fb_season_team_stats(
  country = COUNTRY,
  gender = GENDER,
  tier = TIER,
  season_end_year = SEASON_END_YEAR,
  stat_type = 'league_table'
) |>
  tibble::as_tibble()

init_table <- raw_table |>
  dplyr::transmute(
    team_short = Squad,
    xgd_p90_total = xGD.90,
    # rank = Rk
    rank = dplyr::row_number(desc(Pts * 1000 + GD))
  ) |>
  dplyr::mutate(
    team_short = stringr::str_replace_all(
      team_short,
      ## some teams ordered differently between FBref and fotmob if we don't do this
      c(
        '^NYC' = 'New York City ',
        '^NY' = 'New York',
        '^NE ' = 'New England ',
        '^SJ ' = 'San Jose '
      )
    )
  ) |>
  dplyr::arrange(team_short) |>
  dplyr::bind_cols(
    team_logos |>
      dplyr::select(team) |>
      dplyr::arrange(team)
  ) |>
  dplyr::arrange(rank)


# team_label_order <- agg_gamestate_xgd_with_logos |>
#   dplyr::filter(
#     pre_shot_gamestate == 'Leading'
#   ) |>
#   left_join(init_table, by = join_by(team))
#   dplyr::arrange(prop_duration) |>
#   dplyr::pull(team)
team_label_order <- rev(init_table$team)
## Add extra level for top row for "Overall p90" label
team_label_order <- c(team_label_order, '')
TOTAL_GAMESTATE_LABEL <- 'Overall'

## Could also order by table points
# team_label_order <- c(rev(init_table$team), '')

table <- init_table |>
  dplyr::mutate(
    dplyr::across(
      team,
      \(.x) factor(.x, levels = setdiff(team_label_order, ''))
    )
  ) |> 
  dplyr::arrange(desc(team))

OPPONENT_PLAYOFF_TEAM_PAL <- c(
  'playoff' = 'gold',
  'non_playoff' = 'dodgerblue'
)

plot_by_is_opponent_playoff_team <- function(agg_gamestate_xgd_with_logos, .is_opponent_playoff_team) {
  
  filt_agg_total_xgd_with_strength <- agg_total_xgd_with_strength |>
    filter(
      is_opponent_playoff_team == .is_opponent_playoff_team
    ) |> 
    arrange(desc(xgd_p90))
  team_label_order <- rev(filt_agg_total_xgd_with_strength$team)
  team_label_order <- c(team_label_order, '')
  
  prepped_agg_gamestate_xgd <- agg_gamestate_xgd_with_logos |>
    dplyr::filter(
      is_opponent_playoff_team == .is_opponent_playoff_team
    ) |> 
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

  table_adj <- table |> 
    ## ensure that factors aren't messing this up
    dplyr::mutate(
      team = as.character(team)
    ) |> 
    dplyr::left_join(
      filt_agg_total_xgd_with_strength,
      by = 'team'
    ) |> 
    dplyr::mutate(
      dplyr::across(
        team,
        \(.x) factor(.x, levels = setdiff(team_label_order, ''))
      )
    ) |> 
    dplyr::arrange(desc(team)) |> 
    dplyr::select(
      team,
      xgd_p90_total = xgd_p90
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
        # size = 18,
        margin = grid::unit(c(0, 0, 0, 0), 'pt')
      ),
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
      size = 10 / ggplot2::.pt,
      fontface = 'plain',
      color = COMPLEMENTARY_FOREGROUND_COLOR,
      data = table_adj |> dplyr::arrange(desc(team)) |> head(1),
      vjust = -2,
      lineheight = 1,
      ggplot2::aes(
        x = 1.075,
        y = team,
        label = TOTAL_GAMESTATE_LABEL
      )
    ) +
    ggplot2::geom_text(
      family = FONT,
      size = 10 / ggplot2::.pt,
      fontface = 'plain',
      color = COMPLEMENTARY_FOREGROUND_COLOR,
      data = table_adj,
      ggplot2::aes(
        x = 1.075,
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
      size = 12 / ggplot2::.pt,
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
      legend.position = 'top',
      plot.title = ggtext::element_markdown(size = 18)
    ) +
    ggplot2::labs(
      title = glue::glue("Regular Season xGD per 90 when <span style='color:{GAMESTATE_PAL[['Leading']]}'>Leading</span>, <span style='color:{GAMESTATE_PAL[['Tied']]}'>Tied</span>, and <span style='color:{GAMESTATE_PAL[['Trailing']]}'>Trailing</span>"),
      # subtitle = SUBTITLE_LABEL,
      subtitle = glue::glue("MLS, {SEASON_END_YEAR} Season (as of Sep. 28) <span style='color:{OPPONENT_PLAYOFF_TEAM_PAL[[.is_opponent_playoff_team]]}'>vs. {ifelse(.is_opponent_playoff_team == 'playoff', 'Playoff', 'Non-Playoff')} Teams</span>"),
      y = NULL,
      tag = TAG_LABEL,
      caption = paste0(CAPTION_LABEL, '<br/>**xGD**: Expected goals for minus expected goals conceded'),
      x = '% of Match Time'
    )
  xgd_p90_plot
  
  xgd_p90_plot_path <- file.path(PROJ_DIR, sprintf('%s-mls-playoffs-xgd-p90-vs-%s.png', SEASON_END_YEAR, .is_opponent_playoff_team))
  ggplot2::ggsave(
    xgd_p90_plot,
    filename = xgd_p90_plot_path,
    width = 7.5,
    height = 7.5
  )
  
  add_logo(
    xgd_p90_plot_path,
    logo_path = file.path(PROJ_DIR, 'mls-logo-black-and-white.png'),
    logo_scale = 0.06
  )
}

plot_by_is_opponent_playoff_team(
  agg_gamestate_xgd_with_logos,
  'playoff'
)

plot_by_is_opponent_playoff_team(
  agg_gamestate_xgd_with_logos,
  'non_playoff'
)

