
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

# Reference: https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/
# https://a.espncdn.com/combiner/i?img=/i/leaguelogos/soccer/500/23.png
path_logo <- file.path(dir_proj, 'premier-league.png')
add_logo <-
  function(path_viz,
           path_logo,
           idx_x,
           idx_y,
           logo_scale = 0.1,
           adjust_x = TRUE,
           adjust_y = TRUE,
           path_suffix = '_w_logo') {
    plot <- path_viz %>% magick::image_read()
    logo_raw <- path_logo %>% magick::image_read()

    plot_height <- magick::image_info(plot)$height
    plot_width <- magick::image_info(plot)$width
    
    logo <- magick::image_scale(logo_raw, as.character(round(plot_width * logo_scale)))
    
    info <- magick::image_info(logo)
    logo_width <- info$width
    logo_height <- info$height
    
    x_pos <- plot_width - idx_x * plot_width
    y_pos <- plot_height - idx_y * plot_height
    
    if(adjust_x) {
      x_pos <- x_pos - logo_width
    }
    
    if(adjust_y) {
      y_pos <- y_pos - logo_height
    }
    
    offset <- paste0('+', x_pos, '+', y_pos)
    
    viz <- plot %>% magick::image_composite(logo, offset = offset)
    ext <- path_viz %>% tools::file_ext()
    rgx_ext <- sprintf('[.]%s$', ext)
    
    res <-
      magick::image_write(
        viz,
        path_viz %>% str_replace(rgx_ext, sprintf('%s.%s', path_suffix, ext))
      )
    res
  }

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
  mutate(rnk = row_number(desc(pts))) %>% 
  .fix_team_col() %>% 
  arrange(rnk)
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

states <-
  shots_by_team %>% 
  select(-g_state_opp) %>% 
  mutate(
    across(g_state, ~case_when(.x < 0L ~ 'Behind', .x > 0L ~ 'Ahead', TRUE ~ 'Neutral'))
  ) %>%
  # Just to have something at the end that I don't comment out when testing.
  arrange(season, date, match_id, minute, team)
states

# lvls <- team_mapping$team %>% rev()
lvls <- standings$team %>% rev()
states_by_team <- 
  states %>% 
  group_by(season, team, g_state) %>% 
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
    across(c(matches(rgx_g), -g_state, matches('^shots')), list(p90 = ~90 * .x / dur))
  ) %>% 
  mutate(
    across(team, ~factor(.x, levels = lvls))
  ) %>% 
  relocate(season, team)
states_by_team

states_by_team_long <-
  states_by_team %>% 
  mutate(across(g_state, ~as.character(.x) %>% tolower())) %>% 
  pivot_wider(
    values_from = -any_of(c('season', 'team', 'g_state')),
    names_from = g_state
  ) %>% 
  pivot_longer(
    -c(season, team),
    names_pattern = '(^.*)_(ahead|behind|neutral)',
    names_to = c('stat', 'g_state'),
    values_to = 'value'
  )
states_by_team_long

states_by_team_wide <-
  states_by_team_long %>% 
  pivot_wider(
    names_from = g_state,
    values_from = value
  ) %>% 
  mutate(
    across(c(ahead, behind), list(diff = ~{.x - neutral}))
  )
states_by_team_wide

states_by_team_clean <-
  full_join(
    states_by_team_wide %>% 
      select(-matches('diff$')) %>% 
      pivot_longer(
        -c(season, team, stat),
        names_to = 'g_state',
        values_to = 'value'
      ),
    states_by_team_wide %>% 
      select(season, team, stat, matches('diff')) %>% 
      pivot_longer(
        -c(season, team, stat),
        names_to = 'g_state',
        values_to = 'diff'
      ) %>% 
      mutate(across(g_state, ~str_remove(.x, '_diff$')))
  ) %>% 
  mutate(
    lab = case_when(
      g_state == 'neutral' ~ sprintf('%.2f', value),
      TRUE ~ sprintf('%+.2f', diff)
    )
  ) %>% 
  mutate(
    across(
      g_state, 
      ~factor(.x, levels = c('behind', 'neutral', 'ahead')) %>% 
        fct_recode('Behind\n(Relative)' = 'behind', 'Ahead\n(Relative)' = 'ahead', 'Neutral' = 'neutral')
    )
  ) %>% 
  left_join(standings)
states_by_team_clean

# main ----
states_by_team_agg <-
  states_by_team %>% 
  relocate(season, team, g_state, dur, shots) %>% 
  group_by(season, g_state) %>% 
  summarize(
    cnt = sum(!is.na(dur)),
    across(shots:last_col(), list(mean = ~sum(dur * .x, na.rm = TRUE) / sum(dur)))
  ) %>% 
  ungroup()
states_by_team_agg %>% mutate(across(matches('_mean'), round, 2))

lab_caption <- 'Rates are time-based averages<br/>(accounting for varying time spent in a given game state).'
lab_tag <- '**Viz**: Tony ElHabr | **Data**: understat'
.plot_heatmap <-
  function(stat = 'npxg_ps',
           lab_legend = 'Non-Penalty xG Per Shot',
           lab_format = '%+.2f',
           low = '#5ab4ac',
           mid = '#f5f5f5',
           high = '#d8b365') {
    
    states_by_team_clean %>%
      filter(stat == !!stat) %>% 
      ggplot() +
      aes(y = team, x = g_state)+
      geom_tile(aes(fill = diff)) +
      geom_text(
        family = 'Karla',
        size = 4,
        aes(label = lab)
      ) +
      guides(
        fill = guide_legend(
          title = lab_legend,
          nrow = 1,
          title.position = 'top',
          title.hjust = 0.5
        )
      ) +
      scale_x_discrete(position = 'top') + 
      scale_fill_gradient2(
        labels = function(x) sprintf(lab_format, x),
        midpoint = 0,
        na.value = 'grey80',
        low = low,
        mid = mid,
        high = high
      ) +
      labs(
        y = NULL
      ) +
      theme(
        # axis.text.x.top = element_text(),
        legend.title = element_text(size = 10, hjust = 0.5),
        axis.text.x = element_text(size = 10, hjust = 0.5),
        axis.text.y = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        legend.position = 'top'
      )
  }

viz_npxg_ps <-
  .plot_heatmap(
    stat = 'npxg_ps',
    lab_format = '%+.2f',
    lab_legend = 'Non-Penalty xG Per Shot\nRelative to Neutral Game State',
    low = '#5ab4ac',
    mid = '#f5f5f5',
    high = '#d8b365'
  ) +
  labs(
    x = NULL,
    caption = ' ',
    tag = lab_tag,
    subtitle = 'Teams shoot less efficiently when they\'re down'
  )
viz_npxg_ps

viz_shots_p90 <-
  .plot_heatmap(
    stat = 'shots_p90',
    lab_format = '%+d',
    lab_legend = 'Shots Per 90 Min.\nRelative to Neutral Game State',
    low = '#67a9cf',
    mid = '#f7f7f7',
    high = '#ef8a62'
  ) +
  theme(
    plot.caption = ggtext::element_markdown(),
  ) +
  labs(
    x = NULL,
    # x = 'Game State',
    caption = lab_caption,
    subtitle = 'Teams shoot more often when they are behind'
  )
viz_shots_p90

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
viz_shot_qq <- 
  patchwork::wrap_plots(viz_npxg_ps, viz_teams, viz_shots_p90, widths = c(7, 2, 7)) +
  patchwork::plot_annotation(
    title = 'The Shot Quality vs. Quantity Trade-off',
    theme = theme(
      plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 18, hjust = 0.5)
    )
  )
viz_shot_qq

path_viz_qq <- file.path(dir_proj, sprintf('viz_shot_quality_efficiency_%s.png', season))

ggsave(
  plot = viz_shot_qq,
  filename = path_viz_qq,
  width = 12,
  height = 9,
  type = 'cairo'
)

add_logo(
  path_viz = path_viz_qq,
  path_logo = path_logo,
  idx_x = 0.45,
  idx_y = 0.8
)

# secondary ----
states_npxg_p90_diff <-
  states_by_team_clean %>% 
  filter(stat == 'npxg_p90')

teams_select_init <-
  states_npxg_p90_diff %>%
  select(season, team, g_state, diff) %>%
  filter(g_state != 'Neutral') %>%
  pivot_wider(names_from = g_state, values_from = diff)

teams_lo <-
  teams_select_init %>%
  filter(`Ahead\n(Relative)` < 0 & `Behind\n(Relative)` < 0) %>% 
  pull(team)
teams_lo

teams_hi <-
  teams_select_init %>%
  filter(`Ahead\n(Relative)` > 0 & `Behind\n(Relative)` > 0) %>% 
  pull(team)
teams_hi

# teams_bus <-
#   states_by_team_clean %>%
#   filter(stat == 'npxg_p90') %>%
#   filter(
#     g_state == 'Ahead\n(Relative)' & diff < 0
#   ) %>%
#   pull(team)
# teams_bus <- c('Tottenham', 'West Brom')

# teams_filt <- teams_bus
color_hi <- '#F28E2B'
color_lo <- '#4E79A7'
colors_lst <-
  xengagement::team_accounts_mapping %>%
  select(team, color_pri) %>%
  deframe()
colors_lst

# This seems overly complicated (and is) just cuz I was testing out different stuff.
team_annotations <-
  team_mapping %>% 
  select(team) %>%
  inner_join(states_by_team_clean %>% distinct(team, rnk)) %>% 
  left_join(
    xengagement::team_accounts_mapping %>% 
      select(team, color_pri) 
  ) %>% 
  mutate(
    prefix = case_when(
      team %in% teams_hi ~ glue::glue('<b><span style="color:{color_hi}">'),
      team %in% teams_lo ~ glue::glue('<b><span style="color:{color_lo}">'),
      TRUE ~ NA_character_
    ),
    suffix = case_when(
      team %in% c(teams_hi, teams_lo) ~ '</span></b>',
      TRUE ~ NA_character_
    )
  ) %>%  
  mutate(
    lab = case_when(
      !is.na(prefix) ~ glue::glue('{prefix}{team}{suffix}') %>% as.character(), 
      TRUE ~ as.character(team)
    )
  )
team_annotations

viz_npxg_p90_diff <-
  states_npxg_p90_diff %>% 
  ggplot() +
  aes(y = -rnk, x = g_state) +
  geom_tile(aes(fill = diff)) +
  geom_text(aes(label = lab), family  = 'Karla', size = 4) +
  guides(
    fill = 
      guide_legend(
        title = 'Non-penalty xG Per 90 Min.\nRelative to Neutral Game State',
        title.position = 'top',
        nrow = 1
      )
  ) +
  scale_fill_gradient2(
    midpoint = 0,
    na.value = 'grey80',
    low = '#e9a3c9',
    mid = '#f7f7f7',
    high = '#a1d76a'
  ) +
  scale_x_discrete(position = 'top') +
  scale_y_continuous(
    breaks = seq.int(-20, -1),
    labels = team_annotations %>% arrange(-rnk) %>% pull(lab)
  ) +
  labs(
    y = NULL,
    # x = 'Game State',
    x = NULL,
    title = 'xG By Game State, 2020/21 Premier League',
    subtitle = glue::glue('<b><span style="color:{color_lo}">Some teams</span></b> accumulate xG at a <b><span style="color:{colorspace::darken("#e9a3c9", amount = 0.5)}">lower</span></b> rate both when behind or ahead,<br/> while <b><span style="color:{color_hi}">other teams</span></b> accumulate xG at a <b><span style="color:{colorspace::darken("#a1d76a", amount = 0.3)}">higher</span></b> rate in such game states.'),
    caption = lab_caption,
    tag = lab_tag
  ) + 
  theme(
    # plot.tag.position = c(0.99, 0.01),
    # plot.tag = ggtext::element_markdown(hjust = 1),
    plot.caption = ggtext::element_markdown(size = 12),
    axis.text.y = ggtext::element_markdown(),
    # axis.text.x.top = element_text(),
    plot.title.position = 'plot',
    legend.title = element_text(size = 10, hjust = 0.5),
    plot.title = element_text(size = 22, hjust = 0),
    plot.subtitle = ggtext::element_markdown(size = 14, hjust = 0, color = 'grey50'),
    panel.grid.major = element_blank(),
    legend.position = 'top'
  )
viz_npxg_p90_diff

path_viz_npxg_p90_diff <- file.path(dir_proj, sprintf('viz_npxg_p90_diff_%s.png', season))
ggsave(
  plot = viz_npxg_p90_diff,
  filename = path_viz_npxg_p90_diff,
  width = 9,
  height = 10,
  type = 'cairo'
)

add_logo(
  path_viz = path_viz_npxg_p90_diff,
  path_logo = path_logo,
  idx_x = 0.05,
  idx_y = 1,
  adjust_y = FALSE
)

# Death, taxes, and diminishing returns are the 3 pillars of life. With Soccer ball, the 3rd principle is evident in the trade-off between non-penalty xG per shot (quality) and shots per 90 (quantity).

# When the game is tied, some teams play frantically while others play much more under control. The influence of game state is evident in the change in teams' xG per 90 when they are either ahead or behind.

