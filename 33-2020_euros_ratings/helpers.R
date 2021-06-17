
library(tidyverse)
library(zeallot)
library(lubridate)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 18, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown('Karla', face = 'bold', size = 16, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  plot.caption = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 1),
  # plot.caption = element_text('Karla', size = 12, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.015),
  legend.text = element_text(color = 'gray20', size = 12),
  # strip.text = element_text(color = 'gray20', size = 14),
  # strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

get_dir_proj <- memoise::memoise({
  function() '33-2020_euros_ratings'
})

import_csv <- function(x) {
  file.path(dir_proj, sprintf('%s.csv', x)) %>% 
    read_csv()
}

# made this
import_league_mapping <- function() {
  import_csv('league_mapping') %>% 
    mutate(path = ifelse(!is.na(file_png), file.path('25-202021_soccer_refs', sprintf('%s.png', file_png)), NA_character_)) %>% 
    select(-file_png)
}
league_mapping <- import_league_mapping()

do_import <- memoise::memoise({
  function(col = 'value') {
    col_sym <- sym(sprintf('%s_p90', col))
    leagues_init <-
      import_csv('leagues') %>% 
      rename(league_name = name) %>% 
      mutate(
        # Some manual adjustments prior to using some "auto" name correction (by descending season)
        across(
          league_name,
          ~case_when(
            country == 'USA' & league_name %>% str_detect('Major League Soccer') ~ 'Major League Soccer',
            country == 'China' & league_name %>% str_detect('Super League') ~ 'Super League',
            country == 'Spain' & league_name == 'Primera Division' ~ 'LaLiga',
            TRUE ~ .x
          )
        )
      )

    league_names <-
      leagues_init %>%
      mutate(
        across(league_name, ~str_remove_all(.x, '\\s+[Gg]rp.*'))
      ) %>% 
      mutate(n_char = nchar(league_name)) %>% 
      arrange(league_id, n_char, desc(season)) %>% 
      group_by(league_id) %>% 
      filter(row_number() == 1L) %>% 
      ungroup() %>% 
      select(-season)
    league_names
    
    leagues <- leagues_init %>% select(-league_name) %>% inner_join(league_names)
    leagues
    
    teams <- import_csv('teams')
    players <- 
      import_csv('players') %>% 
      mutate(across(dob, lubridate::date)) %>% 
      rename(player_id = id, player_name = name)
    
    player_xg <- import_csv('player_xg_by_season') %>% rename(xg = xg_value)
    player_ratings <-
      import_csv('player_ratings_by_season') %>% 
      mutate(
        # across(
        #   minutes,
        #   ~case_when(
        #     .x > 4820 ~ games_played * 80,
        #     TRUE ~ .x
        #   )
        # )
        value = offensive_value + defensive_value
      )
    # player_ratings %>% 
    #   select(player_id, team_id, league_id, season, gp1 = games_played, mp1 = minutes) %>%
    #   full_join(
    #     player_xg %>% select(player_id, team_id, league_id, season, gp2 = games_played, mp2 = minutes)
    #   ) -> z
    # z %>% 
    #   filter(gp1 < gp2)
    
    leagues_n <- 
      leagues %>% 
      # filter(!(country %in% c('International', 'Europe'))) %>% 
      count(country, league_id, league_name, sort = TRUE) %>% 
      filter(n > 1L)
    leagues_n
    
    leagues_n <-
      leagues %>% 
      filter(season >= 2012) %>% 
      distinct(country, league_id, league_name, season) %>% 
      # arrange(country, league_id, league_name, season) %>% 
      count(country, league_id, league_name, sort = TRUE) %>% 
      filter(n >= 5L)
    leagues_n
    
    age_grps <-
      tibble(
        from = c(18L, 24L, 27L, 30L),
        to = c(24L, 27L, 30L, 36L)
      )
    age_grps
    
    position_mapping <-
      tibble(
        position_old = c('AM', 'FW', 'M', 'DM', 'D'),
        position = c('A', 'A', 'M', 'M', 'D')
      )
    position_mapping
    
    df_init <-
      list(
        leagues_n %>% select(-n),
        teams %>% distinct(),
        player_ratings,
        player_xg %>% rename(games_played_xg = games_played, minutes_xg = minutes),
        players %>%
          drop_na(position, dob) %>%
          select(player_id, player_name, position, dob) %>%
          mutate(across(position, ~str_remove_all(.x, '\\(.*\\)') %>% str_remove_all('\\,.*$'))) %>%
          filter(position != 'GK') %>%
          rename(position_old = position) %>%
          inner_join(position_mapping) # %>%
        # select(-position_old)
      ) %>% 
      reduce(inner_join) %>%
      ungroup() %>% 
      mutate(
        xg_pm = xg / minutes,
        xg_p90 = xg * xg_pm,
        value_pm = value / minutes,
        value_p90 = 90 * value_pm
      ) %>% 
      rename(v = !!col_sym) %>% 
      filter(minutes > (5 * 90)) %>% 
      mutate(age = lubridate::time_length(lubridate::ymd(sprintf('%s-08-01', season)) - dob, 'year') %>% floor() %>% as.integer()) %>% 
      filter(age >= 18 & age <= 35) %>% 
      distinct()
    df_init
    
    df_init <-
      data.table::as.data.table(df_init %>% mutate(age2 = age, age3 = age))[
        data.table::as.data.table(age_grps), 
        on=.(age2 >= from, age3 < to)
      ] %>% 
      as_tibble() %>% 
      unite('age_grp', age2, age3, sep = '<=x<')
    df_init
    
    # df_init %>% count(age_grp, sort = TRUE)
    # df %>% filter(is.na(v))
    
    df_filt <- df_init %>% filter(minutes >= (20 * 90))
    df_filt
    
    estimate_beta <- function(x) {
      mu <- mean(x)
      var <- var(x)
      alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
      beta <- alpha * (1 / mu - 1)
      list(alpha = alpha, beta = beta)
    }
    
    # estimate_beta(df_filt$xg_p90)
    # estimate_beta(df_filt$v)
    lst <- estimate_beta(df_filt$value_pm)
    lst
    
    df <-
      df_init %>% 
      rename(v_orig = v) %>% 
      mutate(
        idx = row_number(), 
        v = 90 * (value + lst$alpha) / (minutes + lst$alpha + lst$beta)
      ) %>% 
      relocate(idx, v)
    df
  }})

# # Commenting this shit out to try direct vaep comparison
do_modify_v_col <- function(data, direct = FALSE) {
  if(direct) {
    return(
      list(
        data = data %>% mutate(z = v) %>% relocate(idx, z, v),
        agg = NULL,
        v_min = NULL
      )
    )
  }
  v_min <-
    data %>%
    drop_na(v) %>%
    filter(!is.infinite(v)) %>%
    summarize(`_` = min(v)) %>%
    pull(`_`)
  v_min
  
  data <- data %>% mutate(across(v, ~log(.x + abs(!!v_min) + 0.001)))
  
  agg <-
    data %>%
    group_by(league_id, league_name, country, season, position, age_grp) %>%
    summarize(
      n = n(),
      across(
        v,
        list(mean = mean, sd = sd), na.rm = TRUE, .names = '{.fn}'
      )
    ) %>%
    ungroup() %>%
    filter(n > 1L)
  agg
  
  res <-
    data %>%
    inner_join(agg) %>%
    mutate(z = (v - mean) / sd) %>% 
    relocate(idx, v_orig, v, z, mean, sd)
  list(data = res, agg = agg, v_min = v_min)
}

do_plots <- function(data, direct = FALSE) {
  # data %>% count(age_grp, sort = TRUE)
  # data %>% count(position, sort = TRUE)
  if(direct) {
    lims <- c(0, 1.2)
    binwidth <- 0.01
  } else {
    lims <- c(-3, 3)
    binwidth <- 0.1
  }
  p1 <-
    data %>% 
    mutate(across(age_grp, factor)) %>% 
    ggplot() +
    aes(x = z, color = age_grp) +
    geom_density() +
    coord_cartesian(xlim = lims)
  
  p2 <-
    data %>% 
    mutate(across(position, factor)) %>% 
    ggplot() +
    aes(x = z, color = position) +
    geom_density() +
    coord_cartesian(xlim = lims)
  
  p3 <-
    data %>% 
    ggplot() +
    aes(x = z) +
    geom_histogram(binwidth = binwidth) +
    coord_cartesian(xlim = lims)
  list(p1 = p1, p2 = p2, p3 = p3)
}

do_get_data <- function(data, normalize = FALSE) {
  ids <-
    data %>% 
    distinct(player_id, league_id)
  ids
  
  ids_gt1 <-
    ids %>% 
    count(player_id, sort = TRUE) %>% 
    filter(n > 1L) %>% 
    select(-n) %>% 
    inner_join(ids)
  ids_gt1
  
  ids_gt1_meta <-
    ids_gt1 %>% 
    left_join(
      data %>% 
        select(player_id, player_name, idx, team_name, league_id, country, league_name, season, z) %>% 
        unite('league', country, league_name, sep = '_') %>% 
        mutate(across(league, ~str_replace_all(.x, '\\s|[.]', '_') %>% str_replace_all('[_]+', '_'))) %>% 
        left_join(league_mapping %>% select(-path)) %>% 
        select(-league) %>% 
        rename(league = league_lab)
    )
  # ids_gt1_meta %>% count(league, sort = TRUE)
  
  f_rename <- function(suffix) {
    ids_gt1_meta %>% 
      mutate(dummy = 0) %>% 
      select(player_id, player_name, league, idx, team_name, matches('^(season|z)'), dummy) %>% 
      rename_with(~sprintf('%s_%s', .x, suffix), c(league, idx, team_name, matches('^(season|z)')))
  }
  
  res <-
    full_join(
      f_rename(1),
      f_rename(2)
    ) %>% 
    select(-dummy) %>% 
    filter(league_1 != league_2) %>% 
    mutate(z_diff = z_1 - z_2)
  
  if(!normalize) {
    return(list(data = res, agg = NULL))
  }
  agg <-
    res %>%
    # group_by(league_id, league_name, country, season, position, age_grp) %>%
    summarize(
      n = n(),
      across(
        z_diff,
        list(mean = mean, sd = sd), na.rm = TRUE, .names = '{.fn}'
      )
    ) %>%
    ungroup()
  agg
  
  list(
    data = 
      res %>%
      rename(z_diff_orig = z_diff) %>% 
      # inner_join(agg) %>%
      mutate(z_diff = (z_diff_orig - agg$mean) / agg$sd) %>% 
      relocate(z_diff_orig, z_diff),
    agg = agg
  )
}


extract_coefs <- function(fit) {
  fit %>% 
    broom::tidy() %>% 
    select(league = term, estimate) %>% 
    mutate(across(league, ~str_remove_all(.x, '`'))) %>% 
    arrange(desc(estimate))
}

# emperical choice based on df_direct %>% skimr::skim(z)
baseline <- 0.33

.do_filter_season <- function(data, strict = TRUE) {
  if(!strict) {
    return(data)
  }
  data %>% 
    filter((season_2 == (season_1 + 1)) | (season_2 == season_1))
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

do_fit_dummy <- function(data, agg = NULL, strict = TRUE, .mean = agg$mean %||% 0, .sd = agg$sd %||% 1) {
  data_filt <- .do_filter_season(data, strict = strict)
  
  rgx <- 'idx|season|player|team'
  res <-
    data_filt %>% 
    select(player_name, matches('idx'), matches('season'), matches('team'), matches('league'), z_diff) %>% 
    pivot_longer(-c(matches('idx|season|player|team'), z_diff)) %>% 
    mutate(across(name, ~str_remove(.x, 'league_') %>% as.integer())) %>% 
    mutate(across(name, ~if_else(.x == 1L, -1L, 1L))) %>% 
    pivot_wider(names_from = value, values_from = name, values_fill = 0L) %>% 
    # Make this the NA coefficient
    relocate(matches('Eredivisie'), .after = last_col())
  
  fit <- lm(formula(z_diff ~ .), data = res %>% select(-matches('idx|season|player|team')))
  
  coefs <- fit %>% extract_coefs()
  coefs
  
  rnks <- 
    coefs %>% 
    mutate(rnk = row_number(desc(estimate))) %>% 
    arrange(rnk)
  rnks
  
  f_select <- function(suffix, op = 1) {
    rnks %>% 
      mutate(across(league, ~forcats::fct_reorder(.x, op * rnk))) %>% 
      rename_all(~sprintf('%s_%s', .x, suffix)) %>% 
      mutate(dummy = 0L)
  }
  
  vps <-
    full_join(
      f_select(1, -1),
      f_select(2, -1)
    ) %>%
    select(-dummy) %>% 
    mutate(
      diff = estimate_1 - estimate_2,
      vp = .sd * (diff + .mean),
      p = vp / !!baseline
    )
  vps
  
  list(data = res, fit = fit, coefs = coefs, vps = vps)
}

do_compare_coefs <- function(x, y, suffix = c('x', 'y'), sep = '_') {
  suffix_x <- suffix[1]
  suffix_y <- suffix[2]
  col_rnk_x <- sprintf('rnk%s%s', sep, suffix_x)
  col_rnk_y <- sprintf('rnk%s%s', sep, suffix_y)
  col_rnk_x_sym <- sym(col_rnk_x)
  col_rnk_y_sym <- sym(col_rnk_y)
  f <- function(data, col_rnk_sym, sep, suffix) {
    data %>% 
      mutate(!!col_rnk_sym := row_number(desc(estimate))) %>% 
      rename(!!sym(sprintf('estimate%s%s', sep, suffix)) := estimate)
  }
  
  full_join(
    x %>% f(col_rnk_x_sym, sep, suffix_x),
    y %>% f(col_rnk_y_sym, sep, suffix_y)
  ) %>% 
    mutate(rnk_diff = !!col_rnk_x_sym - !!col_rnk_y_sym) %>% 
    arrange(!!col_rnk_x_sym + !!col_rnk_y_sym)
}

get_leagues_filt <- memoise::memoise({function() {
  c('Champions League (Europe)', 'Europa League (Europe)', 'Premier League (England)', 'Serie A (Italy)', 'Bundesliga 1 (Germany)', 'Ligue 1 (France)', 'La Liga (Spain)')
}})

# gplots::col2hex('grey50') %>% scales::show_col()
get_leagues_pal <- memoise::memoise({function() {
  leagues_filt <- get_leagues_filt()
  c('#000000', '#7f7f7f', '#003f5c', '#58508d', '#bc5090', '#ff6361', '#ffa600') %>% setNames(leagues_filt)
}})


pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

lab_tag <- '**Viz** + **Model**: Tony ElHabr | **Data**: @canzhiye'

do_plot_vps_by_season <-
  function(data,
           dir = get_dir_proj(),
           suffix = NULL,
           sep = '_',
           seed = 42,
           .mean,
           .sd,
           ...) {
    if (!is.null(suffix)) {
      suffix <- sprintf('%s%s', sep, suffix)
    }
    
    fits_by_season <-
      data %>% 
      select(season = season_1, z_diff, `Champions League (Europe)`:last_col()) %>% 
      filter(season <= 2020) %>% 
      group_nest(season) %>% 
      mutate(
        fit = map(data, ~lm(formula(z_diff ~ .), data = .x)),
        coefs = map(fit, extract_coefs)
      )
    fits_by_season
    
    leagues_filt <- get_leagues_filt()
    coefs_by_season <-
      fits_by_season %>% 
      select(season, coefs) %>% 
      unnest(coefs) %>% 
      # mutate(across(season, factor))
      mutate(across(season, ~sprintf('%04d-07-01', .x) %>% lubridate::ymd()))
    coefs_by_season
    
    
    f_select <- function(suffix) {
      coefs_by_season %>% 
        rename_with(~sprintf('%s_%s', .x, suffix), -c(season)) %>% 
        mutate(dummy = 0L)
    }
    
    vps <-
      full_join(
        f_select(1),
        f_select(2)
      ) %>%
      select(-dummy) %>% 
      mutate(
        diff = estimate_1 - estimate_2,
        vp = .sd * (diff + .mean),
        p = vp / !!baseline
      )
    vps
    
    vps1 <-
      vps %>% 
      filter(league_1 == 'Champions League (Europe)') %>% 
      mutate(p_inv = 1 - p) %>% 
      select(season, league = league_2, p_inv)
    vps1
    
    vps1_filt <-
      vps1 %>% 
      filter(league %in% leagues_filt)
    vps1_filt %>% filter(season == max(season)) # make sure colors follow this order
    
    vps1_nofilt <-
      vps1 %>% 
      filter(!(league %in% leagues_filt))
    
    seq_date_first <- seq.Date(lubridate::ymd('2010-01-01'), lubridate::ymd('2022-01-01'), by = 'year')
    seq_date_mid <- seq.Date(lubridate::ymd('2010-07-01'), lubridate::ymd('2021-07-01'), by = 'year')
    seq_date <- sort(c(seq_date_first, seq_date_mid))
    labs_date <- c(rep(2010:2021, each = 2), 2022)
    idx <- (seq_along(labs_date) %% 2) == 1L
    idx[length(idx) - 1] <- TRUE
    labs_date[idx] <- ''
    
    pal <- get_leagues_pal()
    date_min <- lubridate::ymd('2012-01-01')
    # browser()
    p <-
      vps1_nofilt %>% 
      drop_na() %>% 
      filter(season >= !!date_min) %>% 
      ggplot() +
      aes(x = season, y = p_inv, group = league) +
      geom_point(color = 'grey80') +
      geom_line(color = 'grey80') +
      geom_point(
        data = vps1_filt,
        size = 3,
        aes(color = league)
      ) +
      geom_line(
        data = vps1_filt,
        size = 1.25,
        aes(color = league)
      ) +
      ggrepel::geom_label_repel(
        data = 
          vps1_filt %>% 
          filter(season == max(season)),
        hjust = 'left',
        seed = seed,
        family = 'Karla',
        fontface = 'bold',
        direction = 'y',
        nudge_x = 10,
        size = pts(10),
        label.size = NA,
        aes(label = league, color = league) # ,
        # ...
      ) +
      scale_color_manual(values = pal) +
      scale_x_date(labels = labs_date, breaks = seq_date, limits = range(date_min, lubridate::ymd('2022-01-01'))) +
      scale_y_continuous(labels = scales::percent, limits = c(0.5, 1)) +
      guides(color = FALSE) +
      theme(
        panel.grid.major.y = element_blank(),
        axis.ticks.x = element_line(color = c('grey80', rep(c(NA, 'grey80'), t = length(seq_date)))),
        panel.grid.major.x = element_line(color = c('grey80', rep(c(NA, 'grey80'), t = length(seq_date))))
      ) +
      labs(
        title = 'How has the competition level changed?',
        tag = lab_tag,
        caption = 'Note that y-axis starts at 50%.',
        subtitle = 'Competition level relative to Champions League',
        y = NULL,
        x = NULL
      )
    p
    
    ggsave(
      plot = p,
      filename = file.path(dir, sprintf('vps1%s.png', suffix)),
      width = 12,
      height = 8,
      type = 'cairo'
    )
    p
  }

plot_heatmap <- function(vps_filt, col) {
  if(col == 'vp') {
    f_scale <- scales::number
    .option <- 'D'
    file <- 'vaep_p90'
    .acc <- 0.01
    title <- 'Expected change in VAEP/90 when transitioning from league A to B'
    subtitle <- NULL
  } else {
    f_scale <- scales::percent
    .option <- 'H'
    file <- 'difficulty'
    .acc <- 1
    title <- 'Relative increase in competition in league A compared to league B'
    subtitle <- 'Using VAEP/90 baseline of 0.33'
  }
  col_sym <- sym(col)
  p <-
    vps_filt %>% 
    ggplot() +
    aes(x = league_2, y = league_1) +
    geom_tile(aes(fill = !!col_sym), alpha = 0.7,  height = 0.95, width = 0.95, show.legend = FALSE) +
    geom_text(aes(label = f_scale(!!col_sym, accuracy = .acc)), size = pts(14), fontface = 'bold') +
    scale_fill_viridis_c(option = .option, begin = 0.1, end = 1) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
    theme(
      plot.title = ggtext::element_markdown(size = 18),
      plot.subtitle = ggtext::element_markdown(size = 16),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.x = element_text(size = 9),
      axis.title.y = element_text(size = 16),
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      tag = lab_tag,
      y = 'League A',
      x = 'League B'
    )
  ggsave(
    plot = p,
    filename = file.path(dir_proj, sprintf('viz_relative_%s.png', file)),
    width = 16,
    height = 8,
    type = 'cairo'
  )
  p
}

