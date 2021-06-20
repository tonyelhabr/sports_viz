
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
  plot.tag.position = c(.01, 0.01),
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
  function(col = 'vaep', adjust = FALSE) {
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
      rename(ovaep = offensive_value, dvaep = defensive_value) %>% 
      mutate(
        vaep = ovaep + dvaep
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
      filter(!(country %in% c('International', 'Europe'))) %>% 
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
        position = c('AM', 'FW', 'M', 'DM', 'D'),
        position_grp = c('A', 'A', 'M', 'M', 'D')
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
          inner_join(position_mapping)
      ) %>% 
      reduce(inner_join) %>%
      ungroup() %>% 
      mutate(
        xg_pm = xg / minutes,
        xg_p90 = 90 * xg_pm,
        vaep_pm = vaep / minutes,
        vaep_p90 = 90 * vaep_pm,
        v = !!col_sym,
        minutes_cutoff = 
          case_when(
            country %in% c('Europe', 'International') ~ 2 * 90,
            TRUE ~ 10 * 90
          )
      ) %>% 
      filter(minutes >= minutes_cutoff) %>% 
      # filter(position == 'A') %>% 
      # filter(season >= 2017) %>% 
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
    
    baseline <-
      df_init %>% 
      summarize(
        across(c(v), list(baseline = median), .names = '{fn}')
      )
    
    baseline_by_grp <-
      df_init %>% 
      group_by(position, age_grp) %>% 
      summarize(
        across(c(v), list(baseline = median), .names = '{fn}')
      ) %>% 
      ungroup()
    
    if(!adjust) {
      df <-
        df_init %>% 
        mutate(idx = row_number(), v_orig = v) %>% 
        relocate(idx)
      return(list(data = df, adjust = NULL, baseline = baseline, baseline_by_grp = baseline_by_grp))
    }
    
    minute_cutoffs <- 
      df_init %>% 
      group_by(season, league_id) %>% 
      summarize(across(minutes, list(cutoff = ~quantile(.x, 0.25)))) %>% 
      ungroup()
    minute_cutoffs
    
    df_filt <- 
      df_init %>% 
      # mutate(
      #   minutes_cutoff = 
      #     case_when(
      #       country %in% c('Europe', 'International') ~ 6 * 90,
      #       TRUE ~ 20 * 90
      #     )
      # ) %>% 
      # filter(minutes >= (20 * 90))
      left_join(minute_cutoffs) %>% 
      filter(minutes >= minutes_cutoff)
    
    estimate_beta <- function(x) {
      mu <- mean(x)
      var <- var(x)
      alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
      beta <- alpha * (1 / mu - 1)
      list(alpha = alpha, beta = beta)
    }
    
    lst <- estimate_beta(df_filt[[sprintf('%s_pm', col)]])
    lst
    
    df <-
      df_init %>%
      rename(v_orig = v) %>%
      mutate(
        idx = row_number(),
        v = 90 * (v_orig + lst$alpha) / (minutes + lst$alpha + lst$beta)
      ) %>%
      relocate(idx, v)
    df
    list(data = df, adjust = lst, baseline = baseline, baseline_by_grp = baseline_by_grp)
  }})

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
    group_by(league_id, league_name, country, season, position_grp, age_grp) %>%
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

do_plots <- function(data, col = 'vaep', direct = FALSE) {
  
  if(col == 'vaep' & direct) {
    lims <- c(0, 1.2)
    binwidth <- 0.01
  } else if (col == 'vaep' & !direct) {
    lims <- c(-3, 3)
    binwidth <- 0.1
  } else if (col == 'xg' & direct) {
    lims <- c(0, 1)
    binwidth <- 0.01
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
    mutate(across(position_grp, factor)) %>% 
    ggplot() +
    aes(x = z, color = position_grp) +
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

do_get_data <- function(data, normalize = FALSE, strict = TRUE) {
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
  
  rgx_rename <- '^(season|idx|team_name|league|z)'
  ids_gt1_meta <-
    ids_gt1 %>% 
    left_join(
      data %>% 
        select(player_id, player_name, matches('position'), age_grp, country, matches(rgx_rename)) %>% 
        unite('league', country, league_name, sep = '_') %>% 
        mutate(across(league, ~str_replace_all(.x, '\\s|[.]', '_') %>% str_replace_all('[_]+', '_'))) %>% 
        left_join(league_mapping %>% select(-path)) %>% 
        select(-league) %>% 
        rename(league = league_lab)
    )
  
  # Position is always the same, so can join on it
  # df_xg %>% distinct(player_id, player_name, position) %>% count(player_id, player_name, sort = TRUE)
  # df_xg %>% distinct(player_id, player_name, season, age_grp) %>% count(player_id, player_name, season, sort = TRUE)
  # df_xg %>% distinct(player_id, player_name, season, age_grp) %>% filter(player_name == 'Sergio Ramos')
  f_rename <- function(suffix) {
    
    ids_gt1_meta %>% 
      mutate(dummy = 0) %>%
      select(
        player_id,
        player_name,
        position,
        position_grp,
        age_grp,
        matches(rgx_rename),
        dummy
      ) %>%
      rename_with(
        ~ sprintf('%s_%s', .x, suffix),
        c(matches(rgx_rename))
      )
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
    group_by(position, age_grp) %>%
    summarize(
      n = n(),
      across(
        z_diff,
        list(mean = mean, sd = sd, median = median), na.rm = TRUE, .names = '{.fn}'
      )
    ) %>%
    ungroup()
  agg
  
  df <-
    res %>%
    rename(z_diff_orig = z_diff) %>% 
    inner_join(agg) %>%
    mutate(z_diff = (z_diff_orig - mean) / sd) %>% 
    relocate(z_diff_orig, z_diff)
  
  df_filt <- .do_filter_season(df, strict = strict)
  list(
    data = df_filt,
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

do_fit_dummy <-
  function(data,
           agg,
           baseline,
           baseline_by_grp,
           col = 'vaep',
           suffix = str_remove(deparse(substitute(data)), '^df_')) {

    rgx <- 'idx|season|player|team'
    res <-
      data %>% 
      # select(player_name, matches('idx'), matches('season'), matches('team'), matches('league'), z_diff) %>% 
      select(player_name, matches(rgx), matches('league_[12]$'), z_diff) %>% 
      pivot_longer(-c(matches(rgx), z_diff)) %>% 
      mutate(across(name, ~str_remove(.x, 'league_') %>% as.integer())) %>% 
      mutate(across(name, ~if_else(.x == 1L, -1L, 1L))) %>% 
      pivot_wider(names_from = value, values_from = name, values_fill = 0L) %>% 
      # Make this the NA coefficient
      relocate(matches('Eredivisie'), .after = last_col())
    
    fit <- lm(formula(z_diff ~ .), data = res %>% select(z_diff, matches('\\(')))
    
    coefs <- fit %>% extract_coefs()
    
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
    
    agg_agg <-
      agg %>% 
      mutate(total = sum(n), frac = n / total) %>%
      summarize(sd = sum(sd * frac), mean = sum(mean * frac), median = sum(median * frac))
    
    vps_init <-
      full_join(
        f_select(1, -1),
        f_select(2, 1)
      ) %>%
      select(-dummy)
    vps_init
    
    vps <-
      vps_init %>% 
      mutate(
        vp = agg_agg$sd * ((estimate_1 - estimate_2) + agg_agg$mean),
        p = vp / baseline$baseline
      )
    vps
    
    vps_by_grp <-
      vps_init %>%
      mutate(dummy = 0) %>% 
      left_join(agg %>% mutate(dummy = 0)) %>% 
      select(-dummy) %>% 
      left_join(baseline_by_grp) %>% 
      mutate(
        vp = sd * ((estimate_1 - estimate_2) + mean),
        p = vp / baseline
      ) # %>% 
      # mutate(across(c(vp, p), ~ifelse(league_1 == league_2,  NA_real_, .x)))
    vps_by_grp

    .filter <- function(data) {
      data %>% 
        filter(rnk_1 >= rnk_2) %>% 
        filter(league_1 != '(Intercept)') %>% 
        filter(league_2 != '(Intercept)')
    }
    
    vps_filt <- vps %>% .filter()
    vps_filt
    
    vps_by_grp_filt <- vps_by_grp %>% .filter()
    vps_by_grp_filt
    
    subtitle <- 'All Field Positions, Ages 18-35, 2012-2020'
    viz_diff_v <- 
      vps_filt %>% 
      plot_heatmap(
        which = 'vp',
        suffix = suffix,
        col = col,
        subtitle = subtitle,
        baseline = baseline$baseline
      )
    viz_diff_v
    
    viz_diff_rel <-
      vps_filt %>% 
      plot_heatmap(
        which = 'p', 
        suffix = suffix, 
        col = col,
        subtitle = subtitle,
        baseline = baseline$baseline
      )
    viz_diff_rel
    
    .filter_f <- function(data) {
      data %>% 
        filter(position == 'FW', age_grp == '18<=x<24') 
    }
    baseline_f <- baseline_by_grp %>% .filter_f() %>% pull(baseline)
    subtitle_f <- 'Forwards, Ages 18-23, 2012-2020'
    vps_filt_f <- vps_by_grp_filt %>% .filter_f()
    viz_diff_v_f <- 
      vps_filt_f %>%
      plot_heatmap(
        which = 'vp',
        suffix = sprintf('%s_fw_young', suffix),
        col = col,
        subtitle = subtitle_f,
        baseline = baseline_f
      )
    viz_diff_v_f
    
    viz_diff_rel_f <- 
      vps_filt_f %>%
      plot_heatmap(
        which = 'p',
        suffix = sprintf('%s_fw_young', suffix),
        col = col,
        subtitle = subtitle_f,
        baseline = baseline_f
      )
    viz_diff_rel_f
    
    list(
      data = res,
      fit = fit,
      coefs = coefs,
      vps = vps,
      vps_by_grp = vps_by_grp,
      viz_diff_v = viz_diff_v,
      viz_diff_rel = viz_diff_rel,
      viz_diff_v_f = viz_diff_v_f,
      viz_diff_rel_f = viz_diff_rel_f
    )
  }

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

lab_tag <- '**Viz** + **Model**: Tony ElHabr | **Data**: @canzhiye'

plot_heatmap <- function(vps_filt, which = 'vp', col = 'vaep', baseline = 0.3, option = 'G', suffix = NULL, subtitle = NULL, sep = '_') {
  # file <- sprintf('%s_p90', col)
  lab <- sprintf('%s/90', ifelse(col == 'vaep', 'atomic VAEP', 'xG'))

  if(which == 'vp') {
    f_scale <- scales::number
    .option <- ifelse(col == 'vaep', 'A', 'E')
    file <- 'vp' # sprintf('%s_vp', file)
    # .fmt <- '%+.2f'
    .acc <- 0.01
    title <- sprintf('Expected change in %s when moving from league A to B', lab)
    .begin <- 0.9
    .end <- 0.1
    subtitle <- sprintf('%s (median %s across all leagues is %0.2f)', subtitle, lab, baseline)
  } else if(which == 'p') {
    f_scale <- scales::percent
    .option <- ifelse(col == 'vaep', 'G', 'F')
    file <- 'p' # sprintf('%s_p', file)
    # .fmt <- '%+.0f%%'
    .acc <- 1
    title <- 'Relative increase in competition level when moving from league A to B'
    # vps_filt <- vps_filt %>% mutate(across(all_of(which), ~.x * -1 * 100))
    .begin <- 0.1
    .end <- 0.9
    vps_filt <- vps_filt %>% mutate(across(all_of(which), ~.x * -1))
    subtitle <- sprintf('Based on %s, using %s baseline of %0.2f (median across all leagues)%s', lab, lab, baseline, ifelse(is.null(suffix), '', sprintf(', %s', subtitle)))
  }
  
  if(!is.null(suffix)) {
    suffix <- sprintf('%s%s', sep, suffix)
  } else {
    suffix <- ''
  }
  
  vps_filt_tax <-
    vps_filt %>% 
    filter(league_1 == 'Bundesliga 1 (Germany)', league_2 == 'Premier League (England)')
  
  col_sym <- sym(which)
  arw_annotate <- arrow(length = unit(3, 'pt'), type = 'closed')
  p <-
    vps_filt %>% 
    ggplot() +
    aes(x = league_2, y = league_1) +
    geom_tile(aes(fill = !!col_sym), alpha = 0.7,  height = 0.95, width = 0.95, show.legend = FALSE) +
    geom_tile(
      data = vps_filt_tax,
      fill = NA_character_,
      color = 'black',
      size = 2
    ) +
    geom_text(
      data = tibble(),
      aes(x = 5, y = 12, label = 'Bundesliga (to Premier League) tax?'),
      # fontface = 'italic',
      family = 'Karla',
      vjust = -1,
      hjust = 0,
      size = pts(16)
    ) +
    annotate(
      geom = 'curve',
      x = 5,
      y = 12,
      # xend = 'Premier League (England)',
      xend = 1.3,
      yend = 'Bundesliga 1 (Germany)',
      size = 1,
      curvature = -0.25,
      arrow = arw_annotate
    ) +
    # geom_text(aes(label = sprintf(.fmt, !!col_sym)), size = pts(14), fontface = 'bold') +
    geom_text(aes(label = f_scale(!!col_sym, accuracy = .acc)), size = pts(14), fontface = 'bold') +
    scale_fill_viridis_c(option = .option, begin = .begin, end = .end) +
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
  p

  if(col == 'vaep') {
    p <-
      p +
      ggtext::geom_richtext(
        data = tibble(),
      aes(x = 7, y = 8, label = 'Atomic VAEP encompasses all on-ball offensive and defensive actions.<br/>A 10% difference could be decomposed in many ways, e.g. 6% change in value from shots,<br/>-2% change in value from dribbles, etc.'),
      fill = NA, 
      label.color = NA,
      # fontface = 'italic',
      family = 'Karla',
      vjust = -1,
      hjust = 0,
      size = pts(12)
    )
  }
  
  ggsave(
    plot = p,
    filename = file.path(dir_proj, sprintf('viz_relative_%s%s.png', file, suffix)),
    width = 16,
    height = 8,
    type = 'cairo'
  )
  p
}

