
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
  plot.subtitle = ggtext::element_markdown('Karla', face = 'bold', size = 18, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  plot.caption = ggtext::element_markdown('Karla', size = 16, color = 'gray20', hjust = 1),
  # plot.caption = element_text('Karla', size = 12, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.01),
  legend.text = element_text(color = 'gray20', size = 14),
  # strip.text = element_text(color = 'gray20', size = 14),
  # strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

get_dir_proj <- memoise::memoise({
  function() '33-2020_euros_ratings'
})

import_csv <- function(x, dir = get_dir_proj()) {
  file.path(dir, sprintf('%s.csv', x)) %>% 
    read_csv()
}

# made this
import_league_mapping <- function(dir = get_dir_proj()) {
  import_csv('league_mapping', dir = dir)
}

do_import <- memoise::memoise({
  function(dir = get_dir_proj(), league_mapping = import_league_mapping()) {

  leagues_init <-
    import_csv('leagues', dir = dir) %>% 
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
  # leagues_init
  # leagues_init %>% count(league_id, sort = TRUE)
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
  
  teams <- import_csv('teams', dir = dir)
  players <- 
    import_csv('players', dir = dir) %>% 
    mutate(across(dob, lubridate::date)) %>% 
    rename(player_id = id, player_name = name)
  
  player_ratings <-
    import_csv('player_ratings', dir = dir) %>% 
    mutate(
      value = offensive_value + defensive_value
    )
  player_ratings
  
  # These have multiple "sub-leagues" (e.g. playoffs, group stages).
  # This is corrected later with a summarize.
  # leagues %>% count(country, league_name, season, sort = TRUE)
  
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
      value_pm = value / minutes,
      v = 90 * value_pm
    ) %>% 
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
  
  # estimate_beta(df_filt$value_pm)
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
  lims <- if(direct) {
    c(0, 1.2)
  } else {
    c(-3, 3)
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
    geom_histogram(binwidth = 0.1) +
    coord_cartesian(xlim = lims)
  list(p1 = p1, p2 = p2, p3 = p3)
}

do_filter_tst <- function(data) {
  data %>% 
    df_trans %>% 
    filter(player_name == 'Timo Werner', league_name == 'Premier League', season == 2021)
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
        left_join(league_mapping) %>% 
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
        list(mean = mean, sd = sd), na.rm = TRUE, .names = 'z_diff_{.fn}'
      )
    ) %>%
    ungroup()
  agg
  
  list(
    data = 
      res %>%
      rename(z_diff_orig = z_diff) %>% 
      # inner_join(agg) %>%
      mutate(z_diff = (z_diff_orig - agg$z_diff_mean) / agg$z_diff_sd) %>% 
      relocate(z_diff_orig, z_diff), # , z_diff_mean, z_diff_sd),
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

do_fit_dummy <- function(data, strict = TRUE) {
  # strict <- FALSE
  data_filt <- .do_filter_season(data, strict = strict)
  
  rgx <- 'idx|season|player|team'
  res <-
    data_filt %>% 
    select(player_name, matches('idx'), matches('season'), matches('team'), matches('league'), z_diff) %>% 
    # mutate(idx = row_number()) %>% 
    pivot_longer(-c(matches('idx|season|player|team'), z_diff)) %>% 
    mutate(across(name, ~str_remove(.x, 'league_') %>% as.integer())) %>% 
    mutate(across(name, ~if_else(.x == 1L, -1L, 1L))) %>% 
    pivot_wider(names_from = value, values_from = name, values_fill = 0L) %>% 
    # select(-idx) %>% 
    # Make this the NA coefficient
    relocate(matches('Eredivisie'), .after = last_col())
    # relocate(matches('season'), .after = last_col())
  
  fit <- lm(formula(z_diff ~ .), data = res %>% select(-matches('idx|season|player|team')))
  
  coefs <- fit %>% extract_coefs()
  coefs
  list(data = res, fit = fit, coefs = coefs)
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

do_plot_coefs_overall <- function(coefs, dir, suffix = NULL, sep = '_') {
  if(!is.null(suffix)) {
    suffix <- sprintf('%s%s', sep, suffix)
  }
  p <-
    coefs %>% 
    drop_na(estimate) %>% 
    mutate(across(league, ~forcats::fct_reorder(.x, estimate))) %>% 
    ggplot() +
    aes(y = league, x = estimate) +
    geom_col(fill = 'grey30', color = NA_character_) +
    theme(
      panel.grid.major.y = element_blank()
    ) +
    labs(
      title = 'Overall League Strength Estimates',
      y = NULL,
      x = 'Estimate'
    )
  p
  
  ggsave(
    plot = p,
    filename = file.path(dir, sprintf('coefs%s.png', suffix)),
    width = 8,
    height = 8,
    type = 'cairo'
  )
  p
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

do_plot_coefs_by_season <- function(data, dir, suffix = NULL, sep = '_', seed = 42, ...) {
  if(!is.null(suffix)) {
    suffix <- sprintf('%s%s', sep, suffix)
  }
  
  fits_by_season <-
    data %>% 
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
  coefs_by_season %>% filter(season == max(season))
  
  
  coefs_by_season_filt <-
    coefs_by_season %>% 
    filter(league %in% leagues_filt)
  coefs_by_season_filt %>% filter(season == max(season)) # make sure colors follow this order
  
  coefs_by_season_nofilt <-
    coefs_by_season %>% 
    filter(!(league %in% leagues_filt))
  
  seq_date_first <- seq.Date(lubridate::ymd('2010-01-01'), lubridate::ymd('2022-01-01'), by = 'year')
  seq_date_mid <- seq.Date(lubridate::ymd('2010-07-01'), lubridate::ymd('2021-07-01'), by = 'year')
  seq_date_mid
  seq_date <- sort(c(seq_date_first, seq_date_mid))
  seq_date
  labs_date <- c(rep(2010:2021, each = 2), 2022)
  labs_date
  idx <- (seq_along(labs_date) %% 2) == 1L
  idx
  idx[length(idx) - 1] <- TRUE
  labs_date[idx] <- ''
  labs_date
  labs_date
  
  pal <- get_leagues_pal()
  p <-
    coefs_by_season_nofilt %>% 
    ggplot() +
    aes(x = season, y = estimate, group = league) +
    geom_point(color = 'grey80') +
    geom_line(color = 'grey80') +
    geom_point(
      data = coefs_by_season_filt,
      size = 3,
      aes(color = league)
    ) +
    geom_line(
      data = coefs_by_season_filt,
      size = 1.25,
      aes(color = league)
    ) +
    ggrepel::geom_label_repel(
      data = 
        coefs_by_season_filt %>% 
        filter(season == max(season)), #  %>% 
      # mutate(across(season, ~.x %m+% months(1),
      hjust = 'left',
      seed = seed,
      family = 'Karla',
      fontface = 'bold',
      direction = 'y',
      nudge_x = 10,
      # point.padding = unit(2, 'lines'),
      size = pts(10),
      label.size = NA,
      # segment.size = 0.2,
      # min.segment.length = 2,
      # box.padding = 1,
      aes(label = league, color = league),
      ...
    ) +
    scale_color_manual(values = pal) +
    scale_x_date(labels = labs_date, breaks = seq_date, limits = range(lubridate::ymd('2013-01-01'), lubridate::ymd('2022-01-01'))) +
    # guides(color = guide_legend(title = '', override.aes = list(size = 3))) +
    guides(color = FALSE) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.ticks.x = element_line(color = c('grey80', rep(c(NA, 'grey80'), t = length(seq_date)))),
      panel.grid.major.x = element_line(color = c('grey80', rep(c(NA, 'grey80'), t = length(seq_date))))
    ) +
    labs(
      title = 'League Strength Coefficients',
      y = 'Coefficient',
      x = NULL
    )
  p
  
  ggsave(
    plot = p,
    filename = file.path(dir, sprintf('coefs_by_season%s.png', suffix)),
    width = 12,
    height = 8,
    type = 'cairo'
  )
  p
}

logit2prob <- function(logit){
  odds <- exp(logit)
  odds / (1 + odds)
}

do_fit_bt <- function(data, strict = TRUE) {
  df <- .do_filter_season(data, strict = strict)
  
  fit <-
    BradleyTerry2::BTm(
      outcome = result, 
      player1 = league_1, 
      player2 = league_2, 
      id = 'league', 
      data =
        df %>% 
        mutate(result = if_else(z_diff < 0, 1L, 0L)) %>% 
        mutate(across(matches('^league'), factor))
    )
  
  # coefs <-
  #   BradleyTerry2::BTabilities(fit) %>% 
  #   as_tibble(rownames = 'league') %>% 
  #   mutate(across(ability, logit2prob)) %>% 
  #   arrange(desc(ability))
  
  coefs <-
    fit %>% 
    broom::tidy() %>% 
    mutate(across(term, ~str_remove(.x, 'league'))) %>% 
    # rename(league = term) %>% 
    select(league = term, estimate) %>% 
    arrange(-estimate)
  coefs
  
  # coefs_trans <-
  #   coefs %>% 
  #   mutate(
  #     u = exp(estimate + 1.96 * std.error),
  #     l = exp(estimate - 1.96 * std.error)
  #   )
  # leagues <- coefs %>% pull(league)
  
  f_select <- function(suffix) {
    coefs %>% 
      select(league, estimate) %>% 
      rename_all(~sprintf('%s_%s', .x, suffix)) %>% 
      mutate(dummy = 0L)
  }
  
  probs <-
    full_join(
      f_select(1),
      f_select(2)
    ) %>% 
    select(-dummy) %>% 
    filter(league_1 != league_2) %>% 
    mutate(
      p = logit2prob(estimate_1 - estimate_2)
    )
  
  list(data = df, fit = fit, coefs = coefs, probs = probs)
}

# https://github.com/stan-dev/bayesplot/blob/master/R/mcmc-intervals.R#L284
# library(bayesplot)
do_mcmc_areas <- function(x,
                          pars = character(),
                          regex_pars = character(),
                          transformations = list(),
                          pars_mapping,
                          ...,
                          area_method = c("equal area", "equal height", "scaled height"),
                          prob = 0.5,
                          prob_outer = 1,
                          point_est = c("median", "mean", "none"),
                          rhat = numeric(),
                          bw = NULL,
                          adjust = NULL,
                          kernel = NULL,
                          n_dens = NULL) {
  
  area_method <- match.arg(area_method)
  
  data <- bayesplot::mcmc_areas_data(
    x, pars, regex_pars, transformations,
    prob = prob, prob_outer = prob_outer,
    point_est = point_est, rhat = rhat,
    bw = bw, adjust = adjust, kernel = kernel, n_dens = n_dens
  )
  # added this
  data <-
    data %>% 
    left_join(pars_mapping) %>%
    select(-parameter) %>% 
    rename(parameter = league) %>% 
    relocate(parameter)
  data
  datas <- split(data, data$interval)
  datas
  no_point_est <- !rlang::has_name(datas, "point")
  datas$point <- if (no_point_est) {
    dplyr::filter(datas$inner, FALSE)
  } else {
    datas$point
  }
  color_by_rhat <- rlang::has_name(data, "rhat_rating")
  
  # faint vertical line at zero if zero is within x_lim
  x_lim <- range(datas$outer$x)
  x_range <- diff(x_lim)
  x_lim[1] <- x_lim[1] - 0.05 * x_range
  x_lim[2] <- x_lim[2] + 0.05 * x_range
  
  layer_vertical_line <- if (0 > x_lim[1] && 0 < x_lim[2]) {
    vline_0(color = "gray90", size = 0.5)
  } else {
    geom_ignore()
  }
  groups <- if (color_by_rhat) {
    rlang::syms(c("parameter", "rhat_rating"))
  } else {
    rlang::syms(c("parameter"))
  }
  
  if (area_method == "equal height") {
    dens_col = ~ scaled_density
  } else if (area_method == "scaled height") {
    dens_col = ~ scaled_density * sqrt(scaled_density)
  } else {
    dens_col = ~ plotting_density
  }
  
  datas$bottom <- datas$outer %>%
    group_by(!!! groups) %>%
    summarise(
      ll = min(.data$x),
      hh = max(.data$x),
      .groups = "drop_last"
    ) %>%
    ungroup()
  
  args_bottom <- list(
    mapping = aes_(x = ~ ll, xend = ~ hh, yend = ~ parameter),
    data = datas$bottom
  )
  args_inner <- list(
    mapping = aes_(height = dens_col, scale = ~ .9),
    data = datas$inner
  )
  args_point <- list(
    mapping = aes_(height = dens_col, scale = ~ .9),
    data = datas$point,
    color = NA
  )
  args_outer <- list(
    mapping = aes_(height = dens_col, scale = ~ .9),
    fill = NA
  )
  
  if (color_by_rhat) {
    args_bottom$mapping <- args_bottom$mapping %>%
      modify_aes_(color = ~ rhat_rating)
    args_inner$mapping <- args_inner$mapping %>%
      modify_aes_(color = ~ rhat_rating,
                  fill = ~ rhat_rating)
    args_outer$mapping <- args_outer$mapping %>%
      modify_aes_(color = ~ rhat_rating)
    # rhat fill color scale uses light/mid/dark colors. The point estimate needs
    # to be drawn with highlighted color scale, so we manually set the color for
    # the rhat fills.
    dc <- diagnostic_colors("rhat", "color")[["values"]]
    args_point$fill <- dc[datas$point$rhat_rating]
  } else {
    args_bottom$color <- 'grey20'# bayesplot:::get_color("dark")
    args_inner$color <- 'grey20' # bayesplot:::get_color("dark")
    args_inner$fill <- 'grey80' # bayesplot:::get_color("light")
    args_point$fill <- 'grey50' # bayesplot:::get_color("mid_highlight")
    args_outer$color <- 'grey20' #  bayesplot:::get_color("dark")
  }
  # An invisible layer that is 2.5% taller than the plotted one
  args_outer2 <- args_outer
  args_outer2$mapping <- args_outer2$mapping %>%
    bayesplot:::modify_aes_(scale = .925)
  args_outer2$color <- NA
  
  layer_bottom <- do.call(geom_segment, args_bottom)
  layer_inner <- do.call(ggridges::geom_ridgeline, args_inner)
  layer_outer <- do.call(ggridges::geom_ridgeline, args_outer)
  layer_outer2 <- do.call(ggridges::geom_ridgeline, args_outer2)
  
  point_geom <- if (no_point_est) {
    geom_ignore
  } else {
    ggridges::geom_ridgeline
  }
  layer_point <- do.call(point_geom, args_point)
  
  # Do something or add an invisible layer
  if (color_by_rhat) {
    scale_color <- bayesplot:::scale_color_diagnostic("rhat")
    scale_fill <- bayesplot:::scale_fill_diagnostic("rhat")
  } else {
    scale_color <- bayesplot:::geom_ignore()
    scale_fill <- bayesplot:::geom_ignore()
  }
  
  p <-
    ggplot(datas$outer) +
    aes_(x = ~ x, y = ~ parameter) +
    layer_vertical_line +
    layer_inner +
    layer_point +
    layer_outer +
    layer_outer2 +
    layer_bottom +
    scale_color +
    scale_fill +
    xlim(x_lim) +
    labs(x = NULL, y = NULL)
  p
}
