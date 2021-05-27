
library(tidyverse)
dir_proj <- '999-dev'
path_stats <- file.path(dir_proj, 'stats.rds')
overwrite <- FALSE
if(!file.exists(path_stats) & overwrite) {
  
  # require(httr)
  # require(jsonlite)
  # Reference: https://github.com/American-Soccer-Analysis/asa-shiny-app/blob/master/global.R
  STAGE <- ifelse(grepl("stage", getwd()), "stage/", "")
  API_PATH <- paste0("https://app.americansocceranalysis.com/", STAGE, "api/v1")
  api_request <- function(path = API_PATH, endpoint, parameters = NULL) {
    parameters_array <- c()
    
    if (length(parameters) > 0) {
      for (i in 1:length(parameters)) {
        tmp_name <- names(parameters[i])
        tmp_value <- parameters[[tmp_name]]
        
        if (all(!is.na(tmp_value)) & all(!is.null(tmp_value))) {
          if (length(tmp_value) > 1) {
            tmp_value <- gsub("\\s+", "%20", paste0(tmp_value, collapse = ","))
          } else {
            tmp_value <- gsub("\\s+", "%20", tmp_value)
          }
          
          parameters_array <- c(parameters_array, paste0(tmp_name, "=", tmp_value))
        }
      }
    }
    
    parameters_array <- ifelse(length(parameters_array) > 0,
                               paste0("?", paste0(parameters_array, collapse = "&")),
                               "")
    browser()
    return(jsonlite::fromJSON(httr::content(httr::GET(paste0(path, endpoint, parameters_array)),
                                      as = "text", encoding = "UTF-8")))
  }

  assemble_endpoint <- function(league, route_prefix = NA, subheader) {
    return(paste0("/", league, "/", tolower(subheader), ifelse(!is.na(route_prefix), paste0("/", route_prefix), "")))
  }
  
  # more ----
  devtools::source_url('https://raw.githubusercontent.com/American-Soccer-Analysis/asa-shiny-app/9a351e745289bb05ca6744934b9d3a6ca7a3ca57/config/leagues.R')
  league_schemas <- names(league_config)
  devtools::source_url('https://raw.githubusercontent.com/American-Soccer-Analysis/asa-shiny-app/9a351e745289bb05ca6744934b9d3a6ca7a3ca57/utils/retrieve_data.R')
  key <- 'mls'
  tms <-
    all_teams[[key]] %>%
    as_tibble()
  tms
  
  gms <-
    all_games[[key]] %>%
    as_tibble()
  gms
  
  # players <-
  #   all_players[[key]] %>% 
  #   as_tibble()
  # players
  
  # debugonce(api_request)
  # seasons <- all_seasons[[key]]
  seasons <- seq.int(2013, 2020)
  seasons
  # stats <-
  #   tibble(
  #     lg = key,
  #     stat = c('xgoals', 'xpass', 'goals-added')
  #   ) %>% 
  #   mutate(
  #     data = 
  #       pmap(
  #         list(lg, stat),
  #         ~api_request(
  #           endpoint = assemble_endpoint(..1, ..2, 'players'),
  #           parameters = list(
  #             season_name = seasons
  #           )
  #         )
  #       )
  #   )
  # stats
  api_request(
    endpoint = assemble_endpoint('mls', 'xgoals', 'games'),
    parameters = list(
      season_name = 2020
    )
  )
  
  stats <-
    tibble(
      lg = key,
      # only xg by match
      stat = c('xgoals')
    ) %>% 
    mutate(
      data = 
        pmap(
          list(lg, stat),
          ~api_request(
            endpoint = assemble_endpoint(..1, ..2, 'games'),
            parameters = list(
              season_name = seasons
            )
          ) %>% 
            as_tibble()
        )
    ) %>% 
    unnest(data) %>% 
    left_join(tms %>% rename_all(~sprintf('away_%s', .x))) %>% 
    left_join(tms %>% rename_all(~sprintf('home_%s', .x))) %>% 
    # Only need to join on `game_id`
    left_join(gms %>% select(-c(date_time_utc, home_team_id, away_team_id)))
  stats
  write_rds(stats, path_stats)
} else {
  stats <- path_stats %>% read_rds()
}

stats_slim <-
  stats %>% 
  mutate(
    date = date_time_utc %>% lubridate::ymd_hms() %>% lubridate::date()
  ) %>%
  select(
    game_id,
    date,
    team_h = home_team_name,
    team_a = away_team_name,
    g_h = home_goals,
    g_a = away_goals,
    xg_h = home_team_xgoals,
    xg_a = away_team_xgoals
  )
stats_slim

.f_rename <- function(side) {
  suffix <- sprintf('_%s$', side)
  stats_slim %>% 
    select(game_id, date, matches(suffix)) %>% 
    rename_all(~str_remove(.x, suffix)) %>% 
    mutate(side = !!side)
}

df <-
  bind_rows(
    .f_rename('h'),
    .f_rename('a')
  ) %>% 
  group_by(team) %>% 
  mutate(idx = row_number(date)) %>% 
  ungroup()
df

df_window <-
  df %>% 
  group_by(team) %>% 
  arrange(idx, .by_group = TRUE) %>% 
  mutate(xg_window = xg %>% slider::slide_vec(mean, .before = 10, .complete = TRUE)) %>% 
  ungroup() %>% 
  arrange(date)

df_window %>% 
  filter(team == 'Toronto FC') %>% 
  ggplot() +
  aes(x = idx, y = g) +
  geom_line(
    aes(y = xg),
    color = 'red'
  ) +
  geom_line(
    aes(y = xg_window),
    color = 'blue'
  )

df %>% 
  count(team, sort = TRUE)
df

df_wide <-
  df %>% 
  group_by(team) %>% 
  mutate(idx = row_number(date) %>% sprintf('x%03d', .)) %>% 
  ungroup() %>%
  select(team, idx, xg) %>% 
  arrange(idx) %>% 
  pivot_wider(
    names_from = idx,
    values_from = xg
  ) %>% 
  arrange(team)
df_wide

mat <- df_wide %>% select(-team) %>% as.matrix()
mat

alpha_q <- quietly(psych::alpha)
alpha <- mat %>% alpha_q()
alpha$result$alpha.drop %>% as_tibble() %>% mutate(idx = row_number()) %>% ggplot() + aes(x = idx, y = raw_alpha) + geom_point()

