library(ffscrapr)
library(ffsimulator)
library(nflreadr)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)

PROJ_DIR <- '95-ffsimulator_2025'
source(file.path(PROJ_DIR, 'helpers.R'))
TODAY <- Sys.Date()
SEASON <- 2025
conn <- ffscrapr::espn_connect(
  season = SEASON,
  league_id = 899513
)

class(conn) <- c("tony_espn_conn", "espn_conn")

# params
n_seasons <- 500
n_weeks <- 14
seed <- 42 # NULL
gp_model <- 'none' # 'simple' # use 'none' to reduce noise with injury guessing
base_seasons <- 2018:2024
actual_schedule <- TRUE
pos_filter <- c('QB', 'RB', 'WR', 'TE') # , 'K')

set.seed(seed)

## Notable players missing ESPN IDs
ffscrapr::dp_playerids() |> 
  dplyr::filter(is.na(espn_id)) |> 
  dplyr::transmute(
    player_name = nflreadr::clean_player_names(name, lowercase = TRUE),
    pos = position,
    team,
    draft_year,
    draft_round,
    draft_pick
  ) |> 
  dplyr::filter(
    draft_year >= 2020,
    draft_round <= 2,
    pos %in% pos_filter
  ) |> 
  dplyr::arrange(draft_pick)

# init_scoring_history <- ffscrapr::ff_scoringhistory(conn, base_seasons)
init_scoring_history <- ff_scoringhistory(conn, base_seasons)

scoring_history <- init_scoring_history |>
  dplyr::filter(pos %in% pos_filter) |> 
  dplyr::mutate(
    player_name = nflreadr::clean_player_names(player_name, lowercase = TRUE)
  )
# scoring_history |> 
#   filter(player_name == 'saquon barkley', season == 2024) |> 
#   select(week, points, rushing_yards, rushing_tds, rushing_first_downs, receiving_yards, receiving_tds, receiving_first_downs)
# scoring_history |> 
#   filter(player_name == 'justin jefferson', season == 2024) |> 
#   select(week, points, rushing_yards, rushing_tds, rushing_first_downs, receiving_yards, receiving_tds, receiving_first_downs)
# scoring_history |> 
#   filter(player_name == 'josh allen', season == 2024) |> 
#   select(week, points, rushing_yards, rushing_tds, rushing_first_downs, passing_yards, passing_tds)
scoring_history |> 
  filter(player_name == 'brock bowers', season == 2024) |> 
  select(week, points, rushing_yards, rushing_tds, rushing_first_downs, passing_yards, passing_tds)
init_latest_rankings <- ffsimulator::ffs_latest_rankings(type = 'draft')
latest_rankings <- init_latest_rankings |>
  dplyr::filter(pos %in% pos_filter) |> 
  dplyr::mutate(
    player = nflreadr::clean_player_names(player, lowercase = TRUE)
  )

latest_rankings_no_espn_id <- latest_rankings |> 
  dplyr::inner_join(
    ffscrapr::dp_playerids() |> 
      dplyr::filter(is.na(espn_id)) |> 
      dplyr::select(fantasypros_id),
    by = dplyr::join_by(fantasypros_id)
  )

latest_rankings_redux <- latest_rankings |> 
  dplyr::anti_join(
    latest_rankings_no_espn_id |> 
      dplyr::select(fantasypros_id),
    by = dplyr::join_by(fantasypros_id)
  ) |> 
  dplyr::bind_rows(
    latest_rankings_no_espn_id |> 
      dplyr::mutate(fantasypros_id = player)
  ) |> 
  dplyr::arrange(pos, ecr)

latest_rankings_redux |> 
  dplyr::filter(pos == 'RB') |> 
  dplyr::arrange(ecr)

franchises <- ffsimulator::ffs_franchises(conn)

init_nflreadr_players <- nflreadr::load_players()
nflreadr_players <- init_nflreadr_players |> 
  dplyr::transmute(
    team = latest_team,
    pos = position,
    gsis_id,
    player_name = nflreadr::clean_player_names(display_name, lowercase = TRUE)
  )
# nflreadr_players |> dplyr::filter(player_name == 'cj stroud')

init_ff_ids <- nflreadr::load_ff_playerids()
ff_ids <- init_ff_ids |> 
  dplyr::transmute(
    team,
    pos = gsub('^PK$', 'K', position),
    player_name = nflreadr::clean_player_names(name, lowercase = TRUE),
    espn_id,
    fantasypros_id
  ) |> 
  dplyr::filter(!is.na(espn_id), !is.na(fantasypros_id))

init_raw_rosters <- ffsimulator::ffs_rosters(conn)

init_rosters <- init_raw_rosters |> 
  tibble::as_tibble() |> 
  dplyr::mutate(
    player_name = nflreadr::clean_player_names(player_name, lowercase = TRUE),
    team = gsub('OAK', 'LVR', team)
  ) |> 
  dplyr::left_join(
    franchises |> dplyr::select(franchise_id, user_name),
    by = dplyr::join_by(franchise_id)
  ) |> 
  dplyr::mutate(
    franchise_name = dplyr::case_when(
      user_name == 'Andrew ElHabr' ~ 'Andrew E.',
      user_name == 'Andrew Lara' ~ 'Andrew L.',
      user_name == 'Manuel Espinosa' ~ 'Manny',
      user_name == 'Juan Avalos' ~ 'ChadGPT',
      user_name == 'Javier Motta' ~ 'Javi',
      TRUE ~ gsub('\\s.*$', '', user_name)
    ),
    .keep = 'unused'
  )

rosters <- init_rosters |> 
  tibble::as_tibble() |> 
  dplyr::filter(!grepl(' tqb$', player_name)) |>
  dplyr::bind_rows(
    init_rosters |> 
      dplyr::filter(grepl(' tqb$', player_name)) |>
      dplyr::select(-c(player_id, player_name, fantasypros_id)) |> 
      dplyr::mutate(
        pos = 'QB'
      ) |> 
      dplyr::inner_join(
        latest_rankings_redux |> 
          dplyr::filter(pos == 'QB', team != 'FA') |> 
          dplyr::group_by(team) |> 
          dplyr::slice_min(ecr, n = 1, with_ties = FALSE) |> 
          dplyr::ungroup() |> 
          dplyr:: select(player_name = player, team, fantasypros_id),
        by = dplyr::join_by(team)
      )
  ) |> 
  dplyr::left_join(
    ff_ids |> 
      dplyr::select(
        player_name, 
        pos, 
        team,
        player_id_fallback = espn_id,
        fantasypros_id_fallback = fantasypros_id
      ),
    by = dplyr::join_by(player_name, team, pos)
  ) |> 
  dplyr::mutate(
    player_id = dplyr::coalesce(player_id, player_id_fallback),
    # fantasypros_id will be NA for dst's and unmapped players 
    fantasypros_id = dplyr::coalesce(fantasypros_id, fantasypros_id_fallback),
    # .keep = 'unused'
  )

rosters |> 
  filter(pos == 'QB')
rosters |> 
  filter(player_name |> str_detect('davante adams'))

league_info <- ffscrapr::ff_league(conn)
# Doesn't return enough because it drops min == 0
# init_lineup_constraints <- ffsimulator::ffs_starter_positions(conn)
# Check to see what raw ESPN endpoint returns
# https://github.com/ffverse/ffscrapr/blob/main/R/espn_starterpositions.R
# l_s_raw <- espn_getendpoint(conn, view = "mSettings")
# l_s <- l_s_raw |> 
#   purrr::pluck("content", "settings", "rosterSettings", "lineupSlotCounts") %>%
#   tibble::enframe(name = "lineup_id", value = "count") %>%
#   dplyr::mutate(pos = ffscrapr:::.espn_lineupslot_map()[as.character(.data$lineup_id)]) %>%
#   tidyr::unnest(c("pos", "count")) |> 
#   dplyr::mutate(
#     min = ifelse(.data$pos %in% c(
#       "QB", "RB", "WR", "TE",
#       "TQB", "DT", "DE", "ER",
#       "LB", "CB", "S",
#       "K", "P", "DST", "HC"
#     ), .data$count, NA_integer_),
#     offense_starters = sum(.data$count * stringr::str_detect(.data$pos, "QB|RB|WR|TE|OP"), na.rm = TRUE),
#     defense_starters = sum(.data$count * stringr::str_detect(.data$pos, "DE|DT|DL|LB|CB|^S$|ER|DP"), na.rm = TRUE),
#     kdst_starters = sum(.data$count * .data$pos %in% c("K", "P", "DST", "HC"), na.rm = TRUE),
#     total_starters = .data$offense_starters + .data$defense_starters + .data$kdst_starters
#   )
# rbwr <- l_s$count[l_s$pos == "RB/WR"]
# wrte <- l_s$count[l_s$pos == "WR/TE"]
# rbwrte <- l_s$count[l_s$pos == "RB/WR/TE"]
# op <- l_s$count[l_s$pos == "OP"]
# dp <- l_s$count[l_s$pos == "DP"]
# dl <- l_s$count[l_s$pos == "DL"]
# db <- l_s$count[l_s$pos == "DB"]
# l_s %>%
#   dplyr::mutate(
#     max = dplyr::case_when(
#       .data$pos == "QB" ~ .data$min + op,
#       .data$pos == "RB" ~ .data$min + rbwr + rbwrte + op,
#       .data$pos == "WR" ~ .data$min + rbwr + wrte + rbwrte + op,
#       .data$pos == "TE" ~ .data$min + wrte + rbwrte + op,
#       .data$pos == "DT" ~ .data$min + dl + dp,
#       .data$pos == "DE" ~ .data$min + dl + dp,
#       .data$pos == "CB" ~ .data$min + db + dp,
#       .data$pos == "S" ~ .data$min + db + dp,
#       TRUE ~ .data$min
#     ),
#   ) %>%
#   dplyr::filter(!is.na(.data$min)) %>%
#   dplyr::select(
#     "pos",
#     "min",
#     "max",
#     dplyr::contains("_starters")
#   )
# init_lineup_constraints

lineup_constraints <- tibble::tibble(
  pos = c('QB', 'RB', 'WR', 'TE', 'K', 'DST', 'DT', 'DE', 'LB', 'CB', 'S'),
  min = c(1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0),
  max = c(1, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1),
  offense_starters = rep(5, 11),
  defense_starters = rep(1, 11),
  kdst_starters = rep(2, 11),
  total_starters = rep(8, 11)
)

adp_outcomes <- ffsimulator::ffs_adp_outcomes(
  scoring_history = scoring_history,
  gp_model = gp_model,
  pos_filter = pos_filter
) |> 
  tibble::as_tibble()

# debugonce(ffsimulator::ffs_generate_projections)
projected_scores <- ffsimulator::ffs_generate_projections(
  adp_outcomes = adp_outcomes,
  latest_rankings = latest_rankings_redux,
  n_seasons = n_seasons,
  weeks = 1:n_weeks, 
  rosters = rosters 
) |> 
  tibble::as_tibble()

roster_scores <- ffsimulator::ffs_score_rosters(
  projected_scores = projected_scores,
  rosters = rosters
) |> 
  tibble::as_tibble()

optimal_scores <- ffsimulator::ffs_optimise_lineups(
  roster_scores = roster_scores,
  lineup_constraints = lineup_constraints,
  pos_filter = pos_filter
) |> 
  tibble::as_tibble()
optimal_scores |> filter(franchise_name == 'Tony', season == 1, week == 1) |> unnest(optimal_player_id, optimal_player_score)

schedule <- ffsimulator::ffs_schedule(conn)
schedules <- ffsimulator::ffs_repeat_schedules(
  n_seasons = n_seasons,
  actual_schedule = schedule
)

summary_week <- ffsimulator::ffs_summarise_week(optimal_scores, schedules)
summary_season <- ffsimulator::ffs_summarise_season(summary_week)
summary_simulation <- ffsimulator::ffs_summarise_simulation(summary_season)

sim <- structure(
  list(
    summary_simulation = summary_simulation,
    summary_season = summary_season,
    summary_week = summary_week,
    roster_scores = roster_scores,
    projected_scores = projected_scores,
    league_info = league_info,
    simulation_params = list(
      n_seasons = n_seasons,
      n_weeks = n_weeks,
      scrape_date = latest_rankings$scrape_date[[1]],
      best_ball = FALSE,
      seed = seed,
      gp_model = gp_model,
      actual_schedule = actual_schedule,
      base_seasons = list(base_seasons),
      pos_filter = list(pos_filter)
    )
  ),
  class = 'ff_simulation'
)
qs::qsave(sim, file.path(PROJ_DIR, sprintf('sim-%s-%s.qs', SEASON, TODAY)))
