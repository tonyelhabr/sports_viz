library(ffscrapr)
library(ffsimulator)
library(nflreadr)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)

PROJ_DIR <- '85-ffsimulator_2024/sleeper'
conn <- ffscrapr::sleeper_connect(
  season = 2024,
  league_id = '1119666990190260224'
)


n_seasons <- 500
n_weeks <- 14
best_ball <- FALSE
seed <- 42 # NULL
gp_model <- 'none' # 'simple' # use 'none' to reduce noise with injury guessing
base_seasons <- 2018:2023
actual_schedule <- TRUE
replacement_level <- FALSE # non-default
pos_filter <- c('QB', 'RB', 'WR', 'TE', 'K')

if (!is.null(seed)) set.seed(seed)

init_scoring_history <- ffscrapr::ff_scoringhistory(conn, seasons = base_seasons)

scoring_history <- init_scoring_history |>
  dplyr::filter(pos %in% pos_filter) |> 
  dplyr::mutate(
    player_name = nflreadr::clean_player_names(player_name, lowercase = TRUE)
  )

init_latest_rankings <- ffsimulator::ffs_latest_rankings(type = 'draft')
latest_rankings <- init_latest_rankings |>
  dplyr::filter(pos %in% pos_filter) |> 
  dplyr::mutate(
    player = nflreadr::clean_player_names(player, lowercase = TRUE)
  )

latest_rankings_no_sleeper_id <- latest_rankings |> 
  dplyr::inner_join(
    ffscrapr::dp_playerids() |> 
      dplyr::filter(is.na(sleeper_id)) |> 
      dplyr::select(fantasypros_id),
    by = dplyr::join_by(fantasypros_id)
  )

latest_rankings_redux <- latest_rankings |> 
  dplyr::anti_join(
    latest_rankings_no_sleeper_id |> 
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
    team = team_abbr,
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
    franchise_name = user_name,
    .keep = 'unused'
  )

rosters <- init_rosters |> 
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
    fantasypros_id = dplyr::coalesce(fantasypros_id, fantasypros_id_fallback, player_name),
    .keep = 'unused'
  )
rosters |> 
  dplyr::filter(pos != 'DST') |> 
  dplyr::filter(is.na(fantasypros_id))

league_info <- ffscrapr::ff_league(conn)
init_lineup_constraints <- ffsimulator::ffs_starter_positions(conn)

lineup_constraints <- init_lineup_constraints |> 
  dplyr::filter(pos != 'DEF')

if(!replacement_level) rosters_rl <- rosters
# if (replacement_level) {
#   rosters_rl <- ffs_add_replacement_level(
#     rosters = rosters,
#     latest_rankings = latest_rankings,
#     franchises = franchises,
#     lineup_constraints = lineup_constraints,
#     pos_filter = pos_filter
#   )
# }

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
  rosters = rosters_rl 
) |> 
  tibble::as_tibble()

roster_scores <- ffsimulator::ffs_score_rosters(
  projected_scores = projected_scores,
  rosters = rosters_rl
) |> 
  tibble::as_tibble()

optimal_scores <- ffsimulator::ffs_optimise_lineups(
  roster_scores = roster_scores,
  lineup_constraints = lineup_constraints,
  pos_filter = pos_filter
) |> 
  tibble::as_tibble()

if (actual_schedule) {
  schedule <- ffsimulator::ffs_schedule(conn)
  schedules <- ffsimulator::ffs_repeat_schedules(
    n_seasons = n_seasons,
    actual_schedule = schedule
  )
}

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
      best_ball = best_ball,
      seed = seed,
      gp_model = gp_model,
      actual_schedule = actual_schedule,
      base_seasons = list(base_seasons),
      pos_filter = list(pos_filter)
    )
  ),
  class = 'ff_simulation'
)
qs::qsave(sim, file.path(PROJ_DIR, 'sim.qs'))
