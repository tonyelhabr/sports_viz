library(ffscrapr)
library(ffsimulator)
library(nflreadr)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)

PROJ_DIR <- '85-ffsimulator_2024'
conn <- ffscrapr::espn_connect(
  season = 2024,
  league_id = 899513
)

n_seasons <- 100
n_weeks <- 14
best_ball <- FALSE
seed <- 42 # NULL
gp_model <- 'simple'
base_seasons <- 2018:2023
actual_schedule <- TRUE
replacement_level <- FALSE # non-default
pos_filter <- c('QB', 'RB', 'WR', 'TE', 'K')
custom_pos_filter <- c('QB', 'RB/WR/TE', 'K') # special
# verbose <- NULL
# return <- 'default'

# sim <- ffsimulator::ff_simulate(
#   conn = conn, 
#   actual_schedule = TRUE,
#   n_seasons = n_seasons, 
#   n_weeks = n_weeks, 
#   seed = seed
# )
# sim

if (!is.null(seed)) set.seed(seed)

scoring_history <- ffscrapr::ff_scoringhistory(conn, seasons = base_seasons)

init_latest_rankings <- ffsimulator::ffs_latest_rankings(type = 'draft')
latest_rankings <- init_latest_rankings |> 
  dplyr::mutate(
    player = nflreadr::clean_player_names(player, lowercase = TRUE)
  )

latest_rankings |> 
  dplyr::filter(pos == 'RB') |> 
  dplyr::arrange(ecr)

franchises <- ffsimulator::ffs_franchises(conn)

init_nflreadr_players <- nflreadr::load_players()
nflreadr_players <- init_nflreadr_players |> 
  dplyr::transmute(
    team = team_abbr,
    pos = position,
    player_name = nflreadr::clean_player_names(display_name, lowercase = TRUE)
  )
nflreadr_players |> 
  filter(player_name == 'cj stroud')

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
      TRUE ~ gsub('\\s.*$', '', user_name)
    ),
    .keep = 'unused'
  )

rosters <- init_rosters |> 
  dplyr::filter(!grepl(' tqb$', player_name)) |>
  dplyr::bind_rows(
    init_rosters |> 
      dplyr::filter(grepl(' tqb$', player_name)) |>
      dplyr::select(-c(player_id, player_name, fantasypros_id)) |> 
      dplyr::mutate(
        pos = 'QB'
      ) |> 
      dplyr::inner_join(
        latest_rankings |> 
          dplyr::filter(pos == 'QB') |> 
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
    fantasypros_id = dplyr::coalesce(fantasypros_id, fantasypros_id_fallback),
    .keep = 'unused'
  )
rosters |> 
  tibble::as_tibble() |> 
  dplyr::filter(is.na(fantasypros_id))

league_info <- ffscrapr::ff_league(conn)
init_lineup_constraints <- ffsimulator::ffs_starter_positions(conn)

lineup_constraints <- dplyr::bind_rows(
  init_lineup_constraints |> 
    dplyr::mutate(
      pos = ifelse(pos == 'TQB', 'QB', pos)
    ),
  tibble::tibble(
    pos = 'RB/WR/TE',
    min = 5,
    max = 5
  )
) |> 
  tidyr::fill(
    offense_starters, 
    defense_starters,
    kdst_starters, 
    total_starters
  )

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
)

# debugonce(ffsimulator::ffs_generate_projections)
projected_scores <- ffsimulator::ffs_generate_projections(
  adp_outcomes = adp_outcomes,
  latest_rankings = latest_rankings,
  n_seasons = n_seasons,
  weeks = 1:n_weeks, 
  rosters = rosters_rl 
)

projected_scores_one_season_week <- projected_scores |> 
  tibble::as_tibble() |>
  dplyr::filter(season == 1) |> 
  dplyr::filter(week == 1)

projected_scores_one_season_week |> dplyr::filter(pos == 'QB')
projected_scores_one_season_week |> dplyr::filter(pos == 'K')
projected_scores_one_season_week |> dplyr::filter(pos == 'RB')
projected_scores_one_season_week |> dplyr::filter(pos == 'WR')

roster_scores <- ffsimulator::ffs_score_rosters(
  projected_scores = projected_scores |> 
    dplyr::mutate(
      pos = ifelse(pos %in% c('RB', 'WR', 'TE'), 'RB/WR/TE', pos)
    ),
  rosters = rosters_rl |> 
    dplyr::mutate(
      pos = ifelse(pos %in% c('RB', 'WR', 'TE'), 'RB/WR/TE', pos)
    )
)

roster_scores_one_season_week <- roster_scores |> 
  tibble::as_tibble() |>
  dplyr::filter(season == 1) |> 
  dplyr::filter(week == 1)

roster_scores_one_season_week |> dplyr::filter(pos == 'QB')
roster_scores_one_season_week |> dplyr::filter(pos == 'K')
roster_scores_one_season_week |> dplyr::filter(pos == 'RB/WR/TE')
roster_scores_one_season_week |> 
  dplyr::filter(franchise_id == '8') |> 
  dplyr::select(
    team,
    pos,
    player_id, 
    player_name,
    ecr,
    projection, 
    gp_model, 
    projected_score,
    pos_rank
  ) |> 
  dplyr::arrange(dplyr::desc(projection))

# debugonce(ffsimulator::ffs_optimise_lineups)
optimal_scores <- ffsimulator::ffs_optimise_lineups(
  roster_scores = roster_scores,
  lineup_constraints = lineup_constraints,
  pos_filter = custom_pos_filter
)

optimal_scores_one_season_week <- optimal_scores |> 
  tibble::as_tibble() |>
  dplyr::filter(season == 1) |> 
  dplyr::filter(week == 1)

optimal_scores_one_season_week |> 
  dplyr::filter(
    franchise_id == '8'
  ) |> 
  tidyr::unnest_longer(c(optimal_player_id, optimal_player_score))

if (actual_schedule) {
  schedule <- ffsimulator::ffs_schedule(conn)
  schedules <- ffsimulator::ffs_repeat_schedules(
    n_seasons = n_seasons,
    actual_schedule = schedule
  )
}

# if (!actual_schedule) {
#   schedules <- ffsimulator::ffs_build_schedules(
#     n_seasons = n_seasons,
#     n_weeks = n_weeks
#     franchises = franchises
#   )
# }

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
  class = "ff_simulation"
)
qs::qsave(sim, file.path(PROJ_DIR, 'sim.qs'))
