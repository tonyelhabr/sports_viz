library(qs)
library(dplyr)
library(tidyr)

PROJ_DIR <- '76-2023_ff_luck'
DATA_DIR <- file.path(PROJ_DIR, 'data')

## data ----
POSITION_MAP <- ffscrapr:::.espn_lineupslot_map()
franchises <- readr::read_csv(file.path(DATA_DIR, 'franchises-all.csv'))
weekly_player_scores <- qs::qread(file.path(DATA_DIR, 'player-scores-all.qs'))
SEASONS <- unique(weekly_player_scores$season)
CURRENT_SEASON <- max(SEASONS)

team_mapping <- franchises |> 
  dplyr::distinct(
    season,
    franchise_id,
    user_name
  )

filt_weekly_player_scores <- weekly_player_scores |> 
  dplyr::filter(season == CURRENT_SEASON) |> 
  dplyr::filter(lineup_slot != 'IR') |> 
  dplyr::mutate(
    pos = ifelse(grepl(' TQB$', user_name), 'QB', pos)
  )

starter_scores <- filt_weekly_player_scores |> 
  dplyr::filter(lineup_slot != 'BE')

bench_scores <- filt_weekly_player_scores |> 
  dplyr::filter(lineup_slot == 'BE')

bench_potential_lineup_slots <- bench_scores |> 
  dplyr::select(
    season,
    week,
    franchise_id,
    player_id,
    eligible_lineup_slots
  ) |> 
  tidyr::unnest_longer(eligible_lineup_slots) |> 
  dplyr::mutate(
    lineup_slot = POSITION_MAP[as.character(eligible_lineup_slots)],
    .keep = 'unused'
  )

cumu_starter_scores <- starter_scores |> 
  dplyr::select(
    season,
    week,
    franchise_id,
    player_id,
    player_score,
    lineup_slot
  ) |> 
  dplyr::arrange(season, week, franchise_id, lineup_slot, player_score) |> 
  dplyr::group_by(season, week, franchise_id, lineup_slot) |> 
  dplyr::mutate(
    rn = dplyr::row_number(player_score),
    cumu_score = cumsum(player_score)
  ) |> 
  dplyr::ungroup()

cumu_bench_scores <- bench_scores |> 
  dplyr::select(
    season,
    week,
    franchise_id,
    player_id,
    player_score
  ) |> 
  dplyr::left_join(
    bench_potential_lineup_slots |> 
      dplyr::select(
        season,
        week,
        franchise_id,
        player_id,
        lineup_slot
      ),
    by = dplyr::join_by(season, week, franchise_id, player_id)
  ) |> 
  # filter(!(lineup_slot %in% c('BE', 'IR'))) |> 
  dplyr::filter(lineup_slot %in% unique(cumu_starter_scores$lineup_slot)) |> 
  dplyr::arrange(season, week, franchise_id, lineup_slot, dplyr::desc(player_score)) |> 
  dplyr::group_by(season, week, franchise_id, lineup_slot) |> 
  dplyr::mutate(
    rn = dplyr::row_number(dplyr::desc(player_score)),
    cumu_score = cumsum(player_score)
  ) |> 
  dplyr::ungroup()

cumu_scores <- cumu_starter_scores |> 
  dplyr::select(
    season,
    week,
    franchise_id,
    lineup_slot,
    starter_rn = rn,
    starter_player_id = player_id,
    starter_player_score = player_score,
    starter_cumu_score = cumu_score
  ) |> 
  dplyr::inner_join(
    cumu_bench_scores |> 
      dplyr::select(
        season,
        week,
        franchise_id,
        lineup_slot,
        bench_rn = rn,
        bench_player_id = player_id,
        bench_player_score = player_score,
        bench_cumu_score = cumu_score
      ),
    by = dplyr::join_by(
      season,
      week,
      franchise_id,
      lineup_slot
    ),
    relationship = 'many-to-many'
  )


cumu_scores |> 
  dplyr::filter(
    bench_cumu_score > starter_cumu_score,
    bench_player_score > starter_player_score
  ) |> 
  filter(
    season == 2023,
    week == 2,
    franchise_id == 5
  ) |> 
  dplyr::group_by(
    season,
    week,
    franchise_id,
    lineup_slot,
    starter_rn
  ) |> 
  dplyr::slice_max(
    bench_rn,
    n = 1,
    with_ties = FALSE
  ) |> 
  dplyr::ungroup() |> 
  dplyr::group_by(
    season,
    week,
    franchise_id,
    lineup_slot,
    bench_rn
  ) |> 
  dplyr::slice_min(
    starter_rn,
    n = 1,
    with_ties = FALSE
  ) |> 
  dplyr::ungroup()

weekly_replacements <- cumu_scores |> 
  dplyr::filter(
    bench_cumu_score > starter_cumu_score,
    bench_player_score > starter_player_score
  ) |> 
  filter(
    season == 2023,
    week == 2,
    franchise_id == 5
  ) |> 
  dplyr::group_by(
    season,
    week,
    franchise_id,
    lineup_slot,
    starter_rn
  ) |> 
  dplyr::slice_max(
    bench_rn,
    n = 1,
    with_ties = FALSE
  ) |> 
  dplyr::ungroup()
weekly_replacements |> 
  filter(
    season == 2023,
    week == 2,
    franchise_id == 5
  )

agg_weekly_replacements <- weekly_replacements |> 
  dplyr::group_by(
    season,
    week,
    franchise_id
  ) |> 
  dplyr::summarize(
    n_optimal_replacements = dplyr::n(),
    score_improvement = sum(bench_player_score - starter_player_score)
  ) |> 
  dplyr::ungroup()

agg_season_replacements <- agg_weekly_replacements |> 
  dplyr::group_by(season, franchise_id) |> 
  dplyr::summarize(
    dplyr::across(
      c(
        n_optimal_replacements,
        score_improvement
      ),
      \(.x) sum(.x)
    )
  ) |> 
  dplyr::ungroup() |> 
  dplyr::left_join(
    team_mapping,
    by = dplyr::join_by(season, franchise_id)
  ) |> 
  dplyr::arrange(dplyr::desc(score_improvement)) |> 
  dplyr::select(
    season,
    user_name,
    n_optimal_replacements,
    score_improvement
  )

## worst managed weeks
weekly_replacements |> 
  dplyr::left_join(
    team_mapping,
    by = dplyr::join_by(season, franchise_id)
  ) |> 
  dplyr::arrange(dplyr::desc(score_improvement)) |> 
  dplyr::select(
    season,
    week,
    user_name,
    franchise_id,
    n_optimal_replacements,
    score_improvement
  )
