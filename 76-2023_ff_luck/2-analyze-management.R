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

nested_cumu_scores <- dplyr::left_join(
  cumu_starter_scores |> 
    dplyr::select(
      season,
      week,
      franchise_id,
      lineup_slot,
      player_id,
      player_score
    ) |> 
    tidyr::nest(
      starters = c(player_id, player_score)
    ) |> 
    dplyr::mutate(
      starters = purrr::map(starters, tibble::deframe)
    ),
  cumu_bench_scores |> 
    dplyr::select(
      season,
      week,
      franchise_id,
      lineup_slot,
      player_id,
      player_score
    ) |> 
    tidyr::nest(
      bench = c(player_id, player_score)
    ) |> 
    dplyr::mutate(
      bench = purrr::map(bench, tibble::deframe)
    ),
  by = dplyr::join_by(
    season,
    week,
    franchise_id,
    lineup_slot
  )
)

swap_bench_and_starters <- function(starters, bench) {
  if (is.null(bench)) {
    return(starters)
  }

  new_starters <- vector('double', length = length(starters))
  nms <- vector('character', length = length(starters))
  for (i in seq_along(starters)) {
    # Find bench values greater than the current starter
    eligible_bench <- bench[bench > starters[i]]
    
    if (length(eligible_bench) > 0) {
      # Find the maximum eligible bench value
      max_bench_value <- max(eligible_bench)
      max_bench_name <- names(eligible_bench[which.max(eligible_bench)])
      
      # Swap the values
      new_starters[i] <- max_bench_value
      nms[i] <- max_bench_name
      
      # Remove the used bench value
      bench[bench == max_bench_value] <- -Inf
    } else {
      new_starters[i] <- starters[i]
      nms[i] <- names(starters[i])
    }
  }

  stats::setNames(new_starters, nms)
}

replacement_scores <- nested_cumu_scores |> 
  dplyr::mutate(
    best = purrr::map2(
      starters,
      bench,
      swap_bench_and_starters
    )
  ) |> 
  dplyr::select(
    season,
    week,
    franchise_id,
    lineup_slot,
    starters,
    best
  ) |> 
  dplyr::mutate(
    starter_player_id = purrr::map(starters, \(.x) names(.x)),
    starter_player_score = purrr::map(starters, \(.x) unname(.x)),
    best_player_id = purrr::map(best, \(.x) names(.x)),
    best_player_score = purrr::map(best, \(.x) unname(.x)),
    .keep = 'unused'
  ) |> 
  tidyr::unnest(
    c(
      starter_player_id, 
      starter_player_score, 
      best_player_id, 
      best_player_score
    )
  ) |> 
  dplyr::mutate(
    dplyr::across(c(starter_player_id, best_player_id), as.integer),
    is_replacement = starter_player_id != best_player_id,
    score_improvement = best_player_score - starter_player_score
  )

agg_weekly_replacements <- replacement_scores |> 
  dplyr::group_by(
    season,
    week,
    franchise_id
  ) |> 
  dplyr::summarize(
    n_replacements = sum(is_replacement),
    score_improvement = sum(score_improvement)
  ) |> 
  dplyr::ungroup()

agg_season_replacements <- agg_weekly_replacements |> 
  dplyr::group_by(season, franchise_id) |> 
  dplyr::summarize(
    n_games = dplyr::n_distinct(week),
    dplyr::across(
      c(
        n_replacements,
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
  dplyr::transmute(
    season,
    user_name,
    n_replacements,
    score_improvement,
    dplyr::across(
      c(
        n_replacements,
        score_improvement
      ),
      list(
        avg = \(.x) .x / n_games
      ),
      .names = 'avg_{.col}'
    )
  )

MAX_WEEK <- agg_weekly_replacements |> 
  filter(season == CURRENT_SEASON) |> 
  pull(week) |> 
  max()

tb_mismanagement_underperformance <- agg_season_replacements |> 
  inner_join(
    franchises |> select(season, user_name, logo)
  ) |> 
  select(
    logo,
    user_name,
    # avg_n_replacements,
    avg_score_improvement
  ) |> 
  gt::gt() |> 
  gt::cols_label(
    logo = ' ',
    user_name = 'Player',
    # avg_n_replacements = gt::html('Avg. # of players<br/>who should have been<br/>swapped with bench<br/>player'),
    avg_score_improvement = gt::html('Avg. score improvement<br/>with optimal lineup choice')
  ) |> 
  gtExtras::gt_theme_538() |> 
  gt::fmt_image(
    columns = 'logo'
  ) |> 
  gt::fmt_number(
    columns = starts_with('avg'),
    decimals = 1
  ) |> 
  gt::tab_header(
    title = gt::md('Who mismanaged their roster the most?'),
    subtitle = gt::md(
      sprintf(
        'Through week %s, %s season', 
        MAX_WEEK, 
        CURRENT_SEASON
      )
    )
  )

gt::gtsave(
  tb_mismanagement_underperformance,
  filename = file.path(PLOTS_DIR, sprintf('%s-mismanagement-underperformance.png', format(Sys.Date(), '%Y-%m-%d'))),
  zoom = 1.5
)

## worst managed weeks
agg_weekly_replacements |> 
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
    n_replacements,
    score_improvement
  )

