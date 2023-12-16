library(readr)
library(dplyr)
library(tidyr)
library(qs)

PROJ_DIR <- '76-2023_ff_luck'
DATA_DIR <- file.path(PROJ_DIR, 'data')

## data ----
POSITION_MAP <- ffscrapr:::.espn_lineupslot_map()
player_scores <- qs::qread(file.path(DATA_DIR, 'player-scores-all.qs'))
team_scores <- readr::read_csv(file.path(DATA_DIR, 'team-scores-all.csv'))
SEASONS <- unique(scores$season)
CURRENT_SEASON <- max(SEASONS)

clean_team_scores <- team_scores |> 
  dplyr::filter(!is.na(result)) |> 
  dplyr::filter(season == CURRENT_SEASON) |> 
  dplyr::mutate(
    dplyr::across(
      c(
        user_name,
        opponent_user_name
      ),
      \(.x) 
      dplyr::case_when(
        .x == 'Andrew ElHabr' ~ 'Andrew E.',
        .x == 'Andrew Lara' ~ 'Andrew L.',
        .x == 'Manuel Espinosa' ~ 'Manny',
        .x == 'Juan Pineda' ~ 'JP',
        TRUE ~ gsub('\\s.*$', '', .x)
      )
    )
  )

filt_player_scores <- player_scores |> 
  dplyr::filter(season == CURRENT_SEASON) |> 
  filter(lineup_slot != 'IR') |> 
  dplyr::mutate(
    pos = ifelse(grepl(' TQB$', player_name), 'QB', pos)
  )

starter_scores <- filt_player_scores |> 
  dplyr::filter(lineup_slot != 'BE')

bench_scores <- filt_player_scores |> 
  dplyr::filter(lineup_slot == 'BE')

bench_potential_lineup_slots <- bench_scores |> 
  select(
    season,
    week,
    franchise_id,
    player_id,
    player_name,
    player_score,
    eligible_lineup_slots
  ) |> 
  tidyr::unnest_longer(eligible_lineup_slots) |> 
  mutate(
    lineup_slot = POSITION_MAP[as.character(eligible_lineup_slots)],
    .keep = 'unused'
  )

cusum_starter_scores <- starter_scores |> 
  select(
    season,
    week,
    franchise_id,
    player_score,
    lineup_slot
  ) |> 
  arrange(season, week, franchise_id, lineup_slot, player_score) |> 
  group_by(season, week, franchise_id, lineup_slot) |> 
  mutate(
    rn = row_number(player_score),
    rev_ecdf = cumsum(player_score)
  ) |> 
  ungroup()

cusum_bench_scores <- bench_scores |> 
  select(
    season,
    week,
    franchise_id,
    player_id,
    player_score
  ) |> 
  left_join(
    bench_potential_lineup_slots |> 
      select(
        season,
        week,
        franchise_id,
        player_id,
        player_score,
        lineup_slot
      ),
    by = join_by(season, week, franchise_id, player_id, player_score)
  ) |> 
  arrange(season, week, franchise_id, lineup_slot, desc(player_score)) |> 
  group_by(season, week, franchise_id, lineup_slot) |> 
  mutate(
    rn = row_number(desc(player_score)),
    ecdf = cumsum(player_score)
  ) |> 
  ungroup()



  inner_join(
    bench_potential_lineup_slots |> 
      select(
        season,
        week,
        franchise_id,
        bench_player_id = player_id,
        bench_player_name = player_name,
        bench_player_score = player_score,
        potential_lineup_slot
      ),
    by = join_by(
      season,
      week,
      franchise_id,
      starter_lineup_slot == potential_lineup_slot
    )
  )
