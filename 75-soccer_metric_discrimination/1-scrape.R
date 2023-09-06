library(worldfootballR)
library(dplyr)
library(lubridate)
library(purrr)

load_fb_advanced_match_stats <- function(country, gender, tier, stat_type, team_or_player, season_end_year = NA) {
  
  urls <- sprintf(
    "https://github.com/JaseZiv/worldfootballR_data/releases/download/fb_advanced_match_stats/%s_%s_%s_%s_%s_advanced_match_stats.rds",
    country,
    gender,
    tier,
    stat_type,
    team_or_player
  )
  
  purrr::map_dfr(urls, \(.x) readRDS(url(.x)))
}

raw_player_match_stats <- load_fb_advanced_match_stats(
  country = "ENG",
  gender = "M",
  tier = "1st",
  stat_type = "summary",
  team_or_player = "player"
)

player_match_stats <- raw_player_match_stats |> 
  dplyr::transmute(
    season = sprintf('%s/%s', Season_End_Year - 1, substr(Season_End_Year, 3, 4)),
    date = lubridate::ymd(Match_Date),
    match_id = basename(dirname(MatchURL)),
    team = Team,
    player = Player,
    minutes_played = Min,
    passes_completed = Cmp_Passes,
    passes_attempted = Att_Passes,
    pass_completion_rate = passes_completed / passes_attempted,
    passes_completed_p90 = 90 * passes_completed / minutes_played
  )
player_match_stats

player_team_season_mapping <- player_match_stats |> 
  dplyr::group_by(
    season,
    team,
    player
  ) |> 
  dplyr::filter(minutes_played > 0L) |> 
  dplyr::summarize(
    dplyr::across(minutes_played, sum),
    matches_played = dplyr::n_distinct(match_id)
  ) |> 
  dplyr::ungroup()

# team_match_stats <- player_match_stats |> 
#   dplyr::group_by(season, match_id, date, team) |> 
#   dplyr::summarize(
#     dplyr::across(c(minutes_played, passes_completed, passes_attempted), sum)
#   ) |> 
#   dplyr::ungroup() |> 
#   dplyr::mutate(
#     pass_completion_rate = passes_completed / passes_attempted,
#     passes_completed_p90 = 90 * pass_completion_rate / minutes_played
#   )

player_season_stats <- player_match_stats |> 
  dplyr::group_by(season, team, player) |> 
  dplyr::summarize(
    dplyr::across(c(minutes_played, passes_completed, passes_attempted), sum)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    pass_completion_rate = passes_completed / passes_attempted,
    passes_completed_p90 = 90 * passes_completed / minutes_played
  )

match_ids <- team_match_stats |>
  dplyr::distinct(season, team, date, match_id) |> 
  dplyr::arrange(season, team, date)

## if this isn't the same number for all seasons, then we'd need to edit the `slice_sample` code in `resample_stats` 
n_matches_by_season <- match_ids |> 
  dplyr::count(season, team, name = 'n_matches') |> 
  dplyr::count(season, n_matches) |> 
  dplyr::slice_max(n, n = 1)

set.seed(42)
resample_stats <- function() {
  resampled_match_ids <- match_ids |> 
    dplyr::group_by(team, season) |> 
    dplyr::slice_sample(n = 38, replace = TRUE) |> 
    dplyr::ungroup() |> 
    dplyr::group_by(season, team) |> 
    dplyr::mutate(
      game_idx = dplyr::row_number(as.numeric(date) + dplyr::row_number())
    ) |> 
    dplyr::ungroup() |> 
    dplyr::arrange(season, team, game_idx)
  
  # resampled_team_match_stats <- resampled_match_ids |> 
  #   dplyr::inner_join(
  #     team_match_stats,
  #     by = dplyr::join_by(season, team, date, match_id)
  #   )
  
  resampled_player_match_stats <- resampled_match_ids |> 
    dplyr::inner_join(
      player_match_stats,
      by = dplyr::join_by(season, team, date, match_id),
      relationship = 'many-to-many'
    )
  
  # resampled_player_season_stats <- resampled_player_match_stats |> 
  #   dplyr::group_by(season, team, player) |> 
  #   dplyr::summarize(
  #     dplyr::across(c(minutes_played, passes_completed, passes_attempted), sum)
  #   ) |> 
  #   dplyr::ungroup() |> 
  #   dplyr::mutate(
  #     pass_completion_rate = passes_completed / passes_attempted,
  #     passes_completed_p90 = 90 * pass_completion_rate / minutes_played
  #   )
  #  
  # list(
  #   match_ids = resampled_match_ids,
  #   # teams = resampled_team_match_stats,
  #   players = resampled_player_match_stats,
  #   player_seasons = resampled_player_season_stats
  # )
  resampled_player_match_stats
}

N_BOOSTRAPS <- 100
resampled_player_match_stats <- purrr::map_dfr(
  rlang::set_names(1:N_BOOSTRAPS),
  \(...) resample_stats(),
  .id = 'bootstrap_id'
) |> 
  dplyr::mutate(bootstrap_id = as.integer(bootstrap_id))

# flatten_bootstraps <- function(resamples, element) {
#   resamples |> 
#   purrr::imap_dfr(
#     \(.x, .y) purrr::pluck(.x, element),
#     .id = 'bootstrap_id'
#   ) |> 
#     dplyr::mutate(bootstrap_id = as.integer(bootstrap_id))
# }
# resampled_player_match_stats <- flatten_bootstraps(resampled_stats, 'players')
# resampled_player_match_stats <- flatten_bootstraps(resampled_stats, 'player_seasons')

resampled_player_season_stats <- resampled_player_match_stats |> 
  dplyr::group_by(bootstrap_id, season, team, player) |> 
  dplyr::summarize(
    dplyr::across(c(minutes_played, passes_completed, passes_attempted), sum)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    pass_completion_rate = passes_completed / passes_attempted,
    passes_completed_p90 = 90 * pass_completion_rate / minutes_played
  )

## season totals
eligible_players <- player_team_season_mapping |> 
  dplyr::filter(minutes_played >= 270)

METRICS <- c(
  'minutes_played',
  'passes_completed',
  'passes_attempted',
  'pass_completion_rate',
  'passes_completed_p90'
)

drop_ineligible_players <- function(df, eligible_players) {
  df |> 
    dplyr::semi_join(
      eligible_players,
      by = dplyr::join_by(season, team, player)
    )
}

pivot_metrics <- function(df) {
  df |> 
    dplyr::select(
      season,
      team,
      player,
      dplyr::all_of(METRICS)
    ) |> 
    tidyr::pivot_longer(
      -c(season, team, player),
      names_to = 'metric',
      values_to = 'value'
    ) 
}

drop_ineligible_players_and_pivot_metrics <- purrr::compose(
  drop_ineligible_players,
  pivot_metrics,
  .dir = 'forward'
)

## https://arxiv.org/pdf/1609.09830.pdf
## eq2 numerator
## or \frac{1}{P}\sum_{p=1}^{P}{BV[X_{spm}]} on p10
## variance within player-season, by metric
## bv
## in the paper, they just calculate this for 1 season, but we can do it for every season
resampled_player_season_variance <- resampled_player_season_stats |> 
  drop_ineligible_players_and_pivot_metrics(eligible_players) |>
  dplyr::group_by(season, team, player, metric) |> 
  dplyr::summarize(
    bv = var(value, na.rm = TRUE)
  ) |> 
  dplyr::ungroup()

average_resampled_player_season_variance <- resampled_player_season_variance |> 
  dplyr::group_by(season, metric) |> 
  dplyr::summarize(
    bv = mean(bv)
  ) |> 
  dplyr::ungroup()

## eq2 denominator
## or \frac{1}{P}\sum_{p=1}^{P}{(X_{spm}-\bar{X}_{s*m})^2} on p10
## sv
## in the paper, they just calculate this for 1 season, but we can do it for every season
pivoted_player_season_stats <- player_season_stats |> 
  drop_ineligible_players_and_pivot_metrics(eligible_players)

season_variance <- pivoted_player_season_stats |>
  dplyr::group_by(season, metric) |> 
  dplyr::summarize(
    sv = var(value, na.rm = TRUE)
  ) |> 
  dplyr::ungroup()

discrimination <- average_resampled_player_season_variance |> 
  dplyr::inner_join(
    season_variance,
    by = dplyr::join_by(season, metric)
  ) |> 
  dplyr::mutate(
    discrimination = 1 - bv / sv
  )

## aggregate over seasons
within_player_variance <- pivoted_player_season_stats |> 
  ## should check for players with the same name
  dplyr::group_by(player, metric) |>
  dplyr::summarize(
    wv = var(value, na.rm = TRUE)
  ) |> 
  dplyr::ungroup()

average_within_player_variance <- within_player_variance |> 
  dplyr::group_by(metric) |> 
  dplyr::summarize(
    wv = mean(wv, na.rm = TRUE)
  ) |> 
  dplyr::ungroup()

total_variance <- pivoted_player_season_stats |> 
  dplyr::group_by(metric) |>
  dplyr::summarize(
    tv = var(value, na.rm = TRUE)
  ) |> 
  dplyr::ungroup()

stability <- average_resampled_player_season_variance |> 
  dplyr::inner_join(
    total_variance,
    by = dplyr::join_by(metric),
    relationship = 'many-to-one'
  ) |>
  dplyr::inner_join(
    average_within_player_variance,
    by = dplyr::join_by(metric)
  ) |> 
  dplyr::mutate(
    stability = 1 - (wv - bv) / (tv - bv)
  )

## For each team
##   1. resample the match IDs with replacment for each season.
##   2. extract player match stats for sampled match IDs.
##   3. summarize team stats for sampled match IDs (should be deterministic).
##   4. repeat steps 1-3 at least 20 times (bootstrap!)

## discrimination: 1 - (BV / SV)
##   where BV = average variance for a given metric across the bootstrapped game logs across all players
##   and SV = single-season variance for a given metric across player-seasons (1 row per season)

## Note that they drop players with less than 250 minutes played for SV.
##   This would be like 10 games played for a 6th man, or about 12% of the season
##   The soccer equivalent might be 4-5 matches, or ~360-450 minutes.