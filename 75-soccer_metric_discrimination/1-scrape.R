library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(lubridate)

load_fb_advanced_match_stats <- function(country, gender, tier, stat_type, team_or_player, season_end_year = NA) {
  
  url <- sprintf(
    'https://github.com/JaseZiv/worldfootballR_data/releases/download/fb_advanced_match_stats/%s_%s_%s_%s_%s_advanced_match_stats.rds',
    country,
    gender,
    tier,
    stat_type,
    team_or_player
  )
  readRDS(url(url))
}

possibly_load_fb_advanced_match_stats <- purrr::possibly(
  load_fb_advanced_match_stats, 
  otherwise = tibble::tibble(),
  quiet = TRUE
)

params <- tidyr::expand_grid(
  country = c('ENG', 'ESP', 'FRA', 'GER', 'ITA'),
  gender = 'M',
  tier = '1st',
  stat_type = c('summary'), # 'passing', 'shooting'),
  team_or_player = 'player'
) |> 
  as.list()

raw_player_match_stats <- purrr::pmap_dfr(
  params,
  possibly_load_fb_advanced_match_stats
) |> 
  dplyr::filter(
    Season_End_Year < current_season_end_year,
    !grepl('GK', 'Pos')
  )

BASE_METRICS <- c(
  'goals',
  'assists',
  'shots',
  'shots_on_target',
  'touches',
  'tackles',
  'interceptions',
  'blocks',
  'xg',
  'npxg',
  'xa',
  'sca',
  'gca',
  'passes_completed',
  'passes_attempted',
  'progressive_passes',
  'carries',
  'progressive_carries',
  'takeons_attempted',
  'successful_takeons'
)

RATE_METRICS <- c(
  'goal_conversion_rate',
  'pass_completion_rate',
  'successful_takeon_rate'
)

ALL_METRICS <- c(
  BASE_METRICS,
  RATE_METRICS
)

player_match_stats <- raw_player_match_stats |> 
  dplyr::transmute(
    league = sprintf('%s-%s-%s', Country, Gender, Tier),
    season = sprintf('%s/%s', Season_End_Year - 1, substr(Season_End_Year, 3, 4)),
    date = lubridate::ymd(Match_Date),
    match_id = basename(dirname(MatchURL)),
    team = Team,
    player = Player,
    minutes_played = Min,
    
    goals = Gls, ## includes pks
    assists = Ast,
    shots = Sh, ## does not include pk attempts
    shots_on_target = SoT,
    touches = Touches,
    tackles = Tkl,
    interceptions = Int,
    blocks = Blocks,
    
    xg = xG_Expected,
    npxg = npxG_Expected,
    xa = xAG_Expected,
    
    sca = SCA_SCA,
    gca = GCA_SCA,
    
    passes_completed = Cmp_Passes,
    passes_attempted = Att_Passes,

    progressive_passes = PrgP_Passes,
    carries = Carries_Carries,
    progressive_carries = PrgC_Carries,
    
    takeons_attempted = Att_Take_Ons,
    successful_takeons = Succ_Take_Ons
  )

coalesce_fraction <- purrr::compose(
  \(num, den) ifelse(num > 0 & den == 0, 0, num / den),
  \(x) dplyr::coalesce(x, 0),
  \(x) ifelse(x > 1, 1, x),
  \(x) ifelse(x < 0, 0, x),
  .dir = 'forward'
)

summarize_all_metric_columns <- function(df, ...) {
  df |> 
    dplyr::group_by(..., league, season, team, player) |> 
    dplyr::summarize(
      dplyr::across(
        c(minutes_played, dplyr::all_of(BASE_METRICS)), 
        sum
      )
    ) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      ## Mark Noble with the epic 1 goal on 0 shots https://fbref.com/en/matches/b56fd899/Watford-West-Ham-United-December-28-2021-Premier-League
      goal_conversion_rate = coalesce_fraction(goals, shots),
      pass_completion_rate = coalesce_fraction(passes_completed, passes_attempted),
      successful_takeon_rate = coalesce_fraction(successful_takeons, takeons_attempted)
    )
}

player_team_season_mapping <- player_match_stats |> 
  dplyr::group_by(league, season, team, player) |>
  dplyr::filter(minutes_played > 0L) |> 
  dplyr::summarize(
    dplyr::across(minutes_played, sum),
    matches_played = dplyr::n_distinct(match_id)
  ) |> 
  dplyr::ungroup()
# player_team_season_mapping |> dplyr::count(league, season, team, player, sort = TRUE)

player_season_stats <- summarize_all_metric_columns(player_match_stats)

MIN_MINUTES_PLAYED <- 270
eligible_player_season_stats <- player_season_stats |> 
  dplyr::filter(minutes_played >= MIN_MINUTES_PLAYED)

eligible_player_match_stats <- player_match_stats |> 
  dplyr::semi_join(
    eligible_player_season_stats,
    by = dplyr::join_by(season, player, team)
  )

match_ids <- player_match_stats |>
  dplyr::distinct(league, season, team, date, match_id) |> 
  dplyr::arrange(league, season, team, date)

set.seed(42)
resample_stats <- function(match_ids, player_match_stats) {
  ## can't just specify to resample 38 matches per team since different leagues 
  ## have different season lengths (Bundesliga, MLS), and because COVID ball
  resampled_match_ids <- match_ids |> 
    dplyr::select(league, season, team, match_id) |> 
    dplyr::group_by(league, season, team) |> 
    dplyr::summarize(
      match_id = list(match_id)
    ) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      match_id = purrr::map(
        match_id,
        \(.x) {
        sample(.x, size = length(.x), replace = TRUE)
      })
    ) |> 
    tidyr::unnest(match_id)
  
  resampled_match_ids |> 
    dplyr::inner_join(
      player_match_stats,
      by = dplyr::join_by(league, season, team, match_id),
      relationship = 'many-to-many'
    )
}

N_BOOSTRAPS <- 20
resampled_player_match_stats <- purrr::map_dfr(
  rlang::set_names(1:N_BOOSTRAPS),
  \(...) resample_stats(match_ids, eligible_player_match_stats),
  .id = 'bootstrap_id'
) |> 
  dplyr::mutate(bootstrap_id = as.integer(bootstrap_id)) |> 
  dplyr::filter(minutes_played < (0.75 * MIN_MINUTES_PLAYED))

resampled_player_season_stats <- resampled_player_match_stats |> 
  summarize_all_metric_columns(bootstrap_id)

pivot_metric_columns <- function(df) {
  df |> 
    dplyr::select(
      league,
      season,
      team,
      player,
      dplyr::all_of(ALL_METRICS)
    ) |> 
    tidyr::pivot_longer(
      -c(league, season, team, player),
      names_to = 'metric',
      values_to = 'value'
    )
}

## https://arxiv.org/pdf/1609.09830.pdf
## eq2 numerator
## or \frac{1}{P}\sum_{p=1}^{P}{BV[X_{spm}]} on p10
## variance within player-season, by metric
## bv
## in the paper, they just calculate this for 1 season, but we can do it for every season
resampled_player_season_variance <- resampled_player_season_stats |> 
  pivot_metric_columns() |> 
  dplyr::group_by(season, team, player, metric) |> 
  dplyr::summarize(
    bv = var(value, na.rm = TRUE)
  ) |> 
  dplyr::ungroup()

average_resampled_player_season_variance <- resampled_player_season_variance |> 
  dplyr::group_by(season, metric) |> 
  dplyr::summarize(
    bv = mean(bv, na.rm = TRUE)
  ) |> 
  dplyr::ungroup()

# average_resampled_player_season_variance |> 
#   dplyr::group_by(metric) |> 
#   dplyr::summarize(bv = mean(bv))

## eq2 denominator
## or \frac{1}{P}\sum_{p=1}^{P}{(X_{spm}-\bar{X}_{s*m})^2} on p10
## sv
## in the paper, they just calculate this for 1 season, but we can do it for every season
pivoted_player_season_stats <- pivot_metric_columns(player_season_stats)

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
discrimination

## aggregate over seasons
within_player_variance <- pivoted_player_season_stats |> 
  ## should check for players with the same name
  dplyr::group_by(league, team, player, metric) |>
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

meta_metrics <- dplyr::inner_join(
  discrimination |> dplyr::select(season, metric, sv, bv, discrimination),
  stability |> dplyr::select(season, metric, tv, wv, stability),
  by = dplyr::join_by(season, metric)
) |> 
  dplyr::select(
    season,
    metric,
    tv,
    sv,
    bv,
    wv,
    discrimination,
    stability
  )

library(ggplot2)
# stability |> 
#   inner_join(
#     meta_metrics |> 
#       filter(!between(stability, 0, 1))
#   )
# pivoted_player_season_stats |> 
#   filter(
#     # metric %in% c('assists_p90', 'gca_p90', 'xa_p90')
#     grepl('p90', metric)
#   ) |> 
#   ggplot() +
#   aes(
#     x = value
#   ) +
#   geom_histogram() +
#   facet_wrap(~metric, scales = 'free')

meta_metrics |> 
  dplyr::filter(season == '2021/22') |> 
  # filter(!grepl('p90', metric)) |> 
  ggplot() +
  aes(
    x = discrimination,
    y = stability
  ) +
  geom_abline(
    linetype = 2
  ) +
  geom_point() +
  ggrepel::geom_text_repel(
    aes(label = metric)
  ) +
  # coord_equal(xlim = c(0, 1), ylim = c(0, 1))
  scale_x_continuous(limits = c(0.3, 1)) +
  scale_y_continuous(limits = c(0.3, 1))


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