
This repo stores my code and plots for sports-related plots. Any “small”
data is stored in the repo. Otherwise, it’s saved locally and
git-ignored here.

The projects are listed in reverse chronological order. A skipped number
corresponds to an idea that I scraped.

# 64. Comparison of FBRef’s new and old data providers

![](64-fbref_xg_sources/fbref_xg_sources.png "FBRef non-penalty xG - goals for Big 5 European leagues")

# 63. 2022 World Cup Height Mismatches

![](63-wc2022_player_height/avg_player_height.png "Short kings vs. Tall boys")

# 62. 2022 USL Championship Finals Preview

![](62-safc/usl_2022_finals_preview_w_logo.png "2022 USL Championship Final Preview")

# 61. Liga MX Shotmap

![](61-sonofacorner_shotmap/liga-mx.png "Non-penalty shot bins for Liga MX's top 6 shooters")

# 60. Card Descriptions

![](60-card_reasons/hand.png "Frequency of 'hand' in text descriptions of yellow and red cards in match news feed for tier 1 and 2 leagues in England, Spain, France, Germany, Italy, and the US.")

![](60-card_reasons/bad.png "Frequency of 'bad' in text descriptions of yellow and red cards in match news feed for tier 1 and 2 leagues in England, Spain, France, Germany, Italy, and the US.")

![](60-card_reasons/violent.png "Frequency of 'violent' in text descriptions of yellow and red cards in match news feed for tier 1 and 2 leagues in England, Spain, France, Germany, Italy, and the US.")

# 59. Expected Points Deep Dive

![](59-xg_xpoints/calib.png "Calibration of implied match outcome probabilities, 2021/22 - 2021/22 English Premier League")

![](59-xg_xpoints/unexpected.png "Which teams had the most unexpected placings? 2014/15 - 2021/22 English Premier League")

# 58. Manchester United Loss Tweets

![](58-loss_tweets/manu_tweets_w_logo.png "Player appearances in end-of-match tweets, Manchester United, 2021/22 Premier League")

# 55. 2021/22 EPL Blocked Shots from Outside the Box

![](55-blocks_outside_box/tiled_oob_blocks_w_logo.png "Percentage of outside-the-box shots blocked in 2021/22 Premier League")

![](55-blocks_outside_box/oob_blocks_and_shots_w_logo.png "Liverpool not only block shots from outside-the-box at historically low rates, but they also conceded a lower percentage of their shots from outside the box, 2017/18 - 2021/22 Premier League")

# 54. 2021/22 UCL Final Nice Dawgs

![](54-202122_ucl_final/nice_dawgs.png "That boy nice vs. He got that dawg in him plot for players on teams in UCL 2021/22 final")

# 53. 2021/22 Premier League Aerial Duels

![](53-duels/top.png "Who was awarded the most aerial duels won + aerial duels not lost by Opta?")

![](53-duels/bot.png "Who was awarded the least aerial duels won + aerial duels not lost by Opta?")

# 52. Premier League Time to Throw In

![](52-manager_footedness/manager_footedness_w_logo.png "Are Left-Footed Managers in La Liga and the Premier League out of Favor?")

![](52-manager_footedness/manager_footedness_left_cis_w_logo.png "Uncertainty in Relative Left-Footed Share")

# 50. Premier League Time to Throw In

![](50-throw_in_times/throw_ins-g_state-w_logo.png "How much time do leading teams waste on throw-ins?")

![](50-throw_in_times/throw_ins-g_state+is_home-w_logo.png "How much time do leading teams waste on throw-ins (including home/away indicator)?")

![](50-throw_in_times/gks-g_state+is_home-w_logo.png "How much time do leading teams waste on goalkicks (including home/away indicator)?")

# 49. Premier League Team Foul Rate by Season

![](49-yellow_cards/fouls_per_match_w_logo.png "Premier League Team Foul Rate by Season since 2012-13")

# 48. 2021-22 Premier League In-game xG Difference

![five horizontal bars per facet, where each facet is for a signle team,
each bar represents one of the last 5 games, fill represents proportion
of match ahead or behind in
xG](48-202122_game_state/202122_xg_game_state_w_logo.png "Proportion of match spent ahead or behind in xG")

# 47. [Pass Network Max Cut](https://tonyelhabr.rbind.io/post/soccer-pass-network-max-cut/)

![pass networks for Liverpool vs. Manchester City 2021-10-03 match,
showing weighted max cut
values](47-formation_symmetry/network-game_id=1549604-max_cut_weighted_norm_w_logo.png "Liverpool vs. Manchester City 2021-10-03 pass networks")

![table showing correlations of various traditional and network stats
with xG and xT diff. / 90, showing that weighted max cut is stronger
than other network stats and has similar strength to pass-derived
stats](47-formation_symmetry/game_x_cors_table.png "Game-level Correlations with xG and xT Diff. / 90")

![horizontal error bars facetted on regression type (with or without
confounders), showing that weighted max cut diff. / 90 effectively plays
no role in describing match outcome in the presense of
confounders](47-formation_symmetry/team_stat_coefs_w_logo.png "Effect of Weighted Max Cut Diff. / 90 is suppressed by confounders")

# 46. EPL “Rage” Fouls

![spiral chart with minutes played on x-axis and count of rage fouls on
y-axis. strands represent one player per
sesaon.](46-euro_fouls/rage_quit_w_inset_and_cancelo.png "Who rage fouls most frequently?")

1.  2021/22 Fantasy Football Luck

![table showing luckiest standing placement in my fantasy football
league over the past 2
seasons](45-202122_ff_luck/ff_luckiest.png "Lucky players")

![table showing unluckiest standing placement in my fantasy football
league over the past 2
seasons](45-202122_ff_luck/ff_unluckiest.png "Destined for the Beer Mile?")

# 44. MLS Golazos

![error bars showing percentage of shots barely on frame in the MLS and
the Big 5 soccer leagues, with MLS as having the highest
percentage](44-mls_bangers/prop.png "MLS has the highest proportion of direct free kicks on target and near goal frame")

![shotmap of 2021 MLS goals where shot is barely within frame (about 20
shots)](44-mls_bangers/shotmap.png "Goals from direct free kicks on target and near goal frame")

![plot of goal frame of shots barely within frame from MLS and big 5
leagues, emphasizing difference in xG on target and xG with size and
league with
color](44-mls_bangers/frame.png "Shots from direct free kicks on target and near goal frame")

# 43. xG Signal

![line plot showing log curve of average xg difference r squared with
rest-of-season xg difference, where the curve exceeds 0.5 at around 11
matches](43-xg_signal/xg_signal.png "When can we start trusting the xG table?")

# 42. 2021-22 Premier League New Players

![faceted waffle plot of 2021-22 english premier league, where each
block represents 90 minutes played by new players on a given team,
facets are for each team, and blocks are colored by
position](42-202122_new_players/202122_epl_new_players_w_logo.png "Which squads have had the largest influx of new players this season?")

# 41. \#USMNT popularity growth drivers

![venn diagram showing mutual follows of @watke\_, @TedLasso, and @USMNT
on
Twitter](41-usmnt_followers/venn.png "Has @watke_ done more for the growth of #USMNT popularity than Ted Lasso?")

# 40. Significant xG Overperformance

![dumbell plot of 10-game rolling xg/shot and g/shot for harry kane and
romelu lukaku over their past 50
matches](40-xg_overperformance/overperformance_w_kane_lukaku.png "When do elite scorers like Kane and Lukaku significantly overperform their xG?")

![dumbell plot of 10-game rolling xg/shot and g/shot for timo werner
over their past 50
matches](40-xg_overperformance/underperformance_w_werner.png "What does significant xG under-performance look like?")

# 38. Football \#viz Twitter

![venn diagram showing about 50 twiter accounts who post soccer viz,
grouped by following, frequency of tweets, and frequency of
images](38-soccer_twitter/venn/Slide1.JPG "Football viz twitter accounts venn diagram")

# 37. CONCACAF’d

![geo-tile map showing average 2nd half stoppage time when matches end
in a draw or with a 1 goal difference for CONCACAF, CONMEBOL, and UEFA
teams in the Gold Cup, Copa America, and Euros since 2015. Brazil and
South American teams (CONMEBOL) have the highest
averages](37-concacaf/stoppage_time_map_w_img.png "CONCACAF geo-tile map")

![table showing average yellows, reds, fouls, and PKs per match for
CONCACAF, CONMEBOL, and UEFA teams in the Gold Cup, Copa America, and
Euros since 2015. CONMEBOL teams have a notably higher number of yellows
and reds](37-concacaf/concacaf_tab.png "CONCACAF vs CONMBEBOL table")

# 36. 2020-21 EPL Team Subreddit Activity

![Chatter plot of most controversial posts on eac 2020-21 EPL Team’s
Subreddit](36-202021_epl_reddit/viz_reddit_controversial_w_logo.png "Chatter plot of most controversial posts on eac 2020-21 EPL Team's Subreddit")

# 35. [Tired: PCA + kmeans, Wired: UMAP + GMM](https://tonyelhabr.rbind.io/post/dimensionality-reduction-and-clustering/)

![Scatter plot example of visualizing UMAP and GMM, using 2 components
and 6
clusters](35-probabilistic_clustering/viz_uncertainty_umap_filt.png "Scatter plot example of visualizing of UMAP and GMM, using 2 components and 6 clusters")

<img src="35-probabilistic_clustering/gt_similarity_jadon_sancho.png" title="Table comparing 2-component PCA and UMAP rankings of player similarity for Jadon Sancho" alt="Table comparing 2-component PCA and UMAP rankings of player similarity for Jadon Sancho" width="40%"/>

# 34. 2020 EUROs Forecast Review

![Table of mean Brier skill score for pre-tournament 2020 EUROs
forecasts](34-2020_euros_review/tb_compare.png "Table of mean Brier skill score for pre-tournament 2020 EUROs forecasts")

![Table of pre-tournament 2020 EUROs forecasts for
champion](34-2020_euros_review/tb_champ.png "Table of pre-tournament 2020 EUROs forecasts for champion")

# 33. Relative League Power Rankings

![Relative increase in competition level when moving from league A to
B](33-2020_euros_ratings/viz_relative_p_vaep_direct2z.png "Relative increase in competition level when moving from league A to B")

![Expected change in xG/90 when moving from league A to
B](33-2020_euros_ratings/viz_relative_vp_xg_direct2z_fw_young.png "Expected change in xG/90 when moving from league A to B")

![Un-adjusted xG/90 change when moving from league A to
B](33-2020_euros_ratings/viz_xg_unadjusted.png "Un-adjusted xG/90 change when moving from league A to B")

# 32. 2020 EUROs

![Which 2020/21 Premier League Teams Have the Most Players in the 2020
Euros?](32-2020_euros/viz_countries_by_team_w_logo.png "Which 2020/21 Premier League Teams Have the Most Players in the 2020 Euros?")

![Match Goals Per 90 for the 2020
EUROs](32-2020_euros/cities_w_logo.png "Match Goals Per 90 for the 2020 EUROs")

# 30. EPL Subjective VAR calls

![EPL Subjective VAR calls by Team since
2019-20](30-var/viz_var_subjective_w_logo.png "EPL Subjective VAR calls by Team since 2019-20")

# 29. 2020-21 EPL Games Started by Young Players

![Animated Bar Plot GIF of 2020-21 EPL Games Started by Young
Players](29-202021_u2x/viz_ux.gif "Animated bar plot GIF showing how the youngest 2021-21 EPL team changes as you change the age cutoff used to determine the definition of young")

# 28. 2020-21 EPL Season Review with VAEP

![Best starting 11 players of the 2020/21 Premier League season
(arranged in a 4-3-3), according to VAEP (Valuing Actions by Estimating
Probabilities). Grealish, Kane, Salah, Jack Harrison, Gundogan,
Raphinha, Cresswell, Zouma, Dunk, Alexander-Arnold, Sam
Johnstone.](28-202021_vaep/viz_team_vaep_p90_w_logo.png "VAEP'S Best XI, 2020-21 EPL")

![Example of how VAEP scores each action in a possession. Example comes
from a 2021-04-04 match between Tottenham and Newcastle, in which Kane
misses a shot and gets net negative
VAEP.](28-202021_vaep/viz_vaep_ex_w_logo.png "Visual Example of VAEP")

![VAEP vs. minutes played for the 2020/21 Premier League season.
Subplots made for each of four position groupings (Forward/Attacker,
Midfielder, Defender,
Goalkeeper).](28-202021_vaep/viz_vaep_by_pos_w_logo.png "VAEP vs. minutes played, by position")

![VAEP vs. transfer market value for the 2020/21 Premier League season.
Subplots made for each of four position groupings (Forward/Attacker,
Midfielder, Defender,
Goalkeeper).](28-202021_vaep/viz_vaep_mkt_by_pos_w_logo.png "VAEP vs. transfer market value, by position")

![VAEP vs. DAVIES for the 2020/21 Premier League
season.](28-202021_vaep/viz_vaep_davies_compare_w_logo.png "VAEP vs. DAVIES, by position")

<img src="28-202021_vaep/metric_yoy_stability.png" title="Year-over-year correlations for VAEP and DAVIES" alt="Table showing year-over-year correlations for VAEP and DAVIES. Both average around 0.45." width="60%"/>

# 27. Pep Guardiola Non-UCL Titles

![Table with Pep Guardiola’s Non-UCL Titles Won Since His Las UCL
Title](27-pep/pep.png "Table with Pep Guardiola's Non-UCL Titles Won Since His Las UCL Title")

![Timeline Plot of Champions League Re-matches between Teams from the
Same
League](27-pep/viz_ucl_rematches_manual.png "Timeline Plot of Champions League Re-matches between Teams from the Same League")

# 25. Soccer Referee Activity Across Major Leagues

![Beeswarm Plot of Referee Statistics By
League](25-202021_soccer_refs/viz_big5_ref_p90.png "Beeswarm Plot of Referee Statistics By League")

# 24. 2020-21 EPL Team Relative Fouling Rate by Game State

![2020-21 EPL Team Fouling Rate Relative to Opponent by Game
State](24-202021_game_state_fouls/viz_foul_p90_diff_by_state.png "2020-21 EPL Team Fouling Rate Relative to Opponent by Game State")

# 23. European Super League Google Trends

![European Super League Google Trends Animated
GIF](23-superleague/superleague_timeline.gif "European Super League Google Trends Animated GIF")

# 22. 2020-21 Chelsea Attack Under Tuchel Compared to Lampard

![Heatmaps for Passes, Touches, and Take-Ons for Kai Havertz and Mason
Mount under Tuchel
vs. Lampard](22-202021_epl_tuchel/viz_tuchel_w_logo.png "Heatmaps for Passes, Touches, and Take-Ons for Kai Havertz and Mason Mount under Tuchel vs. Lampard")

## 21. 2020-21 EPL Foul Tanaka Maps

![2020-21 EPL Fouls Drawn by Position Tanaka
Map](21-202021_epl_heatmap/viz_fouled_by_pos_w_logo.png "2020-21 EPL Fouls Drawn by Position Tanaka Map")

![2020-21 EPL Fouls Drawn by Jack Grealish vs Other Midfielders Tanaka
Map](21-202021_epl_heatmap/viz_fouled_w_logo.png "2020-21 EPL Fouls Drawn by Jack Grealish vs Other Midfielders Tanaka Map")

![2020-21 EPL Fouls Made by Tomas Soucek vs Other Midfielders Tanaka
Map](21-202021_epl_heatmap/viz_fouler_w_logo.png "2020-21 EPL Fouls Made by Tomas Soucek vs Other Midfielders Tanaka Map")

## 20. 2020-21 EPL Game State xG

![2020-21 EPL Game State npxG/Shot and Shot/90
Tables](20-202021_epl_xg_stats/viz_shot_quality_efficiency_2020_w_logo.png "2020-21 EPL Game State npxG/Shot and Shot/90 Tables")

![2020-21 EPL Game State npxG/90
Table](20-202021_epl_xg_stats/viz_npxg_p90_diff_2020_w_logo.png "2020-21 EPL Game State npxG/90 Table")

## 19. Nick Wan New Twitch Followers

![Nick Wan New Follower Time series
Lollipops](19-nickwan/nick_wan_twitch_followers_w_sliced.png "Nick Wan New Follower Time series Lollipops")

## 18. OptaJoe One-Word Summaries

![OptaJoe One-Word Summaries Bubble
Chart](18-OptaJoe/bubble_optajoe_wide_w_logo.png "OptaJoe One-Word Summaries Bubble Chart")

## 17. 2020-21 NBA All-Star Break Net Rating Lollipop Chart

![2020-21 NBA All-Star Break Net Rating Lollipop
Chart](17-nba_allstar_break_nrtg/viz_nba_allstar_break_2021.png "2020-21 NBA All-Star Break Net Rating Lollipop Chart")

## 16. xGPhilosophy xEngagement

![Brighton Engagement Over Expected
Table](16-xGPhilosopy/tb_ex.png "Brighton Engagement Over Expected Table")

![Brighton 2-3 Man U Actual vs. Predicted Favorites and Retweets Scatter
Plot](16-xGPhilosopy/viz_preds_ex.png "Brighton 2-3 Man U Actual vs. Predicted Favorites and Retweets Scatter Plot")

![Brighton 2-3 Man U Retweets SHAP Breakdown Bar
Plot](16-xGPhilosopy/viz_shap_ex.png "Brighton 2-3 Man U Actual vs. Retweets SHAP Breakdown Bar Plot")

![Aggregate SHAP Bar
Plot](16-xGPhilosopy/viz_shap_agg.png "Aggregate SHAP Bar Plot")

## 15. 2020-21 Fantasy Football Plots

After regular season (week 12)

![2020-21 Fantasy Football Standings Simulation
Table](15-2020_ff/viz_standings_tile_2020.png "2020-21 Fantasy Football Standings Simulation Table")

After week 7

![2020-21 Fantasy Football Points For vs. Points Against Scatter
Plot](15-2020_ff/viz_scores_cusum_2020-07.png "2020-21 Fantasy Football Points For vs. Points Against Scatter Plot")

## 14. Non-Negative Matrix Factorization Heat Maps for Soccer Player Tracking

![Non-Negative Matrix Factorization Heat Maps for Soccer Player
Tracking](14-soccer_nnmf/viz_nnmf_dimensions_1to9_r_smooth.jpg "Non-Negative Matrix Factorization Heat Maps for Soccer Player Tracking")

## 13. Premier League Official Radar Charts

![Anthony Taylor Radar
Chart](13-soccer_refs/Anthony_Taylor_w_logo.png "Anthony Taylor Radar Chart")

![Mike Dean Radar
Chart](13-soccer_refs/Mike_Dean_w_logo.png "Mike Dean Radar Chart")

## 12. Comparison of Pitch Control Models

![Comparison of Pitch Control
Models](12-pitch_control_compare/pc_fb_v_spearman___fb_v_vor.gif "Comparison of Pitch Control Models")

## 11. [Python and R Comparison of Spearman (2017) Pitch Control Model](https://tonyelhabr.rbind.io/post/soccer-pitch-control-r/)

![Python and R Comparison of Spearman (2017) Pitch Control
Model](11-pitch_control_spearman/viz_pc_823_combined.png "Python and R Comparison of Spearman (2017) Pitch Control Model")

## 10. Soccer Formation Pitch Ownership

![Voronoi tessellation of a 4-3-3 False 9 vs. 5-4-1 Diamond
Formation](10-soccer_formations/4-3-3-falsenineattack_v_5-4-1-diamond.png "Voronoi tessellation of a 4-3-3 False 9 vs. 5-4-1 Diamond Formation")

## 9. 2020 Champions League Finals, Bayern Munich’s Goal

![PSG 0-1 Bayern Munich, UCL 2020 Finals, Animated Pitch Control and
VAEP](09-2020_ucl_psg_mun_w_vaep/ucl_2020_psg_mun.gif "PSG 0-1 Bayern Munich, UCL 2020 Finals, Animated Pitch Control and VAEP")

## 8. 2020 NBA Playoffs Total Excitement Index

At the end of the playoffs

![2020 NBA Playoffs Total Excitement Index Bar Chart
Race](08-2020_nba_playoffs_excitement_index/2020_nba_playoffs_excitement_index_20201010.gif "2020 NBA Playoffs Total Excitement Index Bar Chart Race")

As of 2020-09-06

![2020 NBA Playoffs Total Excitement Index Bar
Chart](08-2020_nba_playoffs_excitement_index/2020_nba_playoffs_excitement_index_20200907.png "2020 NBA Playoffs Total Excitement Index Bar Chart")

## 7. 2020 Champions League Quarter-Finals, PSG’s 2nd Goal

***Note: This gif really should have had some smoothing applied. The
matrix inversion with `solve()` in the code returns some near infinite
values for some inputs.***

![PSG 2-1 Atalanta, UCL 2020 Quarter-Finals, Animated Pitch
Control](07-2020_ucl_psg_ata/ucl_2020_psg_atl.gif "PSG 2-1 Atalanta, UCL 2020 Quarter-Finals, Animated Pitch Control")

## 6. Texas High School Football and Marching Band Success

![Texas High School Football vs. Band Scatter
Plot](06-tx_hs/tx_hs_fb_band.png "Texas High School Football vs. Band Scatter Plot")

## 5. NBA 3P% Stability Rate

![NBA 3P% Cumulative Average and Stability Rate Line Charts
Animated](05-nba_3fg_stability/nba_3p_stability.gif "NBA 3P% Cumulative Average and Stability Rate Line Charts Animated")

## 4. 2020 Soccer Competition-Adjusted Non-Penalty Goals Per 90 Minutes

![Competition-Adjusted Non-Penalty Goals Per 90 Minutes
Table](04-2020_ucl_npg90_adj/04_ucl_npg90_adj.utf8.png "Competition-Adjusted Non-Penalty Goals Per 90 Minutes Table")

![Competition-Adjusted Non-Penalty Goals Per 90 Minutes Dumbbell
Chart](04-2020_ucl_npg90_adj/ucl_npg90_adj.png "Competition-Adjusted Non-Penalty Goals Per 90 Minutes Dumbbell Chart")

## 3. 2020 NBA Restart Social Justice Messages

![2020 NBA Restart Social Justice Message Bar
Chart](03-2020_nba_social_justice/2020_nba_social_justice.png "2020 NBA Restart Social Justice Message Bar Chart")

## 2. 2019-20 English Premier League First 30 Games

[2019-20 English Premier League First 30 Games Points Minus Expected
Points Lollipop
Chart](02-epl_since_break/epl_before_break_pts_minus_xpts.png "2019-20 English Premier League First 30 Games Points Minus Expected Points Lollipop Chart")

![English Premier League 2019-20 First 30 Games Points vs. Expected
Points Scatter
Plot](02-epl_since_break/epl_before_break_pts_vs_xpts.png "English Premier League 2019-20 First 30 Games Points vs. Expected Points Scatter Plot")

![English Premier League 2019-20 Last 9 Games Simulated Points
vs. Actual Points Ridge
Plot](02-epl_since_break/epl_after_break_pts_sim.png "English Premier League 2019-20 Last 9 Games Simulated Points vs. Actual Points Ridge Plot")

## 1. 2020 NBA Western Conference League Restart Playoff Seeding Probabilities

![2020 NBA Western Conference League Restart Playoff Seeding
Probabilities Bar
Chart](01-nba_seed_p/nba_seed_p.png "2020 NBA Western Conference League Restart Playoff Seeding Probabilities Bar Chart")

# Future Ideas

- In which sport is the leading team most likely to further their lead
  near the end of the game due to an opposition error, overly-aggressive
  tactics by the opposition, etc.? (End of game depends on the sport.
  Ideally, we could use a win probability model to help identify
  end-of-game context, although it would still need some adjustment.)
  Extension: In which sport are gamblers most likely to lose/win due to
  a back-door cover?

- Decision tree to filter down players in a sport to just one player who
  has achieved a very specific set of career totals/averages
  (i.e. [inspiration](https://twitter.com/JacobEGoldstein/status/1296570734047686658))

- Minutes played by new players
  ([inspiration](https://twitter.com/reinhurdler/status/1438172361790996490?s=20))

- Playing time in career at current team sonar
  ([inspiration](https://twitter.com/reinhurdler/status/1432841015858188288?s=20))

- Which positions is it ok to have a weak spot at?
  ([inspiration](https://theathletic.com/2832130/2021/09/19/in-which-positions-can-a-team-have-a-glaring-weakness-but-still-win-the-premier-league-title/))

- Do coaches sub-off players with yellow cards at a higher rate than
  they would otherwise?

- Liverpool’s shots conceded (outside the penalty area) vs. shots
  blocked in 2021/22
