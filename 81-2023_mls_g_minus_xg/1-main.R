# https://github.com/etmckinley/good-lucky-matrix/blob/main/good-lucky%20matrix.qmd
library(itscalledsoccer)
library(readr)
library(dplyr)
library(ggplot2)

client <- AmericanSoccerAnalysis$new()

team_info <- read_csv(
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vTiZfW7pSUWPttpHSMlAwgMyXwdAeLAW6HuoHwZa69FrNpfzqVkM_0DaeAveTG7hvbCSK-HBh31QxIM/pub?gid=95813594&single=true&output=csv"
)

xg_by_team <- client$get_team_xgoals(
  leagues = 'mls',
  season_name = 2023,
  stage_name = 'Regular Season'
) 

xg_by_team_ranks <- xg_by_team |>
  inner_join(
    team_info |> 
      select(
        team_id = team_hashid,
        team = team_short_name
      ),
    by = join_by(team_id)
  ) |> 
  as_tibble() |> 
  transmute(
    team,
    logo = paste0(
      #"https://app.americansocceranalysis.com/club_logos/",
      "https://american-soccer-analysis-headshots.s3.amazonaws.com/club_logos/",
      team_id,
      ".png"
    ),
    g_minus_xg = goals_for - xgoals_for,
    g_minus_xg_conceded = goals_against - xgoals_against,
    offense_overperformance_rank = row_number(desc(g_minus_xg)),
    defense_underperformance_rank = row_number(desc(g_minus_xg_conceded))
  ) |> 
  arrange(offense_overperformance_rank)

pmax(max(abs(xg_by_team_ranks$g_minus_xg)), max(abs(xg_by_team_ranks$g_minus_xg_conceded)))
max_value <- 20
xlims <- c(-max_value, max_value)
ylims = c(-max_value, max_value)

# create data frame with quadrant labels
quadrant_labels <- tibble(
  label = c(
    "Over-performing Offense &\nUnder-performing Defense",
    "Under-performing Offense &\nUnder-performing Defense",
    "Over-performing Offense &\nOver-performing Defense",
    "Under-performing Offense &\nOver-performing Defense"
  ),
  x = c(xlims[2], xlims[1], xlims[2], xlims[1]),
  y = c(ylims[2], ylims[2], ylims[1], ylims[1])
)
