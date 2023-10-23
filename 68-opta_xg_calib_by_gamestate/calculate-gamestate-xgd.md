Calculating Game-state xGD with FBref data
================

## Data pull

First, we pull raw data from pre-saved `{worldfootballR}` release data,
starting with match shots.

``` r
## data scrape
library(worldfootballR)  ## version: 0.6.4.9

## data manipulation
library(dplyr)
library(lubridate)
library(tidyr)
library(rlang)

COUNTRIES <- 'USA'
GENDERS <- 'M'
TIERS <- '1st'
SEASON_END_YEARS <- 2023

raw_shots <- worldfootballR::load_fb_match_shooting(
  country = COUNTRIES,
  gender = GENDERS,
  tier = TIERS,
  season_end_year = SEASON_END_YEARS
)
dplyr::glimpse(raw_shots)
#> Rows: 15,116
#> Columns: 23
#> $ MatchURL         <chr> "https://fbref.com/en/matches/48a684ed/Nashville-S…
#> $ Date             <chr> "2023-02-25", "2023-02-25", "2023-02-25", "2023-02…
#> $ Squad            <chr> "Nashville", "Nashville", "Nashville", "Nashville"…
#> $ Home_Away        <chr> "Home", "Home", "Home", "Home", "Home", "Home", "H…
#> $ Match_Half       <dbl> 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2,…
#> $ Minute           <chr> "6", "13", "31", "34", "45+1", "51", "73", "80", "…
#> $ Player           <chr> "Jacob Shaffelburg", "Sean Davis", "Teal Bunbury",…
#> $ Player_Href      <chr> "/en/players/339a2561/Jacob-Shaffelburg", "/en/pla…
#> $ xG               <chr> "0.39", "0.09", "0.03", "0.25", "0.04", "0.02", "0…
#> $ PSxG             <chr> "0.47", "", "0.06", "0.74", "", "", "", "0.96", ""…
#> $ Outcome          <chr> "Saved", "Off Target", "Saved", "Goal", "Off Targe…
#> $ Distance         <chr> "16", "18", "29", "8", "17", "25", "28", "11", "22…
#> $ `Body Part`      <chr> "Right Foot", "Left Foot", "Right Foot", "Right Fo…
#> $ Notes            <chr> "", "Volley", "Deflected", "Volley", "", "", "", "…
#> $ Player_SCA_1     <chr> "Randall Leal", "Aníbal Godoy", "Jacob Shaffelburg…
#> $ Event_SCA_1      <chr> "Pass (Live)", "Pass (Live)", "Pass (Live)", "Pass…
#> $ Player_SCA_2     <chr> "Walker Zimmerman", "Jack Maher", "Joe Willis", "T…
#> $ Event_SCA_2      <chr> "Pass (Live)", "Pass (Live)", "Pass (Live)", "Foul…
#> $ Competition_Name <chr> "Major League Soccer", "Major League Soccer", "Maj…
#> $ Gender           <chr> "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", …
#> $ Country          <chr> "USA", "USA", "USA", "USA", "USA", "USA", "USA", "…
#> $ Tier             <chr> "1st", "1st", "1st", "1st", "1st", "1st", "1st", "…
#> $ Season_End_Year  <int> 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 20…
```

Given a match URL like
[this](https://fbref.com/en/matches/82503f4e/Atlanta-United-Inter-Miami-September-16-2023-Major-League-Soccer),
`worldfootballR::load_fb_match_shooting()` provides data from the
“Shots” table on the page.

![](c:/users/antho/downloads/match-shots.png)

While it might seem like the shots table is all we’d need to calculate
expected goal difference (xGD), FBref’s match shot log table doesn’t
include own goals. Nonetheless, we can use
`worldfootballR::load_fb_match_summary()` to extract timestamps for own
goals from the “Match Summary” timeline.

![](c:/users/antho/downloads/match-summary.png)

``` r
raw_match_summaries <- worldfootballR::load_fb_match_summary(
  country = COUNTRIES,
  gender = GENDERS,
  tier = TIERS,
  season_end_year = SEASON_END_YEARS
)
dplyr::glimpse(raw_match_summaries)
#> Rows: 9,450
#> Columns: 33
#> $ MatchURL          <chr> "https://fbref.com/en/matches/48a684ed/Nashville-…
#> $ League            <chr> "Major League Soccer", "Major League Soccer", "Ma…
#> $ Match_Date        <chr> "2023-02-25", "2023-02-25", "2023-02-25", "2023-0…
#> $ Matchweek         <chr> "Major League Soccer (Regular Season)", "Major Le…
#> $ Home_Team         <chr> "Nashville SC", "Nashville SC", "Nashville SC", "…
#> $ Home_Formation    <chr> "4-2-3-1", "4-2-3-1", "4-2-3-1", "4-2-3-1", "4-2-…
#> $ Home_Score        <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2…
#> $ Home_xG           <dbl> 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3,…
#> $ Home_Goals        <chr> "Walker Zimmerman · 34&rsquor; Jacob Shaffelburg …
#> $ Home_Yellow_Cards <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",…
#> $ Home_Red_Cards    <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0",…
#> $ Away_Team         <chr> "New York City FC", "New York City FC", "New York…
#> $ Away_Formation    <chr> "4-2-3-1", "4-2-3-1", "4-2-3-1", "4-2-3-1", "4-2-…
#> $ Away_Score        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1…
#> $ Away_xG           <dbl> 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4,…
#> $ Away_Goals        <chr> "", "", "", "", "", "", "", "", "", "", "", "", "…
#> $ Away_Yellow_Cards <chr> "4", "4", "4", "4", "4", "4", "4", "4", "4", "4",…
#> $ Away_Red_Cards    <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0",…
#> $ Game_URL          <chr> "https://fbref.com/en/matches/48a684ed/Nashville-…
#> $ Team              <chr> "New York City FC", "Nashville SC", "Nashville SC…
#> $ Home_Away         <chr> "Away", "Home", "Home", "Away", "Away", "Home", "…
#> $ Event_Time        <dbl> 28, 34, 58, 62, 70, 72, 74, 75, 80, 82, 82, 83, 9…
#> $ Is_Pens           <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, …
#> $ Event_Half        <dbl> 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2…
#> $ Event_Type        <chr> "Yellow Card", "Goal", "Yellow Card", "Yellow Car…
#> $ Event_Players     <chr> "Braian Cufré", "Walker Zimmerman Assist: Fafà Pi…
#> $ Score_Progression <chr> "0:0", "1:0", "1:0", "1:0", "1:0", "1:0", "1:0", …
#> $ Penalty_Number    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ Competition_Name  <chr> "Major League Soccer", "Major League Soccer", "Ma…
#> $ Gender            <chr> "M", "M", "M", "M", "M", "M", "M", "M", "M", "M",…
#> $ Country           <chr> "USA", "USA", "USA", "USA", "USA", "USA", "USA", …
#> $ Tier              <chr> "1st", "1st", "1st", "1st", "1st", "1st", "1st", …
#> $ Season_End_Year   <int> 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2…
```

## Data wrangling

Now we start to clean up the raw data. Starting with the match summary
data, we:

1.  Create a `match_id` field, to make it easy to join this data set
    with the shots data set.
2.  Clean up the time fields, `min` and `min_added`.
3.  Rename existing columns.

``` r
## Extract the from "47880eb7" from "https://fbref.com/en/matches/47880eb7/Liverpool-Manchester-City-November-10-2019-Premier-League"
extract_fbref_match_id <- function(match_url) {
  basename(dirname(match_url))
}

match_summaries <- raw_match_summaries |> 
  dplyr::group_by(MatchURL) |> 
  dplyr::mutate(
    match_summary_rn = dplyr::row_number(dplyr::desc(Event_Time)),
    match_has_no_penalties = all(Event_Type != 'Penalty')
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    match_has_no_goals = Away_Score == 0 & Home_Score == 0
  ) |> 
  ## Drop non-shot events, e.g. card and substitution events. 
  ##   Always keep the first timeline event, so that we're not accidentally dropping matches.
  dplyr::filter(
    Event_Type %in% c('Goal', 'Own Goal', 'Penalty') | 
      ## don't drop games with no goals
      (match_has_no_goals & match_has_no_penalties & match_summary_rn == 1)
  ) |> 
  dplyr::transmute(
    match_id = extract_fbref_match_id(MatchURL),
    season = Season_End_Year,
    gender = Gender,
    tier = Tier,
    date = lubridate::ymd(Match_Date),
    home_team = Home_Team ,
    away_team = Away_Team,
    period = as.integer(Event_Half),
    ## ensure that min always has a value
    min = dplyr::case_when(
      period == 1L & Event_Time > 45L ~ 45L, 
      period == 2L & Event_Time > 90L ~ 90L,
      .default = Event_Time
    ) |> as.integer(),
    min_added = dplyr::case_when(
      period == 1L & Event_Time > 45 ~ Event_Time - 45L, 
      period == 2L & Event_Time > 90 ~ Event_Time - 90L,
      .default = NA_integer_
    ),
    home_g = as.integer(gsub('[:].*$', '', Score_Progression)), ## after event
    away_g = as.integer(gsub('^.*[:]', '', Score_Progression)),
    is_own_goal = Event_Type == 'Own Goal',
    ## out of scope for this, but could be useful for other context
    ## is_penalty = Event_Type == 'Penalty',
    team = Team,
    player = Event_Players
  )
dplyr::glimpse(match_summaries)
#> Rows: 1,729
#> Columns: 15
#> $ match_id    <chr> "48a684ed", "48a684ed", "1861e533", "1861e533", "1861e5…
#> $ season      <int> 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2…
#> $ gender      <chr> "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", …
#> $ tier        <chr> "1st", "1st", "1st", "1st", "1st", "1st", "1st", "1st",…
#> $ date        <date> 2023-02-25, 2023-02-25, 2023-02-25, 2023-02-25, 2023-0…
#> $ home_team   <chr> "Nashville SC", "Nashville SC", "FC Cincinnati", "FC Ci…
#> $ away_team   <chr> "New York City FC", "New York City FC", "Houston Dynamo…
#> $ period      <int> 1, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 2, 2, 1, 1, 2…
#> $ min         <int> 34, 80, 19, 45, 48, 48, 12, 39, 90, 90, 28, 45, 52, 72,…
#> $ min_added   <dbl> NA, NA, NA, 2, NA, NA, NA, NA, 3, 9, NA, 3, NA, NA, NA,…
#> $ home_g      <int> 1, 2, 1, 1, 2, 0, 0, 0, 1, 2, 0, 1, 2, 3, 4, 1, 0, 1, 2…
#> $ away_g      <int> 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1…
#> $ is_own_goal <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
#> $ team        <chr> "Nashville SC", "Nashville SC", "FC Cincinnati", "Houst…
#> $ player      <chr> "Walker Zimmerman Assist: Fafà Picault", "Jacob Shaffel…
```

Next, we start to clean up the shots data frame. We do data wrangling
similar to that for match summaries.

``` r
long_shots <- raw_shots |> 
  dplyr::transmute(
    match_id = extract_fbref_match_id(MatchURL),
    period = as.integer(Match_Half),
    ## convert "45+2" to "45"
    min = ifelse(
      grepl('[+]', Minute),
      as.integer(gsub('(^[0-9]+)[+]([0-9]+$)', '\\1', Minute)), 
      as.integer(Minute)
    ),
    ## convert "45+2" to "2"
    min_added = ifelse(
      grepl('[+]', Minute), 
      as.integer(gsub('(^[0-9]+)[+]([0-9]+$)', '\\2', Minute)), 
      NA_integer_
    ),
    is_home = Home_Away == 'Home',
    team = Squad,
    player = Player,
    is_goal = Outcome == 'Goal',
    xg = as.double(xG)
    ## This is out of the scope of game state calcs, but it's worth noting that 
    ##   this is a fairly safe assumption for penalties. We can also get an
    ##   indication of whether the the shot is a penalty from the match summary,
    ##   which is even safer.
    # is_penalty = dplyr::coalesce((Distance == '13' & round(xg, 2) == 0.79), FALSE)
  )
dplyr::glimpse(long_shots)
#> Rows: 15,116
#> Columns: 9
#> $ match_id  <chr> "48a684ed", "48a684ed", "48a684ed", "48a684ed", "48a684ed…
#> $ period    <int> 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, …
#> $ min       <int> 6, 13, 31, 34, 45, 51, 73, 80, 83, 19, 30, 41, 45, 48, 61…
#> $ min_added <int> NA, NA, NA, NA, 1, NA, NA, NA, NA, NA, NA, NA, 2, NA, NA,…
#> $ is_home   <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FAL…
#> $ team      <chr> "Nashville", "Nashville", "Nashville", "Nashville", "Nash…
#> $ player    <chr> "Jacob Shaffelburg", "Sean Davis", "Teal Bunbury", "Walke…
#> $ is_goal   <lgl> FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FAL…
#> $ xg        <dbl> 0.39, 0.09, 0.03, 0.25, 0.04, 0.02, 0.02, 0.45, 0.04, 0.0…
```

### Accounting for Own goals

Now, for the “ugly” part of all this shot data wrangling–handling own
goals.

First, we inject “synthetic” records into the shots data for every case
where the match summary indicates that there is an own goal.

``` r
long_shots_with_own_goals <- dplyr::bind_rows(
  long_shots |> 
    dplyr::transmute(
      match_id,
      period,
      min,
      min_added,
      is_home,
      team,
      player,
      is_goal,
      xg,
      is_own_goal = FALSE
    ),
  ## synthetic events for own goals
  match_summaries |> 
    dplyr::filter(
      is_own_goal
    ) |> 
    dplyr::transmute(
      match_id,
      period,
      min,
      min_added,
      is_home = team == home_team,
      team,
      player,
      is_goal = TRUE,
      xg = NA_real_,
      is_own_goal = TRUE
    )
)
dplyr::glimpse(long_shots_with_own_goals)
#> Rows: 15,174
#> Columns: 10
#> $ match_id    <chr> "48a684ed", "48a684ed", "48a684ed", "48a684ed", "48a684…
#> $ period      <int> 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1…
#> $ min         <int> 6, 13, 31, 34, 45, 51, 73, 80, 83, 19, 30, 41, 45, 48, …
#> $ min_added   <dbl> NA, NA, NA, NA, 1, NA, NA, NA, NA, NA, NA, NA, 2, NA, N…
#> $ is_home     <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, F…
#> $ team        <chr> "Nashville", "Nashville", "Nashville", "Nashville", "Na…
#> $ player      <chr> "Jacob Shaffelburg", "Sean Davis", "Teal Bunbury", "Wal…
#> $ is_goal     <lgl> FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, F…
#> $ xg          <dbl> 0.39, 0.09, 0.03, 0.25, 0.04, 0.02, 0.02, 0.45, 0.04, 0…
#> $ is_own_goal <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
```

``` r
clean_shots <- long_shots_with_own_goals |> 
  ## Join once to get meta-information about the game
  dplyr::inner_join(
    match_summaries |>
      dplyr::distinct(match_id, home_team, away_team),
    by = dplyr::join_by(match_id),
    relationship = 'many-to-one'
  ) |> 
  dplyr::mutate(
    home_g = dplyr::case_when(
      ## Note that fotmob would list the away team for an own goal but fbref lists the home team
      (is_goal | is_own_goal) & is_home ~ 1L,
      is_own_goal & is_home ~ 1L,
      TRUE ~ 0L
    ),
    away_g = dplyr::case_when(
      (is_goal | is_own_goal) & !is_home ~ 1L,
      TRUE ~ 0L
    ),
    home_xg = dplyr::case_when(
      is_home ~ dplyr::coalesce(xg, 0),
      TRUE ~ 0L ## even for own goals
    ),
    away_xg = dplyr::case_when(
      !is_home ~ dplyr::coalesce(xg, 0),
      TRUE ~ 0L
    )
  ) |>
  dplyr::group_by(match_id) |> 
  dplyr::mutate(
    shot_idx = dplyr::row_number((min + dplyr::coalesce(min_added, 0L)))
  ) |> 
  dplyr::ungroup() |> 
  dplyr::transmute(
    shot_id = sprintf('%s-%02d', match_id, shot_idx),
    match_id,
    period,
    min,
    min_added,
    is_home,
    is_goal,
    is_own_goal,
    player,
    home_team,
    away_team,
    home_g,
    away_g,
    home_xg,
    away_xg
  )
dplyr::glimpse(clean_shots)
#> Rows: 15,174
#> Columns: 15
#> $ shot_id     <chr> "48a684ed-01", "48a684ed-02", "48a684ed-05", "48a684ed-…
#> $ match_id    <chr> "48a684ed", "48a684ed", "48a684ed", "48a684ed", "48a684…
#> $ period      <int> 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1…
#> $ min         <int> 6, 13, 31, 34, 45, 51, 73, 80, 83, 19, 30, 41, 45, 48, …
#> $ min_added   <dbl> NA, NA, NA, NA, 1, NA, NA, NA, NA, NA, NA, NA, 2, NA, N…
#> $ is_home     <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, F…
#> $ is_goal     <lgl> FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, F…
#> $ is_own_goal <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
#> $ player      <chr> "Jacob Shaffelburg", "Sean Davis", "Teal Bunbury", "Wal…
#> $ home_team   <chr> "Nashville SC", "Nashville SC", "Nashville SC", "Nashvi…
#> $ away_team   <chr> "New York City FC", "New York City FC", "New York City …
#> $ home_g      <int> 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ away_g      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ home_xg     <dbl> 0.39, 0.09, 0.03, 0.25, 0.04, 0.02, 0.02, 0.45, 0.04, 0…
#> $ away_xg     <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0…
```

### Double Counting Shot Events

Up to this point, we have one record per shot. But, to calculate goals
and expected goals (xG) conceded for any given team at any given point
in a game, it’s easiest to “double count” each shot event, once from
each team’s perspective.

To do that, we first re-assign (“re-stack”) teams and goals based on the
home and away teams’ perspectives.

``` r
restacked_shots <- dplyr::bind_rows(
  clean_shots |> 
    dplyr::filter(is_home) |> 
    dplyr::transmute(
      shot_id,
      match_id,
      period,
      min,
      min_added,
      is_home,
      is_goal,
      is_own_goal,
      player,
      team = home_team,
      opponent = away_team,
      g = home_g,
      g_conceded = away_g,
      xg = home_xg,
      xg_conceded = away_xg
    ),
  clean_shots |> 
    dplyr::filter(!is_home) |> 
    dplyr::transmute(
      shot_id,
      match_id,
      period,
      min,
      min_added,
      is_home,
      is_goal,
      is_own_goal,
      player,
      team = away_team,
      opponent = home_team,
      g = away_g,
      g_conceded = home_g,
      xg = away_xg,
      xg_conceded = home_xg
    )
)
```

Then, we replicate the whole data frame, indiciating whether we’re
looking at the shot events from a given team’s point of view
(`pov = "primary"`) or their opponents’ point of view (`"secondary"`).

``` r
doublecounted_restacked_shots <- dplyr::bind_rows(
  restacked_shots |> dplyr::mutate(pov = 'primary', .before = 1),
  restacked_shots |> 
    ## re-assign to temporary variable names first, so that way we don't accidentlaly overwrite information
    dplyr::rename(
      team1 = team,
      team2 = opponent,
      g1 = g,
      g2 = g_conceded,
      xg1 = xg,
      xg2 = xg_conceded
    ) |> 
    ## then formally re-assign columns
    dplyr::rename(
      team = team2,
      opponent = team1,
      g = g2,
      g_conceded = g1,
      xg = xg2,
      xg_conceded = xg1
    ) |> 
    dplyr::mutate(
      is_home = !is_home
    ) |> 
    dplyr::mutate(
      pov = 'secondary',
      .before = 1
    )
) |> 
  dplyr::arrange(match_id, shot_id, pov)
dplyr::glimpse(doublecounted_restacked_shots)
#> Rows: 30,348
#> Columns: 16
#> $ pov         <chr> "primary", "secondary", "primary", "secondary", "primar…
#> $ shot_id     <chr> "00069d73-01", "00069d73-01", "00069d73-02", "00069d73-…
#> $ match_id    <chr> "00069d73", "00069d73", "00069d73", "00069d73", "00069d…
#> $ period      <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ min         <int> 2, 2, 4, 4, 9, 9, 20, 20, 24, 24, 32, 32, 34, 34, 35, 3…
#> $ min_added   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ is_home     <lgl> TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRU…
#> $ is_goal     <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
#> $ is_own_goal <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
#> $ player      <chr> "Hany Mukhtar", "Hany Mukhtar", "Teal Bunbury", "Teal B…
#> $ team        <chr> "Nashville SC", "Chicago Fire", "Nashville SC", "Chicag…
#> $ opponent    <chr> "Chicago Fire", "Nashville SC", "Chicago Fire", "Nashvi…
#> $ g           <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ g_conceded  <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ xg          <dbl> 0.04, 0.00, 0.17, 0.00, 0.02, 0.00, 0.12, 0.00, 0.05, 0…
#> $ xg_conceded <dbl> 0.00, 0.04, 0.00, 0.17, 0.00, 0.02, 0.00, 0.12, 0.00, 0…
```

### Calculating Cumulative Goals

Finally, we calculate cumulative goals and xG scored and conceded. Then
it’s straightforward to calculate the game state at any given point in a
match. (Keep in mind that the xG associated with a shot that is scored
and changes the game state is associated with the post-shot game state,
not the pre-shot game state. This may or may not matter for your
individual analysis.)

``` r
cumu_doublecounted_restacked_shots <- doublecounted_restacked_shots |> 
  dplyr::group_by(match_id, team) |> 
  dplyr::mutate(
    dplyr::across(
      c(g, g_conceded),
      list(cumu = cumsum)
    )
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    game_state = g_cumu - g_conceded_cumu
  )
dplyr::glimpse(cumu_doublecounted_restacked_shots)
#> Rows: 30,348
#> Columns: 19
#> $ pov             <chr> "primary", "secondary", "primary", "secondary", "pr…
#> $ shot_id         <chr> "00069d73-01", "00069d73-01", "00069d73-02", "00069…
#> $ match_id        <chr> "00069d73", "00069d73", "00069d73", "00069d73", "00…
#> $ period          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ min             <int> 2, 2, 4, 4, 9, 9, 20, 20, 24, 24, 32, 32, 34, 34, 3…
#> $ min_added       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ is_home         <lgl> TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE,…
#> $ is_goal         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
#> $ is_own_goal     <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
#> $ player          <chr> "Hany Mukhtar", "Hany Mukhtar", "Teal Bunbury", "Te…
#> $ team            <chr> "Nashville SC", "Chicago Fire", "Nashville SC", "Ch…
#> $ opponent        <chr> "Chicago Fire", "Nashville SC", "Chicago Fire", "Na…
#> $ g               <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ g_conceded      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ xg              <dbl> 0.04, 0.00, 0.17, 0.00, 0.02, 0.00, 0.12, 0.00, 0.0…
#> $ xg_conceded     <dbl> 0.00, 0.04, 0.00, 0.17, 0.00, 0.02, 0.00, 0.12, 0.0…
#> $ g_cumu          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ g_conceded_cumu <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ game_state      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
```

Finally, we bring everything together to create a singular data frame
from which it is straightforward to calculate xGD with respect to game
state.

``` r
match_results <- match_summaries |> 
  dplyr::distinct(
    match_id,
    season,
    date,
    home_team,
    away_team
  )

ORDERED_GAME_STATE_LABELS <- c('trailing', 'neutral', 'leading')
game_state_shots <- cumu_doublecounted_restacked_shots |> 
  dplyr::inner_join(
    match_results,
    by = dplyr::join_by(match_id)
  ) |> 
  dplyr::transmute(
    pov,
    match_id,
    season,
    date,
    home_team,
    away_team,
    team,
    player,
    shot_id,
    period,
    min,
    min_added,
    time = min + dplyr::coalesce(min_added, 0L),
    xgd = xg - xg_conceded,
    game_state = cut(
      game_state,
      breaks = c(-Inf, -1, 0, Inf),
      labels = ORDERED_GAME_STATE_LABELS
    )
  ) |> 
  ## intentionally don't include `pov`
  dplyr::group_by(match_id, team) |> 
  dplyr::arrange(shot_id, .by_group = TRUE) |> 
  dplyr::mutate(
    pre_shot_game_state = dplyr::lag(game_state, default = ORDERED_GAME_STATE_LABELS[2])
  ) |> 
  dplyr::ungroup()
dplyr::glimpse(game_state_shots)
#> Rows: 30,348
#> Columns: 16
#> $ pov                 <chr> "secondary", "secondary", "secondary", "seconda…
#> $ match_id            <chr> "00069d73", "00069d73", "00069d73", "00069d73",…
#> $ season              <int> 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023,…
#> $ date                <date> 2023-05-06, 2023-05-06, 2023-05-06, 2023-05-06…
#> $ home_team           <chr> "Nashville SC", "Nashville SC", "Nashville SC",…
#> $ away_team           <chr> "Chicago Fire", "Chicago Fire", "Chicago Fire",…
#> $ team                <chr> "Chicago Fire", "Chicago Fire", "Chicago Fire",…
#> $ player              <chr> "Hany Mukhtar", "Teal Bunbury", "Hany Mukhtar",…
#> $ shot_id             <chr> "00069d73-01", "00069d73-02", "00069d73-03", "0…
#> $ period              <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2,…
#> $ min                 <int> 2, 4, 9, 20, 24, 32, 34, 35, 39, 40, 41, 42, 45…
#> $ min_added           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ time                <dbl> 2, 4, 9, 20, 24, 32, 34, 35, 39, 40, 41, 42, 45…
#> $ xgd                 <dbl> -0.04, -0.17, -0.02, -0.12, -0.05, -0.08, -0.04…
#> $ game_state          <fct> neutral, neutral, neutral, neutral, neutral, ne…
#> $ pre_shot_game_state <fct> neutral, neutral, neutral, neutral, neutral, ne…
```

### Padding end-of-match events

Oh wait! We should probably account for length of time spent in a game
state when contextualizing other statistics, e.g. xGD. To do that
properly, we should add more synthetic records for end of halves.

Unfortunately, FBref does not provide the exact ending minute of each
half, as far as I know. Thus, we’ll “pad” our data with artificial
records to mark the end of halves–the 45th minute in the first half /
90th minute in the second half–using some heuristics.

1.  If there are no shots after the last regular minute in a half, we
    add 3 minutes. (3 minutes is about the median amount of minutes
    allocated for extra time.)
2.  If the last shot is after the last regular minute in a half, we take
    the maximum of (a) adding 3 minutes beyond the last regular minute
    (like (1)) and (b) adding one minute beyond the last shot.

``` r
LAST_MIN_BUFFER <- 3
last_min_pad <- game_state_shots |>
  dplyr::select(
    match_id,
    season,
    date,
    team,
    pre_shot_game_state,
    period,
    time
  ) |> 
  dplyr::group_by(match_id, team, period) |>
  dplyr::slice_max(time, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    xgd = 0,
    last_regular_min = ifelse(period == 1L, 45L, 90L),
    time = pmax(last_regular_min + LAST_MIN_BUFFER, time + 1)
  )

padded_game_state_shots <- dplyr::bind_rows(
  game_state_shots,
  last_min_pad
) |> 
  dplyr::arrange(match_id, time)

game_state_shots_and_durations <- padded_game_state_shots |> 
  dplyr::group_by(match_id, team) |> 
  dplyr::mutate(
    prev_period = dplyr::lag(period),
    prev_time = dplyr::lag(time)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    dur = dplyr::case_when(
      period == 1L & is.na(prev_period) ~ time - 0L,
      period == 2L & period != prev_period ~ time - 45L,
      TRUE ~ time - prev_time
    )
  )
dplyr::glimpse(game_state_shots_and_durations)
#> Rows: 32,320
#> Columns: 20
#> $ pov                 <chr> "secondary", "primary", "secondary", "primary",…
#> $ match_id            <chr> "00069d73", "00069d73", "00069d73", "00069d73",…
#> $ season              <int> 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023,…
#> $ date                <date> 2023-05-06, 2023-05-06, 2023-05-06, 2023-05-06…
#> $ home_team           <chr> "Nashville SC", "Nashville SC", "Nashville SC",…
#> $ away_team           <chr> "Chicago Fire", "Chicago Fire", "Chicago Fire",…
#> $ team                <chr> "Chicago Fire", "Nashville SC", "Chicago Fire",…
#> $ player              <chr> "Hany Mukhtar", "Hany Mukhtar", "Teal Bunbury",…
#> $ shot_id             <chr> "00069d73-01", "00069d73-01", "00069d73-02", "0…
#> $ period              <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ min                 <int> 2, 2, 4, 4, 9, 9, 20, 20, 24, 24, 32, 32, 34, 3…
#> $ min_added           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ time                <dbl> 2, 2, 4, 4, 9, 9, 20, 20, 24, 24, 32, 32, 34, 3…
#> $ xgd                 <dbl> -0.04, 0.04, -0.17, 0.17, -0.02, 0.02, -0.12, 0…
#> $ game_state          <fct> neutral, neutral, neutral, neutral, neutral, ne…
#> $ pre_shot_game_state <fct> neutral, neutral, neutral, neutral, neutral, ne…
#> $ last_regular_min    <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ prev_period         <int> NA, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ prev_time           <dbl> NA, NA, 2, 2, 4, 4, 9, 9, 20, 20, 24, 24, 32, 3…
#> $ dur                 <dbl> 2, 2, 2, 2, 5, 5, 11, 11, 4, 4, 8, 8, 2, 2, 1, …
```

## Data analysis

As the saying goes, “80% of data analysis/science is data cleaning”.
Well, that rings true here, as all we need to do at this point is
perform a few common `{dplyr}` and `{tidyr}` actions to arrive at xGD by
game state.

``` r
agg_game_state_shots <- game_state_shots_and_durations |> 
  dplyr::group_by(
    team,
    pre_shot_game_state
  ) |> 
  dplyr::summarize(
    dplyr::across(
      c(
        xgd,
        dur
      ),
      sum
    )
  ) |> 
  dplyr::ungroup() |> 
  tidyr::pivot_wider(
    names_from = pre_shot_game_state,
    values_from = c(xgd, dur)
  ) |> 
  dplyr::arrange(desc(dur_leading))
agg_game_state_shots
#> # A tibble: 29 × 7
#>    team       xgd_trailing xgd_neutral xgd_leading dur_trailing dur_neutral dur_leading
#>    <chr>             <dbl>       <dbl>       <dbl>        <dbl>       <dbl>       <dbl>
#>  1 Columbus …         2.74       8.51         4.47          590        1335        1465
#>  2 Orlando C…         1.43      -3.82         6.54          559        1603        1212
#>  3 Atlanta U…         2.76      -0.260        2.37          897        1298        1180
#>  4 Sporting …        -4.03      -6.5          5.06          792        1391        1163
#>  5 Houston D…        -5.05      -0.67         8.01          884        1380        1104
#>  6 St. Louis…        -0.74      -4           -3.93          620        1617        1100
#>  7 New Engla…         1.29      -1.46         0.63          559        1702        1054
#>  8 Vancouver…         0.49       7.97         0.99          724        1584        1029
#>  9 Charlotte…        -0.52      -6.23        -7.44          713        1643        1005
#> 10 Los Angel…         1.87       3.71        10.8           697        1716        1001
#> # ℹ 19 more rows
```

Houston Dynamo fans may have noticed that their team seemed to really
perform well towards the end of the season. Indeed, they spent more time
leading matches than all other MLS teams after the beginning of the
Leagues Cup.

``` r
agg_game_state_shots_leagues_cup <- game_state_shots_and_durations |> 
  dplyr::group_by(
    team,
    pre_shot_game_state,
    is_after_leagues_cup = date >= lubridate::ymd('2023-07-21')
  ) |> 
  dplyr::summarize(
    dplyr::across(
      c(
        xgd,
        dur
      ),
      sum
    )
  ) |> 
  dplyr::ungroup() |> 
  tidyr::pivot_wider(
    names_from = pre_shot_game_state,
    values_from = c(xgd, dur)
  ) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    total_dur = sum(dplyr::c_across(dplyr::starts_with('dur')))
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    prop_leading = dur_leading / total_dur
  ) |>
  dplyr::filter(is_after_leagues_cup) |> 
  dplyr::select(
    team,
    xgd_leading,
    dur_leading,
    total_dur,
    prop_leading
  ) |> 
  dplyr::arrange(desc(prop_leading))
agg_game_state_shots_leagues_cup
#> # A tibble: 29 × 5
#>    team                xgd_leading dur_leading total_dur prop_leading
#>    <chr>                     <dbl>       <dbl>     <dbl>        <dbl>
#>  1 Houston Dynamo           2.37           655      1101        0.595
#>  2 Sporting KC              4.87           467       903        0.517
#>  3 Columbus Crew            3.78           527      1110        0.475
#>  4 Atlanta United          -0.86           458       996        0.460
#>  5 Charlotte FC            -1.5            423      1105        0.383
#>  6 Orlando City             4.4            363      1077        0.337
#>  7 Seattle Sounders FC     -1.28           314       962        0.326
#>  8 Portland Timbers        -0.0500         330      1102        0.299
#>  9 Los Angeles FC           5.75           308      1081        0.285
#> 10 Philadelphia Union      -1.65           299      1079        0.277
#> # ℹ 19 more rows
```
