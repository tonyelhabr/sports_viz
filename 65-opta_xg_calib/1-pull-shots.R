library(worldfootballR)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

match_shooting <- load_fb_match_shooting(
  country = 'ENG', #A c('ENG', 'ESP'),
  tier = '1st',
  gender = 'M'
)

shots <- match_shooting |> 
  # janitor::clean_names() |> 
  transmute(
    # match_url = MatchURL,
    country = Country,
    season = Season_End_Year,
    date = ymd(Date),
    half = Match_Half,
    minute = Minute,
    team = Squad,
    player = str_remove(Player, ' \\(.*$'),
    xg = as.numeric(xG),
    psxg = as.numeric(PSxG),
    outcome = Outcome,
    is_penalty = str_detect(Player, '\\(pen\\)'),
    is_goal = outcome == 'Goal',
    distance = as.integer(Distance),
    body_part = `Body Part`,
    sca1 = Event_SCA_1,
    sca2 = Event_SCA_2
  )

shots |> 
  group_by(is_penalty) |> 
  summarize(
    n = n(),
    xg = sum(xg, na.rm = TRUE),
    g = sum(is_goal)
  ) |> 
  ungroup()

n_shots_by_player <- shots |> count(team, player, sort = TRUE)

footedness <- shots |> 
  filter(body_part %in% sprintf('%s Foot', c('Left', 'Right'))) |> 
  count(player, foot = tolower(str_remove(body_part, ' Foot'))) |> 
  pivot_wider(
    names_from = foot,
    values_from = n,
    values_fill = 0L
  ) |> 
  mutate(
    total = left + right # ,
    # left_prop = left / total,
    # right_prop = right / total
  ) |> 
  ebbr::add_ebb_estimate(
    x = right,
    n = total,
    prior_subset = total >= 20,
    cred_level = 0.95
  ) |> 
  mutate(
    foot = ifelse(.fitted >= 0.5, 'right', 'left'),
    is_uncertain = .low < 0.5 & .high >= 0.5
  ) |> 
  arrange(desc(total))
footedness |> filter(is_uncertain)
footedness |> 
  filter(.fitted < 0.5, total < 10) |> 
  count(left, total, sort = TRUE)
footedness |> 
  filter(.fitted < 0.5, total == 10)
footedness |> 
  filter(.raw < 0.5, total >= 10, total <= 12)
