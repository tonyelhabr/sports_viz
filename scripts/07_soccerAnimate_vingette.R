
library(soccerAnimate)
home_team_file <- "https://raw.githubusercontent.com/metrica-sports/sample-data/master/data/Sample_Game_1/Sample_Game_1_RawTrackingData_Home_Team.csv"
away_team_file <- "https://raw.githubusercontent.com/metrica-sports/sample-data/master/data/Sample_Game_1/Sample_Game_1_RawTrackingData_Away_Team.csv"
td <- get_tidy_data(home_team_file, away_team_file)
td
ed <- readr::read_csv("https://raw.githubusercontent.com/metrica-sports/sample-data/master/data/Sample_Game_1/Sample_Game_1_RawEventsData.csv")
goals <- events_info(ed, events = "GOAL")

# all_events <- events_info(ed, events = c("SHOT", "GOAL", "FREE KICK", "CORNER KICK"))
td %>% count(Frame, Second)
goals
td_filt <- td %>% filter(Frame == 99035)
td_filt
soccer_animate(tidy_data = td, ini_time = 3956, end_time = 3960)
td_filt
td %>% filter(Time >= 3956, Time <= 3960)
td %>% distinct(Second) %>% tail()
