
# Data source: https://data.world/sportsvizsunday/2020-september-champions-league-shots
# setup ----
library(tidyverse)
events <- here::here('data-raw', '07', 'CL2020_final8_events.csv') %>% read_csv()
events

tracking <- 
  here::here('data-raw', '07', 'CL2020_final8_tracking.csv') %>% 
  read_csv() %>% 
  select(-1)
tracking

play_filt <- 'PSG [2]-1 Atalanta'
# play_filt <- 'Manchester City [1]-1 Lyon'
events_filt <- events %>% filter(play == play_filt)
tracking_filt <- tracking %>% filter(play == play_filt)
path_tracking_data <- here::here('data', '07_tracking_data.rds')
if(!fs::file_exists(path_tracking_data)) {
  #func to download and melt tracking data
  #will use game 1
  get_tracking_data <- function(file, directory = "metrica-sports/sample-data", x_adj = 120, y_adj = 80) {
    #build url
    url <- paste0("https://raw.githubusercontent.com/", directory, "/master/data/", file)
    #read data
    data <- read_csv(url, skip = 2)
    
    #fix names
    names(data)[grep("^X[0-9]*$", names(data))-1] <- paste0(names(data)[grep("^X[0-9]*$", names(data))-1], "_x")
    names(data)[grep("^X[0-9]*$", names(data))] <- gsub("_x$", "_y", names(data)[grep("^X[0-9]*$", names(data))-1])
    
    #melt it from long to wide
    melted_data <- data %>%
      pivot_longer(cols = starts_with("Player")) %>%
      separate(name, into = c("player", "coord"), sep = "_") %>%
      pivot_wider(names_from = "coord", values_from = "value") %>%
      rename(time = `Time [s]`) %>%
      rename_all(tolower) %>%
      #add the team info
      #scale coords to statsbomb spec
      mutate(team = gsub("(.*)(Home_Team|Away_Team)(\\..*)", "\\2", file)) %>%
      mutate_at(vars(ends_with("x")), ~.x * x_adj) %>%
      mutate_at(vars(ends_with("y")), ~.x * y_adj) %>%
      arrange(player, frame) %>%
      #some missing values on the ball location
      #will just say ball stays where it is when no location data
      #could interpolate but w/e
      mutate(ball_x = zoo::na.locf(ball_x),
             ball_y = zoo::na.locf(ball_y))
    return(melted_data)
  }
  
  #run
  tracking_data <- map_df(
    c("Sample_Game_1/Sample_Game_1_RawTrackingData_Away_Team.csv", 
      "Sample_Game_1/Sample_Game_1_RawTrackingData_Home_Team.csv"),
    get_tracking_data) %>%
    filter(!is.na(x) & !is.na(y))
  
  tracking_data <-
    tracking_data %>%
    group_by(player, team, period) %>%
    #player x,y and time at t + n
    mutate(next_x = lead(x), next_y = lead(y), next_time = lead(time)) %>%
    #to develop velocity arrows per player
    mutate(forward_x = lead(x, 10), forward_y = lead(y, 10)) %>%
    ungroup()
  tracking_data
  write_rds(tracking_data, path_tracking_data)
} else {
  tracking_data <- read_rds(path_tracking_data)
}

#filters for some data to plot
player_spec <- "Player15"
#each frame is 0.04s apart, take 100 frames worth from t = 250
times_spec <- seq(250, by = 0.04, length.out = 100)

#filter
example_data <- tracking_data %>%
  filter(player == player_spec & time %in% times_spec)

#no real reason for these to be functions, but just to
#make it more obvious what we're doing
get_speed <- function(coord, next_coord, time, next_time) {
  #speed in meters per second
  speed = (next_coord - coord) / (next_time - time)
  return(speed)
}

#again very simple for illustrative purposes
get_theta <- function(x_speed, y_speed) {
  hypotenuse_speed = sqrt(x_speed^2 + y_speed^2)
  theta = acos(x_speed / hypotenuse_speed)
  return(theta)
}

x_start <- first(example_data$x)
x_end <- last(example_data$x)
y_start <- first(example_data$y)
y_end <- last(example_data$y)
t_start <- first(example_data$time)
t_end <- last(example_data$time)

#in m/s
speed_x <- get_speed(x_start, x_end, t_start, t_end)
speed_y <- get_speed(y_start, y_end, t_start, t_end)

#convert to degrees
theta <- get_theta(speed_x, speed_y)
theta_deg <- theta * (180/pi)

results <- c(speed_x, speed_y, theta_deg)
names(results) <- c("speed_x", "speed_y", "theta")
print(results)

#another simple function to find mu
get_mu <- function(location, speed) {
  mu = location + speed / 2
  return(mu)
}

mu_x <- get_mu(x_start, speed_x)
mu_y <- get_mu(y_start, speed_y)

get_srat <- function(speed_x, speed_y) {
  #find total velocity
  speed <- sqrt(speed_x^2 + abs(speed_y)^2)
  srat = (speed / 13)^2
  return(srat)
}

srat <- get_srat(speed_x, speed_y)

#allocate a few more variables from our example data
ball_x <- first(example_data$ball_x)
ball_y <- first(example_data$ball_y)

#little bit more complicated but still easy
get_ri <- function(x, y, ball_x, ball_y) {
  ball_diff <- sqrt((x - ball_x) ^ 2 + (y - ball_y)^2)
  ri = 4 + ((ball_diff^3) / ((18^3) / 6))
  return(min(ri, 10))  
}

ri <- get_ri(x_start, y_start, ball_x, ball_y)

get_R <- function(theta) {
  #R fills down first so these aren't the wrong way round
  R = matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow = 2)
  return(R)
}

R <- get_R(theta)

get_S <- function(ri, srat) {
  top_left <- ri * (1 + srat) / 2
  bottom_right <- ri * (1-srat) / 2
  S = matrix(c(top_left, 0, 0, bottom_right), nrow = 2)
}

S <- get_S(ri, srat)

get_Sigma <- function(R, S) {
  inv_R <- solve(R)
  Sigma = R %*% S %*% S %*% inv_R
  return(Sigma)
}

Sigma <- get_Sigma(R, S)

#use statsbomb coords - 120m x 80m pitch
#split into 200x200 rectangles
pitch <- expand.grid(seq(0, 120, length.out = 200), seq(0, 80, length.out = 200)) %>%
  rename(x = Var1, y = Var2)

#function to calculate I as in equation 1/13
calc_I <- function(pitch_area, x, y, mu_x, mu_y, Sigma) {
  #create vectors
  mu <- c(mu_x, mu_y)
  player_loc <- c(x, y)
  
  numerator <- mvtnorm::dmvnorm(as.matrix(pitch_area), mu, Sigma)
  denominator <- mvtnorm::dmvnorm(t(matrix(player_loc)), mu, Sigma)
  #and normalise
  norm_pdf = numerator/denominator
  return(norm_pdf)
}

#column I is the control on pitch area x,y of player I
I <- calc_I(pitch, x_start, y_start, mu_x, mu_y, Sigma)
head(mutate(pitch, I))

#test our functions on one frame of the tracking data
testing_data <- tracking_data %>%
  filter(time == 600) 

calc_PC <- function(time, next_time, ball_x, ball_y, x, y, next_x, next_y, team, player, pitch_area) {
  speed_x <- get_speed(x, next_x, time, next_time)
  speed_y <- get_speed(y, next_y, time, next_time)
  srat <- get_srat(speed_x, speed_y)
  theta <- get_theta(speed_x, speed_y)
  
  mu_x <- get_mu(x, speed_x)
  mu_y <- get_mu(y, speed_y)
  
  ri <- get_ri(x, y, ball_x, ball_y)
  
  R <- get_R(theta)
  S <- get_S(ri, srat)
  
  Sigma <- get_Sigma(R, S)
  
  pitch_area$I <- calc_I(as.matrix(pitch), x, y, mu_x, mu_y, Sigma)
  pitch_area$team <- team
  pitch_area$time <- time
  pitch_area$player <- player
  return(pitch_area)
}

testing_data
#run the pitch control function
pitch_control <- testing_data %>%
  select(time, next_time, ball_x, ball_y, x, y, next_x, next_y, player, team) %>%
  #run func
  pmap_df(., calc_PC, pitch_area = pitch) %>%
  #sum by team and area
  group_by(team, x, y) %>%
  summarise(team_sum = sum(I)) %>%
  pivot_wider(names_from = team, values_from = team_sum) %>%
  #?? - logistic function
  mutate(PC = 1 / (1 + exp(Home_Team - Away_Team)))

#get the position of the ball for this frame
ball_location <- testing_data %>%
  select(ball_x, ball_y) %>%
  unique()

#plot it all
p6 <- ggplot() +
  #pitch layout background
  ggsoccer::annotate_pitch(dimensions = ggsoccer::pitch_statsbomb) +
  #pitch control raster
  geom_tile(data = pitch_control, aes(x = x, y = y, fill = PC), alpha = 0.7) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.5) +
  #players for each team
  #also add in little vector arrows
  geom_segment(data = testing_data, aes(x = x, y = y, xend = forward_x, yend = forward_y, colour = team),
               size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = testing_data, aes(x = x, y = y, colour = team), size = 3) +
  scale_colour_manual(values = c("black", "gold"), guide = FALSE) +
  #ball location
  geom_point(data = ball_location, aes(x = ball_x, y = ball_y),
             colour = "black", fill = "white", shape = 21, size = 2.5, stroke = 2) +
  ggsoccer::theme_pitch()

p6
