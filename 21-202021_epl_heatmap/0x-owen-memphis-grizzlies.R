
# Load packages 
library(tidyverse)
library(metR)
library(nbastatR)
library(extrafont)
library(teamcolors)
library(cowplot)

# Custom theme
theme_owen <- function () { 
  theme_minimal(base_size=11, base_family="Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

# Turn off scientific notation 
options(scipen=999)

# Get NBA team names
tms <- nba_teams()
tms <- tms %>% filter(isNonNBATeam == 0) %>% select(nameTeam, idTeam, slugTeam)

# Get primary team colors 
tm.colors <- teamcolors::teamcolors
tm.colors <- tm.colors %>% filter(league == "nba")
tm.colors <- tm.colors %>% select(name, primary)

# Manually adjusted a few team colors
tm.colors <- tm.colors %>% 
  mutate(primary = case_when(
    name == "Golden State Warriors" ~ "#1D428A",
    name == "Indiana Pacers" ~ "#002D62",
    name == "Los Angeles Lakers" ~ "#552583",
    name == "San Antonio Spurs" ~ "#000000",
    name == "Oklahoma City Thunder" ~ "#EF3B24",
    name == "Charlotte Hornets" ~ "#00788C",
    name == "Utah Jazz" ~ "#00471B",
    name == "New Orleans Pelicans" ~ "#0C2340",
    TRUE ~ primary
  ))

# Load NBA court dimensions from github
devtools::source_url("https://github.com/Henryjean/NBA-Court/blob/main/CourtDimensions.R?raw=TRUE")

# Get shot data (Run only once)
# tm.names <- unique(tms$nameTeam)
# 
# get_shots <- function(team) {
# 
#   df <- teams_shots(teams = team, season_types = "Regular Season", seasons = 2021)
# 
# 
#   return(df)
# 
# }
# 
# shotData <- map_df(tm.names, get_shots)
# 
# write.csv(shotData, "shotData.csv", row.names = FALSE)

# Read in shot data
df <- read.csv("shotData.csv")

# Merge shot data with team names
df <- left_join(df, tms)

# clean up the shot data a bit to fit our court dimensions 
df <- df %>% mutate(locationX = as.numeric(as.character(locationX)) / 10,
                    locationY = as.numeric(as.character(locationY)) / 10 + hoop_center_y)

# Horizontally flip the data
df$locationX <- df$locationX * -1 

# Clean up the Clippers team name for merging purposes 
df <- df %>% 
  mutate(nameTeam = case_when(
    nameTeam == "LA Clippers" ~ "Los Angeles Clippers", 
    TRUE ~ nameTeam   
  )) %>% 
  mutate(slugTeam = case_when(
    nameTeam == "Los Angeles Clippers" ~ "LAC", 
    TRUE ~ slugTeam
  ))

# Filter out any shots greater than 35 feet (too noisy beyond that)
df <- df %>% filter(distanceShot <= 35)

# Set up our function for manually calculating the density of an area h/t -- https://themockup.blog/posts/2020-08-28-heatmaps-in-ggplot2/
get_density <- function(x, y, ...) {
  density_out <- MASS::kde2d(x, y, ...)
  int_x <- findInterval(x, density_out$x)
  int_y <- findInterval(y, density_out$y)
  comb_int <- cbind(int_x, int_y)
  return(density_out$z[comb_int])
}

# Pick a team of interest
team1 <- "Memphis Grizzlies"

# Decide how granular we want to go w/ our density calculation. I've found that 300 works pretty well, but note that it runs a bit slow
n <- 300

# filter data to our team of interest, assign it to tm1
tm1 <- df %>% 
  select(locationX, locationY, nameTeam) %>% 
  filter(nameTeam == team1)

# filter data for every team other than our team of interest, assign it to tm2
tm2 <- df %>% 
  select(locationX, locationY, nameTeam)%>% 
  filter(nameTeam != team1)

# get x/y coords as vectors
tm1_x <- pull(tm1, locationX)
tm1_y <- pull(tm1, locationY)

# get x/y coords as vectors
tm2_x <- pull(tm2, locationX)
tm2_y <- pull(tm2, locationY)

# get x and y range to compute comparisons across
x_rng = range(c(-25, 25))
y_rng = range(c(0, 52))

# Explicitly calculate bandwidth for future use
bandwidth_x <- MASS::bandwidth.nrd(c(tm1_x, tm2_x))
bandwidth_y <- MASS::bandwidth.nrd(c(tm1_y, tm2_y))

bandwidth_calc <- c(bandwidth_x, bandwidth_y)

# Calculate the density estimate over the specified x and y range
d2_tm1 = MASS::kde2d(tm1_x, tm1_y, h = bandwidth_calc, n=n, lims=c(x_rng, y_rng))
d2_tm2 = MASS::kde2d(tm2_x, tm2_y, h = bandwidth_calc, n=n, lims=c(x_rng, y_rng))

# Create a new dataframe that contains the difference in shot density between our two dataframes 
df_diff <- d2_tm1

# matrix subtraction density from tm1 from league average
df_diff$z <- d2_tm1$z - d2_tm2$z

# add matrix col names
colnames(df_diff$z) <- df_diff$y

# Convert list to dataframe with relevant variables and columns
df_diff <- df_diff$z %>% 
  http://as.data.frame() %>% 
  mutate(x_coord = df_diff$x) %>% 
  pivot_longer(-x_coord, names_to = "y_coord", values_to = "z") %>% 
  mutate(y_coord = as.double(y_coord),
         name = team1) %>% 
  ungroup()

# If density is less than league average, make it equal to league average (this will help w/ the contouring)
df_diff$z <- ifelse(df_diff$z < 0, 0, df_diff$z)

# Add team colors 
df_diff <- left_join(df_diff, tm.colors, by = "name")

# Make plot
p <- ggplot() +
  geom_contour_fill(data = df_diff %>% filter(z >= mean(z)), aes(x = x_coord, y = y_coord, z = sqrt(z)))  +
  geom_contour_tanaka(data = df_diff, aes(x = x_coord, y = y_coord, z = sqrt(z)), bins = 5)  +
  coord_fixed(clip = 'off') +
  theme_owen() +
  scale_fill_gradient2(low = 'floralwhite', mid = 'floralwhite', high = df_diff$primary[1]) +
  scale_y_continuous(limits = c(-2.5, 41)) +
  scale_x_continuous(limits = c(-30, 30)) +
  theme(legend.position = 'none',
        line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        plot.margin = margin(.25, .25, .5, .25, "lines"), 
        plot.title = element_text(size = 24, hjust= .5, face = 'bold', margin = margin(t = -25)),
        plot.subtitle = element_text(size = 10, hjust= .5, margin = margin(t = 15)),
        plot.caption = element_text(hjust = .5, size = 7)) +
  geom_path(data = court_points,
            aes(x = x, y = y, group = desc, linetype = dash),
            color = "black", size = .25) +
  labs(title = df_diff$name[1], 
       subtitle = "Where they like to shoot from relative to league average", 
       caption = "Darker areas indicate a higher frequency of shot attempts from the\ncorresponding location relative to league average") +
  geom_curve(aes(x = 0, y = 32.5, xend = 2.5, yend = 14), curvature = -0.2, arrow = arrow(length = unit(0.03, "npc"))) + 
  annotate(geom = 'label', x = 0, y = 33.5, hjust = .5, label = "Memphis loves their floaters", family = "Consolas", size = 3)

p <- cowplot::ggdraw(p) + 
  theme(plot.background = element_rect(fill="floralwhite", color = NA))

ggsave(paste0(df_diff$name[1], ".png"), p, w = 6, h = 6, dpi = 300)