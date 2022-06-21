
suppressPackageStartupMessages(library("tidyverse"))
library("betareg")
library("broom")
theme_set(theme_light(base_size = 12))
theme_update(panel.grid.minor = element_blank())

# Function to transform attendance rates for beta regression because values of 0
# and/or 1 may occur
transform_perc <- function(percentage_vec) {
  # See Cribari-Neto & Zeileis (2010)
  (percentage_vec * (length(percentage_vec) - 1) + 0.5)/length(percentage_vec)
}

# Use data from 2016/17 and 2017/18
df21 <- filter(df1, season_id != 55149) %>%
  mutate(year = droplevels(year),
         home_team = droplevels(home_team),
         away_team = droplevels(away_team)) %>%
  mutate(y = transform_perc(perc))

# Use effect coding, contr.sum(), for teams so that intercept corresponds to
# average across all teams
contrasts(df21$home_team) <- contr.sum(levels(df21$home_team))
colnames(contrasts(df21$home_team)) <- head(levels(df21$home_team), -1)
contrasts(df21$away_team) <- contr.sum(levels(df21$away_team))
colnames(contrasts(df21$away_team)) <- head(levels(df21$away_team), -1)

# Dummy coding
contrasts(df21$weekday) <- contr.treatment(7, base = 6)

m1 <- betareg(y ~ home_team + away_team + weekday + year
              | home_team, data = df21)
m1_est <- tidy(m1)

# Print some estimates
slice(m1_est, 1, 49, seq(2, 69, 8))

# Function to calculate a and b parameter of beta distribution from mu and phi
# used in betareg with link function logit for mu and log for phi
calc_ab <- function(mu, phi) {
  mu <- plogis(mu)
  phi <- exp(phi)
  b <- phi - mu*phi
  a <- phi - b
  return(cbind(a = a, b = b)[, 1:2])
}

# Calculate predicted beta distributions for Flensburg and Lemgo
preds1 <- 
  # Estimates of mu and phi from above
  calc_ab(2.08 + c(1.42, -.42), 2.4 + c(.49, .03)) %>%
  asplit(1) %>%
  setNames(c("SG Flensburg-Handewitt", "TBV Lemgo Lippe")) %>%
  # Calculate densities via dbeta()
  lapply(function(y) data.frame(x = seq(0, 1, .001),
                                y = dbeta(seq(0, 1, .001), y[1], y[2]))) %>%
  enframe("home_team") %>% 
  unnest(value)

# Print some values
slice(preds1, seq(701, by = 100, length.out = 4))

# Plot observed data using densities and rugs
p1 <- df21 %>%
  filter(home_team %in% c("SG Flensburg-Handewitt", "TBV Lemgo Lippe")) %>%
  ggplot(aes(x = perc, fill = home_team)) +
  geom_density(trim = TRUE) +
  geom_rug() +
  facet_wrap(~ home_team) +
  coord_cartesian(xlim = c(.5, 1), ylim = c(0, 12)) +
  scale_fill_viridis_d() +
  labs(y = "Density", x = "Attendance Rate", fill = NULL,
       title = "Beta Regression: Observed and Fitted Attendance Rates") +
  theme(legend.position = "none")

# Add lines corresponding to estimated distributions
p1 <- p1 + 
  geom_line(data = preds1, aes(x = x, y = y),
            inherit.aes = FALSE, size = 1,
            color = rgb(0, 155, 149, maxColorValue = 255))

# Add predicted means
p1 <- p1 + 
  geom_point(data = data.frame(y = 0,
                               x = plogis(2.01 + c(1.49, -.35)),
                               home_team = c("SG Flensburg-Handewitt", "TBV Lemgo Lippe")),
             aes(x = x, y = y), show.legend = FALSE, size = 3,
             color = rgb(0, 155, 149, maxColorValue = 255))
p1
