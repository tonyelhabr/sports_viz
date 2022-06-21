# https://www.tjmahr.com/another-mixed-effects-model-visualization/
library(tidyverse)
library(brms)
library(tidybayes)
library(cowplot)

# Convert to tibble for better printing. Convert factors to strings
sleepstudy <- lme4::sleepstudy %>%
  as_tibble() %>%
  mutate(Subject = as.character(Subject))

# Add two fake participants, as in the earlier partial pooling post
df_sleep <- bind_rows(
  sleepstudy,
  tibble(Reaction = c(286, 288), Days = 0:1, Subject = "374"),
  tibble(Reaction = 245, Days = 0, Subject = "373")
)

df_sleep

b <- brm(
  Reaction ~ Days + (Days | Subject),
  data = df_sleep, 
  seed = 20191125
)
b

col_data <- "#2F2E33"


df_demo <- df_sleep |> 
  filter(Subject %in% c(333, 335, 350, 374))

p_first_pass <- ggplot(df_demo) +
  aes(x = Days, y = Reaction) +
  geom_line(aes(group = Subject), color = col_data) +
  geom_point(color = col_data, size = 2) +
  facet_wrap("Subject", nrow = 1) +
  coord_cartesian(ylim = c(200, 500)) +
  ggtitle("Each participant contributes data") + 
  theme_grey(base_size = 14)
p_first_pass

p_first_pass_tweak <- p_first_pass + 
  theme(plot.title.position = "plot")
p_first_pass_tweak

p_second_pass <- p_first_pass_tweak + 
  scale_x_continuous(breaks = seq(0, 9, by = 2)) +
  labs(x = NULL, y = NULL) +
  theme(
    # Removing things from `theme()` is accomplished by setting them
    # `element_blank()`
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank()
  )
p_second_pass
