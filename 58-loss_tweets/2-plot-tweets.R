library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(tidytext)
library(glue)
library(extrafont)
library(ggplot2)
library(ggtext)
library(tibble)
library(treemapify)

dir_proj <- '58-loss_tweets'
dir_data <- file.path(dir_proj, 'data')

blackish_background <- '#1c1c1c'
gray_points <- '#4d4d4d'
gray_text <- '#999999'

font <- 'Titillium Web'
extrafont::loadfonts(quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = font),
  title = element_text(size = 20, color = 'white'),
  plot.title = ggtext::element_markdown(face = 'bold', size = 16, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown(size = 16, color = '#f1f1f1'),
  axis.text = element_text(color = 'white', size = 14),
  axis.title = element_text(size = 16, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = gray_points),
  panel.grid.minor = element_line(color = gray_points),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = blackish_background, color = blackish_background),
  plot.caption = ggtext::element_markdown(color = 'white', hjust = 1, size = 10, face = 'italic'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 14, color = 'white', hjust = 0),
  plot.tag.position = c(0.01, 0.02),
  panel.background = element_rect(fill = blackish_background, color = blackish_background)
)

manu_color <- '#DA291C'
matches <- file.path(dir_data, 'manu_matches.csv') |> read_csv()
manu_tweets <- file.path(dir_proj, 'manu_tweets.csv') |> 
  read_csv() |> 
  inner_join(
    matches,
    by = 'date'
  )

outcome_lvls <- c('Win', 'Draw or Loss')
separated_player_counts <- manu_tweets |> 
  mutate(
    outcome = ifelse(result != 'W', outcome_lvls[[2]], outcome_lvls[[1]])
  ) |> 
  separate_rows(
    players,
    sep = ','
  ) |> 
  mutate(
    across(players, ~str_remove(.x, '^.*\\s+'))
  )

separated_player_counts |> 
  count(date, outcome) |> 
  count(outcome, n1 = n == 1)

init_player_counts <- separated_player_counts |> 
  count(outcome, player = players, sort = TRUE)

n_players_appearing_only_once <- init_player_counts |> 
  filter(n == 1) |> 
  group_by(outcome) |> 
  summarize(across(n, sum))

player_counts <- bind_rows(
  init_player_counts |> 
    filter(n != 1),
  n_players_appearing_only_once |> 
    transmute(
      outcome, 
      player = sprintf('%d players with 1', n),
      n = 1
    )
  ) |> 
  mutate(
    across(outcome, ~ordered(.x, outcome_lvls)),
    grp = tidytext::reorder_within(player, n, outcome)
  )

y_labels <- player_counts %>% distinct(grp, lab = player)

p <- player_counts |> 
  mutate(
    lab = ifelse(n == 1, player, sprintf('%s (%d)', player, n))
  ) |> 
  ggplot() +
  aes(area = n, label = lab, fill = n) +
  scale_fill_gradient2(
    low = colorspace::lighten(manu_color, 0.75),
    mid = manu_color,
    high = colorspace::darken(manu_color, 0.75)
  ) +
  guides(fill = 'none') +
  geom_treemap() +
  geom_treemap_text(
    # grow = TRUE, 
    reflow = TRUE, 
    colour = 'white',
    family = font
  ) +
  facet_wrap(~outcome) +
  theme(
    strip.text = element_text(size = 14, color = 'white', face = 'bold', hjust = 0)
  ) +
  labs(
    title = glue::glue('Appearances in end-of-match @<span style="color:{manu_color}">ManUtd</span> tweets'),
    subtitle = '2021/22 Premier League',
    tag = '**Viz**: Tony ElHabr',
    caption = 'Multiple players shown in 11 of 16 tweets after wins,<br/>and just 1 of 22 tweets after losses or draws.',
    y = NULL,
    x = NULL
  )
p

path <- file.path(dir_proj, 'manu_tweets.png')
ggsave(
  p,
  filename = path,
  width = 7,
  height = 7
)

add_logo(
  path_viz = path,
  path_logo = sprintf('%s/bruno-fernandes.png', dir_proj),
  path_suffix = '',
  delete = FALSE,
  logo_scale = 0.25,
  idx_x = 0.235,
  idx_y = 0.23
)

add_logo(
  path_viz = path,
  path_logo = sprintf('%s/cr7.png', dir_proj),
  path_suffix = '',
  delete = FALSE,
  logo_scale = 0.15,
  idx_x = 0.8,
  idx_y = 0.16
)

add_logo(
  path_viz = path,
  path_logo = sprintf('%s/epl-logo-white.png', dir_proj),
  path_suffix = '',
  delete = FALSE,
  logo_scale = 0.13,
  idx_x = 0.01,
  idx_y = 0.98,
  adjust_y = FALSE
)

add_logo(
  path_viz = path,
  path_logo = sprintf('%s/manu-logo-white.png', dir_proj),
  logo_scale = 0.06,
  idx_x = 0.15,
  idx_y = 0.98,
  adjust_y = FALSE
)

## "i want to know what Bruno Fernandes did to the Man Utd admin last year. mans was scape-goated by team tweets last season :sob:"
## "CR7 must have that LeBron James no-pic-in-loss-tweet clause" https://twitter.com/KOT4Q/status/1508267758831779849
## "poor Varane was the most recent victim" https://twitter.com/ManUtd/status/1558519945432125443