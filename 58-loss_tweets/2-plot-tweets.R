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

## for image downloads
library(purrr)
library(rvest)
library(cli)
library(qs)

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
  plot.title = ggtext::element_markdown(face = 'bold', size = 20, color = 'white'),
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

dir_img <- file.path(dir_proj, 'img')
dir.create(dir_img)
scrape_fbref_player_img <- function(url, name, overwrite = FALSE) {
  path_img <- file.path(dir_img, sprintf('%s.png', name))
  path <- file.path(dir_data, sprintf('fbref_img-%s.qs', name))
  suffix <- sprintf('for %s.', name)
  if(file.exists(path) & !overwrite) {
    cli::cli_text('{Sys.time()}: Returning early {suffix}')
    return(qs::qread(path))
  }
  Sys.sleep(5)
  page <- rvest::read_html(url)
  img_url <- page |> 
    rvest::html_element(xpath = '//*[@id="meta"]/div[1]/img') |>
    rvest::html_attr('src')
  if(!file.exists(path_img)) {
    download.file(img_url, destfile = path_img, mode = 'wb', quiet = TRUE)
  }
  res <- tibble(
    url = url,
    img_url = img_url,
    player = name,
    path = path_img
  )
  cli::cli_text('{Sys.time()}: Retrieved data {suffix}')
  qs::qsave(res, path)
  res
}

player_urls <- c(
  'Ronaldo' = 'https://fbref.com/en/players/dea698d9/Cristiano-Ronaldo',
  'Fernandes' = 'https://fbref.com/en/players/507c7bdf/Bruno-Fernandes',
  'McTominay' = 'https://fbref.com/en/players/d93c2511/Scott-McTominay',
  'Fred' = 'https://fbref.com/en/players/b853e0ad/Fred',
  'Telles' = 'https://fbref.com/en/players/e73c9bb2/Alex-Telles',
  'Rashford' = 'https://fbref.com/en/players/a1d5bd30/Marcus-Rashford',
  'Elanga' = 'https://fbref.com/en/players/2fba6108/Anthony-Elanga',
  'Wan-Bissaka' = 'https://fbref.com/en/players/9e525177/Aaron-Wan-Bissaka',
  'Dalot' = 'https://fbref.com/en/players/d9565625/Diogo-Dalot'
) |> 
  imap_dfr(scrape_fbref_player_img)

manu_color <- '#DA291C'
matches <- file.path(dir_data, 'manu_matches.csv') |> read_csv()
manu_tweets <- file.path(dir_proj, 'manu_tweets.csv') |> 
  read_csv() |> 
  inner_join(
    matches,
    by = 'date'
  )
outcome_lvls <- c('Win', 'Draw or Loss')
init_player_counts <- manu_tweets |> 
  mutate(
    outcome = ifelse(result != 'W', outcome_lvls[[2]], outcome_lvls[[1]])
  ) |> 
  separate_rows(
    players,
    sep = ','
  ) |> 
  mutate(
    across(players, ~str_remove(.x, '^.*\\s+'))
  ) |> 
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

# wd <- fs::path_wd()
# player_urls$path |> 
#   map(
#     ~file.rename(.x, str_replace(.x, 'png', 'jpg'))
#   )
# 
# y_labels <- player_counts %>% 
#   distinct(grp, player) |> 
#   left_join(
#     player_urls |> across(path, ~str_replace(.x, 'png', 'jpg')),
#     by = 'player'
#   ) |> 
#   mutate(
#     across(path, ~str_replace(.x, 'png', 'jpg')),
#     # lab = glue::glue("<img src={path} width='40' height='40'/>"),
#     # lab = sprintf(
#     #   '%s%s',
#     #   player,
#     #   ifelse(
#     #     !is.na(url), 
#     #     glue::glue("   <img src={path} width='30' height='30'/>"),
#     #     ''
#     #   )
#     # )
#   )
y_labels <- player_counts %>% distinct(grp, lab = player)

x_lvls <- seq(2, 10, by = 2)
p <- player_counts |> 
  ggplot() +
  aes(x = n, y = grp) +
  geom_col(
    # alpha = 0.8,
    fill = manu_color
  ) +
  scale_x_continuous(
    breaks = x_lvls,
    labels = as.character(x_lvls)
  ) +
  scale_y_discrete(name = '', labels = y_labels %>% select(grp, lab) %>% deframe()) +
  facet_wrap(~outcome, scales = 'free_y') +
  theme(
    strip.text = element_text(size = 14, color = 'white', face = 'bold', hjust = 0),
    axis.text.y = ggtext::element_markdown(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = glue::glue('Appearances in end-of-match @<span style="color:{manu_color}">ManUtd</span> score tweets'),
    subtitle = '2021/22 Premier League',
    tag = '**Viz**: Tony ElHabr',
    caption = '<br/>',
    y = NULL,
    x = '# of matches'
  )
p

path <- file.path(dir_proj, 'manu_tweets.png')
ggsave(
  p,
  filename = path,
  width = 10,
  height = 8
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