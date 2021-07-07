
library(tidyverse)
library(rvest)

dir_proj <- '32-2020_euros'

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 24, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown('Karla', face = 'bold', size = 18, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  # plot.caption = ggtext::element_markdown('Karla', size = 16, color = 'gray20', hjust = 1),
  plot.caption = element_text('Karla', size = 12, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.01),
  # legend.text = element_text(color = 'gray20', size = 14),
  # strip.text = element_text(color = 'gray20', size = 14),
  # strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

url <- 'https://www.skysports.com/football/news/19692/12309182/euro-2020-squads-confirmed-squad-lists-and-premier-league-players-at-tournament'
page <- url %>% xml2::read_html()

h3 <- page %>% html_nodes('h3')
h3
countrys <-
  h3 %>% 
  html_text() %>% 
  # .[1:20] %>% 
  str_subset('GROUP|See|Win|Get', negate = TRUE)
countrys
page %>% html_nodes('p') %>% html_nodes('strong')
nodes_p <- page %>% html_nodes('p')
text_p <- nodes_p %>% html_text()
idx_filt <- 7:(7+98)
nodes_filt <- nodes_p[idx_filt]

lst <-
  nodes_filt %>% 
  html_text2(preserve_nbsp = TRUE) %>% 
  str_split(pattern = '\n')
lst[2]

f_strip <- function(x, rgx = 'Goalkeepers') {
  rgx <- sprintf('%s[:] ', rgx)
  res_init <- x %>% keep(~str_detect(.x, rgx))
  if(length(res_init) == 0L) {
    return(NA_character_)
  }
  res_init %>% str_remove(rgx)
}

do_f_strip <- function(rgx) {
  res <- lst %>% map_chr(~f_strip(.x, rgx))
  res[!is.na(res)]
}

agg <-
  tibble(
    g = do_f_strip('Goalkeepers'),
    d = do_f_strip('Defenders'),
    m = do_f_strip('Midfielders'),
    f = do_f_strip('Forwards')
  ) %>% 
  bind_cols(tibble(country = countrys), .)
agg

f_separate <- function(pos) {
  pos_sym <- sym(pos)
  f_replace <- function(x, i) {
    str_replace(x, '(^.*)\\s\\((.*)\\)[.]?$', sprintf('\\%s', i))
  }
  agg %>% 
    mutate(
      across(
        !!pos_sym,
        ~str_replace_all(.x, c('\\(Tottenham Hotspur\\) Connor Roberts' = '(Tottenham Hotspur), Connor Roberts', '\\(Wolves\\) Thorgan Hazard' = '(Wolves), Thorgan Hazard'))
      )
    ) %>% 
    select(country, !!pos_sym) %>% 
    separate_rows(!!pos_sym, sep = '[;,]\\s+') %>% 
    set_names(c('country', 'player')) %>% 
    mutate(
      across(
        player,
        list(
          name = ~f_replace(.x, 1),
          team = ~f_replace(.x, 2)
        ),
        .names = '{fn}'
      ),
      pos = !!pos
    ) %>% 
    select(country, team_sky = team, name, pos)
}

players <- 
  c('g', 'm', 'd', 'f') %>% 
  map_dfr(f_separate)

players %>% 
  filter(name %>% str_detect('Dendoncker')) %>% 
  separate_rows()

team_mapping <-
  tibble(
    team_sky = c('Arsenal', 'Aston Villa', 'Brighton', 'Brighton & Hove Albion', 'Burnley', 'Chelsea', 'Crystal Palace', 'Everton', 'Fulham', 'Leeds', 'Leeds United', 'Leicester', 'Leicester City', 'Liverpool', 'Man City', 'Manchester City', 'Manchester United', 'Newcastle', 'Newcastle United', 'Sheffield United', 'Southampton', 'Tottenham', 'Tottenham Hotspur', 'West Brom', 'West Ham', 'West Ham United', 'Wolves'),
    team = c('Arsenal', 'Aston Villa', 'Brighton', 'Brighton', 'Burnley', 'Chelsea', 'Crystal Palace', 'Everton', 'Fulham', 'Leeds', 'Leeds', 'Leicester', 'Leicester', 'Liverpool', 'Man City', 'Man City', 'Man United', 'Newcastle Utd', 'Newcastle Utd', 'Sheffield Utd', 'Southampton', 'Tottenham', 'Tottenham', 'West Brom', 'West Ham', 'West Ham', 'Wolves')
  )

players_filt <-
  players %>% 
  mutate(
    across(
      team_sky, 
      ~case_when(name == 'Gareth Bale' ~ 'Tottenham', TRUE ~ .x)
    )
  ) %>% 
  inner_join(team_mapping) %>% 
  mutate(across(team, ~str_replace_all(.x, c('Utd' = 'United', 'Man ' = 'Manchester '))))
players_filt

players_filt %>% filter(team == 'Wolves') %>% arrange(name)
# players_filt %>% filter(team == 'Arsenal')
players_filt %>% count(team)

write_csv(players_filt, file.path(dir_proj, 'players.csv'), na = '')

teams_n <-
  players_filt %>% 
  count(team, sort = TRUE) %>% 
  mutate(rnk = row_number(desc(n)))
teams_n %>% pull(team) %>% levels()

countrys_n <-
  players_filt %>% 
  count(country, sort = TRUE) %>% 
  mutate(rnk = row_number(desc(n)))
countrys_n

countrys %>% 
  tibble(country = .) %>% 
  anti_join(countrys_n)

# flags from here: https://github.com/lbenz730/euro_cup_2021
img <- 
  fs::dir_ls(file.path(dir_proj, 'flags')) %>% 
  tibble(path = .) %>% 
  mutate(country = path %>% basename() %>% tools::file_path_sans_ext())
img
players_filt %>% distinct(country) %>% anti_join(img)

df <-
  players_filt %>% 
  left_join(img) %>% 
  left_join(teams_n %>% rename(n_team = n, rnk_team = rnk)) %>% 
  mutate(across(team, ~fct_reorder(.x, n_team))) %>% 
  arrange(team, country) %>% 
  group_by(team, country) %>% 
  mutate(idx1 = row_number(), n = n()) %>% 
  ungroup() %>%
  arrange(team, n, country) %>% 
  group_by(team) %>% 
  mutate(
    idx2 = row_number()
  ) %>% 
  ungroup() %>%
  arrange(team, idx2)
df

lab_tag <- '**Viz**: Tony ElHabr'
# library(ggflags) # have to load in for .flaglist
p1 <-
  df %>% 
  # head(5) %>% 
  ggplot() +
  aes(x = idx2, y = team) +
  # ggflags::geom_flag(aes(country = code), show.legend = FALSE) +
  # ggflags::scale_country() +
  # scale_size(range = c(20, 20)) +
  ggimage::geom_image(
    size = 0.04,
    aes(image = path)
  ) +
  scale_x_continuous(position = 'top') +
  theme(
    plot.caption = ggtext::element_markdown(face = 'italic'),
    plot.title = ggtext::element_markdown(size = 14),
    # panel.grid.major.x = element_blank(),
    # axis.text.x = element_blank(),
    # axis.text.x.bottom = element_text(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = 'Which 2020/21 Premier League Teams Have the Most Players in the 2020 Euros?',
    x = NULL, # '# of Players',
    caption = ' ',
    tag = lab_tag,
    y = NULL
  )
# p1

path_p1 <- file.path(dir_proj, 'viz_countries_by_team.png')
h <- 8
ggsave(
  plot = p1,
  file = path_p1,
  height = 9,
  width = 9,
  type = 'cairo'
)

add_logo(
  path_viz = path_p1,
  path_logo = file.path(dir_proj, 'logo.png'),
  logo_scale = 0.251,
  # delete = FALSE,
  idx_x = 0.04,
  adjust_y = FALSE,
  idx_y = 0.21
)
