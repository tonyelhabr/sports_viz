library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(purrr)

## plotting
library(ggplot2)
library(sysfonts)
library(showtext)
library(ggtext)
library(htmltools)
library(ggpath)
library(scales)
library(glue)

PROJ_DIR <- '79-202324_ucl_club_rankings'

PLOT_RESOLUTION <- 300
WHITISH_FOREGROUND_COLOR <- 'white'
COMPLEMENTARY_FOREGROUND_COLOR <- '#cbcbcb'
BLACKISH_BACKGROUND_COLOR <- '#1c1c1c'
COMPLEMENTARY_BACKGROUND_COLOR <- '#4d4d4d'
FONT <- 'Titillium Web'
sysfonts::font_add_google(FONT, FONT)
sysfonts::font_add('fa-brands', 'Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')
showtext::showtext_auto()
showtext::showtext_opts(dpi = PLOT_RESOLUTION)

ggplot2::theme_set(ggplot2::theme_minimal())
ggplot2::theme_update(
  text = ggplot2::element_text(family = FONT),
  title = ggplot2::element_text(size = 20, color = WHITISH_FOREGROUND_COLOR),
  plot.title = ggtext::element_markdown(face = 'bold', size = 32, color = WHITISH_FOREGROUND_COLOR),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown(size = 24, color = COMPLEMENTARY_FOREGROUND_COLOR),
  axis.text = ggplot2::element_text(color = WHITISH_FOREGROUND_COLOR, size = 14),
  axis.title.x = ggtext::element_markdown(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.title.y = ggtext::element_markdown(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.line = ggplot2::element_blank(),
  panel.grid.major = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor.x = ggplot2::element_blank(),
  panel.grid.minor.y = ggplot2::element_blank(),
  plot.margin = ggplot2::margin(10, 20, 10, 20),
  plot.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR),
  plot.caption = ggtext::element_markdown(size = 16, color = WHITISH_FOREGROUND_COLOR, hjust = 0, face = 'plain'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 16, color = WHITISH_FOREGROUND_COLOR, hjust = 1),
  plot.tag.position = c(0.99, 0.01),
  panel.spacing.x = grid::unit(2, 'lines'),
  panel.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR)
)
ggplot2::update_geom_defaults('text', list(color = WHITISH_FOREGROUND_COLOR, size = 14 / .pt))

CLUBELO_TEAMS <- c(
  'Arsenal',
  'Atletico',
  'Barcelona',
  'Bayern',
  'Dortmund',
  'FC Kobenhavn',
  'Inter',
  'Lazio',
  'Man City',
  'Napoli',
  'Paris SG',
  'Porto',
  'PSV',
  'RB Leipzig',
  'Real Madrid',
  'Sociedad'
)

FINAL_FOUR <- c(
  'Bayern',
  'Dortmund',
  'PSG',
  'Real Madrid'
)

TEAM_NAMES <- c(
  'Arsenal' = 'Arsenal',
  'Atletico' = 'Atl�tico',
  'Barcelona' = 'Barcelona',
  'Bayern' = 'Bayern',
  'Dortmund' = 'Dortmund',
  'FC Kobenhavn' = 'K�benhavn',
  'Inter' = 'Inter',
  'Lazio' = 'Lazio',
  'Man City' = 'Man City',
  'Napoli' = 'Napoli',
  'Paris SG' = 'PSG',
  'Porto' = 'Porto',
  'PSV' = 'PSV',
  'RB Leipzig' = 'RB Leipzig',
  'Real Madrid' = 'Real Madrid',
  'Sociedad' = 'Real Sociedad'
)

PAL <- c(
  'Arsenal' = '#f83a6c',
  'Atl�tico' = '#724cff',
  'Barcelona' = '#c94870',
  'Bayern' = '#ff346a',
  'Dortmund' = '#ffe600',
  'K�benhavn' = '#3c6bb6',
  'Inter' = '#1e73be',
  'Lazio' = '#d6f4ff',
  'Man City' = '#00b7db',
  'Napoli' = '#2d98d1',
  'PSG' = '#ff3941',
  'Porto' = '#057abb',
  'PSV' = '#cdaf6a',
  'RB Leipzig' = '#ff3468',
  'Real Madrid' = '#7599e6',
  'RealSociedad' = '#1a74b9'
)


generate_logo_url <- function(id) {
  sprintf('https://omo.akamai.opta.net/image.php?secure=true&h=omo.akamai.opta.net&sport=football&entity=team&description=badges&dimensions=150&id=%s', id)
}

compared_rankings <- read_csv(
  'https://github.com/tonyelhabr/club-rankings/releases/download/club-rankings/compared-rankings.csv',
  show_col_types = FALSE
)

daily_compared_rankings <- compared_rankings |> 
  filter(
    team_clubelo %in% CLUBELO_TEAMS,
    date >= ymd('2024-02-11')
  ) |> 
  transmute(
    date,
    # logo_url = generate_logo_url(id_opta),
    id_opta,
    team = TEAM_NAMES[team_clubelo],
    rank_opta
  ) |> 
  group_by(date) |> 
  mutate(
    rerank_opta = row_number(rank_opta)
  ) |> 
  ungroup()

weekly_compared_rankings <- daily_compared_rankings |> 
  filter(
    ## arbitrarily choose Tuesday to capture Monday games
    wday(date, label = TRUE) == 'Tue'
  ) |> 
  mutate(
    week = floor_date(date, 'week', week_start = 6)
  ) |> 
  filter(
    !(week %in% INTERNATIONAL_BREAK_SATURDAYS)
  )

first_week <- weekly_compared_rankings |> 
  slice_min(week) |> 
  arrange(rerank_opta)

last_week <- weekly_compared_rankings |> 
  slice_max(week) |> 
  arrange(rerank_opta)

logo_dir <- file.path(PROJ_DIR, 'logos')
dir.create(logo_dir, showWarnings = FALSE)

logo_paths <- set_names(
  last_week$id_opta
) |> 
  map_chr(
    \(id_opta) {
      url <- generate_logo_url(id_opta)
      path <- file.path(logo_dir, paste0(id_opta, ".png"))
      if (file.exists(path)) {
        return(path)
      }
      download.file(url, destfile = path, mode = 'wb')
      path
    }
  )

weekly_compared_rankings$logo_path <- logo_paths[weekly_compared_rankings$id_opta]

CHANGE_PAL <- c(
  'up' = '#2feaa8',
  'down' = '#fb3640'
)

plot <- weekly_compared_rankings |> 
  ggplot() +
  aes(
    x = week,
    y = -rerank_opta
  ) +
  geom_vline(
    data = tibble(
      week = INTERNATIONAL_BREAK_SATURDAYS
    ),
    aes(
      xintercept = week
    ),
    linetype = 2,
    color = COMPLEMENTARY_BACKGROUND_COLOR
  ) +
  geom_line(
    data = weekly_compared_rankings |> filter(!(team %in% FINAL_FOUR)),
    aes(
      color = team
    ),
    alpha = 0.2
  ) +
  geom_line(
    data = weekly_compared_rankings |> filter(team %in% FINAL_FOUR),
    aes(
      color = team
    )
  ) +
  scale_color_manual(
    values = PAL
  ) +
  geom_from_path(
    data = weekly_compared_rankings |> filter(!(team %in% FINAL_FOUR)),
    aes(
      path = logo_path
    ),
    alpha = 0.2,
    width = 0.06
  ) +
  geom_from_path(
    data = weekly_compared_rankings |> filter(team %in% FINAL_FOUR),
    aes(
      path = logo_path
    ),
    width = 0.06
  ) +
  guides(
    color = 'none'
  ) +
  scale_y_continuous(
    breaks = -16:-1,
    labels = first_week |>
      select(
        team,
        first_week_rerank_opta = rerank_opta
      ) |> 
      inner_join(
        last_week |> select(team, last_week_rerank_opta = rerank_opta),
        by = join_by(team)
      ) |> 
      mutate(
        label = paste0(
          "<b><span style='font-size:16pt", 
          dplyr::case_when(
            team %in% FINAL_FOUR ~ ';color:white',
            TRUE ~  ';color:#4d4d4d'
          ),
          "'>", 
          team, 
          "</span></b> <span style='font-size:12pt", 
          dplyr::case_when(
            team %in% FINAL_FOUR ~ ';color:white',
            TRUE ~ ';color:#4d4d4d'
          ),
          "'>(",
          first_week_rerank_opta,
          ## https://albert-rapp.de/posts/ggplot2-tips/08_fonts_and_icons/08_fonts_and_icons.html
          " <span style='font-family:fa-solid",
          dplyr::case_when(
            !(team %in% FINAL_FOUR) ~ ';color:#4d4d4d',
            first_week_rerank_opta > last_week_rerank_opta ~ paste0(';color:', CHANGE_PAL[['up']]),
            first_week_rerank_opta < last_week_rerank_opta ~ paste0(';color:', CHANGE_PAL[['down']]),
            TRUE ~ ''
          ),
          "'>&#x",
          case_when(
            first_week_rerank_opta > last_week_rerank_opta ~ 'e098',
            first_week_rerank_opta < last_week_rerank_opta ~ 'e097',
            TRUE ~ 'f061'
          ),
          ';</span> ',
          last_week_rerank_opta,
          ')</span>'
        )
      ) |> 
      pull(label) |> 
      rev()
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_markdown(),
    axis.text.x = element_text(size = 16)
  ) +
  labs(
    title = 'Weekly changes in relative Opta ranking',
    subtitle = 'UCL 2023/24 R16 Teams, since Feb. 20, 2024',
    caption = glue('**Data**: Opta. Last updated at {Sys.Date()}.'),
    tag = "<span style='font-family:fa-brands'>&#xf099;</span> @Tony ElHabr",
    y = NULL,
    x = NULL
  )

ggsave(
  plot,
  filename = file.path(PROJ_DIR, glue('ucl-opta-rankings-bump-{Sys.Date()}.png')),
  height = 10,
  width = 10
)
