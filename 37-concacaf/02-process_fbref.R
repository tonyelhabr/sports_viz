
library(tidyverse)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = element_text('Karla', face = 'bold', size = 24, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', face = 'bold', size = 16, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  plot.caption = ggtext::element_markdown('Karla', size = 14, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 14, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.01),
  legend.text = element_text(color = 'gray20', size = 14),
  strip.text = element_text(color = 'gray20', size = 14),
  strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

dir_proj <- '37-concacaf'
dir_data <- file.path(dir_proj, 'data')

results <-
  fs::dir_ls(dir_data, regexp = 'results-') %>% 
  tibble(path = .) %>% 
  mutate(
    data = map(path, ~read_rds(.x) %>% mutate(across(matches('xG$'), as.numeric)))
  ) %>% 
  select(-path) %>% 
  unnest(data) %>% 
  janitor::clean_names() %>% 
  select(-matches('_2$')) %>% 
  mutate(
    across(home, ~str_remove(.x, '\\s[a-z]+$')),
    across(away, ~str_remove(.x, '^[a-z]+\\s'))
  )
results %>% count(competition_name)
results %>% filter(!is.na(home_x_g))

results_slim <-
  results %>% 
  mutate(idx_results = row_number()) %>% 
  select(idx_results, date, team_home = home, team_away = away, referee)
results_slim

import_stats <- function(stem = 'summary') {
  fs::dir_ls(dir_data, regexp = sprintf('stats.*%s[-]team[.]rds$', stem)) %>%
    tibble(path = .) %>% 
    mutate(
      data = map(path, ~read_rds(.x))
    ) %>% 
    select(-path) %>% 
    unnest(data) %>% 
    janitor::clean_names() %>% 
    mutate(src = !!stem)
}

stats_summ <- import_stats('summary')
stats_summ %>% count(league)

stats_misc <- import_stats('misc')
stats_misc %>% count(league)
stats_summ %>% 
  filter(
    !(league == 'European Championship' & season == '2021') & !(league == 'Copa America' & season == '2021')
  )

# stats_slim %>% count(league, team_home, team_away, season, date, sort = TRUE) %>% count(league, season, n) %>% arrange(desc(n))
# stats_misc$p_kcon
# bind_rows(
#   stats_summ %>% 
#     filter(
#       (league == 'European Championship' & season == '2021') |
#         (league == 'Copa America' & season == '2021')
#     ),
#   stats_misc %>% rename(p_katt = p_kwon)
# ) %>% 
#   mutate(
#     date = strptime(match_date, '%A %B %d, %Y', tz = 'UTC') %>% lubridate::date()
#   ) %>% 
#   select(
#     league,
#     gender,
#     season,
#     team_home = home_team,
#     team_away = away_team,
#     date,
#     team,
#     # player,
#     mp = min,
#     n_card_y = crd_y,
#     n_card_y2 = x2crd_y,
#     n_card_r = crd_r,
#     n_foul_committed = fls,
#     n_foul_drawn = fld,
#     n_offside = off,
#     n_tackle_won = tkl_w,
#     # n_pk = pk,
#     n_pk_att = p_katt
#   ) %>% 
#   visdat::vis_dat()

stats <-
  bind_rows(
    # Drop these since we have misc data for them
    stats_summ %>% 
      filter(
        !(league == 'European Championship' & season == '2021') &
          !(league == 'Copa America' & season == '2021')
      ),
    stats_misc %>% rename(p_katt = p_kwon)
  )
stats
stats %>% count(league)

stats_slim <-
  stats %>% 
  mutate(
    idx_stats = row_number(),
    date = strptime(match_date, '%A %B %d, %Y', tz = 'UTC') %>% lubridate::date(),
    mutate(
      across(
        league,
        list(comp = ~case_when(
          .x == 'CONCACAF Gold Cup' ~ 'North America',
          .x == 'Copa America' ~ 'South America',
          TRUE ~ 'Europe'
        )
        ),
        .names = '{fn}'
      )
    )
  ) %>% 
  select(
    idx_stats,
    src,
    comp,
    season,
    team_home = home_team,
    team_away = away_team,
    date,
    team,
    mp = min,
    n_card_y = crd_y,
    n_card_y2 = x2crd_y,
    n_card_r = crd_r,
    n_foul_committed = fls,
    n_foul_drawn = fld,
    n_offside = off,
    n_tackle_won = tkl_w,
    # n_pk = pk,
    n_pk_att = p_katt
  ) %>% 
  # 2nd yellow card counts as both a yellow and a red.
  mutate(across(c(n_card_y, n_card_r), ~.x + coalesce(n_card_y2, 0))) %>%
  select(-n_card_y2)
stats_slim

misc_w_refs <-
  stats_slim %>% 
  # Not sure what's the best way to handle these seemingly duplicate numbers. Just picking one and dropping the other.
  select(-n_foul_drawn) %>% 
  rename(n_foul = n_foul_committed) %>% 
  inner_join(results_slim) %>% 
  select(
    idx_stats,
    idx_results,
    src,
    comp,
    season,
    date,
    team_home,
    team_away,
    referee,
    team,
    everything()
  )
misc_w_refs %>% visdat::vis_miss()

.sum <- partial(sum, na.rm = TRUE, ... =)
agg <-
  misc_w_refs %>% 
  group_by(comp) %>% 
  summarize(
    n_game = n() / 2,
    mp = .sum(mp),
    n_card_y = .sum(n_card_y),
    n_card_r = .sum(n_card_r),
    n_foul = .sum(n_foul),
    n_offside = .sum(n_offside),
    n_tackle_won = .sum(n_tackle_won),
    # n_pk = .sum(n_pk),
    n_pk_att = .sum(n_pk_att)
  ) %>% 
  ungroup() %>% 
  mutate(
    # across(c(n_card_y:n_pk_att), ~11 * 90 * .x / mp)
    across(c(n_card_y:n_pk_att), ~.x / n_game)
  ) %>% 
  arrange(desc(n_game)) %>% 
  select(-c(mp, n_offside, n_tackle_won))
agg

write_rds(agg, file.path(dir_proj, 'misc_w_refs_agg.rds'))
