
library(tidyverse)
library(patchwork)
library(metR)
dir_proj <- '21-202021_epl_heatmap'
path_events <- file.path(dir_proj, 'events.rds')
path_forms <- file.path(dir_proj, 'forms.rds')
events <- path_events %>% read_rds()
forms <- path_forms %>% read_rds()
extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = element_text('Karla', face = 'bold', size = 18, color = 'gray20', hjust = 0.5),
  # plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', face = 'bold', size = 14, color = 'gray20'),
  axis.text = element_text('Karla', size = 14),
  # axis.title = element_text(size = 24, face = 'bold'),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  plot.caption = element_text('Karla', size = 12, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.02, 0.01),
  legend.text = element_text(size = 14),
  strip.text = element_text(size = 14),
  strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

fouls <- events %>% filter(type_name == 'Foul')
fouls

subs <-
  events %>% 
  # filter(type_name %in% c('SubstitutionOn', 'SubstitutionOff')) %>% 
  filter(type_name == 'SubstitutionOff') %>% 
  select(season, match_id, match, team_id, team_name, player_id, player_name, type_name, expanded_minute)
subs

events_last <-
  events %>% 
  group_by(match_id) %>% 
  slice_max(expanded_minute) %>% 
  ungroup() %>% 
  select(match_id, expanded_minute)
events_last

subs_grp <-
  subs %>% 
  arrange(expanded_minute) %>% 
  group_by(match_id, team_id, type_name) %>% 
  # This should match the `idx` from `forms`.
  mutate(idx = row_number(expanded_minute) + 1L) %>% 
  ungroup() %>% 
  select(match_id, team_id, idx, expanded_minute)

minutes_init <-
  subs_grp %>% 
  filter(idx == min(idx)) %>% 
  mutate(idx = 1L, expanded_minute = 0L)
minutes_init

minutes_max <-
  subs_grp %>% 
  group_by(match_id, team_id) %>% 
  slice_max(idx, with_ties = FALSE) %>% 
  ungroup() %>% 
  left_join(events_last %>% rename(expanded_minute_last = expanded_minute)) %>% 
  mutate(
    across(idx, ~.x + 1L), 
    across(expanded_minute, ~ifelse(expanded_minute > expanded_minute_last, .x, expanded_minute_last))
  ) %>% 
  select(-expanded_minute_last)
minutes_max

minutes <-
  list(
    minutes_init,
    subs_grp,
    minutes_max
  ) %>% 
  reduce(bind_rows)
minutes

mp <-
  forms %>% 
  left_join(minutes) %>% 
  left_join(
    minutes %>% 
      mutate(across(idx, ~.x - 1L)) %>% 
      rename(expanded_minute_lead1 = expanded_minute)
  ) %>% 
  mutate(dur = expanded_minute_lead1 - expanded_minute) %>% 
  group_by(match_id, player_id, player_name) %>% 
  summarize(across(dur, sum)) %>% 
  ungroup()
mp

matches <- forms %>% distinct(season, match_id, team_name, side) %>% pivot_wider(names_from = side, values_from = team_name)
matches
mp %>% 
  head(10) %>% 
  left_join(matches)

.generate_lineups <- function(forms, subs, .match_id = 1485271) {
  
  forms_filt <- forms %>% filter(match_id == .match_id)
  subs_filt <- subs %>% filter(match_id == .match_id)
  teams <- forms_filt %>% distinct(team_name) %>% pull(team_name)

  .f <- function(team) {
    team <- 'Arsenal'
    forms_team_filt <- forms_filt %>% filter(team_name == team)

    subs_team_filt <-
      subs_filt %>% 
      filter(team_name == team) %>% 
      arrange(expanded_minute) %>% 
      group_by(type_name) %>% 
      # This should match the `idx` from `forms`.
      mutate(idx = row_number(expanded_minute) + 1L) %>% 
      ungroup()
    subs_team_filt
    
    minutes_init <-
      subs_team_filt %>% 
      select(idx, expanded_minute) %>% 
      add_row(idx = 1L, expanded_minute = 0L)
    
    minute_max <- max(minutes_init$expanded_minute) %>% max(. + 1L, 96)
    minutes <-
      minutes_init %>% 
      add_row(idx = max(minutes_init$idx) + 1L, expanded_minute = minute_max) %>% 
      arrange(idx)
    minutes
    
    res <-
      forms_team_filt %>% 
      select(idx, formation_slot, player_id, player_name) %>% 
      left_join(minutes) %>% 
      left_join(
        minutes %>% 
          mutate(idx = idx - 1L) %>% 
          select(idx, expanded_minute_impute = expanded_minute)
      ) %>% 
      mutate(dur = expanded_minute_impute - expanded_minute) %>% 
      group_by(player_id, player_name) %>% 
      summarize(across(dur, sum)) %>% 
      ungroup()
    res
  }

forms_labs <-
  tibble(
    row_max = c(rep(3L, 3), rep(4L, 4), rep(5L, 5), rep(6L, 6), rep(7L, 7), rep(8L, 8)),
    row = c(seq.int(1, 3), seq.int(1, 4), seq.int(1, 5), seq.int(1, 6), seq.int(1, 7), seq.int(1, 8)),
    pos = c(c('G', 'D', 'M'), c('G', 'D', 'M', 'F'), c('G', 'D', 'M', 'M', 'F'), c('G', 'D', 'M', 'M', 'A', 'F'), c('G', 'D', 'M', 'M', 'A', 'A', 'F'), c('G', 'D', 'M', 'M', 'M', 'A', 'A', 'F'))
  )
forms_labs

forms %>% 
  count(player_id, player_name, pos, sort = TRUE) %>% 
  group_by(player_id, player_name) %>%
  mutate(total = sum(n)) %>% 
  mutate(frac = n / total) %>% 
  slice_max(frac, with_ties = FALSE) %>% 
  ungroup() %>% 
  arrange(frac) %>% 
  ggplot() +
  aes(x = frac, y = n) +
  geom_point()

pos <-
  forms %>% 
  count(player_id, player_name, pos) %>% 
  group_by(player_id, player_name) %>%
  mutate(total = sum(n)) %>% 
  mutate(frac = n / total) %>% 
  slice_max(frac, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(player_id, player_name, pos)
pos

fouls_pos <- 
  events %>% 
  left_join(pos) %>% 
  filter(pos != 'G') %>% 
  mutate(across(pos, ~ordered(.x, c('D', 'M', 'F')))) %>% 
  mutate(across(pos, ~fct_recode(.x, 'Defenders' = 'D', 'Midfielders' = 'M', 'Forwards' = 'F')))
fouls_pos

fouls_by_player_top <-
  fouls_pos %>% 
  drop_na(player_id) %>% 
  count(outcome_type_name, pos, player_id, player_name) %>% 
  arrange(outcome_type_name, pos, desc(n)) %>% 
  group_by(outcome_type_name, pos) %>% 
  slice_max(n, n = 3, with_ties = FALSE) %>% 
  ungroup()
fouls_by_player_top

convert_to_xyz <- function(data, n = 200) {
  x <- data %>% pull(x)
  y <- data %>% pull(y)
  # x_rng <- range(x)
  # y_rng <- range(y)
  x_rng <- c(0, 100)
  y_rng <- x_rng
  
  bw_x <- MASS::bandwidth.nrd(x)
  bw_y <- MASS::bandwidth.nrd(y)
  bw_xy <- c(bw_x, bw_y)
  dz <- MASS::kde2d(x, y, h = bw_xy, n = n, lims = c(x_rng, y_rng))
  colnames(dz$z) <- dz$y
  res <- 
    dz$z %>%
    as_tibble() %>% 
    mutate(x = dz$x) %>% 
    pivot_longer(-x, names_to = 'y', values_to = 'z') %>% 
    mutate(y = as.double(y))
  res
}

fouls_xyz <-
  fouls_pos %>% 
  group_nest(pos) %>% 
  mutate(data = map(data, convert_to_xyz)) %>% 
  unnest(data) %>% 
  group_by(pos) %>%
  mutate(across(z, list(norm = ~(.x - min(.x)) / (max(.x) - min(.x))))) %>%
  ungroup()
fouls_xyz

# Hex experimentation
# fouls_xyz %>%
#   filter(pos == 'Defenders') %>% 
#   ggplot() +
#   aes(x = x, y = y) +
#   ggsoccer::annotate_pitch(limits = FALSE) +
#   # ggsoccer::theme_pitch() +
#   # stat_summary_hex(fun = ~mean(.x, na.rm = TRUE)) +
#   coord_fixed(clip = 'on') +
#   stat_summary_hex(
#     mapping = aes(z = z_norm),
#     alpha = 0.7,
#     fun = ~ mean(.x)
#   ) +
#   # scale_fill_steps2(low = "#ffffff", mid = "#e0e0e0", high = "#1094c4") +
#   scale_fill_gradient2(low = 'white', high = 'red') +
#   # scale_fill_viridis_c() +
#   guides(fill = FALSE)


# library(metR)
pos <- c('Defenders', 'Midfielders', 'Forwards')
.plot_by_outcome_type_name <- function(.outcome_type_name) {
  .plot_by_pos <- function(.pos) {
    # .pos <- 'Defenders'
    pal <- case_when(
      .pos == 'Defenders' ~ 'Greens',
      .pos == 'Midfielders' ~ 'Oranges',
      .pos == 'Forwards' ~ 'Purples'
    )
    
    lab_tag <- case_when(
      .pos == 'Defenders' ~ '**Viz**: Tony ElHabr', #  | **Data**: Tedious film sessions',
      TRUE ~ ' '
    )
    
    viz <-
      fouls_xyz %>%
      filter(pos == .pos) %>% 
      ggplot() +
      aes(x = x, y = y, z = z_norm) +
      # ggsoccer::annotate_pitch(limits = FALSE) +
      ggsoccer::annotate_pitch(fill = '#141622', clor = 'white', limits = FALSE) +
      ggsoccer::theme_pitch() +
      geom_contour_filled(bins = 8, alpha = 0.7) +
      # metR::geom_contour_fill(aes(x = x, y = y, z = z_norm), bins = 8, alpha = 0.7) +
      metR::geom_contour_tanaka(bins = 8, alpha = 0.7) +
      guides(fill = FALSE) +
      scale_fill_brewer(palette = pal) +
      coord_fixed(clip = 'off') +
      theme(
        # panel.background = element_rect(fill = 'black'),
        # plot.background = element_rect(fill = 'red')
        # plot.margin = margin(0, 0, 0, 0),
        plot.tag = ggtext::element_markdown(hjust = 0, size = 14),
        plot.tag.position = c(.05, 0.01),
        plot.subtitle = element_text(size = 18, hjust = 0.5)
      ) +
      labs(
        caption = ' ',
        tag = lab_tag,
        subtitle = .pos
      )
    viz
  }
  
  plots <- pos %>% map(.plot_by_pos)
  # plots[[1]]
  viz_fouls_by_pos <- 
    patchwork::wrap_plots(plots) +
    patchwork::plot_annotation(
      title = 'Where Are Fouls %s?',
      theme = theme(
        plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 24, hjust = 0.5)
      )
    )
  viz_fouls_by_pos
  ggsave(
    plot = viz_fouls_by_pos,
    filename = file.path(dir_proj, 'viz_fouls_by_pos.png'),
    width = 10,
    height = 6,
    type = 'cairo'
  )
}

player_name_filt <- 'Tomas Soucek'
fouls_pos1 <- fouls_pos %>% filter(player_name == !!player_name_filt)
fouls_pos2 <- fouls_pos %>% filter(player_name != !!player_name_filt)
x1 <- fouls_pos1 %>% pull(x)
y1 <- fouls_pos1 %>% pull(y)
x2 <- fouls_pos2 %>% pull(x)
y2 <- fouls_pos2 %>% pull(y)
x_rng <- range(c(x1, x2))
y_rng <- range(c(y1, y2))
bw_x <- MASS::bandwidth.nrd(c(x1, x2))
bw_y <- MASS::bandwidth.nrd(c(y1, y2))
bw_xy <- c(bw_x, bw_y)
n <- 200
d21 <- MASS::kde2d(x1, y1, h = bw_xy, n = n, lims = c(x_rng, y_rng))
d22 <- MASS::kde2d(x2, y2, h = bw_xy, n = n, lims = c(x_rng, y_rng))
dz <- d21
dz$z <- d21$z - d22$z
colnames(dz$z) <- dz$y
dzz <- 
  dz$z %>%
  as_tibble() %>% 
  mutate(x = d2$x) %>% 
  pivot_longer(-x, names_to = 'y', values_to = 'z') %>% 
  mutate(y = as.double(y))
dzz

dzz %>% pull(z) %>% mean()

library(metR)
p <- 
  dzz %>% 
  ggplot() +
  aes(x = x, y = y, z = sqrt(z)) +
  geom_contour_fill(
    data = . %>% filter(z >= mean(z))
  ) +
  geom_contour_tanaka(bins = 5)  +
  coord_fixed(clip = 'off') +
  scale_fill_gradient2(low = 'white', mid = 'white', high = 'red')
p
viz <-
  fouls_pos %>% 
  # Need to do this to match WhoScored
  # filter(player_name %in% c('Mohamed Salah', 'Jamie Vardy', 'Danny Ings', 'Harry Kane', '')) %>% 
  # mutate(
  #   across(
  #     c(x, y),
  #     ~case_when(
  #       side == 'away' ~ 1 * (100 - .x),
  #       TRUE ~ .x
  #     )
  #   )
  # ) %>% 
# # Need to do this for symmetry of home/away, with attacking going right?
# mutate(
#   across(
#     c(x, y),
#     ~case_when(
#       side == 'home' ~ 1 * (100 - .x),
#       TRUE ~ .x
#     )
#   )
# ) %>% 
ggplot() +
  ggsoccer::annotate_pitch() +
  ggsoccer::theme_pitch() +
  aes(x = x, y = y) +
  # geom_point(alpha = 1) +
  # geom_label(aes(label = player_name)) +
  # geom_contour(aes(z = ..count..)) +
  geom_density2d_filled() +
  # geom_contour_fill() +
  # guides(color = FALSE)
  # facet_wrap(~pos, scales = 'free') +
  guides(fill = FALSE)
viz
