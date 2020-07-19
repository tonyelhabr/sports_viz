
library(tidyverse)
library(ggtext)
library(teamcolors)

# Data: https://twitter.com/JacobEGoldstein/status/1276694596408315906
# Inspiration: https://twitter.com/experimental361/status/1281573092012126210


tm_cols <- teamcolors::league_pal('nba')
tm_cols['San Antonio Spurs'] <- '#000000' # Use black instead of grey to prevent overlap with current seeding color.

seeds <-
  tibble(
    tm = c(rep('Los Angeles Lakers', 2L), rep('Los Angeles Clippers', 6L), rep('Denver Nuggets', 6L), rep('Utah Jazz', 6L), rep('Houston Rockets', 6L), rep('Oklahoma City Thunder', 6L), rep('Dallas Mavericks', 5L), rep('Memphis Grizzlies', 5L), rep('Portland Trail Blazers', 6L), rep('New Orleans Pelicans', 6L), rep('Sacramento Kings', 6L), rep('San Antonio Spurs', 6L), rep('Phoenix Suns', 6L), rep('Minnesota Timberwolves', 1L), rep('Golden State Warriors', 1L)),
    seed = c(seq.int(1L, 2L), seq.int(1L, 6L), seq.int(2L, 7L), seq.int(2L, 7L), seq.int(2L, 7L), seq.int(2L, 7L), seq.int(3L, 7L), seq.int(8L, 12L), seq.int(8L, 13L), seq.int(8L, 13L), seq.int(8L, 13L), seq.int(8L, 13L), seq.int(8L, 13L), 14L, 15L),
    p = c(c(.997, .003), c(.003, .819, .151, .022, .005, .001), c(.146, .482, .233, .099, .036, .005), c(.022, .242, .34, .231, .119, .046), c(.008, .079, .25, .331, .216, .116), c(.002, .04, .106, .204, .387, .262), c(.006, .049, .131, .241, .572), c(.715, .258, .022, .005, .000), c(.095, .236, .284, .204, .131, .005), c(.102, .232, .251, .216, .150, .049), c(.054, .174, .234, .259, .205, .073), c(.029, .090, .168, .232, .285, .196), c(.004, .010, .041, .084, .229, .632), c(1), c(1))
  )
seeds

seeds_current <-
  tibble(
    tm = c('Los Angeles Lakers', 'Los Angeles Clippers', 'Denver Nuggets', 'Utah Jazz', 'Houston Rockets', 'Oklahoma City Thunder', 'Dallas Mavericks', 'Memphis Grizzlies', 'Portland Trail Blazers', 'New Orleans Pelicans', 'Sacramento Kings', 'San Antonio Spurs', 'Phoenix Suns', 'Minnesota Timberwolves', 'Golden State Warriors'),
    seed = c(1L:4L, 6L, 5L, 7L:15L)
  ) %>% 
  mutate(
    lab_tm = glue::glue('<span style="white-space:pre">{sprintf("%2d ", seed)}</span><b style="color:#000000">{tm}</b>')
  )

hists <-
  seeds %>% 
  group_by(tm) %>% 
  mutate(p_agg = sum(seed * p)) %>% 
  ungroup() %>% 
  mutate(rnk = dense_rank(p_agg)) %>% 
  left_join(seeds_current %>% rename(seed_current = seed)) %>% 
  fill(lab_tm) %>% 
  mutate(
    lab_tm = forcats::fct_reorder(lab_tm, seed_current),
    x_l = seed - 0.5,
    x_r = seed + 0.5,
    y_pos = max(seed_current) - seed_current + 0.5,
    y_b = max(seed_current) - seed_current
  ) %>% 
  mutate(y_t = y_b + p)
hists

lab_tms <- hists %>% pull(lab_tm) %>% levels()

# It's not necessarily true that `rnk` == `seed_current`, so can't just use `rnk`.
hists_current <-
  seeds_current %>% 
  mutate(
    x_l = seed - 0.5,
    x_r = seed + 0.5,
    y_pos = max(seed) - seed + 0.5,
    y_b = max(seed) - seed,
    y_t = max(seed) - seed + 1
  )
hists_current

rects_outer <-
  hists %>% 
  mutate(seed = seed - 0.5) %>% 
  group_by(rnk, tm, y_b_min = y_b) %>% 
  summarize(
    x_l_min = min(x_l),
    x_r_max = max(x_r),
    y_t_max = y_b_min + 1
  ) %>% 
  ungroup()
rects_outer

viz <-
  hists %>% 
  ggplot() +
  aes(x = seed, y = y_pos) +
  geom_vline(
    aes(xintercept = 8.5),
    size = 1.1,
    color = '#808080'
  ) +
  geom_rect(
    data = hists_current,
    aes(xmin = x_l, xmax = x_r, ymin = y_b, ymax = y_t),
    fill = '#bebebe',
    show.legend = FALSE
  ) +
  geom_rect(
    aes(xmin = x_l + 0.1, xmax = x_r - 0.1, ymin = y_b, ymax = y_t, fill = tm),
    show.legend = FALSE
  ) +
  scale_fill_manual(values = tm_cols) +
  geom_rect(
    data = rects_outer,
    aes(xmin = x_l_min, xmax = x_r_max, ymin = y_b_min, ymax = y_t_max, color = tm),
    inherit.aes = FALSE,
    fill = NA,
    size = 1,
    show.legend = FALSE
  ) +
  scale_color_manual(values = tm_cols) +
  scale_y_continuous(
    breaks = seq(0.5, 14.5),
    # limits = c(0, 15.5),
    labels = rev(lab_tms)
  ) +
  scale_x_continuous(
    position = 'top',
    breaks = seq(0.5, 14.5, by = 2)
  ) +
  geom_text(
    data = tibble(x = seq(1, 15, by = 1), y = 15.5, lab = x),
    fontface = 'bold',
    family = 'Arial Narrow',
    size = 5,
    aes(x = x, y = y, label = lab)
  ) +
  theme_minimal(base_family = 'Arial Narrow') +
  theme(
    plot.title = element_text(face = 'bold', size = 14),
    plot.subtitle = element_markdown(size = 14),
    axis.text.x = element_blank(),
    axis.text.y = ggtext::element_markdown(size = 12, hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_line(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_markdown(size = 12, hjust = 0),
    plot.tag = element_markdown(size = 12, hjust = 1),
    plot.caption.position = 'plot',
    plot.tag.position = c(1, 0.01),
    plot.title.position = 'plot'
  ) +
  labs(
    title = 'PROBABILITIES OF PLAYOFF SEEDINGS: NBA WESTERN CONFERENCE, 2020 LEAGUE RESTART',
    subtitle = '<span style="color:#404040">OUTLINES SHOW SPREAD OF POSSIBLE MOVEMENT; INNER CHARTS SHOW PROBABILIITES</span>',
    caption = '<span style="color:#909090">CURRENT SEEDINGS ARE SHADED IN GREY</span>',
    tag = 'Data: @JacobEGoldstein | Inspiration: @experimental361 | Viz: @TonyElHabr',
    x = NULL, 
    y = NULL
  )
viz
ggsave(plot = viz, filename = 'C:/users/aelhabr/desktop/nba_seed_p.png', device = 'png', type = 'cairo', width = 8, height = 7) 
ggsave(plot = viz, filename = 'C:/users/aelhabr/desktop/nba_seed_p_nocairo.png', device = 'png', width = 8, height = 7) 
 
 
