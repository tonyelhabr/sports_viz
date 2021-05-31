
library(tidyverse)

dir_proj <- '30-var'
source(file.path(dir_proj, 'helpers.R'))

res <- 
  2019L:2020L %>% 
  tibble(season = .) %>% 
  mutate(data = map(season, ~do_scrape(.x))) %>% 
  unnest_wider(data) %>% 
  mutate(
    across(season, ~sprintf('%d-%s', season, str_sub(season+1, 3, 4)))
  )
agg <- res %>% select(season, agg) %>% unnest(agg)
agg

subjective_calls <-
  agg %>% 
  select(season, team, matches('^subjective_decision')) %>% 
  pivot_longer(
    -c(season, team),
    names_to = 'side',
    values_to = 'n'
  ) %>% 
  mutate(across(side, ~str_remove(.x, 'subjective_decisions_'))) %>% 
  mutate(n_sign = ifelse(side == 'against', -n, n) %>% na_if(0)) 
subjective_calls

subjective_calls_agg <-
  subjective_calls %>% 
  group_by(season, team) %>% 
  summarize(
    n_net = sum(n_sign, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  arrange(season, desc(n_net)) %>% 
  # mutate(rnk = tidytext::reorder_within(team, n_net, season))
  group_by(season) %>% 
  mutate(rnk = row_number(n_net)) %>% 
  ungroup() %>% 
  mutate(
    lab_rnk = sprintf('%s (%+d)', team, n_net) %>% str_replace('[+]0', '0'),
    across(
      lab_rnk,
      ~case_when(
        team %in% c('Manchester United', 'Arsenal') ~ sprintf('<b><span = style="color:#000000">%s</span></b>', lab_rnk),
        TRUE ~ .x
      )
    )
  )
subjective_calls_agg

df <-
  subjective_calls %>%
  left_join(subjective_calls_agg) %>%
  left_join(
    subjective_calls %>%
      select(season, team, side, n_sign) %>%
      pivot_wider(names_from = side, values_from = n_sign)
  )
df

df_for <- df %>% filter(abs(`for`) >= abs(against) | is.na(against))
df_against <- df %>% filter(abs(`for`) < abs(against) | is.na(`for`))
df_for_for <- df_for %>% filter(side == 'for')
df_against_against <- df_against %>% filter(side == 'against')
df_for_against <- df_for %>% filter(side == 'against')
df_against_for <- df_against %>% filter(side == 'for')

col_pos <- '#00b2a9'
col_neg <- '#ef426f'
pt_buffer <- 0.25
lab_tag <- '**Viz**: Tony ElHabr | **Data**: espn'
p <-
  df %>% 
  ggplot() +
  aes(y = rnk, x = n_sign) +
  facet_wrap(~season, scales = 'fixed') +
  f_segment(
    data = df_for_for,
    aes(x = 0, xend = `for`, y = rnk + pt_buffer, yend = rnk + pt_buffer),
    color = col_pos
  ) +
  f_text(
    data = df_for_for,
    aes(x = 0, y = rnk, label = lab_rnk),
    color = 'black',
    hjust = 1
  ) +
  f_segment(
    data = df_against_against,
    aes(x = 0, xend = against, y = rnk - pt_buffer, yend = rnk - pt_buffer),
    color = col_neg
  ) +
  f_text(
    data = df_against_against,
    aes(x = 0, y = rnk, label = lab_rnk),
    color = 'black',
    hjust = 0
  ) +
  f_segment(
    data = df_for_against %>% filter(`for` != n_net),
    aes(x = `for`, xend = n_net, y = rnk - pt_buffer, yend = rnk - pt_buffer),
    color = col_neg
  ) +
  f_segment(
    data = df_against_for %>% filter(against != n_net),
    aes(x = against, xend = n_net, y = rnk + pt_buffer, yend = rnk + pt_buffer),
    color = col_pos
  ) +
  theme(
    # strip.text = element_text(family = 'Karla', size = 16),
    strip.text.x = element_text(family = 'Karla', face = 'bold', size = 16),
    panel.background = element_rect(color = 'grey80', size = 2),
    # plot.title = element_text(size = 18, hjust = 0),
    plot.subtitle = ggtext::element_markdown(size = 12, hjust = 0),
    # plot.caption = ggtext::element_markdown(size = 12, hjust = 1),
    axis.text.y = element_blank(),
    # plot.caption = element_text(),
    # plot.caption = c(ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0), ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 1)),
    # plot.caption = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0),
    panel.grid.minor.x = element_line(),
    panel.grid.major.y = element_blank()
  ) +
  scale_x_continuous(breaks = seq.int(-8, 8, by = 4)) +
  coord_cartesian(xlim = c(-8, 8), clip = 'off') +
  labs(
    title = 'Are the Odds Ever In Your Team\'s Favor?',
    subtitle = '<b><span = style="color:#000000">Manchester United</span></b> has benefited the most from "subjective" VAR calls over the past two seasons<br/>while <b><span style="color:#000000">Arsenal</span></b> has been on the unlucky end of such calls.',
    x = '"Subjective" VAR Calls For/Against',
    y = NULL,
    tag = lab_tag
  )
p

path_viz_subjective <- file.path(dir_proj, 'viz_var_subjective.png')
h <- 9.5
ggsave(
  plot = p,
  file = path_viz_subjective,
  height = h,
  width = h,
  type = 'cairo'
)

add_logo_epl(
  path_viz = path_viz_subjective,
  idx_x = 0.01,
  adjust_y = FALSE,
  idx_y = 0.99
)

# p2 <- ggplotGrob(p)
# # Find the grob tree containing the right caption (as child)
# k <- which(p2$layout$name=='caption')
# # Copy the 'right caption' text grob in gt
# gt <- p2$grobs[[k]]$children[[1]]
# 
# # Modify content and position of the text grob  
# gt$label <- lab_tag # 'LEFT CAPTION'
# gt$name <- 'GRID.text.left'
# gt$x <- unit(0,'npc')
# gt$hjust <- 0
# 
# # Add gt (left caption) to the title grob containing the right caption
# p2$grobs[[k]] <- grid::addGrob(p2$grobs[[k]], gt)
# grid::grid.draw(p2)

# Some other cool infor in @DaleJohnson's. (Of course Mike Dean has the most overturned calls of any ref!) https://twitter.com/DaleJohnsonESPN/status/1396841267749851136?s=20
