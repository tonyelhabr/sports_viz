
library(tidyverse)
library(extrafont)
library(ggtext)
library(tonythemes)
library(mgcv)
library(arrow)

dir_proj <- '50-throw_in_times'
gray_wv <- rgb(24, 24, 24, maxColorValue = 255)
gray_grid_wv <- rgb(64, 64, 64, maxColorValue = 255)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  title = element_text('Karla', size = 22, color = 'white'),
  plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 22, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown('Karla', size = 18, color = '#f1f1f1'),
  axis.text = element_text('Karla', color = 'white', size = 14),
  axis.title = element_text('Karla', size = 14, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = gray_grid_wv),
  panel.grid.minor = element_line(color = gray_grid_wv),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = gray_wv, color = gray_wv),
  plot.caption = element_text('Karla', color = 'white', hjust = 1, size = 12, face = 'italic'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'white', hjust = 0),
  plot.tag.position = c(0.01, 0.01),
  panel.background = element_rect(fill = gray_wv, color = gray_wv)
)

pal <- c(
  'Behind' = '#ef426f', 
  'Ahead' = '#00b2a9', 
  'Tied' = '#ff8200'
)

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

preds <- file.path(dir_proj, 'preds-throw_ins-g_state.parquet') %>% 
  arrow::read_parquet() %>% 
  filter(period_id == 2L)

gam_preds_90 <- preds %>% 
  group_nest(g_state) %>% 
  transmute(
    g_state,
    gam_pred_90 = map_dbl(
      data, 
      ~mgcv::gam(
        formula(.pred ~ s(minute_lab, bs = 'cs')),
        data = .x
      ) %>% 
        predict(
          newdata = tibble(minute_lab = 90)
        )
    )
  )

p <- preds %>% 
  ggplot() +
  aes(
    x = minute_lab,
    y = .pred,
    color = g_state
  ) +
  geom_vline(
    linetype = '11',
    size = 1,
    color = 'white',
    aes(
      xintercept = 90
    )
  ) +
  geom_smooth(
    size = 1.5,
    method = 'gam',
    formula = formula(y ~ s(x, bs = 'cs')),
    se = FALSE
  ) +
  ggtext::geom_richtext(
    fill = NA_character_,
    label.color = NA_character_,
    family = 'Karla',
    hjust = 0,
    size = pts(20),
    data = gam_preds_90 %>% filter(g_state != 'Ahead'),
    aes(
      x = 92,
      y = gam_pred_90 + 0.2,
      label = sprintf('**%s: %.1f**', g_state, gam_pred_90)
    )
  ) +
  ggtext::geom_richtext(
    fill = NA_character_,
    label.color = NA_character_,
    family = 'Karla',
    hjust = 0,
    size = pts(20),
    data = gam_preds_90 %>% filter(g_state == 'Ahead'),
    aes(
      x = 92,
      y = gam_pred_90 - 0.2,
      label = sprintf('**%s: %.1f**', g_state, gam_pred_90)
    )
  ) +
  guides(
    color = 'none'
  ) +
  scale_color_manual(
    values = pal
  ) +
  scale_x_continuous(
    limits = c(45, 100),
    breaks = c(45, 70, 90)
  ) +
  theme(
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = glue::glue('How much time do <span style="color:{pal[["Ahead"]]}"><b>leading</b></span> teams waste on throw-ins?'),
    subtitle = glue::glue('There is a large difference in time taken for throw-ins when<br/><span style="color:{pal[["Ahead"]]}"><b>ahead</b></span> vs. <span style="color:{pal[["Behind"]]}"><b>behind</b></span> at the 90th minute.'),
    x = 'Minute (second half)',
    y = 'Time between dead ball and throw-in',
    caption = 'Throw-ins after substitutions dropped.\nGAM smoothing applied to output of gradient boosted model\nwith features for minute, period, and game state.',
    tag = '**Viz**: Tony ElHabr | **Data**: 2017/18 - 2021/22 Premier League'
  )
p

path_viz <- file.path(dir_proj, 'throw_ins-g_state.png')
ggsave(
  plot = p,
  filename = path_viz,
  width = 10,
  height = 10 / 1.25
)

tonythemes:::add_logo(
  path_viz = path_viz,
  path_logo = file.path(dir_proj, 'epl-logo-white.png'),
  delete = TRUE,
  path_suffix = '-w_logo',
  logo_scale = 0.1,
  idx_x = 0.01,
  idx_y = 0.98,
  adjust_y = FALSE
)