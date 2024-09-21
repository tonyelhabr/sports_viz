library(tidyverse)
library(RPostgres)
library(MASS)
library(dbplyr)

PROJ_DIR <- '88-asa_thomas_bayes'

font='Roboto'
library(showtext)
font_add_google("Roboto", family = "Roboto")
showtext_auto()

source(file.path(PROJ_DIR, 'tony_fxns.R'))

theme_gplus <- function(...) {
  list(
    ...,
    ## https://analysisevolved.slack.com/archives/CF8JS86Q7/p1690918539121609
    theme_minimal(base_family = font),
    theme(
      text = element_text(color = 'white', size = 20),
      axis.text.y = element_text(color = 'white'),
      axis.text.x = element_text(color = 'white', size = 11),
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = 'top',
      plot.title = element_text(size = 24, hjust = 0),
      plot.title.position = 'plot',
      plot.caption.position = 'plot',
      plot.subtitle = element_text(color = '#6E7275', size = 14),
      plot.caption = element_text(color = '#6E7275', size = 11),
      panel.border = element_blank(),
      panel.grid.major.y = element_line(color = '#6E7275'),
      panel.grid.major.x = element_line(color = '#6E7275'),
      panel.background = element_rect(fill='#15202B', color='#15202B'),
      plot.background = element_rect(fill='#15202B', color='#15202B')
    )
  )
}


ENV_VARS <- Sys.getenv()
get_env_var <- function(var) {
  
  if (var %in% names(ENV_VARS)) {
    ENV_VARS[[var]]
  } else {
    warning(
      paste(
        'No environment variable named', var, 'found.'
      )
    )
  }
}

# pal <- c(
#   "#F82D97",
#   "#FF6A62" ,
#   "#F8FF01",
#   "#2EF8A0",
#   "#01C4E7",
#   "#A510D3"
# )
pal <- c(
  "#17B8E1",
  "#FC5150",
  "#9200C9",
  "#F7FF0B",
  "#31FC8F",
  "#FC7108"
)


con <- dbConnect(
  Postgres(),
  user = get_env_var('asa_user'),
  password = get_env_var('asa_password'),
  host = get_env_var('asa_host'),
  port = as.integer(get_env_var('asa_port')),
  dbname = get_env_var('asa_db_name'),
  sslmode = get_env_var('asa_sslmode')
)


# xg ----------------------------------------------------------------------
raw = 
  bind_rows(
    tbl(con, in_schema("mls", "xgoals")) %>% 
      filter(!is.na(xg_goalkeeper)) %>% 
      dplyr::select(game_id, xg_goalkeeper, xg_shooter, goalkeeper_id, goal, one_on_one) %>%
      rename(player_id = goalkeeper_id) %>% 
      left_join(
        tbl(con, in_schema("all_opta", "players")) %>% 
          dplyr::select(player_id, player_name)
      )%>% 
      left_join(
        tbl(con, in_schema("mls", "games")) %>% 
          dplyr::select(game_id, season_name)
      ) %>% 
      mutate(league = 'mls') %>% 
      as_tibble(),
    tbl(con, in_schema("nwsl", "xgoals")) %>% 
      filter(!is.na(xg_goalkeeper)) %>% 
      dplyr::select(game_id, xg_goalkeeper, xg_shooter, goalkeeper_id, goal, one_on_one) %>%
      rename(player_id = goalkeeper_id) %>% 
      left_join(
        tbl(con, in_schema("all_opta", "players")) %>% 
          dplyr::select(player_id, player_name)
      )%>% 
      left_join(
        tbl(con, in_schema("nwsl", "games")) %>% 
          dplyr::select(game_id, season_name)
      ) %>% 
      mutate(league='nwsl') %>% 
      as_tibble(),
    tbl(con, in_schema("uslc", "xgoals")) %>% 
      filter(!is.na(xg_goalkeeper)) %>% 
      dplyr::select(game_id, xg_goalkeeper, xg_shooter, goalkeeper_id, goal, one_on_one) %>%
      rename(player_id = goalkeeper_id) %>% 
      left_join(
        tbl(con, in_schema("all_opta", "players")) %>% 
          dplyr::select(player_id, player_name)
      )%>% 
      left_join(
        tbl(con, in_schema("uslc", "games")) %>% 
          dplyr::select(game_id, season_name)
      ) %>% 
      mutate(league = 'usl') %>% 
      as_tibble()
  )


# aggregate ---------------------------------------------------------------

aggd = raw %>% group_by(player_id, player_name, league) %>%
  mutate(goal = ifelse(is.na(goal), 0, 1)) %>% 
  summarise(psxg = sum(xg_goalkeeper),
            xg = sum(xg_shooter),
            goals = sum(goal),
            shots = n()) %>% 
  ungroup %>% 
  mutate(psxg_ratio  = goals/psxg,
         xg_ratio = goals/xg)


# plot function -----------------------------------------------------------

plot_cis = function(df, linecolor=pal[5], 
                    medianval = median(xg_adjust$xg_ratio),
                    ntile_text_size=8,
                    ntile_text_side='right',
                    .title='Top Overperformers in MLS by G/xG',
                    .subtitle='On target shots only, Error bars represent 90% CI',
                    xlab='Adjusted G/xG'){
  if(ntile_text_side=='left'){
    ntile_vjust=-2
  }else if(ntile_text_side=='right'){
    ntile_vjust=2
  }
  
  df %>% 
    ggplot() +
    aes(y = player_name) +
    geom_point(aes(x = eb_adj_ratio_mean), color=linecolor) +
    annotate('text',
             x = medianval,
             y=20, label='50th %ile',
             color='white',
             family=font, 
             angle=90, hjust=1, vjust=ntile_vjust,
             size=ntile_text_size)+
    geom_errorbarh(
      aes(
        xmin = eb_adj_ratio_lower_ci_bound,
        xmax = eb_adj_ratio_upper_ci_bound
      ),
      color = linecolor
    ) +
    geom_vline(aes(xintercept = medianval),
               color='white') +
    labs(title = .title,
         subtitle=.subtitle) +
    labs(x=xlab, y=NULL) +
    theme_gplus() +
    theme(panel.grid.major = element_blank())
}
# psxg adjust ------------------------------------------------------------------

psxg_adjust = aggd %>% 
  eb_adjust(ratio_metric = 'psxg_ratio',
            xg_metric = 'psxg',
            goal_metric = 'goals',
            shot_threshold = 50)
psxg_adjust %>% 
  filter(league=='mls') %>% 
  slice_min(eb_adj_ratio_mean, n = 20) %>% 
  mutate(player_name = fct_reorder(player_name, -eb_adj_ratio_mean)) %>% 
  plot_cis(.title = 'Top Overperformers in MLS by G/PSxG',
           medianval = median(psxg_adjust$psxg_ratio),
           xlab = 'Adjusted G/PSxG')

ggsave(file.path(PROJ_DIR, 'mls_psxg_overperformers.png'), units='px',width=1000, height=1000)

psxg_adjust %>% 
  filter(league=='nwsl') %>% 
  slice_min(eb_adj_ratio_mean, n = 20) %>% 
  mutate(player_name = fct_reorder(player_name, -eb_adj_ratio_mean)) %>% 
  plot_cis(.title = 'Top Overperformers in NWSL by G/PSxG',
           medianval = median(psxg_adjust$psxg_ratio),
           xlab = 'Adjusted G/PSxG')

ggsave(file.path(PROJ_DIR, 'nwsl_psxg_overperformers.png'), units='px',width=1000, height=1000)


psxg_adjust %>% 
  filter(league=='mls') %>%
  slice_max(eb_adj_ratio_mean, n = 20) %>% 
  mutate(player_name = fct_reorder(player_name, eb_adj_ratio_mean))  %>% 
  plot_cis(.title = 'Top Underperformers in MLS by G/PSxG',
           medianval = median(psxg_adjust$psxg_ratio),
           xlab = 'Adjusted G/PSxG',
           ntile_text_side = 'left',
           linecolor = pal[2])

ggsave(file.path(PROJ_DIR, 'mls_psxg_underperformers.png'), units='px',width=1000, height=1000)

psxg_adjust %>% 
  filter(league=='nwsl') %>%
  slice_max(eb_adj_ratio_mean, n = 20) %>% 
  mutate(player_name = fct_reorder(player_name, eb_adj_ratio_mean))  %>% 
  plot_cis(.title = 'Top Underperformers in NWSL by G/PSxG',
           medianval = median(psxg_adjust$psxg_ratio),
           xlab = 'Adjusted G/PSxG',
           ntile_text_side = 'left',
           linecolor = pal[2])

ggsave(file.path(PROJ_DIR, 'nwsl_psxg_underperformers.png'), units='px',width=1000, height=1000)

# xg adjust ------------------------------------------------------------------

xg_adjust = aggd %>% 
  eb_adjust(ratio_metric = 'xg_ratio',
            xg_metric = 'xg',
            goal_metric = 'goals',
            shot_threshold = 50)


xg_adjust %>% 
  filter(league=='mls') %>% 
  slice_min(eb_adj_ratio_mean, n = 20) %>% 
  mutate(player_name = fct_reorder(player_name, -eb_adj_ratio_mean)) %>% 
  plot_cis()

ggsave(file.path(PROJ_DIR, 'mls_xg_overperformers.png'), units='px',width=1000, height=1000)

xg_adjust %>% 
  filter(league=='nwsl') %>% 
  slice_min(eb_adj_ratio_mean, n = 20) %>% 
  mutate(player_name = fct_reorder(player_name, -eb_adj_ratio_mean)) %>% 
  plot_cis(.title='Top Overperformers in NWSL by G/xG')

ggsave(file.path(PROJ_DIR, 'nwsl_xg_overperformers.png'), units='px',width=1000, height=1000)


xg_adjust %>% 
  filter(league=='mls') %>% 
  slice_max(eb_adj_ratio_mean, n = 20) %>% 
  mutate(player_name = fct_reorder(player_name, eb_adj_ratio_mean)) %>% 
  plot_cis(.title='Top Underperformers in MLS by G/xG',
           linecolor = pal[2], ntile_text_side = 'left')
ggsave(file.path(PROJ_DIR, 'mls_xg_underperformers.png'), units='px',width=1000, height=1000)

xg_adjust %>% 
  filter(league=='nwsl') %>% 
  slice_max(eb_adj_ratio_mean, n = 20) %>% 
  mutate(player_name = fct_reorder(player_name, eb_adj_ratio_mean)) %>% 
  plot_cis(.title='Top Underperformers in NWSL by G/xG',
           linecolor = pal[2], ntile_text_side = 'left')
ggsave(file.path(PROJ_DIR, 'nwsl_xg_underperformers.png'), units='px',width=1000, height=1000)



# yoy stability -----------------------------------------------------------
yoy = raw %>% group_by(player_id, player_name, season_name) %>%
  mutate(goal = ifelse(is.na(goal), 0, 1)) %>% 
  summarise(psxg = sum(xg_goalkeeper),
            xg = sum(xg_shooter),
            goals = sum(goal),
            shots = n()) %>% 
  ungroup %>% 
  mutate(psxg_ratio  = goals/psxg,
         xg_ratio = goals/xg)

yoy_raw = yoy %>% 
  mutate(xg_ratio = ifelse(shots < 50, NA, xg_ratio),
         psxg_ratio = ifelse(shots < 50, NA, psxg_ratio)) %>%
  group_by(player_id) %>% 
  arrange(season_name, .by_group = T) %>% 
  mutate(prev_xg_ratio = lag(xg_ratio),
         prev_psxg_ratio = lag(psxg_ratio)) %>% 
  ungroup %>% 
  filter(!is.na(prev_xg_ratio),
         !is.na(xg_ratio))

yoy_psxg_adjust = yoy %>% 
  eb_adjust(ratio_metric = 'psxg_ratio',
            xg_metric = 'psxg',
            goal_metric = 'goals',
            shot_threshold = 50)  %>% 
  mutate(xg_ratio = ifelse(shots < 50, NA, xg_ratio),
         psxg_ratio = ifelse(shots < 50, NA, psxg_ratio)) %>%
  group_by(player_id) %>% 
  arrange(season_name, .by_group = T) %>% 
  mutate(prev_xg_ratio = lag(xg_ratio),
         prev_psxg_ratio = lag(eb_adj_ratio_mean)) %>% 
  ungroup %>% 
  filter(!is.na(prev_xg_ratio),
         !is.na(xg_ratio))

yoy_xg_adjust = yoy %>% 
  eb_adjust(ratio_metric = 'xg_ratio',
            xg_metric = 'xg',
            goal_metric = 'goals',
            shot_threshold = 50)  %>% 
  mutate(xg_ratio = ifelse(shots < 50, NA, xg_ratio),
         psxg_ratio = ifelse(shots < 50, NA, psxg_ratio)) %>%
  group_by(player_id) %>% 
  arrange(season_name, .by_group = T) %>% 
  mutate(prev_psxg_ratio = lag(xg_ratio),
         prev_xg_ratio = lag(eb_adj_ratio_mean)) %>% 
  ungroup %>% 
  filter(!is.na(prev_xg_ratio),
         !is.na(eb_adj_ratio_mean))


cor.test(yoy_raw$xg_ratio, yoy_raw$prev_xg_ratio)
cor.test(yoy_xg_adjust$eb_adj_ratio_mean, yoy_xg_adjust$prev_xg_ratio)


cor.test(yoy_raw$psxg_ratio, yoy_raw$prev_psxg_ratio)
cor.test(yoy_psxg_adjust$eb_adj_ratio_mean, yoy_psxg_adjust$prev_psxg_ratio)

library(ggpubr)

yoy_raw %>% 
  ggplot(aes(x=psxg_ratio, y=prev_psxg_ratio)) +
  geom_point(color='darkgrey', size=.75) +
  geom_smooth(method='lm', se=F,
              color=pal[3]) +
  stat_cor(color='white', family='Roboto',
           p.accuracy=.001, size=8) +
  labs(x='G/PSxG', y="Previous Year's G/PSxG",
       title = 'Year over Year Stability of G/PSxG',
       subtitle = 'Minimum 50 shots on target faced in both years')+
  theme_gplus()
ggsave(file.path(PROJ_DIR, 'yoy_psxg.png'), units = 'px', width=1000, height=1000)


yoy_raw %>% 
  ggplot(aes(x=xg_ratio, y=prev_xg_ratio)) +
  geom_point(color='darkgrey', size=.75) +
  geom_smooth(method='lm', se=F,
              color=pal[3]) +
  stat_cor(color='white', family='Roboto',
           p.accuracy=.001, size=8) +
  labs(x='G/xG', y="Previous Year's G/xG",
       title = 'Year over Year Stability of G/xG',
       subtitle = 'Minimum 50 shots on target faced in both years')+
  theme_gplus()
ggsave(file.path(PROJ_DIR, 'yoy_xg.png'), units = 'px', width=1000, height=1000)


yoy_raw %>% 
  ggplot(aes(x=psxg_ratio, y=xg_ratio)) +
  geom_point(color='darkgrey', size=.75) +
  geom_smooth(method='lm', se=F,
              color=pal[6]) +
  stat_cor(color='white', family='Roboto',
           p.accuracy=.001, size=8) +
  labs(x='G/xG', y="G/PSxG",
       title = 'G/xG and G/PSxG measure similar skills',
       subtitle = 'Minimum 50 shots on target faced in season')+
  theme_gplus()
ggsave(file.path(PROJ_DIR, 'psxg_vs_xg.png'), units = 'px', width=1000, height=1000)





# proportion of significant observations ----------------------------------

xg_adjust %>% 
  mutate(lower_bound = eb_adj_ratio_lower_ci_bound,
         upper_bound = eb_adj_ratio_upper_ci_bound) %>% 
  mutate(sign = case_when(
    lower_bound > median(aggd$xg_ratio) & upper_bound >median(aggd$xg_ratio) ~ 'Y',
    lower_bound < median(aggd$xg_ratio) & upper_bound <median(aggd$xg_ratio) ~ 'Y',
    T ~ 'N'
  )) %>% 
  mutate(posneg = ifelse(xg_ratio>median(aggd$xg_ratio),'bad','good')) %>% 
  count(sign, posneg) %>% 
  pivot_wider(names_from = posneg, values_from = n)

psxg_adjust %>% 
  mutate(lower_bound = eb_adj_ratio_lower_ci_bound,
         upper_bound = eb_adj_ratio_upper_ci_bound) %>% 
  mutate(sign = case_when(
    lower_bound > median(aggd$psxg_ratio) & upper_bound >median(aggd$psxg_ratio) ~ 'Y',
    lower_bound < median(aggd$psxg_ratio) & upper_bound <median(aggd$psxg_ratio) ~ 'Y',
    T ~ 'N'
  )) %>% 
  mutate(posneg = ifelse(psxg_ratio>1,'bad','good')) %>% 
  count(sign, posneg) %>% 
  pivot_wider(names_from = posneg, values_from = n)



# function to simulate sensitivity ----------------------------------------

simulate_sensitivity = function(shotmin=10, shotmax=5000, shotstep=10,
                                ntile_df = aggd_hisample, xg_metric = 'xg',
                                ratio_metric='xg_ratio', quantiles = c(.01, .1),
                                prior_dist = xg_prior_dist){
  quantile_values = quantile(ntile_df[[ratio_metric]], quantiles)
  quantile_labels = tibble(
    xg_ratio = quantile_values
  ) %>% 
    mutate(percentile = 
             names(quantile_values) %>% 
             str_remove_all('%') %>% as.numeric() %>% 
             sapply(function(x){100-x}) %>% 
             sapply(function(x){paste0(x, 'th %ile performance')})
    )
  
  simulated_xg_ratio = expand.grid(
    shots = seq(shotmin,shotmax,shotstep),
    xg_pershot = median(ntile_df[[xg_metric]]/ntile_df$shots),
    xg_ratio = quantile_values
  ) %>%
    left_join(quantile_labels) %>% 
    mutate(xg = shots*xg_pershot,
           goals = xg*xg_ratio)%>% 
    eb_adjust(ratio_metric = 'xg_ratio',
              xg_metric = 'xg',
              goal_metric = 'goals',
              shot_threshold = 50,
              force_prior_shape = prior_dist$estimate[1],
              force_prior_rate = prior_dist$estimate[2]) %>% 
    mutate(upper_bound = eb_adj_ratio_upper_ci_bound,
           diff_from_one = median(ntile_df[[ratio_metric]])-upper_bound)%>% 
    dplyr::select(shots, diff_from_one, percentile) %>% 
    mutate(metric = ratio_metric)
}


# simulation to compare metrics -------------------------------------------

xg_prior_dist <- fitdistr(
  aggd %>% filter(shots > 50) %>% pull(xg_ratio),
  dgamma,
  start = list(shape = 1, rate = 1)
)

psxg_prior_dist <- fitdistr(
  aggd %>% filter(shots > 50) %>% pull(psxg_ratio),
  dgamma,
  start = list(shape = 1, rate = 1)
)

aggd_hisample = aggd %>% filter(shots > 50)

simulated_xg_ratio = simulate_sensitivity(
  shotmin=100, shotmax=50000, shotstep=25, ntile_df = aggd_hisample,
  xg_metric = 'xg', ratio_metric = 'xg_ratio', quantiles = c(.01,seq(.05,.4,.05)),
  prior_dist = xg_prior_dist
)

simulated_psxg_ratio = simulate_sensitivity(
  shotmin=100, shotmax=50000, shotstep=25, ntile_df = aggd_hisample,
  xg_metric = 'psxg', ratio_metric = 'psxg_ratio', quantiles = c(.01,seq(.05,.4,.05)),
  prior_dist = psxg_prior_dist
)


sens_plot_dat = bind_rows(simulated_psxg_ratio, simulated_xg_ratio) %>%
  filter(diff_from_one>0) %>%
  group_by(metric, percentile) %>%
  slice_min(n=1, order_by=shots) %>%
  filter(percentile != '60th %ile performance') %>%
  mutate(percentile = str_remove_all(percentile, ' performance'))

library(scales)

sens_plot_dat%>% 
  ggplot(aes(x=shots, y=percentile, group=metric, fill=metric)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y='Performance Percentile',
       x='On Target Shots Needed to Detect Difference from Average Performance',
       title='G/xG Detects Differences Earlier than G/PSxG',
       fill=NULL) +
  scale_fill_manual(labels = c('G/PSxG', 'G/xG'),
                    values=pal) +
  geom_text(aes(label=comma(shots)),
            position = position_dodge(.9),
            hjust=1.1, color="#15202B",
            fontface='bold', size=6)  +
  theme_gplus()+
  theme(legend.position = c(.75,.75),
        legend.background = element_rect(fill='#15202B',
                                         color='#15202B'),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size=28),
        title = element_text(size=24))
ggsave(file.path(PROJ_DIR, 'shots_needed_detection.png'), units = 'px',
       width=1600, height=1000)
# 
# # xg bins - incomplete -----------------------------------------------------------------
# 
# binned = raw %>% 
#   mutate(psxg_bin = case_when(
#     xg_goalkeeper < .1 ~ 'Easy',
#     xg_goalkeeper < .3 ~ 'Medium',
#     T ~ 'Difficult')) %>% 
#   group_by(player_id, player_name, psxg_bin) %>%
#   mutate(goal = ifelse(is.na(goal), 0, 1)) %>% 
#   summarise(psxg = sum(xg_goalkeeper),
#             xg = sum(xg_shooter),
#             goals = sum(goal),
#             shots = n()) %>% 
#   ungroup %>% 
#   mutate(psxg_ratio  = goals/psxg,
#          xg_ratio = goals/xg)
# 
# psxg_adjust_easy = binned %>%
#   filter(psxg_bin == 'Easy') %>% 
#   eb_adjust(ratio_metric = 'psxg_ratio',
#             xg_metric = 'psxg',
#             goal_metric = 'goals',
#             shot_threshold = 50,
#             metric_lo_cutoff = 0)
# 
# psxg_adjust_hard = binned %>%
#   filter(psxg_bin == 'Difficult') %>%  
#   eb_adjust(ratio_metric = 'psxg_ratio',
#             xg_metric = 'psxg',
#             goal_metric = 'goals',
#             shot_threshold = 50,
#             metric_lo_cutoff = 0.)
# 
# psxg_adjust_med = binned %>%
#   filter(psxg_bin == 'Medium') %>%  
#   eb_adjust(ratio_metric = 'psxg_ratio',
#             xg_metric = 'psxg',
#             goal_metric = 'goals',
#             shot_threshold = 50,
#             metric_lo_cutoff = 0.)
# 
# psxg_adjust_easy %>% 
#   slice_min(eb_adj_ratio_mean, n = 20) %>% 
#   mutate(player_name = fct_reorder(player_name, -eb_adj_ratio_mean)) %>% 
#   ggplot() +
#   aes(y = player_name) +
#   geom_point(aes(x = eb_adj_ratio_mean)) +
#   geom_errorbarh(
#     aes(
#       xmin = eb_adj_ratio_mean - eb_adj_ratio_sd,
#       xmax = eb_adj_ratio_mean + eb_adj_ratio_sd
#     )
#   ) +
#   geom_vline(aes(xintercept = median(psxg_adjust_easy$psxg_ratio))) +
#   labs(title = 'Top overperformers on easy shots') +
#   labs(x='Adjusted G/PSxG', y=NULL)
# 
# psxg_adjust_hard %>% 
#   slice_min(eb_adj_ratio_mean, n = 20) %>% 
#   mutate(player_name = fct_reorder(player_name, -eb_adj_ratio_mean)) %>% 
#   ggplot() +
#   aes(y = player_name) +
#   geom_point(aes(x = eb_adj_ratio_mean)) +
#   geom_errorbarh(
#     aes(
#       xmin = eb_adj_ratio_lower_ci_bound,
#       xmax = eb_adj_ratio_upper_ci_bound
#     )
#   ) +
#   geom_vline(aes(xintercept = median(psxg_adjust_hard$psxg_ratio))) +
#   labs(title = 'Top overperformers on difficult shots') +
#   labs(x='Adjusted G/PSxG', y=NULL)
# 
# psxg_adjust_med %>% 
#   slice_min(eb_adj_ratio_mean, n = 20) %>% 
#   mutate(player_name = fct_reorder(player_name, -eb_adj_ratio_mean)) %>% 
#   ggplot() +
#   aes(y = player_name) +
#   geom_point(aes(x = eb_adj_ratio_mean)) +
#   geom_errorbarh(
#     aes(
#       xmin = eb_adj_ratio_lower_ci_bound,
#       xmax = eb_adj_ratio_upper_ci_bound
#     )
#   ) +
#   geom_vline(aes(xintercept = median(psxg_adjust_med$psxg_ratio))) +
#   labs(title = 'Top overperformers on medium shots') +
#   labs(x='Adjusted G/PSxG', y=NULL)
# 
# 
# psxg_adjust_easy %>% 
#   slice_max(eb_adj_ratio_mean, n = 20) %>% 
#   mutate(player_name = fct_reorder(player_name, eb_adj_ratio_mean)) %>% 
#   ggplot() +
#   aes(y = player_name) +
#   geom_point(aes(x = eb_adj_ratio_mean)) +
#   geom_errorbarh(
#     aes(
#       xmin = eb_adj_ratio_lower_ci_bound,
#       xmax = eb_adj_ratio_upper_ci_bound
#     )
#   ) +
#   geom_vline(aes(xintercept = median(psxg_adjust_easy$psxg_ratio))) +
#   labs(title = 'Top underperformers on easy shots') +
#   labs(x='Adjusted G/PSxG', y=NULL)
# 
# psxg_adjust_hard %>% 
#   slice_max(eb_adj_ratio_mean, n = 20) %>% 
#   mutate(player_name = fct_reorder(player_name, eb_adj_ratio_mean)) %>% 
#   ggplot() +
#   aes(y = player_name) +
#   geom_point(aes(x = eb_adj_ratio_mean)) +
#   geom_errorbarh(
#     aes(
#       xmin = eb_adj_ratio_mean - eb_adj_ratio_sd,
#       xmax = eb_adj_ratio_mean + eb_adj_ratio_sd
#     )
#   ) +
#   geom_vline(aes(xintercept = median(psxg_adjust_hard$psxg_ratio))) +
#   labs(title = 'Top underperformers on difficult shots') +
#   labs(x='Adjusted G/PSxG', y=NULL)
# 
# psxg_adjust_med %>% 
#   slice_max(eb_adj_ratio_mean, n = 20) %>% 
#   mutate(player_name = fct_reorder(player_name, eb_adj_ratio_mean)) %>% 
#   ggplot() +
#   aes(y = player_name) +
#   geom_point(aes(x = eb_adj_ratio_mean)) +
#   geom_errorbarh(
#     aes(
#       xmin = eb_adj_ratio_mean - eb_adj_ratio_sd,
#       xmax = eb_adj_ratio_mean + eb_adj_ratio_sd
#     )
#   ) +
#   geom_vline(aes(xintercept = median(psxg_adjust_med$psxg_ratio))) +
#   labs(title = 'Top underperformers on medium shots') +
#   labs(x='Adjusted G/PSxG', y=NULL)
# 
# 
