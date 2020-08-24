
library(tidyverse)
source(here::here('scripts', 'dev', '0x_tx_hs_helpers.R')

fb <- import_fb()
fb_points <- fb %>% filter(key == 'TOTAL POINTS') %>% select(-key)
fb_points

bands <- import_bands() %>% mutate(across(school, ~str_remove(.x, '\\sHS$')), across(isd, ~str_remove(.x, '\\sISD$')))
bands

bands_aug <-
  bands %>% 
  # filter(!is.na(place)) %>% 
  mutate(
    prnk_conf = (tier - min(tier) + 1L) / (max(tier) - min(tier) + 1L)
  ) %>% 
  group_by(year, conf, round) %>% 
  mutate(n = n()) %>% 
  mutate(
    # Need to coalesce for when `place` is NA.
    prnk_place = ((n - place + 1L) / (n - min(place) + 1L)) %>% coalesce(1 / n)
  ) %>% 
  ungroup() %>% 
  mutate(score = ifelse(round == 'Prelims', 0.5, 1) * prnk_conf * prnk_place)
bands_aug

bands_schools_conf <-
  bands_aug %>% 
  group_by(school) %>% 
  mutate(n_app = n()) %>% 
  ungroup() %>% 
  group_by(school, conf, n_app) %>% 
  summarize(n_conf = n()) %>% 
  mutate(frac_app = n_conf / n_app) %>% 
  ungroup()

bands_schools <-
  bands_schools_conf %>% 
  group_by(school) %>% 
  filter(row_number(desc(frac_app)) == 1L) %>% 
  ungroup()

bands_agg <-
  bands_aug %>% 
  group_by(school) %>% 
  summarize(across(score, sum), n = n()) %>% 
  ungroup() %>% 
  inner_join(bands_schools) %>% 
  mutate(rnk = row_number(desc(score))) %>% 
  relocate(rnk) %>% 
  arrange(rnk)
bands_agg

bands_agg %>% 
  filter(conf %in% sprintf('%dA', 4:6)) %>% 
  filter(school != 'Fredericksburg') %>% 
  head(20)
# # A tibble: 20 x 8
# rnk school                   score     n conf  n_app n_conf frac_app
# <int> <chr>                    <dbl> <int> <chr> <int>  <int>    <dbl>
#   1     1 Duncanville             12.88     39 5A       39     34   0.8718
# 2     2 Marcus HS, Flower Mound  8.676    15 5A       15      9   0.6   
# 3     3 Cedar Park               8.565    18 5A       18     11   0.6111
# 4     4 Dickinson                8.136    32 4A       32     26   0.8125
# 5     5 L.D. Bell                8.134    19 5A       19     17   0.8947
# 6     6 Westfield HS, Houston    7.904    23 5A       23     23   1     
# 7     7 Coppell                  7.276    23 5A       23     10   0.4348
# 8     9 Mesquite Poteet          6.641    24 4A       24     20   0.8333
# 9    11 Hebron HS, Carrollton    6.531    14 5A       14      7   0.5   
# 10    13 Friendswood              6.374    19 4A       19     14   0.7368
# 11    15 Georgetown               6.119    21 4A       21     17   0.8095
# 12    16 Leander                  5.509    20 4A       20      8   0.4   
# 13    17 San Antonio Reagan       5.486    14 5A       14      8   0.5714
# 14    18 The Woodlands            5.240    15 5A       15      9   0.6   
# 15    19 Fredericksburg           5.187    26 4A       26     14   0.5385
# 16    20 Vandegrift HS, Austin    5.162    10 4A       10      4   0.4   
# 17    21 Wakeland                 4.961    12 4A       12      6   0.5   
# 18    22 Permian HS, Odessa       4.912    20 5A       20     19   0.95  
# 19    23 Spring                   4.832    19 5A       19     16   0.8421
# 20    24 Bowie HS, Austin         4.821    11 5A       11      8   0.7273 

bands_schools %>% filter(school %>% str_detect('Duncanville'))



fb_points %>% 
  rename(rnk_fb = rnk) %>% 
  mutate(across(school, ~sprintf('%s HS', .x))) %>% 
  filter(school %>% str_detect('^Judson'))

hs_join <-
  inner_join(
    bands_agg %>% rename(rnk_band = rnk),
    fb_points %>% rename(rnk_fb = rnk)
  )
hs_join

bands_agg %>% filter(school %>% str_detect('Jud'))

fb_points %>% filter(school == 'Judson HS')

bands_aug
bands %>% count(tier)
bands %>% count(place) %>% tail()
bands %>% filter(place == 41L)

bands %>% count(conf)
bands %>% filter(conf == 'B')
bands
bands %>% skimr::skim()
bands %>% filter(notes != '')
bands %>% count(round)

bands %>% filter(school_isd %>% str_detect('Katy Taylor'))
bands_schools_n <- df %>% count(school_isd, sort = TRUE)
bands %>% count(conf)
bands %>% filter(conf %in% sprintf('%dA', 4:6)) %>% count(school_isd, sort = T)
bands_schools_n %>% filter(school_isd %>% str_detect('Sundown'))
bands_schools_n %>% filter(school_isd %>% str_detect('Carroll'))
bands %>% filter(school_isd %>% str_detect('Sundown'))
