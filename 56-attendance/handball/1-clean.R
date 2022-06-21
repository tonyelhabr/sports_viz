
dir_proj <- '56-attendance'
dir_handball <- file.path(dir_proj, 'handball')

library(dplyr)
library(purrr)
hbl_raw <- structure(
  list(season_id = c(33759L, 41816L, 55149L),
       name = structure(1:3,
                        .Label = c("Bundesliga 16/17", "Bundesliga 17/18",
                                   "Bundesliga 18/19"),
                        class = "factor"),
       year = structure(1:3, .Label = c("16/17", "17/18", "18/19"), 
                        class = "factor"),
       raw = list(NULL, NULL, NULL)),
  row.names = c(NA, -3L),
  class = c("tbl_df", "tbl", 
            "data.frame"))

for (ii in seq_len(nrow(hbl_raw))) {
  hbl_raw$raw[[ii]] <- jsonlite::fromJSON(
    paste0("https://hbl.fmp.sportradar.com/feeds/de/Europe:Berlin/gismo/fixtures/149/",
           hbl_raw$season_id[ii]))
  Sys.sleep(runif(1, 0, 3))
}

hbl_raw
save(hbl_raw, file = file.path(dir_handball, "hbl_raw.rda"), compress = "bzip2")

get_games <- function(x) {
  x %>%
    select("_id", "title", "round_number", "matches") %>%
    mutate_at("matches", function(x) map(x, "_id")) %>%
    tidyr::unnest(cols = matches)
}

get_season <- function(x) {
  x %>%
    magrittr::extract2("matches") %>%
    map(~select(.x, "_id", "_sid", "code",
                starts_with("location"),
                starts_with("play_date"),
                starts_with("home_team"),
                starts_with("away_team"),
                starts_with("statistics"))) %>%
    map_dfr(~jsonlite::flatten(.))
}

### Extract relevant parts of the JSON lists and store them in column 'dat' ###

hbl_raw$raw2 <- map(hbl_raw$raw, c("doc", "data", "tournament", "phases")) %>%
  map(1) %>%
  map("matchdays") %>%
  map(1)

hbl_raw$tmp1 <- map(hbl_raw$raw2, get_games)
hbl_raw$tmp2 <- map(hbl_raw$raw2, get_season)

hbl_raw$dat <- map2(hbl_raw$tmp1, hbl_raw$tmp2,
                    ~left_join(.x, .y, by = c("matches" = "_id")))

hbl <- tidyr::unnest(hbl_raw, dat) %>%
  select_if(., ~!is.list(.x)) %>% 
  tibble::rowid_to_column() %>% 
  mutate(play_date.date  = lubridate::dmy(play_date.date))

# Rename variables
names(hbl) <- sub("^_", "", names(hbl)) %>%
  sub("^statistics.", "", .) %>%
  sub("^play_date[.]([a-z]+)", "\\1", .) %>%
  sub("_team.name", "_team", .) %>%
  sub("[.]_id", ".id", .) %>%
  sub("[.]_sid", ".sid", .) %>%
  sub("[.]|[.]_", "_", .) %>%
  make.names(unique = TRUE)

hbl$weekday <- factor(hbl$weekday, levels = 1:7)

# Lemgo changed its name some day
hbl$home_team[hbl$home_team == "TBV Lemgo"] <- "TBV Lemgo Lippe"
hbl$away_team[hbl$away_team == "TBV Lemgo"] <- "TBV Lemgo Lippe"

df1 <- hbl %>%
  select(rowid, season_id, year, home_team, away_team,
         capacity, attendance, occupancy,
         round_number, date, weekday,
         # location_id, location_name, location_city,
         home_team_id, home_team_abbreviation,
         away_team_id, away_team_abbreviation)


### Fix attendance and capacity ###

# Fix missing capacity
df1[df1$rowid ==   3 & df1$capacity == 0, "capacity"] <- 6300

# Fix wrong capacity
df1[df1$rowid ==  13 & df1$capacity == 1400, "capacity"] <- 7850
df1[df1$rowid ==  52 & df1$capacity == 4150, "capacity"] <- 10000
df1[df1$rowid == 162 & df1$capacity == 4150, "capacity"] <- 10000
df1[df1$rowid == 160 & df1$capacity == 2251, "capacity"] <- 6211

df1$occupancy <- df1$attendance/df1$capacity*100

df1 <- filter(df1, occupancy < 120) %>% 
  filter(occupancy > 0 | season_id == 55149)

df1[df1$attendance > df1$capacity, "attendance"] <-
  df1[df1$attendance > df1$capacity, "capacity"]

df1$perc <- df1$attendance/df1$capacity


### Further wrangling ###

# Dummy coding of weekday with Saturday as the reference
contrasts(df1$weekday) <- contr.treatment(7, base = 6)

# Center variable 'machtday' (34 games per season)
df1$matchday <- df1$round_number - 17.5

# Sort teams according to final standings in season 2017/18
tmp1 <- c("SG Flensburg-Handewitt", "Rhein-Neckar Löwen", "Füchse Berlin",
          "SC Magdeburg", "THW Kiel", "TSV Hannover-Burgdorf", "MT Melsungen",
          "SC DHfK Leipzig", "TBV Lemgo Lippe", "FRISCH AUF! Göppingen",
          "HSG Wetzlar", "TSV GWD Minden", "HC Erlangen", "TVB 1898 Stuttgart",
          "VfL Gummersbach", "Die Eulen Ludwigshafen", "TuS N-Lübbecke",
          "TV 05/07 Hüttenberg")
df1 <- mutate_at(df1, vars(home_team, away_team),
                 .funs = forcats::fct_relevel, tmp1)

# Wrangle month for better modeling and plotting
df1 <- df1 %>%
  group_by(year, round_number) %>% 
  mutate(
    # Calculate a common Date for all 9 matches in the same round_number for
    #  a better x-value for plotting
    Date = median(date),
    # Date6 = as.Date(cut.Date(Date, "week")) + 6,
    # Only few matches in Aug, Jan, & June, set those to Sep, Feb, & May
    Month = lubridate::month(Date) %>%
      factor(levels = c(8:12, 1:6)) %>%
      forcats::fct_relabel(.,
                           car::recode,
                           recodes = "8=9; 6=5; 1=2",
                           as.factor = FALSE,
                           as.numeric = FALSE)) %>% 
  ungroup

### Contrasts ###

contr.month <- cbind(rep(0:1, each = 4),           # 1st vs 2nd half
                     rbind(codingMatrices::code_diff(4),    # successive diffs
                           matrix(0, 4, 3)),
                     rbind(matrix(0, 4, 3),
                           codingMatrices::code_diff(4))    # successive diffs
)
colnames(contr.month) <- c("half2", "Oct", "Nov", "Dec", "Mar", "Apr", "May")
rownames(contr.month) <- NULL

# Add variable 'Month' for the left_join() below
contr.month <- data.frame(Month = forcats::as_factor(levels(df1$Month)), contr.month)
df1 <- left_join(df1, contr.month, by = "Month")
qs::qsave(df1, file.path(dir_handball, 'handball.qs'))
