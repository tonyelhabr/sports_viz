
library(tidyverse)
library(ggsankey)
# library(ggflags)
library(patchwork)
library(wesanderson)

# Read in data
records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')

drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')

# Fonts
f1 = "Fira Sans Compressed"

# Palette
pal <- wes_palette("Zissou1", 21, "continuous")
pal_mk <- wes_palette("Darjeeling1")

# Set theme
theme_set(
  theme_minimal(base_family = f1) +
    theme(
      legend.title = element_blank(),
      legend.key.size = unit(1, "line"),
      axis.title = element_blank(),
      plot.background = element_rect(fill = "grey97", color = NA),
      plot.margin = margin(10, 10, 10, 10),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey30")
    )
)

# Prepare data
drivers_top <- drivers %>%
  filter(position < 6) %>% 
  mutate(
    player_rec = paste0(player, " - ", total, " records"),
    player_rec = fct_reorder(player_rec, -total)
  )

# nation_records <- drivers %>%
#   mutate(nation = replace_na(nation, "Unknown")) %>% 
#   group_by(nation) %>% 
#   summarise(nation_total = sum(records, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   arrange(-nation_total) %>%
#   mutate(nation = fct_reorder(nation, -nation_total)) %>% 
#   add_column(code = c("us", "au", "nl", "de", "gb", "ca", "at", "br", "ie", "hr", "no", NA, "fr", "si"))

# track_records <- records %>% 
#   count(track, type)
# 
# system_records <- records %>% 
#   count(track, system_played)
# 
drivers_pos <- drivers_top %>%
  distinct(player, position)

records_sankey <- records %>% 
  filter(player %in% drivers_top$player) %>% 
  make_long(player, track) %>% 
  left_join(drivers_pos, by = c("node" = "player")) %>% 
  mutate(color = if_else(!is.na(next_node), pal_mk[position], "grey60"))
records_sankey
pal_mk %>% as.character()

skp <- ggplot(records_sankey, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = color, label = node)) +
  geom_sankey() +
  geom_sankey_text(aes(hjust = if_else(x == "track", 1, 0)),
                   family = f1, size = 3) +
  scale_fill_identity() +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    title = "Mario Kart 64 World Records",
    caption = "Source: Mario Kart World Records · Graphic: Georgios Karamanis"
  ) +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 23, family = f1, face = "bold"),
    plot.caption = element_text(hjust = 0.5)
  )
skp
