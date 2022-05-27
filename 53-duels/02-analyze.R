
library(tidyverse)
library(gt)
library(scales)
library(RColorBrewer)
dir_proj <- '53-duels'
dir_data <- file.path(dir_proj, 'data')

init_opta <- file.path(dir_proj, 'opta_aerials.csv') |> read_csv()
init_sb <- file.path(dir_proj, 'misc.rds') |> read_rds()

clean_opta <- init_opta |> 
  mutate(
    across(player_name, ~str_remove(.x, '\\<.*$')),
    across(team_name, ~str_remove(.x, '\\,$')),
    across(position, ~str_remove(.x, '^\\,') |> str_squish()),
    mutate(
      across(position, ~str_remove(.x, '\\,.*$') |> str_remove('\\(.*$') |> str_replace('DMC', 'DM'))
    ),
    across(c(matches, matches('^aerials')), ~parse_number(.x) |> replace_na(0) |> as.integer()),
  )
clean_opta

opta <- clean_opta |> 
  group_by(player_name, position) |> 
  summarize(
    across(where(is.numeric), sum, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  mutate(
    aerials = aerials_won + aerials_lost
  ) |> 
  rename_with(~sprintf('%s_opta', .x), -c(player_name))
opta

init_sb |> 
  filter(player_name == 'Dan Burn')

clean_sb <- init_sb |> 
  select(
    player_name,
    player_url,
    team_name = squad,
    aerials_won = won_aerial_duels,
    aerials_lost = lost_aerial_duels
  ) |> 
  distinct()

sb <- clean_sb |> 
  group_by(player_name, player_url) |> 
  summarize(
    across(matches('^aerials'), sum, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  mutate(
    aerials = aerials_won + aerials_lost
  ) |> 
  rename_with(~sprintf('%s_sb', .x), -c(player_name))

diffs <- inner_join(
  opta,
  sb,
  by = 'player_name'
) |> 
  mutate(
    aerials_total = aerials_opta + aerials_sb,
    diff_total = aerials_opta - aerials_sb,
    diff_opta = aerials_won_opta - aerials_lost_opta,
    diff_sb = aerials_won_sb - aerials_lost_sb,
    diff_won = aerials_won_opta - aerials_won_sb,
    diff_lost = -(aerials_lost_opta - aerials_lost_sb),
    diffs = diff_won + diff_lost,
    dnd = diff_opta - diff_sb
  ) |> 
  arrange(desc(abs(diff_total)))
diffs |> filter(diff_total > 0)
diffs_filt <- diffs |>
  filter(mins_played_opta > 900, position_opta != 'GK') |> 
  arrange(desc(abs(dnd)))

library(rvest)
dir_img <- file.path(dir_proj, 'img')
dir.create(dir_img, showWarnings = FALSE)
scrape_player_img <- function(player_name, url, overwrite = FALSE) {
  path <- file.path(dir_img, sprintf('%s.jpg', player_name))
  if(file.exists(path) & !overwrite) {
    return(path)
  }
  page <- rvest::read_html(url)
  url_img <- page |> rvest::html_element(xpath = '//*[@id="meta"]/div[1]/img') |> rvest::html_attr('src')
  download.file(url_img, destfile = path, mode = 'wb')
  path
}

top_diffs <- diffs_filt |> 
  slice_max(dnd, n = 10, with_ties = FALSE) |> 
  arrange(desc(dnd))
bot_diffs <- diffs_filt |> 
  slice_min(dnd, n = 10, with_ties = FALSE) |> 
  arrange(dnd)

topbot_diffs <- bind_rows(
  top_diffs |> mutate(group = 'top'),
  bot_diffs |> mutate(group = 'bot')
) |> 
  mutate(
    local_path_img = map2_chr(player_name, player_url_sb, scrape_player_img)
  )

url_opta <- 'https://i.imgur.com/ANqRiKr.png'
url_sb <- 'https://i.imgur.com/5NulVuZ.png'
tag_sb <- sprintf("<img src='%s' width='24' height='24'>", url_sb)
tag_opta <- sprintf("<img src='%s' width='20' height='24'>", url_opta)
md_sb <- gt::md(tag_sb)
md_opta <- gt::md(tag_opta)

# Reference: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset4=theme-code3
.gt_theme_538 <- function(data, ...) {
  data %>%
    gt::opt_all_caps() |> 
    gt::opt_table_font(
      font = list(
        gt::google_font('Karla'),
        gt::default_fonts()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = 'bottom', color = 'transparent', weight = gt::px(3)
      ),
      locations = gt::cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    gt::tab_options(
      column_labels.background.color = 'white',
      heading.border.bottom.style = 'none',
      table.border.top.width = gt::px(3),
      table.border.top.style = 'none',
      table.border.bottom.style = 'none',
      column_labels.font.weight = 'normal',
      column_labels.border.top.style = 'none',
      column_labels.border.bottom.width = gt::px(2),
      column_labels.border.bottom.color = 'black',
      row_group.border.top.style = 'none',
      row_group.border.top.color = 'black',
      row_group.border.bottom.width = gt::px(1),
      row_group.border.bottom.color = 'white',
      stub.border.color = 'white',
      stub.border.width = gt::px(0),
      data_row.padding = gt::px(3),
      source_notes.font.size = 12,
      source_notes.border.lr.style = 'none',
      table.font.size = 15,
      heading.align = 'left',
      ...
    ) %>% opt_css(
      "tbody tr:last-child {
    border-bottom: 2px solid #ffffff00;
      }
    ",
    add = TRUE
    )
}

mi <- pmin(rng_diff_won$diff_won, rng_diff_lost$diff_won, rng_dnd$diff_won)
ma <- pmax(rng_diff_won$diff_won, rng_diff_lost$diff_won, rng_dnd$diff_won)

.pal <- function(x) {
  hex_neg = RColorBrewer::brewer.pal(7, 'PRGn')[1:3]
  hex_pos = RColorBrewer::brewer.pal(7, 'PRGn')[5:7]
  hex_mid = '#F7F7F7'
  f_neg <- scales::col_numeric(
    palette = c(hex_neg, hex_mid),
    domain = c(mi, 0)
  )
  f_pos <- scales::col_numeric(
    palette = c(hex_mid, hex_pos),
    domain = c(0, ma)
  )
  ifelse(x < 0, f_neg(x), f_pos(x))
}

.gt_col_numeric <- function(gt_object, columns = NULL){

  gt::data_color(
    gt_object,
    columns = {{ columns }},
    colors = .pal,
    alpha = 0.9
  )
}

make_table <- function(.group) {
  adj1 <- switch(
    .group,
    'top' = 'most',
    'bot' = 'least'
  )
  tb <- topbot_diffs |> 
    filter(group == .group) |> 
    select(
      player_name,
      local_path_img,
      aerials_won_sb,
      aerials_won_opta,
      diff_won,
      aerials_lost_sb,
      aerials_lost_opta,
      diff_lost,
      dnd
    ) |> 
    gt::gt() |> 
    .gt_theme_538() |> 
    gt::cols_label(
      .list = 
        list(
          player_name = 'Player',
          local_path_img = ' ',
          aerials_won_sb = md_sb,
          aerials_won_opta = md_opta,
          diff_won = 'Diff.',
          aerials_lost_sb = md_sb,
          aerials_lost_opta = md_opta,
          diff_lost = 'Diff.',
          dnd = gt::html('Total<br/>Diff.')
        )
    ) |> 
    gt::text_transform(
      locations = gt::cells_body(
        'local_path_img'
      ),
      fn = function(x) {
        gt::local_image(
          filename = x,
          height = 25
        )
      }
    ) |> 
    gt::tab_spanner(
      label = 'Won',
      columns = c('aerials_won_sb', 'aerials_won_opta', 'diff_won')
    ) |> 
    gt::tab_spanner(
      label = 'Lost',
      columns = c('aerials_lost_sb', 'aerials_lost_opta', 'diff_lost')
    ) |> 
    gt::tab_header(
      title = gt::md(sprintf("Who was awarded the **%s** ***aerial duels won*** + ***aerial duels not lost*** by Opta <img src='%s' width='12' height='14'> compared to StatsBomb <img src='%s' width='15' height='15'>?", adj1, url_opta, url_sb))
    ) |> 
    gt::tab_footnote(
      locations = cells_column_labels('diff_won'),
      footnote = 'Opta minus Statsbomb'
    ) |> 
    gt::tab_footnote(
      locations = cells_column_labels('diff_lost'),
      footnote = 'Statsbomb minus Opta'
    ) |> 
    .gt_col_numeric(
      columns = 'diff_won'
    ) |> 
    .gt_col_numeric(
      columns = 'diff_lost'
    ) |> 
    .gt_col_numeric(
      columns = 'dnd'
    ) |> 
    gt::tab_source_note(
      gt::md('**Table**: Tony ElHabr')
    )
  gt::gtsave(
    tb,
    filename = file.path(dir_proj, sprintf('%s.png', .group)),
    vheight = 1100,
    vwidth = 550,
    zoom = 2
  )
  tb
}

c('top', 'bot') |> walk(make_table)
