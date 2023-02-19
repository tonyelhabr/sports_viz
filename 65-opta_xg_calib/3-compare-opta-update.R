suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(forcats)
  library(tidyr)
  library(scales)
  library(knitr)
})

proj_dir <- '65-opta_xg_calib'
data_dir <- file.path(proj_dir, 'data')
clean_updated_shots_path <- file.path(data_dir, 'clean_updated_shots.rds')
# clean_updated_shots_path <- 'c:/users/antho/documents/projects/sports_viz/65-opta_xg_calib/data/clean_updated_shots.rds'
updated_np_shots <- read_rds(clean_updated_shots_path) |> 
  filter(!is_penalty) 

compare_by <- function(shots, ...) {
  shots |> 
    group_by(...) |> 
    summarize(
      n_shots = n(),
      n_same_npxg = sum(round(old_xg, 2) == round(new_xg, 2), na.rm = TRUE),
      n_old_npxg_is_higher = sum(round(old_xg, 2) > round(new_xg, 2), na.rm = TRUE),
      new_npxg = sum(new_xg, na.rm = TRUE),
      old_npxg = sum(old_xg, na.rm = TRUE)
    ) |> 
    ungroup() |> 
    transmute(
      ...,
      n_shots,
      n_same_npxg,
      n_diff_npxg = n_shots - n_same_npxg,
      n_old_npxg_is_higher,
      n_new_npxg_is_higher = n_diff_npxg - n_old_npxg_is_higher,
      prop_diff_npxg = n_diff_npxg / n_shots,
      prop_old_npxg_is_higher = n_old_npxg_is_higher / n_diff_npxg
    ) |> 
    arrange(desc(n_shots))
}

updated_np_shots |>
  compare_by() |> 
  glimpse()

meta_updated_shots_cols <- c(
  'country',
  'date',
  'half',
  'minute',
  'team',
  'player'
)

discretized_updated_np_shots <- updated_np_shots |> 
  select(
    all_of(meta_updated_shots_cols),
    
    new_xg,
    old_xg,
    is_goal,
    
    country,
    distance,
    sca1,
    body_part,
    is_from_deflection,
    is_from_volley,
    is_free_kick,
    is_primary_foot
  ) |> 
  mutate(
    across(
      country,
      factor
    ),
    across(
      distance,
      ~cut(
        .x,
        breaks = c(seq(0, 18, by = 2), 20, 25, 30, 35, Inf)
      )
    ),
    across(sca1, ~na_if(.x, '')),
    across(
      sca1, 
      ~.x |> 
        str_remove_all( '\\(|\\)') |> 
        str_replace_all('\\s|[-]', '_') |> 
        tolower() |> 
        factor()
    ),
    across(
      c(
        is_from_deflection,
        is_from_volley,
        is_free_kick,
        is_primary_foot
      ),
      ~ifelse(.x, 'yes', 'no') |> 
        factor()
    ),
    across(
      c(is_primary_foot, body_part, sca1, distance), 
      ~fct_explicit_na(.x, na_level = 'missing')
    )
  )
glimpse(discretized_updated_np_shots)

updated_np_shot_diffs <- discretized_updated_np_shots |> 
  select(-all_of(meta_updated_shots_cols)) |> 
  pivot_longer(
    -c(is_goal, old_xg, new_xg),
    names_to = 'feature',
    values_to = 'group'
  ) |> 
  compare_by(feature, group)
updated_np_shot_diffs

slice_and_tabularize_np_shot_diffs <- function(op, n = 6) {
  updated_np_shot_diffs |> 
    filter(n_shots >= 100) |> 
    arrange(op(prop_old_npxg_is_higher)) |> 
    head(n) |> 
    transmute(
      Feature = sprintf('`%s`', feature),
      Group = sprintf('`%s`', ifelse(feature == "distance", as.character(group), paste0('"', group, '"'))),
      `# of non-penalty shots` = scales::comma(n_shots),
      `# of shots with changed npxG` = sprintf('%s (%.1f%%)', scales::comma(n_diff_npxg), 100 * prop_diff_npxg),
      `# of shots with lower post-update npxG of those that changed` = sprintf('%s (%.1f%%)', scales::comma(n_old_npxg_is_higher), 100 * prop_old_npxg_is_higher),
    ) |> 
    knitr::kable()
}

slice_and_tabularize_np_shot_diffs(`-`)
slice_and_tabularize_np_shot_diffs(`+`)
