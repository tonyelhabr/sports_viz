
library(tidyverse)
library(qs)

dir_proj <- '59-xg_xpoints'
sim_placings <- file.path(dir_proj, 'sim_placings.qs') |> qs::qread()

unexpected_sim_placings <- bind_rows(
  sim_placings |> 
    filter(rank == sim_rank) |> 
    slice_min(cumu_prop, n = 10, with_ties = FALSE),
  sim_placings |> 
    filter(rank == sim_rank) |> 
    slice_min(inv_cumu_prop, n = 10, with_ties = FALSE)
)
sim_placings |> 
  filter(team == 'Brighton and Hove Albion', season == '2020/2021')

## some teams don't have a placing?
unjoined <- table |> 
  distinct(season, team, rank) |> 
  anti_join(
    sim_placings |> 
      filter(rank == sim_rank) |> 
      distinct(season, team)
  )

unjoined |> 
  filter(season == '2020/2021', team == 'Wolverhampton')