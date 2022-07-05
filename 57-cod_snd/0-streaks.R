
library(tidyverse)

## http://keyonvafa.com/hot-hand/
get_post_streak_prob <- function(n, k, p = 0.5) {
  tosses <- rbinom(n, 1, p)
  runs <- rle(tosses)
  n_neg_after <- length(which(runs$values == 1 & runs$lengths >= k))
  n_pos_after <- sum(runs$lengths[which(runs$values == 1 & runs$lengths >= k)] - k)
  
  ## edge case
  if (n %in% cumsum(runs$lengths)[which(runs$values == 1 & runs$lengths >= k)]) {
    n_neg_after <- n_neg_after - 1
  }
  
  n_pos_after / (n_pos_after + n_neg_after)
}

simulate_post_streak_prob <- function(sims = 1000, ...) {
  rerun(
    sims,
    get_post_streak_prob(...)
  ) |> 
    flatten_dbl() |> 
    mean(na.rm = TRUE)
}

runs <- crossing(
  n = 1:100,
  k = 1:3
) |> 
  mutate(
    p = map2_dbl(n, k, ~simulate_post_streak_prob(sims = 1000, n = ..1, k = ..2))
  )
runs |>
  arrange(n, k) |>
  mutate(
    across(k, factor)
  ) |> 
  ggplot() +
  aes(x = n, y = p, color = k, group = k) +
  geom_step() +
  geom_hline(aes(yintercept = 0.5))
