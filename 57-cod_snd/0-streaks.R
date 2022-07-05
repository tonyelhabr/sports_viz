
# http://skranz.github.io//r/2019/03/13/HotHand.html
library(purrr)

get_streaks <- function(x) {
  rle_x <- rle(x)
  len <- rle_x$lengths
  end <- cumsum(len)
  start <- c(1, end[-length(end)] + 1)
  data.frame(value = rle_x$values, length = len, start = start, end = end)
}

get_after_streak_inds <- function(x, k) {
  streaks <- get_streaks(x)
  
  streaks <- streaks[streaks$length >= k & streaks$value == 1, , drop = FALSE]
  
  if (NROW(streaks) == 0) {
    return(NULL)
  }
  
  inds <- streaks$start + k
  
  max_len <- max(streaks$length)
  len <- k + 1
  while (len <= max_len) {
    streaks <- streaks[streaks$length >= len, , drop = FALSE]
    inds <- c(inds, streaks$start + len)
    len <- len + 1
  }
  
  # ignore indices above n and sort for convenience
  sort(inds[inds <= length(x)])
}

occurences_after_streak <- function(n, k, p = 0.5) {
  
  x <- sample(0:1, n, replace = TRUE, prob = c(1 - p, p))
  
  inds <- get_after_streak_inds(x, k)
  
  if (length(inds) == 0) {
    return(NA_real_)
  }
  df <- tibble(idx = 1:length(x), x = x, streak = 0)
  df[inds, ]$streak <- 1
  df
}

average_prob_after_streak <- function(n, k, p = 0.5) {
  
  x <- sample(0:1, n, replace = TRUE, prob = c(1 - p, p))
  
  inds <- get_after_streak_inds(x, k)
  
  if (length(inds) == 0) {
    return(NA_real_)
  }
  
  mean(x[inds], na.rm = TRUE)
}

summarize_prob_after_streak <- function(x) {
  n <- length(x)
  m <- mean(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)
  list(
    mean = m,
    lower = m - sd / sqrt(n), 
    upper = m + sd / sqrt(n)
  )
}

simulate_prob_after_streak <- function(sims = 1000, ...) {
  rerun(
    sims,
    average_prob_after_streak(...)
  ) |> 
    flatten_dbl() |> 
    summarize_prob_after_streak()
}

simulate_prob_after_streak(sims = 1000, n = 11, k = 1)
simulate_prob_after_streak(sims = 1000, n = 11, k = 2)
simulate_prob_after_streak(sims = 1000, n = 11, k = 2, p = 6 / 11)
simulate_prob_after_streak(sims = 1000, n = 100, k = 3)
simulate_prob_after_streak(sims = 1000, n = 30, k = 3, p = .54)

runs <- crossing(
  n = 1:100,
  k = 1:3
) |> 
  mutate(
    z = map2_dbl(n, k, ~simulate_prob_after_streak(sims = 1000, n = ..1, k = ..2)$mean)
  )
runs
runs |>
  arrange(k, idx) |>
  mutate(
    across(k, factor)
  ) |> 
  ggplot() +
  aes(x = idx, y = z, color = k, group = k) +
  geom_step() +
  geom_hline(aes(yintercept = 0.5))

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
