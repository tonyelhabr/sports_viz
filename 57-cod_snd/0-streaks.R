#' @param n An integer giving the number of trials
#' @param k An integer giving the prior-streak size.
#' @examples 
#' repeat_success(10,2)
#' repeat_success(100, 5)
repeat_success <- function(n, k, p = 0.5, seed = 1) {
  # withr::local_seed(seed)
  path <- sample(c(1, 0), size = n, replace = TRUE, prob = c(p, 1 - p))
  successes <- which(path == 1)
  
  streaks <- rep(0, length(successes))
  for (i in 1:length(successes)) {
    j <- successes[i]
    if (j > k)
      streaks[i] <- ifelse(sum(path[(j-k):(j-1)]) == k, 1, 0)
  }
  list(
    successes = successes, 
    streaks = streaks,
    path = path,
    prop_streaks = sum(streaks) / sum(path)
  )
}
repeat_success(3, 1)
repeat_success(k = 5, n = 100, p = 1 - 0.75)

# nsims <- 5000
nsims <- 1000
# n <- 100
n <- 3
p <- 0.5
results = rep(NA, nsims)
k <- 2
for (sim in 1:nsims) {
  tosses <- rbinom(n, k, p)
  candidates <- which(tosses == 1) + 1
  observed_candidates <- candidates[candidates <= n]
  # if(length(observed_candidates) == 0) {
  #   results[sim] <- 0
  # } else {
  #   results[sim] <- sum(tosses[observed_candidates]) / length(observed_candidates)
  # }
  results[sim] <- sum(tosses[observed_candidates]) / length(observed_candidates)
}
mean(na.omit(results))

tosses <- rbinom(n, 1, p)
candidates <- which(tosses == 1) + 1
observed_candidates <- candidates[candidates <= n]
sum(tosses[observed_candidates]) / length(observed_candidates)

library(purrr)
library(tibble)
get_streaks <- function(x) {
  rle_x <- rle(x)
  length <- rle_x$lengths
  end <- cumsum(len)
  start <- c(1, end[-length(end)]+1)
  data.frame(value = rle_x$values, length = length, start = start, end = end)
}

get_after_streak_inds <- function(x, k) {
  streaks <- get_streaks(x)
  
  # Keep only streaks of specified value 
  # that have at least length k
  streaks <- streaks[streaks$length >= k & streaks$value == 1, , drop=FALSE]
  
  if (NROW(streaks) == 0) {
    return(NULL)
  }
  
  # Index directly after streaks of length k
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

average_prob_after_streak <- function(n, k, p = 0.5) {
  # Simulate n iid bernoulli draws
  x <- sample(0:1, n, replace = TRUE, prob = c(1 - p, p))
  
  # Find these indeces of x that come directly
  # after a streak of k elements of specified value
  inds <- get_after_streak_inds(x, k)
  
  # If no run of at least k subsequent numbers of value exists
  # return NULL (we will dismiss this observation)
  if (length(inds) == 0) {
    return(NULL)
  }
  
  mean(x[inds])
}

summarize_prob_after_streak <- function(x) {
  n <- length(x)
  m <- mean(x)
  sd <- sd(x)
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

simulate_prob_after_streak(
  sims = 1000, n = 3, k = 1
)

simulate_prob_after_streak(
  sims = 1000, n = 30, k = 3
)
hitprob_after_3hits  = unlist(replicate(10000, average_prob_after_streak(n = 30, k = 3), simplify=FALSE))
mean(hitprob_after_3hits)
