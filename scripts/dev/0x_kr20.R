
library(rcrtan)
library(tidyverse)

bh_depend2 <- bh_depend %>% as_tibble()
bh_depend2['total'] <- rowSums(bh_depend[, 2:31])
bh_depend2

.prep_kr <- function(data, items, total = NULL) {
  
  if(!is.null(total)){
    colnames(data)[which(colnames(data) == total)] <- 'total'
  }
  
  if(length(items) == 1L & is.null(total)){
    stop('If you do not have item-level data, you need to fill the \'total\' argument with the name of a column that contains the total raw scores.')
  }
  data
}

# To make interactive use of the functions straightforward.
data = bh_depend2
items = 2:31
total = 'title'

kr20 <- function(data, items, total = NULL) {

  res <- data
  data <- .prep_kr(data, items, total)
  
  if(length(items) == 1L){
    warning('Call `kr21` if length(items) == 1')
    return(res)
  }

  sigma_y <- 
    data %>%
    summarise(across(items, var)) %>%
    gather(key, value) %>%
    summarise(sigma_y = sum(value)) %>% 
    pull(sigma_y)
  
  k <- items %>% length()
  
  sigma_x <-
    data %>%
    select(items) %>%
    purrrlyr::by_row(sum, .collate = 'rows', .to = 'total') %>%
    summarise(v = var(total)) %>%
    pull(v)
  
  kr20 <- (k / (k - 1)) * (1 - (sigma_y / sigma_x))
  kr20
}

kr21 <- function(data, items, total = NULL) {
  
  res <- data
  data <- .prep_kr(data, items, total)
  
  if(length(items) == 1L){
    stopifnot('Although `length(items) == 1L`, the value of `items` should be greater than 1' = items > 1)
    n_item <- items
  } else {
    warning('Call `kr20` if length(items) > 1')
    return(res)
  }
  
  m <- 
    data %>%
    summarise(m = mean(total)) %>%
    pull(m)
  
  s <- 
    data %>%
    summarise(s = sd(total)) %>%
    pull(s)

  kr21 <- (n_item / (n_item - 1)) * (1 - ((m * (n_item - m)) / (n_item * (s^2))))
  kr21
}
bh_depend2 %>% relocate(total)
kr20(bh_depend2, items = 2:31)
kr21(bh_depend2, items = 30, total = 'total')

# Testing how it works with continuous variables
bh_depend2 %>% 
  # select(-total) %>%
  select(-matches('^L')) %>% 
  mutate_at(vars(-ID, -total), ~as.double(.) %>% if_else(. == 1, runif(1), .)) %>% 
  kr20(items = 2:21)

res <-
  c(3:31) %>% 
  tibble(k = .) %>% 
  mutate(kr20 = map_dbl(k, ~kr20(bh_depend2, 2:.x)))
  
viz <-
  res %>% 
  ggplot() +
  aes(x = k, y = kr20) +
  geom_point()
viz

res <-
  c(3:31) %>% 
  tibble(k = .) %>% 
  mutate(kr20 = map_dbl(k, ~kr20(rcrtan::bh_gstudy, 2:.x)))
res

viz <-
  res %>% 
  ggplot() +
  aes(x = k, y = kr20) +
  geom_point()
viz

set.seed(42L)
x <- 
  # matrix(rep(c(1L, 0L, 0L, 1L), 25), nrow = 10, ncol = 10, byrow = T) %>% 
  # matrix(round(runif(3000)), nrow = 100, ncol = 30, byrow = T) %>% 
  # matrix(round(rnorm(100, mean = 0.3, sd = 0.2)), nrow = 100, ncol = 30, byrow = T) %>% 
  # as_tibble() %>% 
  rep(c(1L: 30L), 100L) %>% 
  tibble(value = .) %>% 
  mutate(z = value %% 3) %>% 
  mutate(z = if_else(z >= 1, 1, 0)) %>% 
  pivot_wider(names_from = 'value', values_from = 'z', names_prefix = 'v')
  janitor::clean_names() %>% 
  # purrrlyr::by_row(sum, .collate = 'rows', .to = 'total') %>% 
  mutate(id = row_number()) %>% 
  relocate(id)
x
# x %>% pivot_longer(matches('^v')) %>% filter(!value %in% c(0, 1))

res <-
  c(3:31) %>% 
  tibble(k = .) %>% 
  mutate(kr20 = map_dbl(k, ~kr20(x, 2:.x))) 
res

viz <-
  res %>% 
  ggplot() +
  aes(x = k, y = kr20) +
  geom_point()
viz
