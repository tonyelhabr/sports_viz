
.f_score_similarity = function(data) {
  data %>% mutate(score = (cosine + jaccard + jw) * (soundex + 1)) 
}

# I want all combinations, not just the upper triangle
.comb_all <- function(data) {
  # a <- as.data.frame(t(combn(data, 2)), stringsAsFactors = FALSE)
  # rowser()
  a <- as.data.frame(gtools::permutations(length(data), 2, data, repeats.allowed=TRUE))
  structure(a, class = c("tbl_df", "tbl", "data.frame"))
  
}

join_fuzzily <- function(x, y, col = 'z', suffix = c('x', 'y'), f_score = .f_score_similarity, strict = TRUE) {
  # col = 'z'
  # suffix = c('x', 'y')
  
  col_sym <- col %>% sym()
  col_x_sym <- sprintf('%s_%s', col, suffix[1]) %>% sym()
  col_y_sym <- sprintf('%s_%s', col, suffix[2]) %>% sym()
  
  nms_x <- x %>% names()
  nms_y <- y %>% names()
  nms_share <- intersect(nms_x, nms_y)
  
  .f_rename <- function(data, s) {
    data %>%
      mutate(dummy = !!col_sym) %>% 
      rename_with(~sprintf('%s_%s', .x, s), one_of(nms_share))
  }
  
  df_fulljoin <- 
    full_join(
      x %>% .f_rename(suffix[1]),
      y %>% .f_rename(suffix[2])
    ) %>% 
    relocate(dummy)
  df_fulljoin
  
  df_nomatch_tidy <-
    df_fulljoin %>% 
    filter(is.na(!!col_x_sym) | is.na(!!col_y_sym)) %>% 
    # filter_at(vars(matches('df_'))
    select(matches(sprintf('%s_', col))) %>% 
    gather(key = 'src', value = 'z') %>% 
    mutate(across(src, ~str_remove(., sprintf('%s_', col)))) %>% 
    drop_na() %>% 
    distinct(src, !!col_sym) %>% 
    arrange(src, !!col_sym)
  df_nomatch_tidy
  
  match_x <- df_nomatch_tidy %>% filter(src == suffix[1]) %>% pull(!!col_sym)
  
  # debugonce(tidystringdist::tidy_comb_all)
  df_nomatch_combos <-
    df_nomatch_tidy %>% 
    pull(!!col_sym) %>% 
    # tidystringdist::tidy_comb_all() %>% 
    .comb_all() %>% 
    arrange(V1, V2) %>% 
    filter(V1 != V2)
  df_nomatch_combos
  
  df_nomatch_sdist <- df_nomatch_combos %>% tidystringdist::tidy_stringdist()
  df_nomatch_sdist
  
  df_nomatch_sdist_arr <-
    df_nomatch_sdist %>% 
    filter(V1 %in% match_x) %>% 
    filter(!(V2 %in% match_x)) %>% 
    f_score() %>% 
    group_by(V1) %>% 
    # arrange(cosine, jaccard, jw, soundex) %>% 
    mutate(rnk1 = row_number(score)) %>% 
    ungroup() %>% 
    group_by(V2) %>% 
    mutate(rnk2 = row_number(score)) %>% 
    ungroup() %>% 
    arrange(V1, rnk1)#  %>% 
  # rename(!!col_x_sym := V1, !!col_y_sym := V2)
  df_nomatch_sdist_arr
  
  if(strict) {
    res <-
      df_nomatch_sdist_arr %>% 
      group_by(V2) %>% 
      slice_min(rnk2) %>% 
      ungroup() %>% 
      group_by(V1) %>%
      slice_min(rnk1) %>% 
      ungroup()
    res
  } else {
    res <-
      df_nomatch_sdist_arr %>% 
      filter(rnk == 1L) %>% 
      inner_join(
        df_nomatch_tidy %>% 
          filter(src == suffix[1]) %>% 
          select(!!col_x_sym := z)
      )
  }
  res <-
    res %>% 
    rename(!!col_x_sym := V1, !!col_y_sym := V2)
  res
}
