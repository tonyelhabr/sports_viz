
.f_score_similarity = function(data) {
  data %>% mutate(score = (cosine + jaccard + jw) * (soundex + 1)) 
}

join_fuzzily <- function(x, y, col = 'z', suffix = c('x', 'y'), f_score = .f_score_similarity) {
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
    drop_na()
  df_nomatch_tidy

  df_nomatch_combos <-
    df_nomatch_tidy %>% 
    pull(z) %>% 
    tidystringdist::tidy_comb_all()
  df_nomatch_combos
  
  df_nomatch_sdist <- df_nomatch_combos %>% tidystringdist::tidy_stringdist()
  df_nomatch_sdist
  
  df_nomatch_sdist_arr <-
    df_nomatch_sdist %>% 
    rename(!!col_x_sym := V1, !!col_y_sym := V2) %>% 
    # mutate(score = (cosine + jaccard + jw) * (soundex + 1)) %>% 
    f_score() %>% 
    # mutate(score = osa) %>% 
    # mutate(across(osa:soundex, ~row_number(desc(.x)))) %>% 
    group_by(!!col_x_sym) %>% 
    # arrange(cosine, jaccard, jw, soundex) %>% 
    mutate(rnk = row_number(score)) %>% 
    ungroup() %>% 
    arrange(!!col_x_sym, rnk)
  df_nomatch_sdist_arr
  
  df_rematched <-
    df_nomatch_sdist_arr %>% 
    filter(rnk == 1L) %>% 
    inner_join(
      df_nomatch_tidy %>% 
        filter(src == suffix[1]) %>% 
        select(!!col_x_sym := z)
    )
  df_rematched
}
