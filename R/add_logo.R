

# path_logo <- file.path(dir_proj, 'premier-league.png')
add_logo <-
  function(path_viz,
           path_logo,
           idx_x,
           idx_y,
           logo_scale = 0.1,
           adjust_x = TRUE,
           adjust_y = TRUE,
           path_suffix = '_w_logo',
           delete = TRUE) {
    plot <- path_viz %>% magick::image_read()
    logo_raw <- path_logo %>% magick::image_read()
    
    plot_height <- magick::image_info(plot)$height
    plot_width <- magick::image_info(plot)$width
    
    logo <- magick::image_scale(logo_raw, as.character(round(plot_width * logo_scale)))
    
    info <- magick::image_info(logo)
    logo_width <- info$width
    logo_height <- info$height
    
    x_pos <- plot_width - idx_x * plot_width
    y_pos <- plot_height - idx_y * plot_height
    
    if(adjust_x) {
      x_pos <- x_pos - logo_width
    }
    
    if(adjust_y) {
      y_pos <- y_pos - logo_height
    }
    
    offset <- paste0('+', x_pos, '+', y_pos)
    
    viz <- plot %>% magick::image_composite(logo, offset = offset)
    ext <- path_viz %>% tools::file_ext()
    rgx_ext <- sprintf('[.]%s$', ext)
    
    res <-
      magick::image_write(
        viz,
        path_viz %>% stringr::str_replace(rgx_ext, sprintf('%s.%s', path_suffix, ext))
      )
    res
    
    if(!delete) {
      return(res)
    }
    fs::file_delete(path_viz)
    res
  }

add_logo_epl <- purrr::partial(add_logo, path_logo = file.path('data', 'premier-league.png'), ... = )
add_logo_ll <- purrr::partial(add_logo, path_logo = file.path('data', 'la-liga-150px.png'), ... = )
