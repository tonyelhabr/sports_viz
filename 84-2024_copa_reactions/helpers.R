library(qs)

manage_io_operations <- function(
    f, 
    ...,
    .name, 
    .data_dir,
    .overwrite = FALSE, 
    .sleep = TRUE,
    .delay_range = c(1, 2)
) {
  
  path <- file.path(.data_dir, paste0(.name, '.qs'))
  
  if (file.exists(path) & isFALSE(.overwrite)) {
    return(qs::qread(path))
  }
  if (isTRUE(.sleep)) {
    Sys.sleep(runif(1, .delay_range[1], .delay_range[2]))
  }
  message(sprintf('%s: Getting %s.', Sys.time(), .name))
  res <- f(...)
  qs::qsave(res, path)
  res
}