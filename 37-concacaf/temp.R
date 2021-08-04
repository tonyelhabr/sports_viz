
plot_grid <- function(x, ...) {
  x <- by_team_geo_init
  x$col <- factor(x$col, levels = 0:(max(x$col) + 1))
  x$row <- factor(x$row, levels = rev(0:(max(x$row) + 1)))
  x %>% 
    ggplot() +
    aes(x = col, y = row) +
    geom_rect(
      xmin = as.numeric(x$col) - 0.5,
      xmax = as.numeric(x$col) + 0.5,
      ymin = as.numeric(x$row) - 0.5,
      ymax = as.numeric(x$row) + 0.5,
      ,,,
    ) +
    # geom_text(aes(label = text)) +
    ylim(levels(x$row)) +
    xlim(levels(x$col))
}
