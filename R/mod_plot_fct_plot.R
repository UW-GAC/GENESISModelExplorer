.generate_plot <- function(dat, x_var, y_var) {
  p <- ggplot(dat, aes_string(x = as.name(x_var), y = as.name(y_var))) +
    geom_point()
  return(p)
}
