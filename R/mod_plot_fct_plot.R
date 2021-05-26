#' Generate a plot from a dataset
#'
#' @noRd
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 coord_flip
.generate_plot <- function(dat, x_var, y_var = NULL, group = NULL) {
  # This is using functions in the var_selector module. TODO: improve this?
  type_x <- .detect_variable_type(dat[[x_var]])

  if (!is.null(group) && .detect_variable_type(dat[[group]]) == QUANTITATIVE) {
    stop("Cannot group by a quantitative variable.")
  }

  if (is.null(y_var)) {
    p <- ggplot(dat, aes_string(x = as.name(x_var)))
    if (type_x == QUANTITATIVE) {
      p <- p + geom_histogram(aes_string(fill = group))
    } else if (type_x == CATEGORICAL) {
      p <- p + geom_bar(aes_string(fill = group))
    }
  }
  else {
    type_y <- .detect_variable_type(dat[[y_var]])

    p <- ggplot(dat, aes_string(x = as.name(x_var), y = as.name(y_var)))
    if (type_x == QUANTITATIVE & type_y == QUANTITATIVE) {
      # Show a scatterplot.
      p <- p + geom_point(aes_string(color = group))
    } else if (type_x == QUANTITATIVE & type_y == CATEGORICAL) {
      # Show a flipped boxplot.
      # We have to recreate p because we need to use coord_flip.
      p <- ggplot(dat, aes_string(y = as.name(x_var), x = as.name(y_var))) +
        geom_boxplot(aes_string(fill = group)) +
        coord_flip()
    } else if (type_x == CATEGORICAL & type_y == QUANTITATIVE) {
      # Show a boxplot.
      p <- p + geom_boxplot(aes_string(fill = group))
    } else if (type_y == CATEGORICAL & type_y == CATEGORICAL) {
      # Maybe we don't want to allow this?
      stop("Cannot plot two categorical variables against each other.")
    }
  }
  return(p)
}
