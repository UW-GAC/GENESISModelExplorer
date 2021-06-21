QUANTITATIVE <- "quantitative"
CATEGORICAL <- "categorical"

#' Return NULL if a string is not truthy, otherwise return the string itself.
#' @noRd
.check_truthiness <- function(x) {
  if (shiny::isTruthy(x)) {
    x
  } else {
    NULL
  }
}

.get_variable_names <- function(x) {
  return(setdiff(names(x), "sample.id"))
}

.detect_variable_type <- function(variable, n_categories_threshold = 10) {
  if (length(unique(variable)) <= n_categories_threshold) return(CATEGORICAL)

  cls <- class(variable)
  variable_type <- switch(
      cls,
      "numeric" = QUANTITATIVE,
      "integer" = QUANTITATIVE,
      "character" = CATEGORICAL,
      sprintf("unknown type: %s", cls)
    )
  variable_type
}

#' Generate a plot from a dataset
#'
#' @noRd
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_hex
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 geom_abline
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_violin
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 theme
.generate_plot <- function(dat, x_var,
  y_var = NULL,
  group_var = NULL,
  facet_var = NULL,
  hexbin = FALSE,
  abline = FALSE,
  smooth_line = FALSE,
  lm = FALSE,
  yintercept = FALSE,
  violin = FALSE,
  nbins = 30,
  density = FALSE,
  hide_legend = FALSE,
  proportion = FALSE
) {
  # This is using functions in the var_selector module. TODO: improve this?
  type_x <- .detect_variable_type(dat[[x_var]])

  # Check categorical variables
  group_var_str <- NULL
  if (!is.null(group_var) && .detect_variable_type(dat[[group_var]]) == QUANTITATIVE) {
    stop("Cannot group by a quantitative variable.")
  } else if (!is.null(group_var)) {
    group_var_str <- as.name(group_var)
  }
  facet_var_str <- NULL
  if (!is.null(facet_var) && .detect_variable_type(dat[[facet_var]]) == QUANTITATIVE) {
    stop("Cannot facet by a quantitative variable.")
  } else if (!is.null(facet_var)) {
    facet_var_str <- as.name(facet_var)
  }


  if (is.null(y_var)) {
    p <- ggplot(dat, aes_string(x = as.name(x_var)))
    if (type_x == QUANTITATIVE) {
      # Density plot or histogram.
      if (density) {
        if (proportion) {
          pos_string <- "fill"
        } else {
          pos_string <- "identity"
        }
        p <- p + geom_density(aes_string(fill = group_var_str), position = pos_string, alpha = 0.5)
      } else {
        if (proportion) {
          pos_string <- "fill"
        } else {
          pos_string <- "stack"
        }
        p <- p + geom_histogram(aes_string(fill = group_var_str), position = pos_string, bins = nbins)
      }
    } else if (type_x == CATEGORICAL) {
      # Bar plot.
      if (proportion) {
        pos_string <- "fill"
      } else {
        pos_string <- "stack"
      }
      p <- p + geom_bar(aes_string(fill = group_var_str), position = pos_string)
    }
  }
  else {
    type_y <- .detect_variable_type(dat[[y_var]])

    p <- ggplot(dat, aes_string(x = as.name(x_var), y = as.name(y_var)))
    if (type_x == QUANTITATIVE & type_y == QUANTITATIVE) {

      if (hexbin) {
        p <- p + geom_hex(aes_string(), bins = nbins)
      } else {
        p <- p + geom_point(aes_string(color = group_var_str))
      }

      if (abline) {
        p <- p + geom_abline()
      }

      if (smooth_line) {
        p <- p + geom_smooth()
      }

      if (lm) {
        p <- p + geom_smooth(formula = y ~ x, method = 'lm')
      }

      if (yintercept) {
        p <- p + geom_hline(yintercept = 0)
      }

    } else if (type_x == QUANTITATIVE & type_y == CATEGORICAL) {
      # Show a flipped boxplot.
      # We have to recreate p because we need to use coord_flip.
      p <- ggplot(dat, aes_string(y = as.name(x_var), x = as.name(y_var)))

      if (violin) {
        p <- p +
          geom_violin(aes_string(fill = group_var_str), draw_quantiles = 0.5)
      } else {
        p <- p +
          geom_boxplot(aes_string(fill = group_var_str))
      }
      p <- p +
        coord_flip()

    } else if (type_x == CATEGORICAL & type_y == QUANTITATIVE) {
      # Show a boxplot.
      p <- ggplot(dat, aes_string(x = as.name(x_var), y = as.name(y_var)))

      if (violin) {
        p <- p +
          geom_violin(aes_string(fill = group_var_str), draw_quantiles = 0.5)
      } else {
        p <- p +
          geom_boxplot(aes_string(fill = group_var_str))
      }

      if (yintercept) {
        p <- p + geom_hline(yintercept = 0)
      }

    } else if (type_y == CATEGORICAL & type_y == CATEGORICAL) {
      # Maybe we don't want to allow this?
      stop("Cannot plot two categorical variables against each other.")
    }
  }

  if (!is.null(facet_var_str)) {
    p <- p + facet_wrap(facet_var_str)
  }

  if (hide_legend) {
    p <- p + theme(legend.position = "none")
  }

  return(p)
}
