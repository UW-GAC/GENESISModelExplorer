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

# Return the type of the plot
BOXPLOT <- "box plot"
HISTOGRAM <- "histogram"
BARPLOT <- "bar plot"
SCATTERPLOT <- "scatterplot"
HEXBIN <- "hexbin plot"
DENSITY <- "density plot"
VIOLIN <- "violin plot"
.get_plot_type <- function(x_type, y_type = NULL, density = FALSE, violin = FALSE, hexbin = FALSE) {
  # Check that specified types are valid.
  stopifnot(x_type %in% c(QUANTITATIVE, CATEGORICAL))
  stopifnot(is.null(y_type) | y_type %in% c(QUANTITATIVE, CATEGORICAL))

  if (is.null(y_type)) {
    if (x_type == CATEGORICAL) {
      return(BARPLOT)
    } else {
      if (density) {
        return(DENSITY)
      } else {
        return(HISTOGRAM)
      }
    }
  } else {
    if (x_type == QUANTITATIVE & y_type == QUANTITATIVE) {
      if (hexbin) {
        return(HEXBIN)
      } else {
        return(SCATTERPLOT)
      }
    } else if (x_type == QUANTITATIVE & y_type == CATEGORICAL) {
      if (violin) {
        return(VIOLIN)
      } else {
        return(BOXPLOT)
      }
    } else if (x_type == CATEGORICAL & y_type == QUANTITATIVE) {
      if (violin) {
        return(VIOLIN)
      } else {
        return(BOXPLOT)
      }
    } else if (x_type == CATEGORICAL & y_type == CATEGORICAL) {
      stop("Cannot plot two categorical variables.")
    }
  }
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
  # plot type options
  hexbin = FALSE,
  violin = FALSE,
  density = FALSE,
  # general options
  abline = FALSE,
  smooth_line = FALSE,
  lm = FALSE,
  yintercept = FALSE,
  nbins_histogram = 30,
  nbins_hexbin = 30,
  hide_legend = FALSE,
  proportion = FALSE
) {
  # This is using functions in the var_selector module. TODO: improve this?
  type_x <- .detect_variable_type(dat[[x_var]])
  type_y <- if (is.null(y_var)) NULL else .detect_variable_type(dat[[y_var]])

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

  plot_type <- .get_plot_type(type_x, type_y, density = density, violin = violin, hexbin = hexbin)

  if (is.null(y_var)) {
    p <- ggplot(dat, aes_string(x = as.name(x_var)))
    # 1d plots
    if (plot_type == HISTOGRAM) {
      if (proportion) pos_string <- "fill" else pos_string <- "stack"
      p <- p + geom_histogram(aes_string(fill = group_var_str), position = pos_string, bins = nbins_histogram)
    } else if (plot_type == DENSITY) {
      if (proportion) pos_string <- "fill" else pos_string <- "identity"
      p <- p + geom_density(aes_string(fill = group_var_str), position = pos_string, alpha = 0.5)
    } else if (plot_type == BARPLOT) {
      if (proportion) pos_string <- "fill" else pos_string <- "stack"
      p <- p + geom_bar(aes_string(fill = group_var_str), position = pos_string)
    }
  } else {

    p <- ggplot(dat, aes_string(x = as.name(x_var), y = as.name(y_var)))

    if (plot_type == SCATTERPLOT) {
      p <- p + geom_point(aes_string(color = group_var_str))
      if (abline) p <- p + geom_abline()
      if (smooth_line) p <- p + geom_smooth()
      if (lm) p <- p + geom_smooth(formula = y ~ x, method = 'lm')
    } else if (plot_type == HEXBIN) {
      p <- p + geom_hex(aes_string(), bins = nbins_hexbin)
      if (abline) p <- p + geom_abline()
      if (smooth_line) p <- p + geom_smooth()
      if (lm) p <- p + geom_smooth(formula = y ~ x, method = 'lm')
    } else if (plot_type == BOXPLOT) {
      p <- p + geom_boxplot(aes_string(fill = group_var_str))
    } else if (plot_type == VIOLIN) {
      p <- p + geom_violin(aes_string(fill = group_var_str), draw_quantiles = 0.5)
    }
  }

  if (yintercept) p <- p + geom_hline(yintercept = 0)

  if (!is.null(facet_var_str)) {
    p <- p + facet_wrap(facet_var_str)
  }

  if (hide_legend) {
    p <- p + theme(legend.position = "none")
  }

  return(p)
}
