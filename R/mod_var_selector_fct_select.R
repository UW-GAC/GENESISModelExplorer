QUANTITATIVE <- "quantitative"
CATEGORICAL <- "categorical"

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
