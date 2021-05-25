QUANTITATIVE <- "quantitative"
CATEGORICAL <- "categorical"

.get_variable_names <- function(x) {
  return(setdiff(names(x), "sample.id"))
}
