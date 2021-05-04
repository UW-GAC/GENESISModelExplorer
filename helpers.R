# Hard code data loading statements here for now.
.load_phenotype <- function(filename) {
  # TODO: Check that it's a valid phenotype file.
  phen <- get(load(filename)) %>%
    pData() %>%
    # Change names to have prefix "Phenotype: "
    rename_with(.fn = ~ paste0("Phenotype: ", .x), -sample.id)
}

.load_model <- function(filename) {
  # TODO: Check that it's a valid model file.
  print(filename)
  tmp <- get(load(filename))
  print(names(tmp))
  null_model <- get(load(filename))$fit %>%
    # Change names to have prefix "Model: "
    rename_with(.fn = ~ paste0("Model: ", .x), -sample.id)
  null_model
}

.load_data <- function(null_model_file, phenotype_file) {
  null_model <- .load_model(null_model_file)
  phen <- .load_phenotype(phenotype_file)
  # TODO: Check that sample sets are compatible.
  dat <- null_model %>%
    inner_join(phen, by = "sample.id") %>%
    select(sample.id, everything())
    print(names(dat))

  dat
}


QUANTITATIVE = "quantitative"
CATEGORICAL = "categorical"
.detect_variable_type <- function(variable, n_categories_threshold = 10) {
  # Right now we only care about numeric vs. categorical, I think?
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
