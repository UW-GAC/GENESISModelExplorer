# Hard code data loading statements here for now.
.load_phenotype <- function(filename) {
  phen <- get(load(filename)) %>%
    pData() %>%
    # Change names to have prefix "Phenotype: "
    rename_with(.fn = ~ paste0("Phenotype: ", .x), -sample.id)
}

.load_model <- function(filename) {
  null_model <- get(load(filename))$fit %>%
    # Change names to have prefix "Model: "
    rename_with(.fn = ~ paste0("Model: ", .x), -sample.id)
  null_model
}

.load_data <- function(null_model_file, phenotype_file) {
  null_model <- .load_model(null_model_file)
  phen <- .load_phenotype(phenotype_file)
  dat <- null_model %>%
    inner_join(phen, by = "sample.id")
    print(names(dat))

  dat
}

dat <- .load_data(
  null_model_file = "testdata/null_model.RData",
  phenotype_file = "testdata/1KG_phase3_subset_annot.RData"
)
