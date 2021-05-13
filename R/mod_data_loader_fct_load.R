.load_null_model <- function(filename) {
  tmp <- get(load(filename))

  # Check that it's a valid nuull model file.
  allowed_classes <- c("GENESIS.nullMixedModel", "GENESIS.nullModel")
  cls <- class(tmp)
  if (!(class(tmp) %in% allowed_classes)) {
    err <- sprintf("%s is not a valid GENESIS null model.", filename)
    stop(err)
  }
  # Some users may be using older null model formatss.
  tmp <- GENESIS:::.updateNullModelFormat(tmp)

  null_model <- tmp$fit %>%
    # Change names to have prefix "Model: "
    dplyr::rename_with(.fn = ~ paste0("Model: ", .x), -sample.id)
  # No rownames.
  rownames(null_model) <- NULL
  null_model
}

.load_phenotype_file <- function(filename) {}

.load_data <- function(null_model_filename, phenotype_filename) {}

.check_null_model <- function(null_model) {}

.check_phenotype <- function (phen) {}

.check_data <- function(null_model, phen) {}
