#' Load a null model file
#' @noRd
#' @importFrom rlang .data
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
    dplyr::rename_with(.fn = ~ paste0("Model: ", .x), -.data$sample.id) %>%
    tibble::as_tibble()
  # No rownames.
  rownames(null_model) <- NULL
  null_model
}

#' Load a phenotype file
#' @noRd
#' @importFrom rlang .data
.load_phenotype <- function(filename) {
  tmp <- get(load(filename))
  # Convert to data frame if necessary.
  if (class(tmp) == "AnnotatedDataFrame") tmp <- Biobase::pData(tmp)
  if (!("data.frame" %in% class(tmp))) {
    err <- "Phenotype file must be a data frame or AnnotatedDataFrame."
    stop(err)
  }
  # Check if sample.id exists.
  if (!("sample.id" %in% names(tmp))) {
    err <- "Phenotype file must include sample.id."
    stop(err)
  }
  phen <- tmp %>%
    # Change names to have prefix "Phenotype: "
    dplyr::rename_with(.fn = ~ paste0("Phenotype: ", .x), -.data$sample.id) %>%
    tibble::as_tibble()
}

#' Load and combine null model and phenotype files
#' @noRd
#' @importFrom rlang .data
.load_data <- function(null_model_filename, phenotype_filename) {
  null_model <- .load_null_model(null_model_filename)
  phen <- .load_phenotype(phenotype_filename)

  # Check that sample sets are compatible.
  # All samples in the null model must be in the phenotype file.
  chk <- length(setdiff(null_model$sample.id, phen$sample.id))
  if (chk > 0) {
    err <- "Phenotype file must contain all sample.ids in the null model."
    stop(err)
  }

  dat <- null_model %>%
    dplyr::inner_join(phen, by = "sample.id") %>%
    dplyr::select(.data$sample.id, tidyselect::everything())

  dat
}