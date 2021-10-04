#' Load a null model file
#' @noRd
#' @importFrom rlang .data
.load_null_model <- function(filename) {
  tmp <- get(load(filename))

  # Check that it's a valid nuull model file.
  allowed_classes <- c("GENESIS.nullMixedModel", "GENESIS.nullModel")
  cls <- class(tmp)
  if (!(class(tmp) %in% allowed_classes)) {
    err <- sprintf("Null model is not a valid GENESIS null model.", filename)
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

  # Check for duplicates
  if (any(duplicated(tmp$sample.id))) {
    err <- "Phenotype file must not contain duplicated sample.ids."
    stop(err)
  }
  phen <- tmp %>%
    # Change names to have prefix "Phenotype: "
    dplyr::rename_with(.fn = ~ paste0("Phenotype: ", .x), -.data$sample.id) %>%
    tibble::as_tibble()
}

.load_genotype <- function(filename) {
  geno <- readRDS(filename)

  # Check names
  required_names <- c("sample.id")
  missing <- setdiff(required_names, names(geno))
  if (length(missing) > 0) {
    errmsg <- "Genotype file must have a sample.id column"
    stop(errmsg)
  }

  # Check if there are any sample columns.
  if (ncol(geno) <= length(required_names)) {
    errmsg <- "Genotype file must contain variant columns."
    stop(errmsg)
  }

  geno <- geno %>%
    # Change names to have prefix "Phenotype: "
    dplyr::rename_with(.fn = ~ paste0("Genotype: ", .x), -.data$sample.id) %>%
    tibble::as_tibble()

  geno
}

#' Load and combine null model and phenotype files
#' @noRd
#' @importFrom rlang .data
.load_data <- function(null_model_filename, phenotype_filename, genotype_filename = NULL, updateProgress = NULL) {

  # better progress bar based on file sizes.
  nm_size <- file.info(null_model_filename)$size
  phen_size <- file.info(phenotype_filename)$size
  if (isTruthy(genotype_filename)) {
    geno_size <- file.info(genotype_filename)$size
  } else {
    geno_size <- 0
  }
  total_size <- 1.1 * (nm_size + phen_size + geno_size)

  if (is.function(updateProgress)) {
    updateProgress(value = 0, detail = "phenotype file..")
  }
  phen <- .load_phenotype(phenotype_filename)

  if (is.function(updateProgress)) {
    updateProgress(value = (phen_size) / total_size, detail = "null model file...")
  }
  null_model <- .load_null_model(null_model_filename)

  if (isTruthy(genotype_filename)) {
    if (!is.null(updateProgress))
    updateProgress((phen_size + nm_size + geno_size) / total_size, detail = "genotype file...")
    geno <- .load_genotype(genotype_filename)
  } else {
    geno <- NULL
  }

  if (is.function(updateProgress)) {
    updateProgress((phen_size + nm_size) / total_size, detail = "combining...")
  }

  # Check that sample sets are compatible.
  # All samples in the null model must be in the phenotype file.
  chk <- length(setdiff(null_model$sample.id, phen$sample.id))
  if (chk > 0) {
    err <- "Phenotype file must contain all sample.ids in the null model."
    stop(err)
  }

  # All samples in the null model must be in the genotype file.
  if (!is.null(geno)) {
    chk <- length(setdiff(null_model$sample.id, geno$sample.id))
    if (chk > 0) {
      err <- "Genotype file must contain all sample.ids in the null model."
      stop(err)
    }
  }

  dat <- null_model %>%
    dplyr::left_join(phen, by = "sample.id") %>%
    dplyr::select(.data$sample.id, tidyselect::everything())

  if (!is.null(geno)) {
    dat <- dat %>%
      dplyr::left_join(geno, by = "sample.id")
  }

  if (is.function(updateProgress)) {
    updateProgress(value = 1, detail = "done!")
  }

  dat
}
