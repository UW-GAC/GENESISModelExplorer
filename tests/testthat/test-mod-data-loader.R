test_that("load null model works as expected", {
  nullmod_file <- system.file("extdata", "null_model.RData", package="shinyNullModel")
  nm <- get(load(nullmod_file))
  out <- .load_null_model(nullmod_file)
  expect_s3_class(out, "tbl_df")
  expect_equal(out$sample.id, nm$fit$sample.id)
  vars <- setdiff(names(out), "sample.id")
  expect_true(all(stringr::str_starts(vars, "Model: "))) # Appends model: to the names
})

test_that("load null model data updates old null models", {
  nullmod_file <- system.file("extdata", "null_model_old.RData", package="shinyNullModel")
  nm <- get(load(nullmod_file))
  expect_warning(out <- .load_null_model(nullmod_file), "updated")
  expect_s3_class(out, "tbl_df")
  expect_equal(out$sample.id, nm$sample.id)
  vars <- setdiff(names(out), "sample.id")
  expect_true(all(stringr::str_starts(vars, "Model: "))) # Appends model: to the names
})

test_that("load null model fails iwth incorrect data type", {
  nm <- c(1, 2, 3)
  tmpfile <- tempfile()
  save(nm, file = tmpfile)
  expect_error(.load_null_model(tmpfile), "valid GENESIS null model")
  unlink(tmpfile)
})

test_that("load phenotype works as expected with AnnotatedDataFrame", {
  phenotype_file <- system.file("extdata", "phenotype.RData", package="shinyNullModel")
  phen <- get(load(phenotype_file))
  out <- .load_phenotype(phenotype_file)
  expect_s3_class(out, "tbl_df")
  expect_equal(out$sample.id, phen$sample.id)
  vars <- setdiff(names(out), "sample.id")
  expect_true(all(stringr::str_starts(vars, "Phenotype: "))) # Appends model: to the names
})

test_that("load phenotype works as expected with data frame", {
  # Save as a temporary file
  phen <- get(load(system.file("extdata", "phenotype.RData", package="shinyNullModel"))) %>%
    Biobase::pData()
  tmpfile <- tempfile()
  save(phen, file = tmpfile)
  out <- .load_phenotype(tmpfile)
  expect_s3_class(out, "tbl_df")
  expect_equal(out$sample.id, phen$sample.id)
  vars <- setdiff(names(out), "sample.id")
  expect_true(all(stringr::str_starts(vars, "Phenotype: "))) # Appends model: to the names
  unlink(tmpfile)
})

test_that("load phenotype fails if sample.id is missing", {
  # Remove sample id to make sure the function returns an error.
  tmp <- get(load(system.file("extdata", "phenotype.RData", package="shinyNullModel")))
  tmp <- tmp[, Biobase::varLabels(tmp) != "sample.id"]
  tmpfile <- tempfile()
  save(tmp, file = tmpfile)
  expect_error(.load_phenotype(tmpfile), "must include sample.id")
  unlink(tmpfile)
})

test_that("load phenotype fails with incorrect data type", {
  # Save a file with a incorrect data type.
  tmp <- 1:3
  tmpfile <- tempfile()
  save(tmp, file = tmpfile)
  expect_error(.load_phenotype(tmpfile), "must be a data frame or AnnotatedDataFrame")
  unlink(tmpfile)
})
