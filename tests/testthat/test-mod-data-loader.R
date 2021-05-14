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
})
