### TESTSERVER tests
# testServer(mod_data_loader_server, {
#   expect_true(TRUE)
#   browser()
#   session$setInputs(use_example_data = TRUE)
# })

### SHINYDRIVER TESTS
# These may need to use ShinyDriver because we require a button click.
app <- shinytest::ShinyDriver$new(path="../../dev/run_dev.R")
test_that("loads example data", {
  app$setInputs(`data_loader_ui_1-use_example_data` = TRUE)
  app$click("data_loader_ui_1-load_data_button")
  #browser()
  tmp <- app$getAllValues() # for some reason the next step output doesn't match if I don't run this first?
  output <- app$getValue("data_loader_ui_1-data_loaded_message")
  expect_match(output, "900 samples loaded")
  skip("add test that data is correct?")
})
app$stop()

app <- shinytest::ShinyDriver$new(path="../../dev/run_dev.R")
test_that("loads user data", {
  tmp_pheno <- get(load(system.file("extdata", "phenotype.RData", package="shinyNullModel")))
  user_pheno_file <- withr::local_file("pheno.RData")
  save(tmp_pheno, file = user_pheno_file)

  # Temporary null model file with fewer samples.
  user_nullmod_file <- withr::local_file("nullmod.RData")
  null_model <- get(load(system.file("extdata", "null_model.RData", package="shinyNullModel")))
  null_model$fit <- null_model$fit[1:100, ]
  save(null_model, file = user_nullmod_file)

  app$uploadFile(`data_loader_ui_1-null_model_file` = user_nullmod_file)
  app$uploadFile(`data_loader_ui_1-phenotype_file` = user_pheno_file)
  app$click("data_loader_ui_1-load_data_button")
  tmp <- app$getAllValues() # for some reason the next step output doesn't match if I don't run this first?
  output <- app$getValue("data_loader_ui_1-data_loaded_message")
  expect_match(output, "100 samples loaded")
  skip("add test that data is correct?")

})
app$stop()


app <- shinytest::ShinyDriver$new(path="../../dev/run_dev.R")
test_that("fails when only null model file is specified", {
  # tmp_pheno <- get(load(system.file("extdata", "phenotype.RData", package="shinyNullModel")))
  # user_pheno_file <- withr::local_file("pheno.RData")
  # save(tmp_pheno, file = user_pheno_file)

  # Temporary null model file with fewer samples.
  user_nullmod_file <- withr::local_file("nullmod.RData")
  null_model <- get(load(system.file("extdata", "null_model.RData", package="shinyNullModel")))
  null_model$fit <- null_model$fit[1:100, ]
  save(null_model, file = user_nullmod_file)

  app$uploadFile(`data_loader_ui_1-null_model_file` = user_nullmod_file)
  # app$uploadFile(`data_loader_ui_1-phenotype_file` = user_pheno_file)
  app$click("data_loader_ui_1-load_data_button")
  tmp <- app$getAllValues() # for some reason the next step output doesn't match if I don't run this first?
  output <- app$getValue("data_loader_ui_1-data_loaded_message")
  expect_match(output, "")
  skip("add test for error?")
})
app$stop()

app <- shinytest::ShinyDriver$new(path="../../dev/run_dev.R")
test_that("fails when only phenotype file is specified", {
  tmp_pheno <- get(load(system.file("extdata", "phenotype.RData", package="shinyNullModel")))
  user_pheno_file <- withr::local_file("pheno.RData")
  save(tmp_pheno, file = user_pheno_file)

  #app$uploadFile(`data_loader_ui_1-null_model_file` = user_nullmod_file)
  app$uploadFile(`data_loader_ui_1-phenotype_file` = user_pheno_file)
  app$click("data_loader_ui_1-load_data_button")
  tmp <- app$getAllValues() # for some reason the next step output doesn't match if I don't run this first?
  output <- app$getValue("data_loader_ui_1-data_loaded_message")
  expect_match(output, "")
  skip("add test for error?")
})
app$stop()

# test_that("fails to load user data when only one file is specified", {
# })

### OLD TESTS
# test_that(".get_data_reactive loads example data", {
#   expected <- .load_data(
#     system.file("extdata", "null_model.RData", package="shinyNullModel"),
#     get(load(system.file("extdata", "phenotype.RData", package="shinyNullModel")))
#   )
#   input <- list(use_example_data = TRUE)
#   out <- .get_data_reactive(input)
#   expect_equal(out, expected)
# })
#
# test_that(".get_data_reactive loads user data", {
#
#   null_model <- getobj(system.file("extdata", "null_model.RData", package="shinyNullModel"))
#   # Hacky way to make a different null model file
#   null_model$fit <- null_model$fit[1:10, ]
#   nm_file <- withr::local_file()
#   save(nm, file = nm_file)
#
#   phen <- get(load(system.file("extdata", "phenotype.RData", package="shinyNullModel")))
#   phen <- phen[1:10, ]
#   phen_file <- withr::local_file()
#   save(phen, file = phen_file)
#
#   input <- list(
#
#   )
# })
#
# test_that(".get_data_reactive loads example data before user data", {})
#
# test_that("does something", {
#   testServer(mod_data_loader_server, {
#     print("start")
#     session$setInputs(use_example_data = TRUE)
#     print(head(data_reactive()))
#     expect_equal(1, 1)
#     print('done')
# #    print(head(data_reactive()))
#   })
# })
# test_that("example data is loaded if requested", {
#     expected <- .load_data(
#       system.file("extdata", "null_model.RData", package="shinyNullModel"),
#       system.file("extdata", "phenotype.RData", package="shinyNullModel")
#     )
#   shiny::testServer(server, {
#     session$setInputs(use_example_data = TRUE)
#     expect_equal(nrow(data_reactive()), 1)#, expected)
#   })
# })

app$stop()
