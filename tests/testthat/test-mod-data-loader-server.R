id <- "data_loader_ui_1"

test_that("loads example data", {
  app <- shinytest::ShinyDriver$new(path="../../dev/run_dev.R")
  app$setInputs(use_example_data = TRUE)
  #app$click(NS("load_data_button", id = "data_loader_ui_1"))
  app$click("load_data_button")
})


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
