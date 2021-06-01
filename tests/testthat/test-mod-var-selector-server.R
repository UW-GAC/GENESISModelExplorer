# test_that("x variable names update", {
#   n <- 100
#   dat <- tibble::tibble(
#     a = rnorm(n),
#     b = rnorm(n),
#     c = sample(letters[1:3], n, replace = T),
#     d = sample(letters[1:3], n, replace = T)
#   )
#   argr <- list(r = list(data_loader = list(dataset = dat)))
#   testServer(mod_var_selector_server, args = argr, {
#     skip("figure out how to write test")
#   })
#
# })

test_that("return values", {
  n <- 100
  dat <- reactiveVal(tibble::tibble(
    a = rnorm(100),
    b = rnorm(100),
    c = sample(letters[1:3], 100, replace = T),
    d = sample(letters[1:3], 100, replace = T)
  ))
  testServer(mod_var_selector_server, args = list(dataset = dat), {
    expected_names <- c("x_var", "y_var")
    expect_type(session$returned, "list")
    expect_equal(names(session$returned), expected_names)
    expect_null(session$returned$x_var())
    expect_null(session$returned$y_var())
  })
})

test_that("x variable names update", {
  n <- 100
  dat <- reactiveVal(tibble::tibble(
    a = rnorm(100),
    b = rnorm(100),
    c = sample(letters[1:3], 100, replace = T),
    d = sample(letters[1:3], 100, replace = T)
  ))
  testServer(mod_var_selector_server, args = list(dataset = dat), {
    session$setInputs(x = "a")
    expect_equal(session$returned$x_var(), "a")
  })
})

test_that("y variable names update", {
  n <- 100
  dat <- reactiveVal(tibble::tibble(
    a = rnorm(100),
    b = rnorm(100),
    c = sample(letters[1:3], 100, replace = T),
    d = sample(letters[1:3], 100, replace = T)
  ))
  testServer(mod_var_selector_server, args = list(dataset = dat), {
    session$setInputs(y = "b")
    expect_equal(session$returned$y_var(), "b")
  })
})
