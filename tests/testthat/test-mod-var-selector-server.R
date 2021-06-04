test_that("return values", {
  n <- 100
  dat <- reactiveVal(tibble::tibble(
    a = rnorm(100),
    b = rnorm(100),
    c = sample(letters[1:3], 100, replace = T),
    d = sample(letters[1:3], 100, replace = T)
  ))
  testServer(mod_var_selector_server, args = list(dataset = dat), {
    expected_names <- c(
      "x_var",
      "y_var",
      "group_var",
      "facet_var",
      "hexbin",
      "abline",
      "loess"
    )
    expect_type(session$returned, "list")
    expect_equal(names(session$returned), expected_names)
    expect_null(session$returned$x_var())
    expect_null(session$returned$y_var())
    expect_null(session$returned$group_var())
    expect_null(session$returned$facet_var())
    expect_null(session$returned$abline())
    expect_null(session$returned$loess())
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
    expect_equal(session$returned$y_var(), NULL)
    expect_equal(session$returned$group_var(), NULL)
    expect_equal(session$returned$facet_var(), NULL)
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
    expect_equal(session$returned$x_var(), NULL)
    expect_equal(session$returned$y_var(), "b")
    expect_equal(session$returned$group_var(), NULL)
    expect_equal(session$returned$facet_var(), NULL)
    expect_equal(session$returned$abline(), NULL)
  })
})

test_that("group variable names update", {
  n <- 100
  dat <- reactiveVal(tibble::tibble(
    a = rnorm(100),
    b = rnorm(100),
    c = sample(letters[1:3], 100, replace = T),
    d = sample(letters[1:3], 100, replace = T)
  ))
  testServer(mod_var_selector_server, args = list(dataset = dat), {
    session$setInputs(group = "c")
    expect_equal(session$returned$x_var(), NULL)
    expect_equal(session$returned$y_var(), NULL)
    expect_equal(session$returned$group_var(), "c")
    expect_equal(session$returned$facet_var(), NULL)
    expect_equal(session$returned$abline(), NULL)
  })
})


test_that("facet variable names update", {
  n <- 100
  dat <- reactiveVal(tibble::tibble(
    a = rnorm(100),
    b = rnorm(100),
    c = sample(letters[1:3], 100, replace = T),
    d = sample(letters[1:3], 100, replace = T)
  ))
  testServer(mod_var_selector_server, args = list(dataset = dat), {
    session$setInputs(facet = "c")
    expect_equal(session$returned$x_var(), NULL)
    expect_equal(session$returned$y_var(), NULL)
    expect_equal(session$returned$group_var(), NULL)
    expect_equal(session$returned$facet_var(), "c")
    expect_equal(session$returned$abline(), NULL)
  })
})

test_that("hexbin update", {
  n <- 100
  dat <- reactiveVal(tibble::tibble(
    a = rnorm(100),
    b = rnorm(100),
    c = sample(letters[1:3], 100, replace = T),
    d = sample(letters[1:3], 100, replace = T)
  ))
  testServer(mod_var_selector_server, args = list(dataset = dat), {
    session$setInputs(hexbin = TRUE)
    expect_equal(session$returned$x_var(), NULL)
    expect_equal(session$returned$y_var(), NULL)
    expect_equal(session$returned$group_var(), NULL)
    expect_equal(session$returned$facet_var(), NULL)
    expect_equal(session$returned$hexbin(), TRUE)
    expect_equal(session$returned$abline(), NULL)
  })
})


test_that("abline", {
  n <- 100
  dat <- reactiveVal(tibble::tibble(
    a = rnorm(100),
    b = rnorm(100),
    c = sample(letters[1:3], 100, replace = T),
    d = sample(letters[1:3], 100, replace = T)
  ))
  testServer(mod_var_selector_server, args = list(dataset = dat), {
    session$setInputs(abline = TRUE)
    expect_equal(session$returned$x_var(), NULL)
    expect_equal(session$returned$y_var(), NULL)
    expect_equal(session$returned$group_var(), NULL)
    expect_equal(session$returned$facet_var(), NULL)
    expect_equal(session$returned$hexbin(), NULL)
    expect_equal(session$returned$abline(), TRUE)
  })
})

test_that("loess", {
  n <- 100
  dat <- reactiveVal(tibble::tibble(
    a = rnorm(100),
    b = rnorm(100),
    c = sample(letters[1:3], 100, replace = T),
    d = sample(letters[1:3], 100, replace = T)
  ))
  testServer(mod_var_selector_server, args = list(dataset = dat), {
    session$setInputs(loess = TRUE)
    expect_equal(session$returned$x_var(), NULL)
    expect_equal(session$returned$y_var(), NULL)
    expect_equal(session$returned$group_var(), NULL)
    expect_equal(session$returned$facet_var(), NULL)
    expect_equal(session$returned$hexbin(), NULL)
    expect_equal(session$returned$abline(), NULL)
    expect_equal(session$returned$loess(), TRUE)
  })
})
