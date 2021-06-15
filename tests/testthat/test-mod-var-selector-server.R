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
      "loess",
      "lm",
      "yintercept",
      "violin",
      "nbins",
      "density"
    )
    expect_type(session$returned, "list")
    expect_equal(names(session$returned), expected_names)
    expect_null(session$returned$x_var())
    expect_null(session$returned$y_var())
    expect_null(session$returned$group_var())
    expect_null(session$returned$facet_var())
    expect_null(session$returned$abline())
    expect_null(session$returned$loess())
    expect_null(session$returned$lm())
    expect_null(session$returned$yintercept())
    expect_null(session$returned$violin())
    expect_null(session$returned$density())
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
    expect_equal(session$returned$lm(), NULL)
    expect_equal(session$returned$yintercept(), NULL)
    expect_equal(session$returned$violin(), NULL)
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
    expect_equal(session$returned$lm(), NULL)
    expect_equal(session$returned$yintercept(), NULL)
    expect_equal(session$returned$violin(), NULL)
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
    expect_equal(session$returned$lm(), NULL)
    expect_equal(session$returned$yintercept(), NULL)
    expect_equal(session$returned$violin(), NULL)
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
    expect_equal(session$returned$lm(), NULL)
    expect_equal(session$returned$yintercept(), NULL)
    expect_equal(session$returned$violin(), NULL)
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
    expect_equal(session$returned$lm(), NULL)
    expect_equal(session$returned$yintercept(), NULL)
    expect_equal(session$returned$violin(), NULL)
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
    expect_equal(session$returned$lm(), NULL)
    expect_equal(session$returned$yintercept(), NULL)
    expect_equal(session$returned$violin(), NULL)
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
    expect_equal(session$returned$lm(), NULL)
    expect_equal(session$returned$yintercept(), NULL)
    expect_equal(session$returned$violin(), NULL)
  })
})

test_that("lm", {
  n <- 100
  dat <- reactiveVal(tibble::tibble(
    a = rnorm(100),
    b = rnorm(100),
    c = sample(letters[1:3], 100, replace = T),
    d = sample(letters[1:3], 100, replace = T)
  ))
  testServer(mod_var_selector_server, args = list(dataset = dat), {
    session$setInputs(lm = TRUE)
    expect_equal(session$returned$x_var(), NULL)
    expect_equal(session$returned$y_var(), NULL)
    expect_equal(session$returned$group_var(), NULL)
    expect_equal(session$returned$facet_var(), NULL)
    expect_equal(session$returned$hexbin(), NULL)
    expect_equal(session$returned$abline(), NULL)
    expect_equal(session$returned$loess(), NULL)
    expect_equal(session$returned$lm(), TRUE)
    expect_equal(session$returned$yintercept(), NULL)
    expect_equal(session$returned$violin(), NULL)
  })
})


test_that("yintercept", {
  n <- 100
  dat <- reactiveVal(tibble::tibble(
    a = rnorm(100),
    b = rnorm(100),
    c = sample(letters[1:3], 100, replace = T),
    d = sample(letters[1:3], 100, replace = T)
  ))
  testServer(mod_var_selector_server, args = list(dataset = dat), {
    session$setInputs(yintercept = TRUE)
    expect_equal(session$returned$x_var(), NULL)
    expect_equal(session$returned$y_var(), NULL)
    expect_equal(session$returned$group_var(), NULL)
    expect_equal(session$returned$facet_var(), NULL)
    expect_equal(session$returned$hexbin(), NULL)
    expect_equal(session$returned$abline(), NULL)
    expect_equal(session$returned$loess(), NULL)
    expect_equal(session$returned$lm(), NULL)
    expect_equal(session$returned$yintercept(), TRUE)
    expect_equal(session$returned$violin(), NULL)
  })
})

test_that("violin", {
  n <- 100
  dat <- reactiveVal(tibble::tibble(
    a = rnorm(100),
    b = rnorm(100),
    c = sample(letters[1:3], 100, replace = T),
    d = sample(letters[1:3], 100, replace = T)
  ))
  testServer(mod_var_selector_server, args = list(dataset = dat), {
    session$setInputs(violin = TRUE)
    expect_equal(session$returned$x_var(), NULL)
    expect_equal(session$returned$y_var(), NULL)
    expect_equal(session$returned$group_var(), NULL)
    expect_equal(session$returned$facet_var(), NULL)
    expect_equal(session$returned$hexbin(), NULL)
    expect_equal(session$returned$abline(), NULL)
    expect_equal(session$returned$loess(), NULL)
    expect_equal(session$returned$lm(), NULL)
    expect_equal(session$returned$yintercept(), NULL)
    expect_equal(session$returned$violin(), TRUE)
  })
})

test_that("nbins", {
  n <- 100
  dat <- reactiveVal(tibble::tibble(
    a = rnorm(100),
    b = rnorm(100),
    c = sample(letters[1:3], 100, replace = T),
    d = sample(letters[1:3], 100, replace = T)
  ))
  testServer(mod_var_selector_server, args = list(dataset = dat), {
    session$setInputs(nbins = 50)
    expect_equal(session$returned$x_var(), NULL)
    expect_equal(session$returned$y_var(), NULL)
    expect_equal(session$returned$group_var(), NULL)
    expect_equal(session$returned$facet_var(), NULL)
    expect_equal(session$returned$hexbin(), NULL)
    expect_equal(session$returned$abline(), NULL)
    expect_equal(session$returned$loess(), NULL)
    expect_equal(session$returned$lm(), NULL)
    expect_equal(session$returned$yintercept(), NULL)
    expect_equal(session$returned$violin(), NULL)
    expect_equal(session$returned$nbins(), 50)
  })
})
