context("test-mod-plot-fct") #  - required for vdiffr
library(vdiffr)

test_that(".check_truthiness", {
  expect_null(.check_truthiness(""))
  expect_equal(.check_truthiness("x"), "x")
  expect_equal(.check_truthiness("x23"), "x23")
  expect_equal(.check_truthiness("23x"), "23x")
  expect_equal(.check_truthiness("Model: outcome"), "Model: outcome")
})

test_that("generate plot with xvar only",{
  set.seed(123)
  n <- 100
  dat <- data.frame(
    a = rnorm(n),
    b = rnorm(n),
    c = sample(letters[1:3], n, replace = T),
    d = sample(letters[1:3], n, replace = T)
  )

  # histogram
  expect_doppelganger("x histogram", .generate_plot(dat, "a"))
  # bar plot
  expect_doppelganger("x barplot", .generate_plot(dat, "c"))
})

test_that("generate plot with xvar and group",{
  set.seed(123)
  n <- 100
  dat <- data.frame(
    a = rnorm(n),
    b = rnorm(n),
    c = sample(letters[1:3], n, replace = T),
    d = sample(letters[1:3], n, replace = T)
  )

  # histogram
  expect_doppelganger("x histogram grouped", .generate_plot(dat, "a", group_var = "c"))
  # bar plot
  expect_doppelganger("x barplot grouped", .generate_plot(dat, "c", group_var = "d"))
  # not allowed - grouping by a quantitative variable.
  expect_error(.generate_plot(dat, "a", group_var = "a"), "Cannot group")
  expect_error(.generate_plot(dat, "a", group_var = "b"), "Cannot group")
  expect_error(.generate_plot(dat, "c", group_var = "a"), "Cannot group")
})


test_that("generate plot with xvar and yvar only", {
  # fake data
  set.seed(123)
  n <- 100
  dat <- data.frame(
    a = rnorm(n),
    b = rnorm(n),
    c = sample(letters[1:3], n, replace = T),
    d = sample(letters[1:3], n, replace = T)
  )
  # scatterplot
  expect_doppelganger("xy scatterplot", .generate_plot(dat, "a", "b"))
  # boxplot
  expect_doppelganger("xy boxplot", .generate_plot(dat, "c", "a"))
  # flipped boxplot
  expect_doppelganger("xy flipped boxplot", .generate_plot(dat, "a", "c"))
  # error
  expect_error(.generate_plot(dat, "c", "d"), "two categorical variables")
})

test_that("generate plot with group specified", {
  # fake data
  set.seed(123)
  n <- 100
  dat <- data.frame(
    a = rnorm(n),
    b = rnorm(n),
    c = sample(letters[1:3], n, replace = T),
    d = sample(letters[1:3], n, replace = T),
    group = sample(letters[1:3], n, replace = T),
    e = rnorm(n)
  )
  # scatterplot
  expect_doppelganger("xy scatterplot grouped", .generate_plot(dat, "a", "b", group_var = "group"))
  # boxplot
  expect_doppelganger("xy boxplot grouped", .generate_plot(dat, "c", "a", group_var = "group"))
  # flipped boxplot
  expect_doppelganger("xy flipped boxplot grouped", .generate_plot(dat, "a", "c", group_var = "group"))
  # errors
  expect_error(.generate_plot(dat, "c", "d", group_var = "group"), "two categorical variables")
  expect_error(.generate_plot(dat, "a", "b", group_var = "a"), "Cannot group")
  expect_error(.generate_plot(dat, "a", "b", group_var = "b"), "Cannot group")
  expect_error(.generate_plot(dat, "a", "b", group_var = "e"), "Cannot group")
  expect_error(.generate_plot(dat, "c", "a", group_var = "a"), "Cannot group")
  expect_error(.generate_plot(dat, "c", "a", group_var = "b"), "Cannot group")
  expect_error(.generate_plot(dat, "a", "c", group_var = "a"), "Cannot group")
  expect_error(.generate_plot(dat, "a", "c", group_var = "b"), "Cannot group")

})

test_that("variable names with spaces", {
  set.seed(123)
  n <- 100
  dat <- data.frame(
    a = rnorm(n),
    b = rnorm(n),
    c = sample(letters[1:3], n, replace = T),
    d = sample(letters[1:3], n, replace = T),
    group = sample(letters[1:3], n, replace = T),
    e = rnorm(n)
  )
  names(dat) <- paste("var", names(dat))
  # No errors:
  # no y variable
  .generate_plot(dat, "var a") # histogram
  .generate_plot(dat, "var c") # bar plot
  .generate_plot(dat, "a", group_var = "c") # grouped histogram
  .generate_plot(dat, "c", group_var = "d") # grouped bar plot
  # with y variable
  .generate_plot(dat, "var a", "var b") # scatterplot
  .generate_plot(dat, "var c", "var a") # boxplot
  .generate_plot(dat, "var a", "var c") # flipped boxplot
  .generate_plot(dat, "var a", "var b", group_var = "var group") # grouped scatterplot
  .generate_plot(dat, "var c", "var a", group_var = "var group") # grouped boxplot
  .generate_plot(dat, "var a", "var c", group_var = "var group") # grouped flipped boxplot
  expect_true(TRUE)
})
