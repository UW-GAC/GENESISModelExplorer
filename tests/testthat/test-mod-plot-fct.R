context("test-mod-plot-fct") #  - required for vdiffr
library(vdiffr)

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
  expect_doppelganger("scatterplot", .generate_plot(dat, "a", "b"))
  # boxplot
  expect_doppelganger("boxplot", .generate_plot(dat, "c", "a"))
  # flipped boxplot
  expect_doppelganger("flipped boxplot", .generate_plot(dat, "a", "c"))
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
    group = sample(letters[1:3], n, replace = T)
  )
  # scatterplot
  expect_doppelganger("scatterplot grouped", .generate_plot(dat, "a", "b", group = "group"))
  # boxplot
  expect_doppelganger("boxplot grouped", .generate_plot(dat, "c", "a", group = "group"))
  # flipped boxplot
  expect_doppelganger("flipped boxplot grouped", .generate_plot(dat, "a", "c", group = "group"))
  # error
  expect_error(.generate_plot(dat, "c", "d", group = "group"), "two categorical variables")
})
