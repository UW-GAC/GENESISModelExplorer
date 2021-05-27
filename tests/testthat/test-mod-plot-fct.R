context("test-mod-plot-fct") #  - required for vdiffr
library(vdiffr)

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
  expect_doppelganger("x histogram grouped", .generate_plot(dat, "a", group = "c"))
  # bar plot
  expect_doppelganger("x barplot grouped", .generate_plot(dat, "c", group = "d"))
  # not allowed - grouping by a quantitative variable.
  expect_error(.generate_plot(dat, "a", group = "a"), "Cannot group")
  expect_error(.generate_plot(dat, "a", group = "b"), "Cannot group")
  expect_error(.generate_plot(dat, "c", group = "a"), "Cannot group")
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
  expect_doppelganger("xy scatterplot grouped", .generate_plot(dat, "a", "b", group = "group"))
  # boxplot
  expect_doppelganger("xy boxplot grouped", .generate_plot(dat, "c", "a", group = "group"))
  # flipped boxplot
  expect_doppelganger("xy flipped boxplot grouped", .generate_plot(dat, "a", "c", group = "group"))
  # errors
  expect_error(.generate_plot(dat, "c", "d", group = "group"), "two categorical variables")
  expect_error(.generate_plot(dat, "a", "b", group = "a"), "Cannot group")
  expect_error(.generate_plot(dat, "a", "b", group = "b"), "Cannot group")
  expect_error(.generate_plot(dat, "a", "b", group = "e"), "Cannot group")
  expect_error(.generate_plot(dat, "c", "a", group = "a"), "Cannot group")
  expect_error(.generate_plot(dat, "c", "a", group = "b"), "Cannot group")
  expect_error(.generate_plot(dat, "a", "c", group = "a"), "Cannot group")
  expect_error(.generate_plot(dat, "a", "c", group = "b"), "Cannot group")

})
