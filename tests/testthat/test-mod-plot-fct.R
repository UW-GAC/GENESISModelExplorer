context("test-mod-plot-fct") #  - required for vdiffr
library(vdiffr)

# Create some test data.
set.seed(123)
n <- 100
dat <- data.frame(
  quant1 = rnorm(n),
  quant2 = rnorm(n),
  cat1 = sample(letters[1:3], n, replace = T),
  cat2 = sample(letters[1:3], n, replace = T),
  group = sample(letters[1:3], n, replace = T),
  quant3 = rnorm(n)
)

test_that(".check_truthiness", {
  expect_null(.check_truthiness(""))
  expect_equal(.check_truthiness("x"), "x")
  expect_equal(.check_truthiness("x23"), "x23")
  expect_equal(.check_truthiness("23x"), "23x")
  expect_equal(.check_truthiness("Model: outcome"), "Model: outcome")
})

test_that("generate plot with xvar only",{
  # histogram
  expect_doppelganger("x histogram", .generate_plot(dat, "quant1"))
  # bar plot
  expect_doppelganger("x barplot", .generate_plot(dat, "cat1"))
})

test_that("generate plot with xvar and group",{
  # histogram
  expect_doppelganger("x histogram grouped", .generate_plot(dat, "quant1", group_var = "cat1"))
  # bar plot
  expect_doppelganger("x barplot grouped", .generate_plot(dat, "cat1", group_var = "cat2"))
  # not allowed - grouping by a quantitative variable.
  expect_error(.generate_plot(dat, "quant1", group_var = "quant1"), "Cannot group")
  expect_error(.generate_plot(dat, "quant1", group_var = "quant2"), "Cannot group")
  expect_error(.generate_plot(dat, "cat1", group_var = "quant1"), "Cannot group")
})


test_that("generate plot with xvar and yvar only", {
  # scatterplot
  expect_doppelganger("xy scatterplot", .generate_plot(dat, "quant1", "quant2"))
  # boxplot
  expect_doppelganger("xy boxplot", .generate_plot(dat, "cat1", "quant1"))
  # flipped boxplot
  expect_doppelganger("xy flipped boxplot", .generate_plot(dat, "quant1", "cat1"))
  # error
  expect_error(.generate_plot(dat, "cat1", "cat2"), "two categorical variables")
})

test_that("generate plot with group specified", {
  # scatterplot
  expect_doppelganger("xy scatterplot grouped", .generate_plot(dat, "quant1", "quant2", group_var = "group"))
  # boxplot
  expect_doppelganger("xy boxplot grouped", .generate_plot(dat, "cat1", "quant1", group_var = "group"))
  # flipped boxplot
  expect_doppelganger("xy flipped boxplot grouped", .generate_plot(dat, "quant1", "cat1", group_var = "group"))
  # errors
  expect_error(.generate_plot(dat, "cat1", "cat2", group_var = "group"), "two categorical variables")
  expect_error(.generate_plot(dat, "quant1", "quant2", group_var = "quant1"), "Cannot group")
  expect_error(.generate_plot(dat, "quant1", "quant2", group_var = "quant2"), "Cannot group")
  expect_error(.generate_plot(dat, "quant1", "quant2", group_var = "quant3"), "Cannot group")
  expect_error(.generate_plot(dat, "cat1", "quant1", group_var = "quant1"), "Cannot group")
  expect_error(.generate_plot(dat, "cat1", "quant1", group_var = "quant2"), "Cannot group")
  expect_error(.generate_plot(dat, "quant1", "cat1", group_var = "quant1"), "Cannot group")
  expect_error(.generate_plot(dat, "quant1", "cat1", group_var = "quant2"), "Cannot group")

})

test_that("variable names with spaces", {
  tmp_dat <- dat
  names(tmp_dat) <- paste("var", names(tmp_dat))
  # No errors:
  # no y variable
  .generate_plot(tmp_dat, "var quant1") # histogram
  .generate_plot(tmp_dat, "var cat1") # bar plot
  .generate_plot(tmp_dat, "var quant1", group_var = "var cat1") # grouped histogram
  .generate_plot(tmp_dat, "var cat1", group_var = "var cat2") # grouped bar plot
  # with y variable
  .generate_plot(tmp_dat, "var quant1", "var quant2") # scatterplot
  .generate_plot(tmp_dat, "var cat1", "var quant1") # boxplot
  .generate_plot(tmp_dat, "var quant1", "var cat1") # flipped boxplot
  .generate_plot(tmp_dat, "var quant1", "var quant2", group_var = "var group") # grouped scatterplot
  .generate_plot(tmp_dat, "var cat1", "var quant1", group_var = "var group") # grouped boxplot
  .generate_plot(tmp_dat, "var quant1", "var cat1", group_var = "var group") # grouped flipped boxplot
  expect_true(TRUE)
})
