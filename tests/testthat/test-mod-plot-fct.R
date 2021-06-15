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
  quant3 = rnorm(n),
  facet = sample(letters[1:3], n, replace = T)
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

test_that("generate plot with xvar and facet",{
  # histogram
  expect_doppelganger("x histogram faceted", .generate_plot(dat, "quant1", facet_var = "facet"))
  # bar plot
  expect_doppelganger("x barplot faceted", .generate_plot(dat, "cat1", facet_var = "facet"))
  # not allowed - grouping by a quantitative variable.
  expect_error(.generate_plot(dat, "quant1", facet_var = "quant1"), "Cannot facet")
  expect_error(.generate_plot(dat, "quant1", facet_var = "quant2"), "Cannot facet")
  expect_error(.generate_plot(dat, "cat1", facet_var = "quant1"), "Cannot facet")
})

test_that("generate plot with xvar, group, facet",{
  # histogram
  expect_doppelganger("x histogram grouped faceted", .generate_plot(dat, "quant1", group_var = "group", facet_var = "facet"))
  # bar plot
  expect_doppelganger("x barplot grouped faceted", .generate_plot(dat, "cat1", group_var = "group", facet_var = "facet"))
  # not allowed - grouping by a quantitative variable.
  expect_error(.generate_plot(dat, "quant1", facet_var = "quant1"), "Cannot facet")
  expect_error(.generate_plot(dat, "quant1", facet_var = "quant2"), "Cannot facet")
  expect_error(.generate_plot(dat, "cat1", facet_var = "quant1"), "Cannot facet")
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

test_that("generate plot with hexbin option", {
  expect_doppelganger("xy hexbin", .generate_plot(dat, "quant1", "quant2", hexbin = TRUE))
  expect_doppelganger("xy hexbin grouped", .generate_plot(dat, "quant1", "quant2", group = "cat1", hexbin = TRUE))
})

test_that("generate plot with abline option", {
  expect_doppelganger("xy abline", .generate_plot(dat, "quant1", "quant2", abline = TRUE))
})

test_that("generate plot with loess option", {
  expect_doppelganger("xy loess", .generate_plot(dat, "quant1", "quant2", loess = TRUE))
  expect_doppelganger("xy loess grouped", .generate_plot(dat, "quant1", "quant2", group = "group", loess = TRUE))
  expect_doppelganger("xy loess facet", .generate_plot(dat, "quant1", "quant2", facet = "facet", loess = TRUE))
})

test_that("generate plot with lm option", {
  expect_doppelganger("xy lm", .generate_plot(dat, "quant1", "quant2", lm = TRUE))
  expect_doppelganger("xy lm grouped", .generate_plot(dat, "quant1", "quant2", group = "group", lm = TRUE))
  expect_doppelganger("xy lm facet", .generate_plot(dat, "quant1", "quant2", facet = "facet", lm = TRUE))
})

test_that("generate plot with lm and loess option", {
  expect_doppelganger("xy loess lm", .generate_plot(dat, "quant1", "quant2", lm = TRUE, loess = TRUE))
})

test_that("generate plot with yintercept line", {
  expect_doppelganger("xy scatterplot yintercept", .generate_plot(dat, "quant1", "quant2", yintercept = TRUE))
  expect_doppelganger("xy boxplot yintercept", .generate_plot(dat, "cat1", "quant1", yintercept = TRUE))
  expect_doppelganger("xy flipped boxplot yintercept", .generate_plot(dat, "quant1", "cat1", yintercept = TRUE))
})

test_that("generate xy plot with group specified", {
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

test_that("generate xy plot with facet specified", {
  # scatterplot
  expect_doppelganger("xy scatterplot faceted", .generate_plot(dat, "quant1", "quant2", facet_var = "facet"))
  # boxplot
  expect_doppelganger("xy boxplot faceted", .generate_plot(dat, "cat1", "quant1", facet_var = "facet"))
  # flipped boxplot
  expect_doppelganger("xy flipped boxplot faceted", .generate_plot(dat, "quant1", "cat1", facet_var = "facet"))
  # errors
  expect_error(.generate_plot(dat, "cat1", "cat2", group_var = "group"), "two categorical variables")
  expect_error(.generate_plot(dat, "quant1", "quant2", facet_var = "quant1"), "Cannot facet")
  expect_error(.generate_plot(dat, "quant1", "quant2", facet_var = "quant2"), "Cannot facet")
  expect_error(.generate_plot(dat, "quant1", "quant2", facet_var = "quant3"), "Cannot facet")
  expect_error(.generate_plot(dat, "cat1", "quant1", facet_var = "quant1"), "Cannot facet")
  expect_error(.generate_plot(dat, "cat1", "quant1", facet_var = "quant2"), "Cannot facet")
  expect_error(.generate_plot(dat, "quant1", "cat1", facet_var = "quant1"), "Cannot facet")
  expect_error(.generate_plot(dat, "quant1", "cat1", facet_var = "quant2"), "Cannot facet")

})

test_that("variable names with spaces", {
  tmp_dat <- dat
  names(tmp_dat) <- paste("var", names(tmp_dat))
  # No errors:
  # no y variable
  .generate_plot(tmp_dat, "var quant1") # histogram
  .generate_plot(tmp_dat, "var cat1") # bar plot
  .generate_plot(tmp_dat, "var quant1", group_var = "var group") # grouped histogram
  .generate_plot(tmp_dat, "var cat1", group_var = "var group") # grouped bar plot
  .generate_plot(tmp_dat, "var quant1", facet_var = "var facet") # grouped histogram
  .generate_plot(tmp_dat, "var cat1", facet_var = "var facet") # grouped bar plot
  # with y variable
  .generate_plot(tmp_dat, "var quant1", "var quant2") # scatterplot
  .generate_plot(tmp_dat, "var cat1", "var quant1") # boxplot
  .generate_plot(tmp_dat, "var quant1", "var cat1") # flipped boxplot
  .generate_plot(tmp_dat, "var quant1", "var quant2", group_var = "var group") # grouped scatterplot
  .generate_plot(tmp_dat, "var cat1", "var quant1", group_var = "var group") # grouped boxplot
  .generate_plot(tmp_dat, "var quant1", "var cat1", group_var = "var group") # grouped flipped boxplot
  .generate_plot(tmp_dat, "var quant1", "var quant2", facet_var = "var facet") # faceted scatterplot
  .generate_plot(tmp_dat, "var cat1", "var quant1", facet_var = "var facet") # faceted boxplot
  .generate_plot(tmp_dat, "var quant1", "var cat1", facet_var = "var facet") # faceted flipped boxplot
  expect_true(TRUE)
})

test_that("violin plots", {
  # boxplot
  expect_doppelganger("xy boxplot violin", .generate_plot(dat, "cat1", "quant1", violin = TRUE))
  expect_doppelganger("xy boxplot grouped violin", .generate_plot(dat, "cat1", "quant1", group_var = "group", violin = TRUE))
  # flipped boxplot
  expect_doppelganger("xy flipped boxplot violin", .generate_plot(dat, "quant1", "cat1", violin = TRUE))
  expect_doppelganger("xy flipped boxplot grouped violin", .generate_plot(dat, "quant1", "cat1", group_var = "group", violin = TRUE))
})

test_that("nbins", {
  expect_doppelganger("x histogram bins 2", .generate_plot(dat, "quant1", nbins = 2))
  expect_doppelganger("x histogram bins 100", .generate_plot(dat, "quant1", nbins = 100))
  expect_doppelganger("x histogram bins grouped", .generate_plot(dat, "quant1", group_var = "group", nbins = 10))
  expect_doppelganger("xy hexbin bins 2", .generate_plot(dat, "quant1", "quant2", hexbin = TRUE, nbins = 2))
  expect_doppelganger("xy hexbin bins 100", .generate_plot(dat, "quant1", "quant2", hexbin = TRUE, nbins = 100))
  expect_doppelganger("xy hexbin bins grouped", .generate_plot(dat, "quant1", "quant2", group_var = "group", hexbin = TRUE, nbins = 20))
})


test_that("density plot", {
  expect_doppelganger("x density", .generate_plot(dat, "quant1", density = TRUE))
  expect_doppelganger("x density grouped", .generate_plot(dat, "quant1", group_var = "group", density = TRUE))
})

test_that("hide legend", {
  expect_doppelganger("x grouped hide legend", .generate_plot(dat, "quant1", group_var = "group", hide_legend = TRUE))
  expect_doppelganger("xy boxplot hide legend", .generate_plot(dat, "cat1", "quant1", group_var = "group", hide_legend = TRUE))
})

test_that("proportion", {
  expect_doppelganger("x histogram proportion", .generate_plot(dat, "quant1", proportion = TRUE, nbins = 5))
  expect_doppelganger("x histogram grouped proportion", .generate_plot(dat, "quant1", group_var = "group", proportion = TRUE, nbins = 5))
  expect_doppelganger("x density proportion", .generate_plot(dat, "quant1", proportion = TRUE, density = TRUE))
  expect_doppelganger("x density grouped proportion", .generate_plot(dat, "quant1", group_var = "group", proportion = TRUE, density = TRUE))
})
