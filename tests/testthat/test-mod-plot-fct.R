context("test-mod-plot-fct") #  - required for vdiffr
library(vdiffr)

test_that(".get_variable_names works properly", {
  x <- tibble::tibble(
    x = 1:2,
    y = letters[1:2]
  )
  expect_equal(.get_variable_names(x), c("x", "y"))
  x$sample.id <- c("sample1", "sample2")
  expect_equal(.get_variable_names(x), c("x", "y"))
  # Removes spaces
  names_with_spaces <- c("var with space 1", "var with space 2", "sample.id")
  names(x) <- names_with_spaces
  expect_equal(.get_variable_names(x), names_with_spaces[1:2])
  names_with_colon <- c("Model: var with space 1", "Model: var with space 2", "sample.id")
  names(x) <- names_with_colon
  expect_equal(.get_variable_names(x), names_with_colon[1:2])
})


test_that(".detect_variable_type works properly", {
  expect_equal(.detect_variable_type(1:100), QUANTITATIVE) # integers
  expect_equal(.detect_variable_type(rnorm(100)), QUANTITATIVE) # numeric
  expect_equal(.detect_variable_type(sample(letters, 100, replace = T)), CATEGORICAL) # character
  expect_equal(.detect_variable_type(sample(letters[1:2], 100, replace = T)), CATEGORICAL) # other character
  expect_equal(.detect_variable_type(sample(0:1, 100, replace = T)), CATEGORICAL)
  expect_equal(.detect_variable_type(sample(0:1, 100, replace = T), n_categories_threshold = 0), QUANTITATIVE)
  expect_equal(.detect_variable_type(1:10, n_categories_threshold = 9), QUANTITATIVE)
  expect_equal(.detect_variable_type(1:10, n_categories_threshold = 10), CATEGORICAL)
  expect_equal(.detect_variable_type(1:10, n_categories_threshold = 11), CATEGORICAL)
})

# Create some test data.
set.seed(123)
n <- 1000
dat <- data.frame(
  quant1 = rnorm(n),
  cat1 = sample(letters[1:3], n, replace = T, prob = c(1, 2, 5)),
  cat2 = sample(letters[1:3], n, replace = T),
  group = sample(letters[1:3], n, replace = T),
  quant3 = rnorm(n),
  facet = sample(letters[1:3], n, replace = T)
) %>%
dplyr::mutate(
  quant2 = 2 * quant1^2 + 0.5 * rnorm(n)
)

test_that(".check_truthiness", {
  expect_null(.check_truthiness(""))
  expect_equal(.check_truthiness("x"), "x")
  expect_equal(.check_truthiness("x23"), "x23")
  expect_equal(.check_truthiness("23x"), "23x")
  expect_equal(.check_truthiness("Model: outcome"), "Model: outcome")
})

test_that(".get_plot_type works correctly", {
  # x quantitative
  expect_equal(.get_plot_type(QUANTITATIVE, y_type = NULL), HISTOGRAM)
  expect_equal(.get_plot_type(QUANTITATIVE, y_type = NULL, density = TRUE), DENSITY)
  expect_equal(.get_plot_type(QUANTITATIVE, y_type = NULL, violin = TRUE), HISTOGRAM)
  expect_equal(.get_plot_type(QUANTITATIVE, y_type = NULL, hexbin = TRUE), HISTOGRAM)

  # x categorical
  expect_equal(.get_plot_type(CATEGORICAL, y_type = NULL), BARPLOT)
  expect_equal(.get_plot_type(CATEGORICAL, y_type = NULL, density = TRUE), BARPLOT)
  expect_equal(.get_plot_type(CATEGORICAL, y_type = NULL, violin = TRUE), BARPLOT)
  expect_equal(.get_plot_type(CATEGORICAL, y_type = NULL, hexbin = TRUE), BARPLOT)

  # xy quantitaive
  expect_equal(.get_plot_type(QUANTITATIVE, y_type = QUANTITATIVE), SCATTERPLOT)
  expect_equal(.get_plot_type(QUANTITATIVE, y_type = QUANTITATIVE, density = TRUE), SCATTERPLOT)
  expect_equal(.get_plot_type(QUANTITATIVE, y_type = QUANTITATIVE, violin = TRUE), SCATTERPLOT)
  expect_equal(.get_plot_type(QUANTITATIVE, y_type = QUANTITATIVE, hexbin = TRUE), HEXBIN)

  # xy quantitative categorical
  expect_equal(.get_plot_type(QUANTITATIVE, y_type = CATEGORICAL), BOXPLOT)
  expect_equal(.get_plot_type(QUANTITATIVE, y_type = CATEGORICAL, density = TRUE), BOXPLOT)
  expect_equal(.get_plot_type(QUANTITATIVE, y_type = CATEGORICAL, violin = TRUE), VIOLIN)
  expect_equal(.get_plot_type(QUANTITATIVE, y_type = CATEGORICAL, hexbin = TRUE), BOXPLOT)

  # xy categorical quantitative
  expect_equal(.get_plot_type(CATEGORICAL, y_type = QUANTITATIVE), BOXPLOT)
  expect_equal(.get_plot_type(CATEGORICAL, y_type = QUANTITATIVE, density = TRUE), BOXPLOT)
  expect_equal(.get_plot_type(CATEGORICAL, y_type = QUANTITATIVE, violin = TRUE), VIOLIN)
  expect_equal(.get_plot_type(CATEGORICAL, y_type = QUANTITATIVE, hexbin = TRUE), BOXPLOT)

  # two categorical
  expect_error(.get_plot_type(CATEGORICAL, y_type = CATEGORICAL), "two categorical variables")
  expect_error(.get_plot_type(CATEGORICAL, y_type = CATEGORICAL, density = TRUE), "two categorical variables")
  expect_error(.get_plot_type(CATEGORICAL, y_type = CATEGORICAL, violin = TRUE), "two categorical variables")
  expect_error(.get_plot_type(CATEGORICAL, y_type = CATEGORICAL, hexbin = TRUE), "two categorical variables")
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

test_that("generate plot with smooth_line option", {
  expect_doppelganger("xy smooth_line", .generate_plot(dat, "quant1", "quant2", smooth_line = TRUE))
  expect_doppelganger("xy smooth_line grouped", .generate_plot(dat, "quant1", "quant2", group = "group", smooth_line = TRUE))
  expect_doppelganger("xy smooth_line facet", .generate_plot(dat, "quant1", "quant2", facet = "facet", smooth_line = TRUE))
})

test_that("generate plot with lm option", {
  expect_doppelganger("xy lm", .generate_plot(dat, "quant1", "quant2", lm = TRUE))
  expect_doppelganger("xy lm grouped", .generate_plot(dat, "quant1", "quant2", group = "group", lm = TRUE))
  expect_doppelganger("xy lm facet", .generate_plot(dat, "quant1", "quant2", facet = "facet", lm = TRUE))
})

test_that("generate plot with lm and smooth_line option", {
  expect_doppelganger("xy smooth_line lm", .generate_plot(dat, "quant1", "quant2", lm = TRUE, smooth_line = TRUE))
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
  expect_doppelganger("x histogram bins 2", .generate_plot(dat, "quant1", nbins_histogram = 2))
  expect_doppelganger("x histogram bins 100", .generate_plot(dat, "quant1", nbins_histogram = 100))
  expect_doppelganger("x histogram bins grouped", .generate_plot(dat, "quant1", group_var = "group", nbins_histogram = 10))
  expect_doppelganger("x histogram bins with hexbin option", .generate_plot(dat, "quant1", nbins_hexbin = 2))
  expect_doppelganger("xy hexbin bins 2", .generate_plot(dat, "quant1", "quant2", hexbin = TRUE, nbins_hexbin = 2))
  expect_doppelganger("xy hexbin bins 100", .generate_plot(dat, "quant1", "quant2", hexbin = TRUE, nbins_hexbin = 100))
  expect_doppelganger("xy hexbin bins grouped", .generate_plot(dat, "quant1", "quant2", group_var = "group", hexbin = TRUE, nbins_hexbin = 20))
  expect_doppelganger("x hexbin bins with histogram option", .generate_plot(dat, "quant1", "quant2", hexbin = TRUE, nbins_histogram = 2))
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
  expect_doppelganger("x histogram proportion", .generate_plot(dat, "quant1", proportion = TRUE, nbins_histogram = 5))
  expect_doppelganger("x histogram grouped proportion", .generate_plot(dat, "quant1", group_var = "group", proportion = TRUE, nbins_histogram = 5))
  expect_doppelganger("x density proportion", .generate_plot(dat, "quant1", proportion = TRUE, density = TRUE))
  expect_doppelganger("x density grouped proportion", .generate_plot(dat, "quant1", group_var = "group", proportion = TRUE, density = TRUE))
})

test_that("smooth line with many data points", {
  set.seed(123)
  n <- 60000
  dat_large <- data.frame(
    quant1 = rnorm(n),
    cat1 = sample(letters[1:3], n, replace = T),
    cat2 = sample(letters[1:3], n, replace = T),
    group = sample(letters[1:3], n, replace = T),
    quant3 = rnorm(n),
    facet = sample(letters[1:3], n, replace = T)
  ) %>%
  dplyr::mutate(
    quant2 = 2 * quant1^2 + 0.5 * rnorm(n)
  )
  expect_doppelganger("x smoothed large data", .generate_plot(dat_large, "quant1", "quant2", smooth_line = TRUE))
})

test_that(".get_allowed_plot_types", {
  # just x variable
  expect_equal(.get_allowed_plot_types(QUANTITATIVE, y_type = NULL), c(HISTOGRAM, DENSITY))
  expect_equal(.get_allowed_plot_types(CATEGORICAL, y_type = NULL), c(BARPLOT))
  # x and y variables
  expect_equal(.get_allowed_plot_types(QUANTITATIVE, y_type = QUANTITATIVE), c(HEXBIN, SCATTERPLOT))
  expect_equal(.get_allowed_plot_types(QUANTITATIVE, y_type = CATEGORICAL), c(BOXPLOT, VIOLIN))
  expect_equal(.get_allowed_plot_types(CATEGORICAL, y_type = QUANTITATIVE), c(BOXPLOT, VIOLIN))
  # not allowed
  expect_equal(length(.get_allowed_plot_types(CATEGORICAL, y_type = CATEGORICAL)), 0)
  # other errors
  expect_error(.get_allowed_plot_types("foo"), "variable type must be")
  expect_error(.get_allowed_plot_types(QUANTITATIVE, y_type = "foo"), "variable type must be")
})
