# Test data for tests.
testdata <- tibble::tibble(
  sample.id = sprintf("samp%s", 1:100),
  quant1 = rnorm(100),
  quant2 = rnorm(100),
  cat1 = sample(letters[1:3], 100, replace = T),
  cat2 = sample(letters[1:3], 100, replace = T),
  group = sample(letters[1:3], 100, replace = T),
  facet = sample(letters[1:3], 100, replace = T)
)

# Helper function to set all default inputs.
# This way we don't have to update every single test if we add a new input.
set_default_inputs <- function(session) {
  session$setInputs(
    x = "",
    y = "",
    group = "",
    facet = "",
    hexbin = FALSE,
    abline = FALSE,
    smooth_line = FALSE,
    lm = FALSE,
    yintercept = FALSE,
    nbins_histogram = 30,
    nbins_hexbin = 30,
    density = FALSE,
    hide_legend = FALSE,
    proportion = FALSE,
    violin = FALSE,
    plot_type = ""
  )
}


test_that("conditional options", {
  # Reactive input arguments
  n <- 100
  dat <- reactiveVal(testdata)
  testServer(mod_plot_server, args = list(dataset = dat), {
    set_default_inputs(session)
    session$setInputs(x = "cat1", y = "")
    skip("Figure out why calling output$show_options_1d produces an error")
    # I have no idea how to test this. Just calling output$show_options_1d produces an error.
    # expect_equal(output$show_options_1d, TRUE)
    # expect_equal(output$show_options_1d_quant, FALSE)
    # expect_equal(output$show_options_2d, FALSE)
    # expect_equal(output$show_options_2d_quant, FALSE)
    # expect_equal(output$show_options_2d_cat, FALSE)
    #
    # set_default_inputs(session)
    # session$setInputs(x = "quant1", y = "")
    # expect_equal(output$show_options_1d, TRUE)
    # expect_equal(output$show_options_1d_quant, TRUE)
    # expect_equal(output$show_options_2d, FALSE)
    # expect_equal(output$show_options_2d_quant, FALSE)
    # expect_equal(output$show_options_2d_cat, FALSE)
    #
    # set_default_inputs(session)
    # session$setInputs(x = "quant1", y = "quant2")
    # expect_equal(output$show_options_1d, FALSE)
    # expect_equal(output$show_options_1d_quant, FALSE)
    # expect_equal(output$show_options_2d, TRUE)
    # expect_equal(output$show_options_2d_quant, TRUE)
    # expect_equal(output$show_options_2d_cat, FALSE)
    #
    # set_default_inputs(session)
    # session$setInputs(x = "quant1", y = "cat1")
    # expect_equal(output$show_options_1d, FALSE)
    # expect_equal(output$show_options_1d_quant, FALSE)
    # expect_equal(output$show_options_2d, TRUE)
    # expect_equal(output$show_options_2d_quant, FALSE)
    # expect_equal(output$show_options_2d_cat, TRUE)
    #
    # set_default_inputs(session)
    # session$setInputs(x = "cat1", y = "quant1")
    # expect_equal(output$show_options_1d, FALSE)
    # expect_equal(output$show_options_1d_quant, FALSE)
    # expect_equal(output$show_options_2d, TRUE)
    # expect_equal(output$show_options_2d_quant, FALSE)
    # expect_equal(output$show_options_2d_cat, TRUE)
    #
    # set_default_inputs(session)
    # session$setInputs(x = "cat1", y = "cat2")
    # expect_equal(output$show_options_1d, FALSE)
    # expect_equal(output$show_options_1d_quant, FALSE)
    # expect_equal(output$show_options_2d, TRUE)
    # expect_equal(output$show_options_2d_quant, FALSE)
    # expect_equal(output$show_options_2d_cat, FALSE)

  })
})

test_that("test plot is created with x variable only", {
  # Reactive input arguments
  n <- 100
  dat <- reactiveVal(testdata)
  testServer(mod_plot_server, args = list(dataset = dat), {
    # No plot to begin with
    expect_error(output$plot)
    set_default_inputs(session)
    session$setInputs(x = "quant1", plot_type = HISTOGRAM)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.

    session$setInputs(plot_type = DENSITY)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.

    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("test plot is created with x and group variables only", {
  # Reactive input arguments
  n <- 100
  dat <- reactiveVal(testdata)
  testServer(mod_plot_server, args = list(dataset = dat), {
    # No plot to begin with
    expect_error(output$plot)
    set_default_inputs(session)
    session$setInputs(x = "quant1", group = "group", plot_type = HISTOGRAM)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.

    session$setInputs(plot_type = DENSITY)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.

    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("test plot is created with x and y variables", {
  # Reactive input arguments
  n <- 100
  dat <- reactiveVal(testdata)
  testServer(mod_plot_server, args = list(dataset = dat), {
    # No plot to begin with
    expect_error(output$plot)
    set_default_inputs(session)
    session$setInputs(x = "quant1", y = "quant2", plot_type = HEXBIN)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.

    session$setInputs(plot_type = SCATTERPLOT)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.

    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("test plot is created with x, y, and group variables", {
  # Reactive input arguments
  n <- 100
  dat <- reactiveVal(testdata)
  testServer(mod_plot_server, args = list(dataset = dat), {
    # No plot to begin with
    expect_error(output$plot)
    set_default_inputs(session)
    session$setInputs(x = "quant1", y = "quant2", group = "group", plot_type = HEXBIN)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.

    session$setInputs(plot_type = SCATTERPLOT)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.

    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("test plot is created with x, y, and facet variables", {
  # Reactive input arguments
  n <- 100
  dat <- reactiveVal(testdata)
  testServer(mod_plot_server, args = list(dataset = dat), {
    # No plot to begin with
    expect_error(output$plot)
    set_default_inputs(session)
    session$setInputs(x = "quant1", y = "quant2", facet = "facet", plot_type = HEXBIN)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.

    session$setInputs(plot_type = SCATTERPLOT)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.

    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("test plot is created with x, y, group, and facet variables", {
  # Reactive input arguments
  n <- 100
  dat <- reactiveVal(testdata)
  testServer(mod_plot_server, args = list(dataset = dat), {
    # No plot to begin with
    expect_error(output$plot)
    set_default_inputs(session)
    session$setInputs(x = "quant1", y = "quant2", group = "group", facet = "facet", plot_type = HEXBIN)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.

    session$setInputs(plot_type = SCATTERPLOT)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.

    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})


test_that("plot is created with abline option", {
  n <- 100
  dat <- reactiveVal(testdata)
  testServer(mod_plot_server, args = list(dataset = dat), {
    # No plot to begin with
    expect_error(output$plot)
    set_default_inputs(session)
    session$setInputs(x = "quant1", y = "quant2", plot_type = SCATTERPLOT, abline = TRUE)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.
    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("plot is created with smooth_line option", {
  n <- 100
  dat <- reactiveVal(testdata)
  testServer(mod_plot_server, args = list(dataset = dat), {
    # No plot to begin with
    expect_error(output$plot)
    set_default_inputs(session)
    session$setInputs(x = "quant1", y = "quant2", plot_type = SCATTERPLOT, smooth_line = TRUE)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.
    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("plot is created with lm option", {
  n <- 100
  dat <- reactiveVal(testdata)
  testServer(mod_plot_server, args = list(dataset = dat), {
    # No plot to begin with
    expect_error(output$plot)
    set_default_inputs(session)
    session$setInputs(x = "quant1", y = "quant2", plot_type = SCATTERPLOT, lm = TRUE)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.
    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("plot is created with yintercept option", {
  n <- 100
  dat <- reactiveVal(testdata)
  testServer(mod_plot_server, args = list(dataset = dat), {
    # No plot to begin with
    expect_error(output$plot)
    set_default_inputs(session)
    session$setInputs(x = "quant1", y = "quant2", plot_type = SCATTERPLOT, yintercept = TRUE)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.
    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("plot is created with hide legend option", {
  n <- 100
  dat <- reactiveVal(testdata)
  testServer(mod_plot_server, args = list(dataset = dat), {
    # No plot to begin with
    expect_error(output$plot)
    set_default_inputs(session)
    session$setInputs(x = "quant1", y = "quant2", group = "group", plot_type = SCATTERPLOT, hide_legend = TRUE)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.
    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("plot_type input updates correctly", {
  skip("requires shinyTest")
})
