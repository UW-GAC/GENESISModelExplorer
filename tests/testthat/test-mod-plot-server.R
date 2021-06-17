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
    violin = FALSE
  )
}

test_that("test plot type display", {
  # Reactive input arguments
  n <- 100
  dat <- reactiveVal(testdata)
  testServer(mod_plot_server, args = list(dataset = dat), {
    set_default_inputs(session)
    session$setInputs(x = "cat1", y = "")
    expect_equal(plot_type(), BARPLOT)

    set_default_inputs(session)
    session$setInputs(x = "quant1", y = "")
    expect_equal(plot_type(), HISTOGRAM)

    set_default_inputs(session)
    session$setInputs(x = "quant1", y = "", density = TRUE)
    expect_equal(plot_type(), DENSITY)

    set_default_inputs(session)
    session$setInputs(x = "quant1", y = "quant2")
    expect_equal(plot_type(), SCATTERPLOT)

    set_default_inputs(session)
    session$setInputs(x = "quant1", y = "quant2", hexbin = TRUE)
    expect_equal(plot_type(), HEXBIN)

    set_default_inputs(session)
    session$setInputs(x = "quant1", y = "cat1")
    expect_equal(plot_type(), BOXPLOT)

    set_default_inputs(session)
    session$setInputs(x = "quant1", y = "cat1", violin = TRUE)
    expect_equal(plot_type(), VIOLIN)

    set_default_inputs(session)
    session$setInputs(x = "cat1", y = "quant1")
    expect_equal(plot_type(), BOXPLOT)

    set_default_inputs(session)
    session$setInputs(x = "cat1", y = "quant1", violin = TRUE)
    expect_equal(plot_type(), VIOLIN)
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
    session$setInputs(x = "quant1")
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
    session$setInputs(x = "quant1", group = "group")
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
    session$setInputs(x = "quant1", y = "quant2")
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
    session$setInputs(x = "quant1", y = "quant2", group = "group")
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
    session$setInputs(x = "quant1", y = "quant2", facet = "facet")
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
    session$setInputs(x = "quant1", y = "quant2", group = "group", facet = "facet")
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.
    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("plot is created with hexbin option", {
  n <- 100
  dat <- reactiveVal(testdata)
  testServer(mod_plot_server, args = list(dataset = dat), {
    # No plot to begin with
    expect_error(output$plot)
    set_default_inputs(session)
    session$setInputs(x = "quant1", y = "quant2", hexbin = TRUE)
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
    session$setInputs(x = "quant1", y = "quant2", abline = TRUE)
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
    session$setInputs(x = "quant1", y = "quant2", smooth_line = TRUE)
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
    session$setInputs(x = "quant1", y = "quant2", lm = TRUE)
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
    session$setInputs(x = "quant1", y = "quant2", yintercept = TRUE)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.
    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("plot is created with density option", {
  n <- 100
  dat <- reactiveVal(testdata)
  testServer(mod_plot_server, args = list(dataset = dat), {
    # No plot to begin with
    expect_error(output$plot)
    set_default_inputs(session)
    session$setInputs(x = "quant1", density = TRUE)
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
    session$setInputs(x = "quant1", y = "quant2", group = "group", hide_legend = TRUE)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.
    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})
