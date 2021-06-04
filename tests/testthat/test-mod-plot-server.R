# Test data for tests.
testdata <- tibble::tibble(
  quant1 = rnorm(100),
  quant2 = rnorm(100),
  cat1 = sample(letters[1:3], 100, replace = T),
  cat2 = sample(letters[1:3], 100, replace = T)
)


test_that("test plot is created with x variable only", {
  # Reactive input arguments
  n <- 100
  dat <- reactiveVal(testdata)
  selected <- list(
    x_var = reactiveVal("quant1"),
    y_var = reactiveVal(""),
    group_var = reactiveVal(""),
    facet_var = reactiveVal(""),
    hexbin = reactiveVal(FALSE),
    abline = reactiveVal(FALSE)
  )
  testServer(mod_plot_server, args = list(dataset = dat, selections = selected), {
    # No plot to begin with
    expect_error(output$plot)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.
    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("test plot is created with x and group variables only", {
  # Reactive input arguments
  n <- 100
  dat <- reactiveVal(testdata)
  selected <- list(
    x_var = reactiveVal("quant1"),
    y_var = reactiveVal(""),
    group_var = reactiveVal("cat1"),
    facet_var = reactiveVal(""),
    hexbin = reactiveVal(FALSE),
    abline = reactiveVal(FALSE)
  )
  testServer(mod_plot_server, args = list(dataset = dat, selections = selected), {
    # No plot to begin with
    expect_error(output$plot)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.
    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("test plot is created with x and y variables", {
  # Reactive input arguments
  n <- 100
  dat <- reactiveVal(testdata)
  selected <- list(
    x_var = reactiveVal("quant1"),
    y_var = reactiveVal("quant2"),
    group_var = reactiveVal(""),
    facet_var = reactiveVal(""),
    hexbin = reactiveVal(FALSE),
    abline = reactiveVal(FALSE)
  )
  testServer(mod_plot_server, args = list(dataset = dat, selections = selected), {
    # No plot to begin with
    expect_error(output$plot)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.
    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("test plot is created with x, y, and group variables", {
  # Reactive input arguments
  n <- 100
  dat <- reactiveVal(testdata)
  selected <- list(
    x_var = reactiveVal("quant1"),
    y_var = reactiveVal("quant2"),
    group_var = reactiveVal("cat1"),
    facet_var = reactiveVal(""),
    hexbin = reactiveVal(FALSE),
    abline = reactiveVal(FALSE)
  )
  testServer(mod_plot_server, args = list(dataset = dat, selections = selected), {
    # No plot to begin with
    expect_error(output$plot)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.
    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("test plot is created with x, y, and facet variables", {
  # Reactive input arguments
  n <- 100
  dat <- reactiveVal(testdata)
  selected <- list(
    x_var = reactiveVal("quant1"),
    y_var = reactiveVal("quant2"),
    group_var = reactiveVal(""),
    facet_var = reactiveVal("cat2"),
    hexbin = reactiveVal(FALSE),
    abline = reactiveVal(FALSE)
  )
  testServer(mod_plot_server, args = list(dataset = dat, selections = selected), {
    # No plot to begin with
    expect_error(output$plot)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.
    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("test plot is created with x, y, group, and facet variables", {
  # Reactive input arguments
  n <- 100
  dat <- reactiveVal(testdata)
  selected <- list(
    x_var = reactiveVal("quant1"),
    y_var = reactiveVal("quant2"),
    group_var = reactiveVal("cat1"),
    facet_var = reactiveVal("cat2"),
    hexbin = reactiveVal(TRUE),
    abline = reactiveVal(FALSE)
  )
  testServer(mod_plot_server, args = list(dataset = dat, selections = selected), {
    # No plot to begin with
    expect_error(output$plot)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.
    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("plot is created with hexbin option", {
  n <- 100
  dat <- reactiveVal(testdata)
  selected <- list(
    x_var = reactiveVal("quant1"),
    y_var = reactiveVal("quant2"),
    group_var = reactiveVal("cat1"),
    facet_var = reactiveVal("cat2"),
    hexbin = reactiveVal(TRUE),
    abline = reactiveVal(FALSE)
  )
  testServer(mod_plot_server, args = list(dataset = dat, selections = selected), {
    # No plot to begin with
    expect_error(output$plot)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.
    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})

test_that("plot is created with abline option", {
  n <- 100
  dat <- reactiveVal(testdata)
  selected <- list(
    x_var = reactiveVal("quant1"),
    y_var = reactiveVal("quant2"),
    group_var = reactiveVal("cat1"),
    facet_var = reactiveVal("cat2"),
    hexbin = reactiveVal(FALSE),
    abline = reactiveVal(TRUE)
  )
  testServer(mod_plot_server, args = list(dataset = dat, selections = selected), {
    # No plot to begin with
    expect_error(output$plot)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.
    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})
