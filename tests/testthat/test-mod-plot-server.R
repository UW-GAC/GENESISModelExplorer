# Test data for tests.
testdata <- tibble::tibble(
  a = rnorm(100),
  b = rnorm(100),
  c = sample(letters[1:3], 100, replace = T),
  d = sample(letters[1:3], 100, replace = T)
)


test_that("test plot is created with x variable only", {
  # Reactive input arguments
  n <- 100
  dat <- reactiveVal(testdata)
  selected <- list(
    x_var = reactiveVal("a"),
    y_var = reactiveVal(""),
    group_var = reactiveVal(""),
    facet_var = reactive("")
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
    x_var = reactiveVal("a"),
    y_var = reactiveVal(""),
    group_var = reactiveVal("c"),
    facet_var = reactive("")
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
    x_var = reactiveVal("a"),
    y_var = reactiveVal("b"),
    group_var = reactiveVal(""),
    facet_var = reactive("")
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
    x_var = reactiveVal("a"),
    y_var = reactiveVal("b"),
    group_var = reactiveVal("c"),
    facet_var = reactive("")
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
    x_var = reactiveVal("a"),
    y_var = reactiveVal("b"),
    group_var = reactiveVal(""),
    facet_var = reactive("d")
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
    x_var = reactiveVal("a"),
    y_var = reactiveVal("b"),
    group_var = reactiveVal("c"),
    facet_var = reactive("d")
  )
  testServer(mod_plot_server, args = list(dataset = dat, selections = selected), {
    # No plot to begin with
    expect_error(output$plot)
    session$setInputs(plot_button = TRUE)
    output$plot # Confirm that the plot can be accessed without an error.
    # Note that this does not test if the plot is correct. We'll need to add snapshot tests for that.
  })
})
