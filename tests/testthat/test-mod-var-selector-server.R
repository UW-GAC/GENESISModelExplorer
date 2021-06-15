test_that("return values", {
  n <- 100
  dat <- reactiveVal(tibble::tibble(
    a = rnorm(100),
    b = rnorm(100),
    c = sample(letters[1:3], 100, replace = T),
    d = sample(letters[1:3], 100, replace = T)
  ))
  testServer(mod_var_selector_server, args = list(dataset = dat), {
    expected_names <- c(
      "x_var",
      "y_var",
      "group_var",
      "facet_var",
      "hexbin",
      "abline",
      "loess",
      "lm",
      "yintercept",
      "violin",
      "nbins",
      "density",
      "hide_legend"
    )
  })
})
