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
