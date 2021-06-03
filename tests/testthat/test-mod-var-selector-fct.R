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
