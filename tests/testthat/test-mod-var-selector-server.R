test_that("x variable names update", {
  n <- 100
  dat <- tibble::tibble(
    a = rnorm(n),
    b = rnorm(n),
    c = sample(letters[1:3], n, replace = T),
    d = sample(letters[1:3], n, replace = T)
  )
  argr <- list(r = list(data_loader = list(dataset = dat)))
  testServer(mod_var_selector_server, args = argr, {
    skip("figure out how to write test")
  })

})
