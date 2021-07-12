test_that("server runs", {
  testServer(mod_overview_server, {
    session$setInputs(button = TRUE)
    # This test is just to check that the server runs properly, and the button
    # exists, so we can test something that will always be TRUE to avoid an
    # empty test message.
    expect_true(TRUE)
  })
})
