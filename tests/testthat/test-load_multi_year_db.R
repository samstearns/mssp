test_that("Multi-year db generates correctly.", {
  expect_no_error(load_enhanced_puf_file(2013))
})
