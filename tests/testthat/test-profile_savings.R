test_that("Profile savings executes without error", {
  a <- load_enhanced_puf_file(2022)
  expect_no_error(profile_savings(a, "A1490"))
})
