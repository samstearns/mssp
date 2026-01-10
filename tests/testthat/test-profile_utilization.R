test_that("Profile utilization metrics works.", {
  a <- load_puf_file(2022)
  expect_no_error(profile_utilization(a, "A1490"));
})
