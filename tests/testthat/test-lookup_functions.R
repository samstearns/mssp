test_that("Lookup functions work", {
  a <- load_puf_file(2023)
  expect_no_error(lookup_aco_num(a, "Lahey"))
  expect_no_error(lookup_aco_by_state(a, "Massachusetts"))
})
