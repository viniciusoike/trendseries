test_that("Adding series work", {
  expect_true(is.data.frame(ts_to_df(AirPassengers)))
  expect_true(tibble::is_tibble(ts_to_df(AirPassengers)))
  expect_error(ts_to_df(1:10))
})
