test_that("Adding series work", {
  expect(is.data.frame(ts_to_df(AirPassengers)))
  expect(tibble::is.tibble(ts_to_df(AirPassengers)))
  expect_error(ts_to_df(1:10))
})
