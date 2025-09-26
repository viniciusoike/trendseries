test_that("Functions handle short time series appropriately", {
  # Very short series
  short_ts <- ts(c(100, 110, 105, 115, 120), frequency = 4)

  # Some methods should work
  hp_short <- extract_trends(short_ts, methods = "hp", .quiet = TRUE)
  expect_s3_class(hp_short, "ts")

  loess_short <- extract_trends(short_ts, methods = "loess", .quiet = TRUE)
  expect_s3_class(loess_short, "ts")

  # Hamilton filter should fail with short series
  expect_error(
    extract_trends(short_ts, methods = "hamilton", .quiet = TRUE),
    "Time series too short"
  )

  # BK filter should fail with very short series
  expect_error(
    extract_trends(short_ts, methods = "bk", .quiet = TRUE),
    "fixed lag length"
  )
})

test_that("Functions handle missing values appropriately", {
  # Create series with missing values
  # Convert data to ts object
  ts_with_na <- ts(gdp_construction$gdp_construction, start = c(1996, 1), frequency = 4)
  ts_with_na[5:7] <- NA

  # HP filter should work with some missing values (hpfilter handles them)
  hp_with_na <- extract_trends(ts_with_na, methods = "hp", .quiet = TRUE)
  expect_s3_class(hp_with_na, "ts")
})

test_that("Functions validate input parameters correctly", {
  # Convert data to ts object
  ts_data <- ts(gdp_construction$gdp_construction, start = c(1996, 1), frequency = 4)

  # Invalid window (negative)
  expect_error(
    extract_trends(ts_data, methods = "ma", window = -5),
    "positive"
  )

  # Invalid window (zero)
  expect_error(
    extract_trends(ts_data, methods = "ma", window = 0),
    "positive"
  )

  # Invalid band parameter (wrong length)
  expect_error(
    extract_trends(ts_data, methods = "bk", band = c(6)),
    "length 2"
  )

  # Invalid band parameter (negative values)
  expect_error(
    extract_trends(ts_data, methods = "bk", band = c(-2, 8)),
    "positive"
  )
})

test_that("Functions handle different frequencies correctly", {
  # Annual data (should work with most methods)
  annual_ts <- ts(c(100, 105, 110, 115, 120, 118, 125, 130), frequency = 1)

  # Should fail frequency validation
  expect_error(
    extract_trends(annual_ts, methods = "hp"),
    "Only monthly.*quarterly"
  )

  # Daily data (should also fail)
  daily_ts <- ts(rnorm(100), frequency = 365)
  expect_error(
    extract_trends(daily_ts, methods = "hp"),
    "Only monthly.*quarterly"
  )
})

test_that("Functions handle extreme parameter values", {
  # Convert data to ts object
  ts_data <- ts(gdp_construction$gdp_construction, start = c(1996, 1), frequency = 4)

  # Very high lambda for HP filter
  hp_extreme <- extract_trends(ts_data, methods = "hp", smoothing = 1000000, .quiet = TRUE)
  expect_s3_class(hp_extreme, "ts")

  # Very low lambda for HP filter
  hp_low <- extract_trends(ts_data, methods = "hp", smoothing = 0.1, .quiet = TRUE)
  expect_s3_class(hp_low, "ts")

  # Extreme loess span
  loess_extreme <- extract_trends(ts_data, methods = "loess", smoothing = 0.99, .quiet = TRUE)
  expect_s3_class(loess_extreme, "ts")
})

test_that("Unified parameter system works consistently", {
  # Convert data to ts object
  ts_data <- ts(vehicles$vehicles, start = c(2001, 1), frequency = 12)

  # Test that window parameter affects all MA methods consistently
  ma_window12 <- extract_trends(ts_data, methods = c("ma", "alma"), window = 12, .quiet = TRUE)
  ma_window6 <- extract_trends(ts_data, methods = c("ma", "alma"), window = 6, .quiet = TRUE)

  expect_type(ma_window12, "list")
  expect_type(ma_window6, "list")

  # Results should be different with different windows
  expect_false(identical(
    as.numeric(ma_window12$ma),
    as.numeric(ma_window6$ma)
  ))
  expect_false(identical(
    as.numeric(ma_window12$alma),
    as.numeric(ma_window6$alma)
  ))

  # Test that smoothing parameter affects smoothing methods
  smooth_03 <- extract_trends(ts_data, methods = c("loess", "ewma"), smoothing = 0.3, .quiet = TRUE)
  smooth_07 <- extract_trends(ts_data, methods = c("loess", "ewma"), smoothing = 0.7, .quiet = TRUE)

  expect_false(identical(
    as.numeric(smooth_03$loess),
    as.numeric(smooth_07$loess)
  ))
  expect_false(identical(
    as.numeric(smooth_03$ewma),
    as.numeric(smooth_07$ewma)
  ))
})